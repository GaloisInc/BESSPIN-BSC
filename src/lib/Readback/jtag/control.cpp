#include "control.h"

/*
static FILE *sg_fp = NULL;

static void log (unsigned int x) {
  if (sg_fp) {
    fprintf(sg_fp, "%x\n", x);
    fclose(sg_fp);
    sg_fp = fopen("debug.txt","a");
  }
}
*/

enum RdBackControlReqType { Reserved0, Reserved1, Reserved2, Reserved3,
			    Reserved4, Reserved5, RdBackCmd, RdBackStore };

class RdBackControlReq
{
private:
  RdBackControlReqType  m_type;
  unsigned int          m_data;
public:
  RdBackControlReq (RdBackControlReqType type, unsigned int data)
    : m_type(type)
    , m_data(data)
  {
    unsigned int maxdata = 1 << 29;
    if (data >= maxdata) {
      std::cerr << "Error: RdBackControlReq data too large: " << std::dec << data
		<< ". Must be less than " << maxdata << std::endl;
      m_data = maxdata - 1 ;
    }
  }

  RdBackControlReq ()
    : m_type(Reserved0)
    , m_data(0)
  {
  }

  unsigned int getBits() const
  {
    unsigned int data = m_data & 0x1FFFFFFF;
    data = data | ((m_type & 0x7) << 29);
    return data;
  }

  static const char * toString (const RdBackControlReqType & c)
  {
    switch (c) {
    case Reserved0:   return "Reserved0: "  ; break ;
    case Reserved1:   return "Reserved1: "  ; break ;
    case Reserved2:   return "Reserved2: "  ; break ;
    case Reserved3:   return "Reserved3: "  ; break ;
    case Reserved4:   return "Reserved4:"   ; break ;
    case Reserved5:   return "Reserved5:"   ; break ;
    case RdBackCmd:   return "RdBackCmd:"   ; break ;
    case RdBackStore: return "RdBackStore:" ; break ;
    }
    return "ERROR: RdBackControlReqType enum" ;
  }

  friend std::ostream & operator<< (std::ostream &os, const RdBackControlReq &req)
  {
    os << "{RdBackControlReq " << toString(req.m_type) << " cnt " << std::dec << req.m_data << "}" ;
    return os ;
  }
};


  JtagRdBackControl::JtagRdBackControl (const std::string &cablename, const unsigned int &device)
    : LuminaControl()
  {
    // setup cable and device
    m_cable = interface_registry<cable>::shared_instance().create_object_of(cablename.c_str());
    if (m_cable->open() == -1) {
      delete m_cable;
      throw string("No supported JTAG cable found!");
    }
    
    m_jtag = new jtag(m_cable);

    int devices = m_jtag->get_chain();
    if (devices == 0) {
      delete m_jtag;
      delete m_cable;
      throw string ("No JTAG chain found!");
    }
    
    m_jtag->detect_chain();

    m_jtag->tap_test_logic_reset();
    if (m_jtag->select_device(device) == -1) {
      throw string("Selected JTAG device out of range!");
    }

    m_jtag->send_instruction("USER1");

    //sg_fp = fopen("debug.txt","w");
    
    // verify we have a properly connected hardware portion
    unsigned long long sig = m_jtag->read_user1_register(RdBackControlReq(RdBackCmd,7).getBits());
    if (sig != 0x426C75654A544147LL) {
      stringstream os;
      os << "JTAG chain located, but access to hardware is missing  expected 0x426C75654A544147, received 0x" << std::hex << sig << endl;
      delete m_jtag;
      delete m_cable;
      throw os.str();
    }
    m_logfile.Debug("Located JTAG Readback Hardware!");
    
    unsigned long long dna = m_jtag->read_user1_register(RdBackControlReq(RdBackCmd,11).getBits());
    m_logfile.Debug("DNA = %016llx", dna);
  }
  
  JtagRdBackControl::~JtagRdBackControl()
  {
    m_jtag->write_user1_register(RdBackControlReq(RdBackCmd,0).getBits()); // clear to  FSM
    m_jtag->write_user1_register(RdBackControlReq(RdBackCmd,1<<27).getBits()); // finish to FSM
    
    delete m_jtag;
    delete m_cable;
  }

  bool JtagRdBackControl::readState()
  {
    unsigned long long status, readback_data;
    unsigned int result = 0;
    uint32_t data;
    bool command_ovfl, ready;

    bool debug = false;
    unsigned int nonrdy_timeout = (debug ? 20 : 2000);

    cout << "readState" << endl;

    if (debug) {
      unsigned long long t = m_jtag->read_user1_register(RdBackControlReq(RdBackCmd,4).getBits());
      printf("Cycle: %llx\n", t);
      unsigned long long s = m_jtag->read_user1_register(RdBackControlReq(RdBackCmd,3).getBits());
      printf("Status: %llx\n", s);
    }

    // Send a request for a Readback response packet
    m_jtag->write_user1_register(RdBackControlReq(RdBackCmd,8).getBits());

    unsigned int nonrdy_iterations = 0;
    unsigned int recvd_packets = 0;
    bool done = false;
    do {
      status = m_jtag->read_user1_register(RdBackControlReq(RdBackCmd,3).getBits());
      command_ovfl = status & (1<<2);
      readback_data = m_jtag->read_user1_register(RdBackControlReq(RdBackCmd,2).getBits());
      ready = (readback_data & 0x0000000100000000LL);
      data = (readback_data & 0x00000000FFFFFFFFLL);

      if (debug)
	printf("READ: %llx\n", readback_data);

      if (ready) {

	// Error detection: reset the count of non-ready reads
	nonrdy_iterations = 0;

	if (debug)
	  printf("DATA  = %08X\n", data);
	m_logfile.Debug("DATA  = %08X\n", data);

	// dequeue to prepare the next data beat
	m_jtag->read_user1_register(RdBackControlReq(RdBackCmd,10).getBits());

	// give the word to the Readback core
	// (returns whether this is the end of the data)
	//
	result = m_rdbackCallback(m_rdbackCallbackParm, data);

	// if we are at the last sample of this session
	if (result == 1) {
	  if (debug)
	    printf("LAST SAMPLE\n");
	  // Error detection: record that a response was received
	  recvd_packets ++;
	}
      } else {
	if (debug)
	  printf("NOT READY\n");
	// Error detection: increment the count of non-ready reads
	nonrdy_iterations ++;
      }

      if (command_ovfl) {
	throw string("The command stream input overflowed and will not be consistent.  Please reset the HARDWARE and try again!");
      }

      // If we get nothing after a specified number of reads, stop
      if (nonrdy_iterations >= nonrdy_timeout)
	done = true;
      // If we've received something, give it some more reads, but then stop
      if (recvd_packets && (nonrdy_iterations >= nonrdy_timeout))
	done = true;
      // If we have received 4, it's likely repeating forever, so stop
      if (recvd_packets == 4)
	done = true;
    } while (!done);

    // Report errors
    if (recvd_packets == 0) {
      cout << "ERROR: Timeout waiting for response" << endl;
      return false;
    } else if (recvd_packets > 1) {
      cout << "ERROR: Received multiple responses" << endl;
      return false;
    }
    
    return true;
  }


  bool JtagRdBackControl::sendRdBackClear ()
  {
    unsigned int code = 0;
    waitForReady();
    RdBackControlReq req(RdBackCmd, code);
    //log(req.getBits());
    m_jtag->write_user1_register(req.getBits());
    return true;
  }
  
  bool JtagRdBackControl::sendRdBackStore (unsigned int code)
  {
    waitForReady();
    RdBackControlReq req(RdBackStore, code);
    //log(req.getBits());
    m_jtag->write_user1_register(req.getBits());
    return true;
  }

  bool JtagRdBackControl::sendRdBackFinish (unsigned int config)
  {
    unsigned int code = (config & 0x7FFFFFF) | (1<<27);
    waitForReady();
    RdBackControlReq req(RdBackCmd, code);
    //log(req.getBits());
    m_jtag->write_user1_register(req.getBits());
    return true;
  }
  
  bool JtagRdBackControl::sendRdBackBreakCode (unsigned int config)
  {
    unsigned int code = (config & 0xFFFF) | (1<<28);
    waitForReady();
    RdBackControlReq req(RdBackCmd, code);
    //log(req.getBits());
    m_jtag->write_user1_register(req.getBits());
    return true;
  }

  void JtagRdBackControl::waitForReady()
  {
    int iterations = 2000;
    unsigned long long status = 0;
    do {
      ::usleep(100);
      status = m_jtag->read_user1_register(RdBackControlReq(RdBackCmd,3).getBits());
    } while((iterations-- > 0) && (status & (1<<1)));
    
    if (iterations == 0) {
      throw string ("Timed out waiting for CommandQ to empty!");
    }
  }
