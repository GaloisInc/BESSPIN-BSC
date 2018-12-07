#include <stdint.h>
#include <iostream>
#include <sstream>

#include "SceMiLuminaControl.hpp"

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


SceMiLuminaControl::SceMiLuminaControl(const char *instname, const char *paramfile)
  : LuminaControl()
{
  // Initialize SceMi
  int sceMiVersion = SceMi::Version( SCEMI_VERSION_STRING );
  SceMiParameters params( paramfile );
  m_scemi = SceMi::Init( sceMiVersion, & params );
  if (! m_scemi) {
    throw std::runtime_error ("Could not initialize SceMi");
    exit(1);
  }

  // Instantiate the SceMi proxies
  std::string inport_name(instname);
  inport_name.append("_req_in");
  m_req_in = new InportProxyT< BitT <32> > ("", inport_name.c_str(), m_scemi);
  std::string outpipe_name(instname);
  outpipe_name.append("_rdback_out");
  m_rdback_out = new OutpipeXactorT< BitT <32> > ("", outpipe_name.c_str());
      
  // Start SceMi service loop
  m_serviceThread = new SceMiServiceThread (m_scemi);
}
  
SceMiLuminaControl::~SceMiLuminaControl()
{
  if (m_serviceThread) {
    m_serviceThread->stop();
    m_serviceThread->join();
    delete m_serviceThread;
    m_serviceThread = 0;
  }

  if (m_req_in)
    delete m_req_in;
  if (m_rdback_out)
    delete m_rdback_out;

  // Shutdown scemi
  if (m_scemi)
    SceMi::Shutdown(m_scemi);
}

bool SceMiLuminaControl::readState()
{
  if (! m_scemi) return false;

  bool debug = false;

  unsigned int end_of_data;

  std::cout << "readState" << std::endl;

  // Send a request for a Readback response packet
  m_req_in->sendMessage( RdBackControlReq(RdBackCmd,8).getBits() );

  if (debug) printf("Request sent\n");
  do {
    BitT<32> data;
    m_rdback_out->receive(data);
    if (debug) printf("Data received: %x\n", (unsigned int)data);
    // give the word to the Readback core
    end_of_data = m_rdbackCallback(m_rdbackCallbackParm, (unsigned int)data);
    if (debug) if (end_of_data) printf("LAST\n");
  } while (! end_of_data);
    
  return true;
}

bool SceMiLuminaControl::sendRdBackClear ()
{
  if (! m_scemi) return false;

  unsigned int code = 0;
  RdBackControlReq req(RdBackCmd, code);
  m_req_in->sendMessage( req.getBits() );
  return true;
}
  
bool SceMiLuminaControl::sendRdBackStore (unsigned int code)
{
  if (! m_scemi) return false;

  RdBackControlReq req(RdBackStore, code);
  m_req_in->sendMessage( req.getBits() );
  return true;
}

bool SceMiLuminaControl::sendRdBackFinish (unsigned int config)
{
  if (! m_scemi) return false;

  unsigned int code = (config & 0x7FFFFFF) | (1<<27);
  RdBackControlReq req(RdBackCmd, code);
  m_req_in->sendMessage( req.getBits() );
  return true;
}
  
bool SceMiLuminaControl::sendRdBackBreakCode (unsigned int config)
{
  if (! m_scemi) return false;

  unsigned int code = (config & 0xFFFF) | (1<<28);
  RdBackControlReq req(RdBackCmd, code);
  m_req_in->sendMessage( req.getBits() );
  return true;
}

