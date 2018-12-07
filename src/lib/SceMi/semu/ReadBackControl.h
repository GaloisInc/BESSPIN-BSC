// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// C++ transactor side for the control module mkRdBackControl
// which can be found in $BLUESPECDIR/lib/BSVSource/Readback/SceMiReadback.bsv

#include <stdlib.h>
#include <stdint.h>
#include <vector>
#include <map>
#include <string>

#include "BitT.h"
#include "BSVVectorT.h"
#include "BSVMaybeT.h"
#include "InportProxyT.h"
#include "OutportQueueT.h"
#include "OutpipeXactorT.h"
#include "StampedT.h"

#include "RdBackControl.hpp"

// Direct map to BSV source
namespace RdBack {

  typedef BSVMaybeT<BitT<32> > RdBackData;
  typedef BitT<32>             RdBackPayload;

  enum   SimCommand { Edges, Query, Stop, Resume, RdBackOn, RdBackOff, RdBackCmd, RdBackStore };

  class SimControlReq
  {
  private:
    SimCommand m_command;
    unsigned int m_steps;
  public:
    SimControlReq (SimCommand cmd, unsigned int count )
      : m_command (cmd)
      , m_steps(count)
    {
      unsigned int maxcount = 1 << 29;
      if (count >= maxcount) {
	std::cerr << "Error: SimCommand data too large: " << std::dec << count
		  << ". Must be less than " << maxcount << std::endl;
	m_steps = maxcount - 1 ;
      }
    }

    SimControlReq ()
      : m_command (Query)
      , m_steps(0)
    {
    }

    unsigned int setMessageData ( SceMiMessageData &msg, const unsigned int off=0) const
    {
      unsigned int myoff = off;
      msg.SetBitRange (myoff, 28,  m_steps);     myoff += 29;
      msg.SetBitRange (myoff,  2,  m_command);   myoff += 3;
      return myoff ;
    }

    unsigned int getBitSize ()
    {
      return 32;
    }

    unsigned int getBits() const
    {
      unsigned int data = m_steps & 0x1FFFFFFF;
      data = data | ((m_command & 0x7) << 29);
      return data;
    }

    static const char * toString (const SimCommand & c)
    {
      switch (c) {
	case Edges:       return "Edges: "      ; break ;
	case Query:       return "Query: "      ; break ;
	case Stop:        return "Stop:  "      ; break ;
	case Resume:      return "Resume:"      ; break ;
	case RdBackOn:    return "RdBackOn:"    ; break ;
	case RdBackOff:   return "RdBackOff:"   ; break ;
	case RdBackCmd:   return "RdBackCmd:"   ; break ;
	case RdBackStore: return "RdBackStore:" ; break ;
      }
      return "ERROR: SimCommand enum" ;
    }

    friend std::ostream & operator<< (std::ostream &os, const SimControlReq &req)
    {
      os << "{SimControl " << toString(req.m_command) << " cnt " << std::dec << req.m_steps << "}" ;
      return os ;
    }
  };

  class SimStatusResp
  {
  private:
    unsigned int m_remaining;
    bool         m_running;
    bool         m_freerunning;
    bool         m_readback;
  public:
    SimStatusResp ( const SceMiMessageData *msg, unsigned int & off )
      : m_remaining(0)
      , m_running(false)
      , m_freerunning(false)
      , m_readback(false)
    {
      m_remaining   = msg->GetBitRange( off, 28); off += 29;
      m_readback    = msg->GetBitRange(off,0); off+= 1;
      m_freerunning = msg->GetBitRange(off,0); off+= 1;
      m_running     = msg->GetBitRange (off, 0);   off += 1;
    }

    SimStatusResp ()
      : m_remaining(0)
      , m_running(false)
      , m_freerunning(false)
      , m_readback(false)
    {
    }

    inline bool isRunning() { return m_running; }
    inline bool isFreeRunning() { return m_freerunning ; }
    inline bool isRdBackOn() { return m_readback ; }
    inline unsigned int cyclesRemaining() { return m_remaining; }

    friend std::ostream & operator<< (std::ostream &os, const SimStatusResp &resp)
    {
      os << "{SimStatus" << std::boolalpha << std::dec << resp.m_running << resp.m_freerunning << " cnt " << resp.m_remaining << "}" ;
      return os ;
    }
  };

  class SimulationControl : public RdBackControl
  {
  protected:
    std::string                               m_name;
    int                                       m_data_count;
    std::map<uint32_t,std::vector<uint64_t> > m_data_map;
    uint32_t                                  m_lastaddress;
    uint32_t                                  m_timestamp_highword;
    uint32_t                                  m_data_prev;
    bool                                      m_data_polarity;
    int                                       m_timeout;
    bool                                      m_first;

  private:
    InportProxyT<SimControlReq>               m_inport ;
    OutportQueueT<StampedT< SimStatusResp > > m_outport;
    OutpipeXactorT<RdBackPayload>             m_rdbackpipe;

  public:
    static void logRdBack(void *context) {
      SimulationControl *xactor = static_cast<SimulationControl*>(context);
      xactor->processRdBackMsg();
    }

    SimulationControl (const std::string & hier, const std::string & inst, SceMi *scemi)
      : m_name("RdBackSimControl")
      , m_data_count(0)
      , m_inport(hier, inst + "_req_in", scemi)
      , m_outport(hier, inst +"_resp_out", scemi)
      , m_rdbackpipe(hier, inst +"_rdback_out")
    {
      m_timeout = 5;
      const char *timeout_char = getenv("BSC_SCEMI_SIMCONTROL_TIMEOUT");
      if (timeout_char != NULL) {
        int timeout = 0;
        int found = sscanf(timeout_char, "%d", &timeout );
        if (found == 1) {
          if (timeout > 0 ) {
            m_timeout = timeout;
            std::cerr << std::dec
                      << "Simlution control message timeout has been set to "
                      << timeout << " seconds" << std::endl;
          } else {
            std::cerr << std::dec
                      << "Simlution control message timeout must be greater than 0 "
                      << timeout << " seconds" << std::endl;
          }
        }
      }

      // Set callback for watching readback data
      m_rdbackpipe.setCanRecvNotifyCallBack(logRdBack,this);
      m_first = true;
    }

  private:
    // Disallow default and copy constructors
    SimulationControl & operator= (const SimulationControl &);
    SimulationControl( const SimulationControl &);

  public:
    unsigned int processRdBackMsg() {
      RdBackPayload data;
      bool success = true;
      while (success) {
	success = m_rdbackpipe.receiveNB(data);
	if (m_rdbackCallback != NULL && success) {
	  /* 	//	sleep(100); */
	  /* 	//	std::cout << "ITEM: " << data  << std::endl; */
	  m_rdbackCallback(m_rdbackCallbackParm, data);
	}
      }
      return 0; // unused
    }

    bool sendCommand (const SimControlReq &control)
    {
      bool sent = m_inport.sendMessageTimed (control,m_timeout);
      if (!sent) {
	std::cerr << "Warning: Clock control message " << control
		  << " has timed out after " << std::dec << m_timeout
		  << " seconds" << std::endl;
      }
      return sent;
    }
    bool sendCommand (const SimCommand cmd, const unsigned int edges =0 )
    {
      return sendCommand (SimControlReq(cmd, edges));
    }

    bool sendRdBackClear()
    {
      return sendCommand (SimControlReq(RdBackCmd, 0));
    }
    bool sendRdBackStore (unsigned int code)
    {
      return sendCommand(SimControlReq(RdBackStore, code));
    }
    bool sendRdBackFinish(unsigned int config)
    {
      return sendCommand(SimControlReq(RdBackCmd, (config & 0x7FFFFFF) | (1<<27)));
    }
    bool sendRdBackBreakCode(unsigned int code)
    {
      return sendCommand(SimControlReq(RdBackCmd, (code & 0xFFFF) | (1<<28)));
    }

    // Note that this function blocks if there is no message to receive.
    // This funciton should NOT be called from a scemi call back as
    // deadlock will occur.
    StampedT<SimStatusResp> getStatusStamped ()
    {
      return m_outport.getMessage() ;
    }
    SimStatusResp getStatus ()
    {
      StampedT<SimStatusResp> m = getStatusStamped();
      return m.getData();
    }

    // This version doesn't block, returning true if data is received
    bool getStatusNonBlocking (StampedT<SimStatusResp> &t)
    {
      return m_outport.getMessageNonBlocking(t) ;
    }
    bool getStatusNonBlocking (SimStatusResp &t)
    {
      StampedT<SimStatusResp> msg(t);
      bool stat = getStatusNonBlocking(msg);
      t = msg.getData();
      return stat;
    }

    // This version blocks, but with a timeout / timespec
    bool getStatusTimed (StampedT<SimStatusResp> &t, struct timespec *expiration)
    {
      return m_outport.getMessageTimed(t, expiration) ;
    }
    bool getStatusTimed (SimStatusResp &t, struct timespec *expiration)
    {
      StampedT<SimStatusResp> msg(t);
      bool stat = getStatusTimed(msg, expiration);
      t = msg.getData();
      return stat;
    }

    // This version blocks, but with a timeout / seconds + microseconds
    bool getStatusTimed (StampedT<SimStatusResp> &t, const time_t & delta_seconds,  const long & delta_microseconds=0)
    {
      return m_outport.getMessageTimed(t,  delta_seconds, delta_microseconds);
    }
    bool getStatusTimed (SimStatusResp &t, const time_t & delta_seconds,  const long & delta_microseconds=0)
    {
      StampedT<SimStatusResp> msg(t);
      bool stat = getStatusTimed(msg,  delta_seconds, delta_microseconds);
      t = msg.getData();
      return stat;
    }
  };
}
