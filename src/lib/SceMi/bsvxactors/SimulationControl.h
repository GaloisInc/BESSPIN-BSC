// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// C++ transactor side for the Bluespec simulation control module mkSimulationControl
// which can be found in $BLUESPECDIR/lib/BSVSource/SceMi/SceMiUtils.bsv

#include "InportProxyT.h"
#include "OutportQueueT.h"
#include "StampedT.h"
#include <stdlib.h>


// Direct map to BSV source
enum   SimCommand { Edges, Query, Stop, Resume };

class SimControlReq {
 private:
  SimCommand m_command;
  unsigned int m_steps;
 public:
  SimControlReq (SimCommand cmd, unsigned int count )
    : m_command (cmd)
    , m_steps(count)
  {
    unsigned int maxcount = 1 << 30;
    if (count >= maxcount) {
      std::cerr << "Error: SimCommand edge count too large: " << std::dec << count
           << ". Must be less than " << maxcount << std::endl;
      m_steps = maxcount - 1 ;
    }
  }
  SimControlReq ()
    : m_command (Query)
    , m_steps(0)
    {}

  unsigned int setMessageData ( SceMiMessageData &msg, const unsigned int off=0) const {
    unsigned int myoff = off;
    msg.SetBitRange (myoff, 29,  m_steps);     myoff += 30;
    msg.SetBitRange (myoff,  1,  m_command);   myoff += 2;
    return myoff ;
  };
  unsigned int getBitSize () {
    return 32;
  }

  static const char * toString (const SimCommand & c) {
    switch (c) {
    case Edges:  return "Edges: " ; break ;
    case Query:  return "Query: " ; break ;
    case Stop:   return "Stop:  " ; break ;
    case Resume: return "Resume:" ; break ;
    }
    return "ERROR: SimCommand enum" ;
  }

  friend std::ostream & operator<< (std::ostream &os, const SimControlReq &req) {
    os << "{SimControl " << toString(req.m_command) << " cnt " << std::dec << req.m_steps << "}" ;
    return os ;
  };
};

class SimStatusResp {
 private:
  unsigned int m_remaining;
  bool         m_running;
  bool         m_freerunning;
 public:
  SimStatusResp ( const SceMiMessageData *msg, unsigned int & off )
    : m_remaining(0), m_running(false), m_freerunning(false)
  {
    m_remaining = msg->GetBitRange( off, 29); off += 30;
    m_freerunning = msg->GetBitRange(off,0); off+= 1;
    m_running = msg->GetBitRange (off, 0);   off += 1;
  }
  // So we can create a reference
  SimStatusResp () : m_remaining(0), m_running(false), m_freerunning(false) {}

  bool isRunning() { return m_running; }
  bool isFreeRunning() { return m_freerunning ; }
  unsigned int cyclesRemaining() { return m_remaining; }

  unsigned int getBitSize () {
    return 32;
  }

  friend std::ostream & operator<< (std::ostream &os, const SimStatusResp &resp) {
    os << "{SimStatus " << std::boolalpha << std::dec << "running:" << resp.m_running << " free-running:" << resp.m_freerunning << " cnt:" << resp.m_remaining << "}" ;
    return os ;
  };
};


class SimulationControl {
 private:
  InportProxyT<SimControlReq>               m_inport ;
  OutportQueueT<StampedT< SimStatusResp > > m_outport;
  int                                       m_timeout ;

 public:
  SimulationControl (const std::string & hier, const std::string & inst, SceMi *scemi)
    : m_inport(hier, inst + "_req_in", scemi)
    , m_outport(hier, inst +"_resp_out", scemi)
    , m_timeout (5)             // 5 second timeout
    {
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
    }

 private:
  // Disallow default and copy constructors
  SimulationControl & operator= (const SimulationControl &);
  SimulationControl( const SimulationControl &);

 public:
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

  void drainStatusMessages()
  {
    StampedT<SimStatusResp> msg;
    bool stat = true;
    while (stat)
      stat = m_outport.getMessageNonBlocking(msg) ;
  }

};
