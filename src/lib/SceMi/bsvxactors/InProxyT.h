//-*- C++ -*-x
// Copyright (c) 2012 -- 2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>

#include "XactorCore.h"

#ifndef BS_SCEMI_DEBUG
#define BS_SCEMI_DEBUG false
#endif

// requirements on T
// constructor setMessageData(MsgPacket)
template <typename T>
class InProxyT : public XactorCore {
public:
  // Constructor
  InProxyT(const std::string &name, const std::string &path, const XactorAdapter::SInpipe & type)
    : XactorCore(name, path, type)
  {}
  InProxyT(const std::string &name, const std::string &path, const XactorAdapter::SInport & type)
    : XactorCore(name, path, type)
  {}
  InProxyT(const std::string &name, const std::string &path, const XactorAdapter::SPipes & type)
    : XactorCore(name, path, XactorAdapter::InPipe)
  {}
  InProxyT(const std::string &name, const std::string &path, const XactorAdapter::SPorts & type)
    : XactorCore(name, path, XactorAdapter::InPort)
  {}

  // Destructor
  ~InProxyT()
  {
  }

private:
  // Copy constructors are disabled
  InProxyT & operator= (const InProxyT &);
  InProxyT( const InProxyT &);

public:
  // Blocking Send
  void send(const T &t)
  {
    MsgPacket p((t.getBitSize()+7)/8);
    t.setMessageData(p);
    XactorCore::send(p);
  }

  // Blocking Send and then waits for ack from HW that message has been delivered
  void sendAcknowledge(const T &t)
  {
    send(t);
    XactorCore::waitSendAck();
  }

  // Non-Blocking Send
  bool sendNB(const T &t)
  {
    MsgPacket p((t.getBitSize()+7)/8);
    t.setMessageData(p);
    return XactorCore::sendNB(p);
  }

  // Blocking with timeout
  bool sendT(const T &t, struct timespec *expiration)
  {
    MsgPacket p((t.getBitSize()+7)/8);
    t.setMessageData(p);
    return XactorCore::sendT(p, expiration);
  }

  // Blocking with timeout
  bool sendT(const T &t, const time_t &delta_seconds, const long &delta_microseconds=0)
  {
    MsgPacket p((t.getBitSize()+7)/8);
    t.setMessageData(p);
    return XactorCore::sendT(p, delta_seconds, delta_microseconds);
  }

  // Blocking Send a Vector of elements
  bool send (const std::vector<T> &vt)
  {
    unsigned vsize = vt.size();
    if (vsize == 0) return true;

    unsigned offset = 0;
    T t;
    MsgPacket p(vt.size() * ((t.getBitSize()+7)/8) );
    for (unsigned i = 0 ; i < vsize ; ++ i) {
      offset = vt[i].setMessageData(p, offset);
      offset = next8 (offset);
    }
    XactorCore::send(p);
    return true;
  }

  // Send a Vector of elements
  bool sendNB (const std::vector<T> &vt)
  {
    unsigned vsize = vt.size();
    if (vsize == 0) return true;
    if (!canSend()) return false;

    unsigned offset = 0;
    T t;
    MsgPacket p(vt.size() * ((t.getBitSize()+7)/8) );
    for (unsigned i = 0 ; i < vsize ; ++ i) {
      offset = vt[i].setMessageData(p, offset);
      offset = next8 (offset);
    }
    XactorCore::send(p);
    return true;
  }

  // Blocking Send and then waits for ack from HW that message has been delivered
  void sendAcknowledge(const std::vector<T> &vt)
  {
    send(vt);
    XactorCore::waitSendAck();
  }

private:
  static inline unsigned int next8(unsigned int x) {
    return 8*((x+7)/8);
  }


};
