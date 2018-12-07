// Copyright (c) 2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>

#include "XactorCore.h"

#ifndef BS_SCEMI_DEBUG
#define BS_SCEMI_DEBUG false
#endif

// requirements on T
// constructor setMessageData(MsgPacket)
template <typename T>
class InpipeXactorT : public XactorCore {
public:
  // Constructor
  InpipeXactorT(const std::string &name, const std::string &path)
    : XactorCore(name, path, XactorAdapter::InputOnly)
  {
  }

  // Destructor
  ~InpipeXactorT()
  {
  }

private:
  // Copy constructors are disabled
  InpipeXactorT & operator= (const InpipeXactorT &);
  InpipeXactorT( const InpipeXactorT &);

public:
  // Blocking Send
  void send(const T &t)
  {
    MsgPacket p((t.getBitSize()+7)/8);
    t.setMessageData(p);
    XactorCore::send(p);
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
};
