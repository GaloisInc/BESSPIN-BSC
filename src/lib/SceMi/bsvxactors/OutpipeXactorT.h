//-*- C++ -*-x
// Copyright (c) 2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>

#include "XactorCore.h"
#include "MsgPacket.h"

// requirements on T
// constructor T(const SceMiMessageData *)
// friend std::ostream operator<< (std::ostream &, const T &)

template <typename T>
class OutpipeXactorT : public XactorCore {
public:
  // Constructor
  OutpipeXactorT(const std::string &name, const std::string &path)
    : XactorCore(name, path, XactorAdapter::OutputOnly)
  {
  }

  // Destructor
  ~OutpipeXactorT()
  {
  }

private:
  // Copy constructors are disabled
  OutpipeXactorT & operator= (const OutpipeXactorT &);
  OutpipeXactorT( const OutpipeXactorT &);

public:
  /// returns number of elements available
  virtual unsigned canReceive (unsigned req)  const {
    T t;
    unsigned bytesAvail = XactorCore::canReceive(req * ((t.getBitSize()+7u)/8u)) ;
    return (bytesAvail*8u) / (t.getBitSize()); // integer division
  }
  // Blocking Receive
  void receive(T &t)
  {
    MsgPacket p;
    uint32_t offset = 0;
    unsigned psize = (t.getBitSize()+7)/8;
    XactorCore::receive(p, psize);
    t = T(static_cast<const SceMiMessageDataInterface*>(&p), offset);
  }

  // Non-Blocking Receive
  bool receiveNB(T &t)
  {
    MsgPacket p;
    uint32_t offset = 0;
    bool stat = false;
    unsigned psize = (t.getBitSize()+7)/8;
    stat = XactorCore::receiveNB(p, psize);
    if (stat) {
      t = T(static_cast<const SceMiMessageDataInterface*>(&p), offset);
    }
    return stat;
  }

  // Blocking with Timeout
  bool receiveT(T &t, struct timespec *expiration)
  {
    MsgPacket p;
    uint32_t offset = 0;
    bool stat = false;
    unsigned psize = (t.getBitSize()+7)/8;
    stat = XactorCore::receiveT(p, psize, expiration);
    if (stat) {
      t = T(static_cast<const SceMiMessageDataInterface*>(&p), offset);
    }
    return stat;
  }

  // Blocking with Timeout
  bool receiveT(T &t, const time_t &delta_seconds, const long &delta_microseconds=0)
  {
    MsgPacket p;
    uint32_t offset = 0;
    bool stat = false;
    unsigned psize = (t.getBitSize()+7u)/8u;
    stat = XactorCore::receiveT(p, psize, delta_seconds, delta_microseconds);
    if (stat) {
      t = T(static_cast<const SceMiMessageDataInterface*>(&p), offset);
    }
    return stat;
  }
};
