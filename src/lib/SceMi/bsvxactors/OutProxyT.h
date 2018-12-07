//-*- C++ -*-x
// Copyright (c) 2012 -- 2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>

#include "XactorCore.h"

// requirements on T
// constructor T(const SceMiMessageData *)
// friend std::ostream operator<< (std::ostream &, const T &)

template <typename T>
class OutProxyT : public XactorCore {
public:
  // Constructor
 OutProxyT(const std::string &name, const std::string &path, const XactorAdapter::SOutpipe & type)
    : XactorCore(name, path, type)
  {}
 OutProxyT(const std::string &name, const std::string &path, const XactorAdapter::SOutport & type)
    : XactorCore(name, path, type)
  {}
 OutProxyT(const std::string &name, const std::string &path, const XactorAdapter::SPipes & type)
   : XactorCore(name, path, XactorAdapter::OutPipe)
  {}
 OutProxyT(const std::string &name, const std::string &path, const XactorAdapter::SPorts & type)
   : XactorCore(name, path, XactorAdapter::OutPort)
  {}

  // Destructor
  ~OutProxyT()
  {
  }

private:
  // Copy constructors are disabled
  OutProxyT & operator= (const OutProxyT &);
  OutProxyT( const OutProxyT &);

public:
  /// returns number of elements available
  virtual unsigned canReceive (unsigned req)  const {
    T t;
    unsigned bytesAvail = XactorCore::canReceive(req * ((t.getBitSize()+7u)/8u)) ;
    return (bytesAvail*8u) / (t.getBitSize()); // integer division
  }
  /// Blocking single element Receive
  void receive(T &t)
  {
    MsgPacket p;
    uint32_t offset = 0;
    unsigned psize = (t.getBitSize()+7u)/8u;
    XactorCore::receive(p, psize);
    t = T(&p, offset);
  }

  /// Non-Blocking single element Receive
  bool receiveNB(T &t)
  {
    MsgPacket p;
    uint32_t offset = 0;
    bool stat = false;
    unsigned psize = (t.getBitSize()+7u)/8u;
    stat = XactorCore::receiveNB(p, psize);
    if (stat) {
      t = T(&p, offset);
    }
    return stat;
  }

  /// Blocking single element receive with Timeout
  bool receiveT(T &t, struct timespec *expiration)
  {
    MsgPacket p;
    uint32_t offset = 0;
    bool stat = false;
    unsigned psize = (t.getBitSize()+7u)/8u;
    stat = XactorCore::receiveT(p, psize, expiration);
    if (stat) {
      t = T(&p, offset);
    }
    return stat;
  }

  /// Blocking signle element receive with Timeout
  bool receiveT(T &t, const time_t &delta_seconds, const long &delta_microseconds=0)
  {
    MsgPacket p;
    uint32_t offset = 0;
    bool stat = false;
    unsigned psize = (t.getBitSize()+7u)/8u;
    stat = XactorCore::receiveT(p, psize, delta_seconds, delta_microseconds);
    if (stat) {
      t = T(&p, offset);
    }
    return stat;
  }


  /// Vector Receive
  ///  Receive some elements from the dut.
  /// If minReturned > 0 this blocks until the minimum number of elements are available
  /// If minReturned == 0,  this is a non-blocking operation
  /// \param vt -- vector of elements returned,  received elements are added
  /// \param minReturned -- minimum number of elements to  block for
  /// \param maxReturned -- maximum number of element to return 0 == unlimited
  /// \return -- number of elements received.
  unsigned receive(std::vector<T> &vt, unsigned minReturned=0, unsigned maxReturned=0)
  {
    T t;

    // determine number of elements available.
    unsigned psize = (t.getBitSize()+7u)/8u;
    unsigned bytes = XactorCore::canReceive(minReturned * psize);
    unsigned elements = bytes/psize; // truncating integer division

    unsigned getElements;
    // If we have enough the max size
    if (elements < minReturned) {
      getElements = minReturned;
    }
    else if (maxReturned != 0) {
      getElements = std::min (maxReturned, elements);
    }
    else {
      getElements = elements;
    }

    if (getElements != 0) {
      MsgPacket p;
      uint32_t offset = 0;

      unsigned recvSize = getElements * psize;
      XactorCore::receive(p, recvSize);
      vt.reserve (vt.size() + getElements);

      for (unsigned i = 0; i < getElements; ++ i) {
        vt.push_back ( T(&p, offset) );
        offset = next8(offset);
      }

    }
    return getElements;
  }

  /// TODO  need timed version of Vector receive
  /// TODO peek methods
  /// TODO handle EOM

private:
  static inline unsigned int next8(unsigned int x) {
    return 8u*((x+7u)/8u);
  }


};
