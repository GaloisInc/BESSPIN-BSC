// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include "scemi.h"
#include <iostream>

// Wrapper class for a BSVType, adding the SceMiMessage timestamp with the data

template <class T>
class StampedT {
 protected:
  SceMiU64 m_time_stamp;
  T        m_data;

 public:

  StampedT()
    : m_time_stamp(0)
    , m_data()
  {}
  StampedT(const T &d, SceMiU64 ts=0)
    : m_time_stamp(ts)
    , m_data(d)
  {}
  StampedT (const SceMiMessageData *msg, unsigned int &off)
    : m_time_stamp(msg->CycleStamp())
    , m_data(msg,off)
  {}
  unsigned int setMessageData ( SceMiMessageData &msg, const unsigned int off=0) const {
    // Timestamp  cannot be added to an outgoing message
    return m_data.setMessageData (msg,off);
  }
  friend std::ostream & operator<< (std::ostream &os, const StampedT &ms) {
    os << std::dec << "{cycle: " << ms.m_time_stamp << " data: " << ms.m_data << "}" ;
    return os;
  }

  SceMiU64 getTimeStamp() const {
    return m_time_stamp;
  }

  const T & getData() const {
    return m_data;
  }

  T & getData() {
    return m_data;
  }

};
