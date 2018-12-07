// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include "BitT.h"

template <class T>
class BSVProbeT : public BSVType {
protected:
  BitT<64> m_cycle;
  T        m_probe;
public:
  // Contructors
  BSVProbeT () {}

  BSVProbeT (const SceMiMessageData *msg, unsigned int &off)
    : m_cycle (msg, off)
    , m_probe (msg, off)
  {}

  unsigned int setMessageData (SceMiMessageData &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_cycle.setMessageData( msg, running );
    running = m_probe.setMessageData( msg, running );
    return running;
  }
  friend std::ostream & operator<< (std::ostream &os, const BSVProbeT &obj) {
    os << "{cycle " ;
    os << ( (long long unsigned int) (obj.m_cycle.get64()) );
    os << " data " << obj.m_probe << "}";
    return os;
  }

  SceMiU64   getCycleStamp () const { return m_cycle.get64(); }
  const T &  getProbe()       const { return m_probe; }

  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "Probes#(" ;
    m_probe.getBSVType(os);
    os << ")";
    return os;
  }

  virtual unsigned int getBitSize () const {
    return m_cycle.getBitSize() + m_probe.getBitSize();
  }
  virtual std::ostream & getBitString (std::ostream &os) const {
    os << m_cycle.getBitString(os) ;
    os << m_probe.getBitString(os) ;
    return os;
  }
  virtual std::ostream & getBitStringRange (std::ostream &os, unsigned int from,
					    unsigned int to) const {
    os << "BSVProbeT getBitStringRange() not implemented" ;
    return os;
  }
  virtual const char * getClassName() const {
    return "BSVProbeT";
  }

  virtual BSVKind getKind() const {
    return BSVType::BSV_Struct;
  }
  virtual unsigned int getMemberCount () const {
    return 2;
  }
  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
    case 0: return & m_cycle ;
    case 1: return & m_probe ;
    }
    return 0;
  }
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
    case 0: return "cycle";
    case 1: return "probe";
    }
    return 0;
  };

};
