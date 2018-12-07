// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include "BitT.h" 

template <class T>
class BSVCaptureT : public BSVType {
protected:
  BitT<32> m_runlength;
  T        m_capture;
public:
  // Contructors
  BSVCaptureT () {}

  BSVCaptureT (const SceMiMessageData *msg, unsigned int &off) 
    : m_runlength (msg, off)
    , m_capture (msg, off)
  {}

  unsigned int setMessageData (SceMiMessageData &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_runlength.setMessageData( msg, running );
    running = m_capture.setMessageData( msg, running );
    return running;
  }
  friend std::ostream & operator<< (std::ostream &os, const BSVCaptureT &obj) {
    os << "{runlength " ;
    os << ( (long long unsigned int) (obj.m_runlength.get()) );
    os << " data " << obj.m_capture << "}";
    return os;
  }

  SceMiU64   getRunLength() const { return m_runlength.get(); }
  const T &  getCapture()       const { return m_capture; }

  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "Captures#(" ;
    m_capture.getBSVType(os);
    os << ")";
    return os;
  }

  virtual unsigned int getBitSize () const {
    return m_runlength.getBitSize() + m_capture.getBitSize();
  }
  virtual std::ostream & getBitString (std::ostream &os) const {
    os << m_runlength.getBitString(os) ;
    os << m_capture.getBitString(os) ;
    return os;
  }
  virtual std::ostream & getBitStringRange (std::ostream &os, unsigned int from,
					    unsigned int to) const {
    os << "BSVCaptureT getBitStringRange() not implemented" ;
    return os;
  }
  virtual const char * getClassName() const {
    return "BSVCaptureT";
  }

  virtual BSVKind getKind() const {
    return BSVType::BSV_Struct;
  }

  virtual unsigned int getMemberCount () const {
    return 2;
  }
  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
    case 0: return & m_runlength ;
    case 1: return & m_capture ;
    }
    return 0;
  }
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
    case 0: return "cycle";
    case 1: return "capture";
    }
    return 0;
  };

};
