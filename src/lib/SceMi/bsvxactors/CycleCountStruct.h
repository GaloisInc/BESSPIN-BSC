// Automaticlly generated by: ::SceMiMsg
// DO NOT EDIT
// C++ Class with SceMi Message passing for Bluespec type:  PowerPrimitives::CycleCountStruct
// Generated on: Tue Sep 13 14:53:37 EDT 2011
// Bluespec version: 2011.08.beta6 2011-08-31 25169

#pragma once

#include "bsv_scemi.h"
#include "ClockIx.h"
#include "PowerIx.h"

class CycleCountStruct : public BSVType {
 public:
  BitT<27> m_dta ;
  PowerIx m_pd ;
  ClockIx m_clock ;
  BitT<1> m_valid ;

  CycleCountStruct ()
    : m_dta()
    , m_pd()
    , m_clock()
    , m_valid()
  {}

  CycleCountStruct ( const SceMiMessageDataInterface *msg, unsigned int &off )
    : m_dta(msg, off)
    , m_pd(msg, off)
    , m_clock(msg, off)
    , m_valid(msg, off)
  {}

  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_dta.setMessageData( msg, running );
    running = m_pd.setMessageData( msg, running );
    running = m_clock.setMessageData( msg, running );
    running = m_valid.setMessageData( msg, running );
    if (running != off + 32 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 32) << std::endl;
    }
    return running;
  }

  friend std::ostream & operator<< (std::ostream &os, const CycleCountStruct &obj) {
    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "valid " << obj.m_valid ;os << " " ;
    os << "clock " << obj.m_clock ;os << " " ;
    os << "pd " << obj.m_pd ;os << " " ;
    os << "dta " << obj.m_dta ;os << "}" ;
    return os;
  }

  virtual std::ostream & getBitString (std::ostream & os) const {
    m_valid.getBitString (os);
    m_clock.getBitString (os);
    m_pd.getBitString (os);
    m_dta.getBitString (os);
  return os;
  }
  

  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "PowerPrimitives::CycleCountStruct" ;
    return os;
  }

  virtual unsigned int getBitSize () const {
    return 32;
  }

  virtual const char * getClassName() const {
    return "CycleCountStruct" ;
  }

  virtual BSVKind getKind() const {
    return BSV_Struct ;
  }

  virtual unsigned int getMemberCount() const {
    return 4;
  };
  
  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
      case 0: return & m_valid;
      case 1: return & m_clock;
      case 2: return & m_pd;
      case 3: return & m_dta;
      default: std::cerr << "Index error in getMember for class CycleCountStruct" << std::endl ;
    };
    return 0;
  };
  
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
      case 0: return "valid";
      case 1: return "clock";
      case 2: return "pd";
      case 3: return "dta";
      default: std::cerr << "Index error in getMemberName for class CycleCountStruct" << std::endl ;
    };
    return 0;
  };
  
  
};

