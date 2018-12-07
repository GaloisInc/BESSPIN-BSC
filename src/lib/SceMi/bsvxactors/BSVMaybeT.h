// Copyright 2012, Bluespec Inc.  ALL RIGHTS RESERVED
#pragma once

#include "BSVType.h"
#include "BSVVoid.h"
#include <cstdio>
#include <sstream>
#include <iomanip>

template <class T>
  class BSVMaybeT : public BSVType {
public:

  // Enumeration type for the tag
  enum TAG {
    tag_Invalid=0,
    tag_Valid=1
  };

  SceMiU32 the_tag;
  BSVVoid  m_Invalid;
  T        m_Valid;

  BSVMaybeT ()
    : the_tag(0)
    , m_Invalid ()
    , m_Valid ()
  {}
  
  BSVMaybeT (const SceMiMessageDataInterface *msg, unsigned int &off)
    : the_tag(0)
    , m_Invalid ()
    , m_Valid ()  {
    T x;
    unsigned int tmpoff = off;
    the_tag = msg->GetBitRange (off + x.getBitSize(), 0);
    switch (the_tag) {
      case tag_Invalid: m_Invalid = BSVVoid(msg, tmpoff) ; break ;
      case tag_Valid: m_Valid     = T(msg, tmpoff) ; break ;
      default: std::cerr << "bad tag fields in MaybeT constructor" << std::endl;
    };
    off += 1 + x.getBitSize();
  }
  
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    T x;
    msg.SetBitRange(off + x.getBitSize(), 0, the_tag);
    switch (the_tag) {
      case tag_Invalid: m_Invalid.setMessageData (msg, off); break;
      case tag_Valid:   m_Valid.setMessageData (msg, off); break;
      default: std::cerr << "bad tag fields in BSVMaybeT<T> setMessageData" << std::endl;
    }
    return off + 1 + x.getBitSize();
  }
  
  friend std::ostream & operator<< (std::ostream &os, const BSVMaybeT &obj) {
    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    SceMiU32 tag = obj.the_tag;
    switch (tag) {
      case  tag_Invalid: os << "{Invalid " << obj.m_Invalid << "}" ; break;
      case  tag_Valid: os << "{Valid " << obj.m_Valid << "}" ; break;
      default: std::cerr << "bad tag fields in BSVMaybeT<T> operator<<" << std::endl;
    }
    return os;
  }
  
  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "Maybe#( " ;
    T x;
    x.getBSVType (os) ;
    os << ")"  ;
    return os;
  }

  virtual unsigned int getBitSize () const {
    T x;
    return 1 + x.getBitSize();
  }

  virtual std::ostream & getBitString (std::ostream & os) const {
    T x;
    for ( int i = 0 ; i >= 0 ; --i) {
       os << ( ((the_tag & (0x01 << i)) != 0) ? '1' : '0' ) ;
    }
    switch (the_tag) {
      case tag_Invalid:
        os << std::setw(x.getBitSize()) << std::setfill('0') << '0' ;
        m_Invalid.getBitString (os);
        break;
      case tag_Valid:
        m_Valid.getBitString (os);
        break;
      default:
        std::cerr << "bad tag fields in MaybeT::getBitString()" << std::endl;
        os << std::setw(x.getBitSize()) << std::setfill('0') << '0' ;
        break;
    }
  return os;
  }
  

  virtual const char * getClassName() const {
    return "MaybeT" ;
  }

  virtual BSVKind getKind() const {
    return BSV_TaggedUnion ;
  }

  virtual SceMiU32 getTaggedUnionTag () const {
    return the_tag;
  }
  virtual unsigned int getTaggedUnionTagWidth () const {
    return 1;
  }

  virtual unsigned int getMemberCount() const {
    return 2;
  };
  
  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
      case 0: return & m_Invalid;
      case 1: return & m_Valid;
      default: std::cerr << "Index error in getMember for class MaybeT" << std::endl ;
    };
    return 0;
  };
  
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
      case 0: return "Invalid";
      case 1: return "Valid";
      default: std::cerr << "Index error in getMemberName for class MaybeT" << std::endl ;
    };
    return 0;
  };
};

