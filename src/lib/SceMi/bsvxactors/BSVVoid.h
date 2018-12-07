// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include "BSVType.h"

// Wrapper class for Bluespec's "void" type and their conversion to and from SceMiMessages
class BSVVoid : public BSVType {
public:
  BSVVoid() {} ;
  BSVVoid(const SceMiMessageDataInterface *msg, unsigned int &off) {
  }
  BSVVoid(std::string &msg) {
  }
  unsigned int setMessageData ( SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    return off;
  }
  friend std::ostream & operator<< (std::ostream &os, const BSVVoid &d) {
    BSVType::PutTo * override = lookupPutToOverride ( d.getClassName() ) ;
    if ( override != 0 ) {
      return override(os, d);
    }
    return os;
  }
  virtual std::ostream & getBitStringRange (std::ostream &os, unsigned int from,
					    unsigned int to) const {
    return os;
  }
  virtual std::ostream & getBitString (std::ostream &os ) const {
    return os;
  }
  virtual std::ostream & getBSVType (std::ostream &os) const {
    os << "void";
    return os;
  }
  virtual unsigned int getBitSize () const {
    return 0;
  }
  virtual const char * getClassName() const {
    return "BSVoid";
  }
  virtual BSVKind getKind() const {
    return BSV_Primitive ;
  }
  virtual unsigned int getMemberCount () const                { return 0; }
  virtual BSVType * getMember (unsigned int idx)              { return 0; };
  virtual const char * getMemberName (unsigned int idx) const { return 0; };
};
