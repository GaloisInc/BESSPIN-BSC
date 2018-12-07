// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include "BSVType.h"
#include <cstdio>
#include <sstream>

// Wrapper Class for Bluespec's Vector#(n,t) type and its conversion to and from SceMiMessages

template <unsigned int N, class T>
  class BSVVectorT : public BSVType {
 public:
  T v[N];
  BSVVectorT ()
    : v()
  {
    if ((N == 0)) {
      throw std::logic_error ("illegal size in class BSVVectorT<T, N>,  N must be > 0.");
    }
  }
  BSVVectorT (const SceMiMessageDataInterface *msg, unsigned int &off) {
    if ((N == 0)) {
      throw std::logic_error ("illegal size in class BSVVectorT<T, N>,  N must be > 0.");
    }
    for (unsigned int i = 0 ; i < N ; ++i) {
      v[i] = T(msg, off) ;
    }
  }
  unsigned int setMessageData ( SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int running = off ;
    for (unsigned int i = 0 ; i < N ; ++i) {
      running = v[i].setMessageData ( msg, running );
    }
    return running ;
  }
  friend std::ostream & operator<< (std::ostream &os, const BSVVectorT &d) {
    BSVType::PutTo * override = lookupPutToOverride ( d.getClassName() ) ;
    if ( override != 0 ) {
      return override(os, d);
    }
    os << "{" ;
    for (unsigned int i = 0 ; i < N; ++i) {
      if (i!=0) os << " " ;
      os << std::dec << "V[" << i << "] " << d.v[i] ;
    }
    os << "}" ;
    return os;
  }
  T & operator[] (size_t idx) {
    if (idx >= N) throw std::logic_error ("illegal index accessing BSVVectorT<T, N>,  0 <= i < N");
    return v[idx];
  }
  const T & operator[] (size_t idx) const {
    if (idx >= N) throw std::logic_error ("illegal index accessing BSVVectorT<T, N>,  0 <= i < N");
    return v[idx];
  }
  virtual std::ostream & getBitString ( std::ostream & os ) const {
    for (unsigned int i = 0 ; i < N ; ++i ) {
      // MSB to LSB
      v[N-(i+1)].getBitString(os);
    }
    return os;
  }
  virtual std::ostream & getBitStringRange (std::ostream &os, unsigned int from,
					    unsigned int to) const {
    os << "BSVVectorT getBitStringRange() not implemented" ;
    return os;
  }
  virtual std::ostream & getBSVType (std::ostream &os ) const {
    os << "Vector#( " << std::dec << N << ", " ;
    T x;
    x.getBSVType (os) ;
    os << ")"  ;
    return os;
  }
  virtual unsigned int getBitSize () const {
    T x;
    return N * (x.getBitSize()) ;
  }

  virtual const char * getClassName () const {
    return "VectorT";
  }
  virtual BSVKind getKind() const {
    return BSV_Vector ;
  }

  virtual unsigned int getMemberCount() const {
    return N;
  }
  virtual BSVType * getMember (unsigned int idx) {
    if (idx < N) return & v[idx];
    return 0;
  }
  virtual const char * getMemberName (unsigned int idx) const {
    static char buf[12];
    sprintf (buf, "V%d", idx);
    return buf;
  }
};
