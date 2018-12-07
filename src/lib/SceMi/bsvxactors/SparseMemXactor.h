// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <iostream>
#include <fstream>

#include "scemi.h"
#include "BSVType.h"
#include "BitT.h"
#include "OutportProxyT.h"
#include "InportQueueT.h"

class local_RAM_Request_Bit_31_Bit_64 : public BSVType {
 public:
  BitT<31> m_address ;
  BitT<64> m_data ;
  BitT<1> m_read ;

  local_RAM_Request_Bit_31_Bit_64 ()
    : m_address()
    , m_data()
    , m_read()
  {}

  local_RAM_Request_Bit_31_Bit_64 ( const SceMiMessageData *msg, unsigned int &off )
    : m_address(msg, off)
    , m_data(msg, off)
    , m_read(msg, off)
  {}

  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_address.setMessageData( msg, running );
    running = m_data.setMessageData( msg, running );
    running = m_read.setMessageData( msg, running );
    if (running != off + 96 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 96) << std::endl;
    }
    return running;
  }

  friend std::ostream & operator<< (std::ostream &os, const local_RAM_Request_Bit_31_Bit_64 &obj) {
    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "read " << obj.m_read ;os << " " ;
    os << "data " << obj.m_data ;os << " " ;
    os << "address " << obj.m_address ;os << "}" ;
    return os;
  }

  virtual std::ostream & getBitString (std::ostream & os) const {
    m_read.getBitString (os);
    m_data.getBitString (os);
    m_address.getBitString (os);
    return os;
  }

  virtual std::ostream & getBitStringRange (std::ostream &os, unsigned int from,
					    unsigned int to) const {
    os << "SparseMemXactord getBitStringRange() not implemented" ;
    return os;
  }

  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "SceMiSharedMemory::RAM_Request#(Bit#(31),Bit#(64))" ;
    return os;
  }

  virtual unsigned int getBitSize () const {
    return 96;
  }

  virtual const char * getClassName() const {
    return "local_RAM_Request_Bit_31_Bit_64" ;
  }

  virtual BSVKind getKind() const {
    return BSV_Struct ;
  }

  virtual unsigned int getMemberCount() const {
    return 3;
  };

  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
      case 0: return & m_read;
      case 1: return & m_data;
      case 2: return & m_address;
      default: std::cerr << "Index error in getMember for class local_RAM_Request_Bit_31_Bit_64" << std::endl ;
    };
    return 0;
  };

  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
      case 0: return "read";
      case 1: return "data";
      case 2: return "address";
      default: std::cerr << "Index error in getMemberName for class local_RAM_Request_Bit_31_Bit_64" << std::endl ;
    };
    return 0;
  };


};

extern "C" {
void hash_init();
void hash_write(const unsigned long long a,const unsigned long long d, const unsigned char m);
unsigned long long hash_read(const unsigned long long a);
}

// ProbesXactor class which handles virtual memories instantiated by
//  mkSharedMem

class SparseMemXactor {
  typedef unsigned long long MemAddr;
  typedef unsigned long long MemData;
  typedef unsigned char msk;

  typedef BitT<1>  Bool;
  typedef BitT<64> SceMiMemData;
  typedef BitT<31> SceMiMemAddr;

 protected:
  OutportProxyT<local_RAM_Request_Bit_31_Bit_64 >  m_request;
  InportQueueT<BitT<64> >                    m_response;

  bool m_debug;
 private:

  // Disallow default and copy constructors
  SparseMemXactor & operator= (const SparseMemXactor &);
  SparseMemXactor(const SparseMemXactor &);

  SparseMemXactor(bool debug, const std::string & hier, const std::string & instname, SceMi *scemi);
  ~SparseMemXactor();


 public:

  // Constructor given a param file which contains all the
  static SparseMemXactor *init(bool debug,
			       const std::string & hier, const std::string & instname,
			       SceMi *scemi);

  static SparseMemXactor *init(const std::string & hier, const std::string & instname,
			       SceMi *scemi);

  // Shutdown the transactor (delete the static SparseMemXactor)
  static void shutdown();

  // Insert back-door methods for reading/writing the memory:
  MemData readMem(const MemAddr &adr) {
    return hash_read(adr);
  }

  void writeMem(const MemAddr &adr, const MemData &dta) {
    hash_write(adr, dta, 0);
  }

  // This call back is executed when the scemi transactor has data.
  void receiveCallBack(const local_RAM_Request_Bit_31_Bit_64 & dta);

  static void  SreceiveCallBackT(void *x, const local_RAM_Request_Bit_31_Bit_64 &dta) {
    SparseMemXactor *tx = (SparseMemXactor *) x;
    tx->receiveCallBack (dta) ;
  };

};
