// Copyright (c) 2013, Bluespec, Inc.  ALL RIGHT RESERVED
#pragma once

// C++ transactor side for controlling the Si570 part, a clock generator, present on
// some Xilinx Series-7 and later boards.

#include <string>
#include <iostream>

#include "BitT.h"
#include "bsv_scemi.h"
#include "InportQueueT.h"
#include "OutportQueueT.h"

/// C++ class representing the hardware structure Si570Controller::Si570Resp
/// This class has been automatically generated.
class Si570Resp : public BSVType {
 public:
  BitT<7> m_n1 ;
  BitT<3> m_hsdiv ;
  BitT<38> m_rfreq ;

  /// A default constructor
  Si570Resp ()
    :  m_n1()
    , m_hsdiv()
    , m_rfreq()
  {}

  /// Constructor for object from a SceMiMessageData object
  /// @param msg -- the scemi message object
  /// @param off -- the starting bit offset, updated to next bit position
  Si570Resp ( const SceMiMessageDataInterface *msg, unsigned int &off )
    : m_n1(msg, off)
    , m_hsdiv(msg, off)
    , m_rfreq(msg, off)
  {}

  /// Converts this object into its bit representation for sending as a SceMi message
  /// @param msg -- the message object written into
  /// @param off -- bit position off set in message
  /// @return next free bit position for writing
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_n1.setMessageData( msg, running );
    running = m_hsdiv.setMessageData( msg, running );
    running = m_rfreq.setMessageData( msg, running );
    if (running != off + 48 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 48) << std::endl;
    }
    return running;
  }

  /// overload the put-to operator for Si570Resp
  friend std::ostream & operator<< (std::ostream &os, const Si570Resp &obj) {
    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "rfreq " << obj.m_rfreq ;os << " " ;
    os << "hsdiv " << obj.m_hsdiv ;os << " " ;
    os << "n1 " << obj.m_n1 ;os << "}" ;
    return os;
  }

  /// Adds to the stream the bit representation of this structure object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream & os) const {
    m_rfreq.getBitString (os);
    m_hsdiv.getBitString (os);
    m_n1.getBitString (os);
  return os;
  }
  

  /// Accessor for the BSVType name for this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "Si570Controller::Si570Resp" ;
    return os;
  }

  /// Accessor on the size of the object in bits
  /// @return the bit size
  virtual unsigned int getBitSize () const {
    return 48;
  }

  /// returns the class name for this object
  virtual const char * getClassName() const {
    return "Si570Resp" ;
  }

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const {
    return BSV_Struct ;
  }

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount() const {
    return 3;
  }
  
  /// Accessor to member objects
  /// @param idx -- member index
  /// @return BSVType * to this object or null
  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
      case 0: return & m_rfreq;
      case 1: return & m_hsdiv;
      case 2: return & m_n1;
      default: std::cerr << "Index error in getMember for class Si570Resp" << std::endl ;
    };
    return 0;
  }
  
  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
      case 0: return "rfreq";
      case 1: return "hsdiv";
      case 2: return "n1";
      default: std::cerr << "Index error in getMemberName for class Si570Resp" << std::endl ;
    };
    return 0;
  }
};

/// C++ class representing the hardware structure Si570Controller::Si570Params
/// This class has been automatically generated.
class Si570Req : public BSVType {
 public:
  BitT<7> m_n1 ;
  BitT<3> m_hsdiv ;
  BitT<38> m_rfreq ;
  BitT<1> m_rnw;

  /// A default constructor
  Si570Req ()
    :  m_n1()
    , m_hsdiv()
    , m_rfreq()
    , m_rnw()
  {}

  /// Constructor for object from a SceMiMessageData object
  /// @param msg -- the scemi message object
  /// @param off -- the starting bit offset, updated to next bit position
  Si570Req ( const SceMiMessageDataInterface *msg, unsigned int &off )
    : m_n1(msg, off)
    , m_hsdiv(msg, off)
    , m_rfreq(msg, off)
    , m_rnw(msg, off)
  {}

  /// Converts this object into its bit representation for sending as a SceMi message
  /// @param msg -- the message object written into
  /// @param off -- bit position off set in message
  /// @return next free bit position for writing
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_n1.setMessageData( msg, running );
    running = m_hsdiv.setMessageData( msg, running );
    running = m_rfreq.setMessageData( msg, running );
    running = m_rnw.setMessageData( msg, running );
    if (running != off + 49 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 49) << std::endl;
    }
    return running;
  }

  /// overload the put-to operator for Si570Req
  friend std::ostream & operator<< (std::ostream &os, const Si570Req &obj) {
    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "rfreq " << obj.m_rfreq ;os << " " ;
    os << "hsdiv " << obj.m_hsdiv ;os << " " ;
    os << "n1 " << obj.m_n1 ;os << " " ;
    os << "rnw " << obj.m_rnw ;os << "}";
    return os;
  }

  /// Adds to the stream the bit representation of this structure object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream & os) const {
    m_rfreq.getBitString (os);
    m_hsdiv.getBitString (os);
    m_n1.getBitString (os);
    m_rnw.getBitString (os);
  return os;
  }
  

  /// Accessor for the BSVType name for this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "Si570Controller::Si570Req" ;
    return os;
  }

  /// Accessor on the size of the object in bits
  /// @return the bit size
  virtual unsigned int getBitSize () const {
    return 49;
  }

  /// returns the class name for this object
  virtual const char * getClassName() const {
    return "Si570Req" ;
  }

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const {
    return BSV_Struct ;
  }

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount() const {
    return 4;
  }
  
  /// Accessor to member objects
  /// @param idx -- member index
  /// @return BSVType * to this object or null
  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
      case 0: return & m_rfreq;
      case 1: return & m_hsdiv;
      case 2: return & m_n1;
      case 3: return & m_rnw;
      default: std::cerr << "Index error in getMember for class Si570Req" << std::endl ;
    };
    return 0;
  }
  
  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
      case 0: return "rfreq";
      case 1: return "hsdiv";
      case 2: return "n1";
      case 3: return "rnw";
      default: std::cerr << "Index error in getMemberName for class Si570Req" << std::endl ;
    };
    return 0;
  }
};


class Si570Xactor {
private:
  InportQueueT<Si570Req>   m_request;
  OutportQueueT<Si570Resp> m_response;

  double                   m_fxtal;
  unsigned long            m_frequency;

  double                   m_rfreq;
  unsigned long            m_hsdiv;
  unsigned long            m_n1;

public:
  Si570Xactor(const std::string &hier, const std::string &inst, SceMi *scemi)
    : m_request(hier, inst + "_xrequest", scemi)
    , m_response(hier, inst + "_xresponse", scemi)
    , m_frequency(156250000)
    , m_rfreq(0.0)
    , m_hsdiv(0)
    , m_n1(0)
  {
  }

  ~Si570Xactor()
  {
  }

private:
  Si570Xactor();
  Si570Xactor(const Si570Xactor &);

private:
  void read_settings()
  {
    Si570Req  request;
    Si570Resp response;
    
    request.m_rnw = 1;
    m_request.sendMessage(request);
    response = m_response.getMessage();

    m_rfreq = rfreq_decode(response.m_rfreq.get64());
    m_hsdiv = hsdiv_decode(response.m_hsdiv.get());
    m_n1    = n1_decode(response.m_n1.get());
    m_frequency = static_cast<unsigned long>((m_fxtal * m_rfreq) / (static_cast<double>(m_hsdiv) * static_cast<double>(m_n1)));
  }

  void write_settings()
  {
    Si570Req request;
    
    request.m_rnw   = 0;
    request.m_rfreq = rfreq_encode(m_rfreq);
    request.m_hsdiv = hsdiv_encode(m_hsdiv);
    request.m_n1    = n1_encode(m_n1);
    m_request.sendMessage(request);
  }

  unsigned long hsdiv_decode(const unsigned long &hsdivenc)
  {
    if (hsdivenc == 4 || hsdivenc == 6)
      return 0;

    return hsdivenc + 4;
  }

  unsigned long hsdiv_encode(const unsigned long &hsdivdec)
  {
    if (hsdivdec == 8 || hsdivdec == 10 || (hsdivdec < 4) || (hsdivdec > 11)) {
      throw std::string("The encoded version of HS_DIV cannot represent 8 or 10");
    }
    return hsdivdec - 4;
  }

  unsigned long n1_decode(const unsigned long &n1enc)
  {
    return n1enc + 1;
  }

  unsigned long n1_encode(const unsigned long &n1dec)
  {
    return n1dec - 1;
  }

  double rfreq_decode(const unsigned long long &rfreqenc)
  {
    return ((double)rfreqenc / (double)(1 << 28));
  }

  unsigned long long rfreq_encode(const double &rfreqdec)
  {
    return (unsigned long long)((double)rfreqdec * (1 << 28));
  }

public:
  void initialize() {
    read_settings();
    m_fxtal = (double)(((double)m_frequency * (double)m_hsdiv * (double)m_n1) / ((double)m_rfreq));
  }

  unsigned long getFrequency() {
    return m_frequency;
  }

  double setFrequency(const double &Hz) {
    read_settings();

    double fdco_new = Hz * static_cast<double>(m_hsdiv) * static_cast<double>(m_n1);
    bool found = false;

    // clean up divider values to get a proper fdco with new desired frequency 
    if ((fdco_new < 4.85e9) || (fdco_new > 5.67e9)) {
      for(unsigned long hs = 4; hs < 12;) {
	for(unsigned long n1 = 1; n1 <  128;) {
	  // clean up illegal values
	  if (hs == 8) hs = 9;
	  if (hs == 10) hs = 11;
	  if ((n1 != 1) && ((n1 % 2) == 1)) n1 += 1;

	  //std::cerr << "Attempting hsdiv=" << hs << " n1=" << n1 << std::endl;

	  double fdco_test = Hz * static_cast<double>(hs) * static_cast<double>(n1);
	  if (fdco_test > 4.85e9 && fdco_test < 5.67e9) {
	    m_hsdiv = hs;
	    m_n1 = n1;
	    found = true;
	    goto done;
	  }
	  n1 += 1;
	}
	hs += 1;
      }
    } else {
      found = true;
    }

  done:
    if (found) {
      fdco_new = Hz * static_cast<double>(m_hsdiv) * static_cast<double>(m_n1);
      m_rfreq  = fdco_new / m_fxtal;
      write_settings();
    } else {
      throw std::string("Cannot set clock rate to desired speed.");
    }

    m_frequency = static_cast<unsigned long>((m_fxtal * m_rfreq) / (static_cast<double>(m_hsdiv) * static_cast<double>(m_n1)));
    //Cast from a double to an unsigned long back to a double as the return value.  Is this right?
    return static_cast<double>(m_frequency);
  }
};

