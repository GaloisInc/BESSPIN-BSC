//-*- C++ -*-x
// C++ Class with SceMi Message passing for  TLM3Api::TLMRequest

#pragma once

#include "bsv_scemi.h"
#include "sized_types.h"
#include "TLMResponseParams.h"
#include "TLMPayload.h"
#include "TLMBurstLong.h"
#include <vector>

/// Data class representing a TLMResponse.
/// This class represents a TLMResponse.  A request consists of 3
/// fields, the header, the burst lenght and the payload.
/// This class is
/// templated on data width, which must match
/// the widths of the underlying hardware xactor.
/// Valid data widths are 8, 16, 32, 64, 128, 256, 512, and 1024.
/// Valid USERDSIZE are: 0, 32, and 64.
template <unsigned int DATASIZE=32, unsigned int USERDSIZE=0>
class TLMResponse : public BSVType {
public:
  /// The response header
  TLMResponseParams m_header;
  /// user data
  BitT<USERDSIZE> m_userdata;
  /// The response payload
  mutable TLMPayload<DATASIZE,USERDSIZE>  m_payload;
  /// The burst lenght for the reponse
  unsigned m_b_length;

  /// \brief Empty constructor
  TLMResponse ()
    : m_header()
    , m_userdata()
    , m_payload()
    , m_b_length(1)
  {}

  ///  Constructor from Message stream.
  /// A TLMResponse object is constructed from the message buffer
  /// with data taken from the bit offset.
  /// \param msg -- pointer to message buffer class
  /// \param off -- initial bit offset into the message buffer
  /// (modified by call)
  TLMResponse ( const SceMiMessageDataInterface *msg, unsigned int &off )
    : m_header()
    , m_userdata()
    , m_payload()
    , m_b_length(1)
  {
    m_header = TLMResponseParams(msg, off);
    TLMBurstLong blength(msg, off);
    m_b_length = 1 + (unsigned) blength;
    off = next32(off);

    // Only write responses have user data
    if (m_header.m_command == TLMCommand::e_WRITE) {
      m_userdata = BitT<USERDSIZE> (msg, off);
    }

    unpackPayload(msg, off);
    off = next32(off);
  }

  ///  Piece-wise constructor for payload from message buffer.
  /// This method unpacks the message data populating the payload
  /// field.   It is not expected that will be be called 
  /// outside of the class.
  /// \param msg -- pointer to message buffer class
  /// \param off -- initial bit offset into the message buffer
  /// (modified by call)
  /// \return the bit off for the next item in message buffer.
  void unpackPayload(const SceMiMessageDataInterface *msg, unsigned int &off ){
    // insure that payload is sized correctly
    setPayload();

    m_payload.unpackPayload(msg, off);
  }

  ///  Populates message buffer from object.
  /// A portion of the message buffer is populated from the contents
  /// of the class object.
  /// \param msg -- message buffer to be populated
  /// \param off -- initial bit offset into the message buffer
  /// \return the bit off for the next item in message buffer.
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    setPayload();
    unsigned int running = off;

    TLMBurstLong blength = (int) (m_b_length-1);

    running = m_header.setMessageData( msg, running );
    running = blength.setMessageData (msg, running);

    running = next32(running);

    // Only write response have user data
    if (m_header.m_command == TLMCommand::e_WRITE) {
      running = m_userdata.setMessageData(msg,running);
    }

    running = m_payload.setMessageData(msg, running );
    return next32(running);
  }

  ///  Get the payload size in words.
  /// returns the size of the payload, in "datasize" words for the
  /// TLMRequest based on the command and burst length set in the header.
  /// \return size of data words for the request
  unsigned int getPayloadSize() const {
    unsigned int burstl = m_b_length ;
    return (m_header.m_command == TLMCommand::e_READ) ? (burstl) : 0;
  }

  ///  Resizes the payload to match the header fields.
  /// Resizes the payload field according to the command and burst
  /// lenght information in the header field.   Payload data may be
  /// dropped; new data is set to 0.
  void setPayload() const {
    // insure that payload is sized correctly
    m_payload.resize(getPayloadSize(), false);
  }

  /// PutTo operation, tcl friendly
  friend std::ostream & operator<< (std::ostream &os, const TLMResponse &obj) {
    // insure that payload is sized correctly
    obj.setPayload();

    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "header " << obj.m_header ;os << " " ;
    os << "b_length " << std::dec << obj.m_b_length; os << " ";

    if ( (USERDSIZE != 0) && (obj.m_header.m_command == TLMCommand::e_WRITE)) {
        os << "userdata " << obj.m_userdata << " " ;
    }
    os << "payload " << obj.m_payload ;
    os << "}" ;
    return os;
  }

  /// return a portion of the data
  virtual std::ostream & getBitString (std::ostream & os) const {

    // insure that payload is sized correctly
    setPayload();
    std::string pad;
    TLMBurstLong blength = (int) (m_b_length - 1);

    // Payload to header -- this is backwards...
    m_payload.getBitString(os);

    if (m_header.m_command == TLMCommand::e_WRITE) {
      m_userdata.getBitString(os);
    }

    // Pad string out to 32 bits
    unsigned int hsize = m_header.getBitSize() + blength.getBitSize();
    pad.resize(32-hsize,'_');
    os << pad ;

    blength.getBitString(os);

    m_header.getBitString (os);

    return os;
  }


  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << std::dec << "TLMResponse #(" << DATASIZE << "," << USERDSIZE << " )";
    return os;
  }

  /// returns the size in bit of the packed object
  virtual unsigned int getBitSize () const {
    return getHeaderBitSize() + getPayloadBitSize();
  }

  /// returns the size in 32-bit words of the packed object
  virtual unsigned int getWordSize() const {
    return bits_to_words(getBitSize());
  }

  ///  returns the size in bits of the packed header
  unsigned int getHeaderBitSize() const {
    unsigned usr = (m_header.m_command == TLMCommand::e_WRITE) ? USERDSIZE : 0;
    return (32 + usr);
  }

  /// returns the size in 32-bit words of the packed header
  unsigned int getHeaderWordSize() const {
    return  bits_to_words(getHeaderBitSize());
  }

  /// returns the size in bits of the packed payload field
  unsigned int getPayloadBitSize() const {
    // insure that payload is sized correctly
    setPayload();
    return m_payload.getBitSize() ;
  }
  /// returns the size in 32-bit words) of the packed payload field
  unsigned int getPayloadWordSize() const {
    return  bits_to_words(getPayloadBitSize());
  }

  /// Calculates the packed 32-bit word size if the payload is psize
  ///
  /// Utility function used for expected response size; there are never byte enables with 
  /// a response
  unsigned int getWordSizeByPayload (unsigned int psize) {
    if (psize == 0) {
      // write response -- header + user 
      return bits_to_words(32 + USERDSIZE);
    }
    return bits_to_words(32 + (psize * (next32(DATASIZE) + USERDSIZE)));
  }

  /// returns the class name
  virtual const char * getClassName() const {
    return "TLMResponse";
    //os << std::dec << "TLMResponse<" << DATASIZE << "," << USERDSIZE << " >";
  }

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const {
    return BSV_Struct ;
  }

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount() const {
    return 3;
  };

  /// Accessor to member objects
  /// @param idx -- member index
  /// @return BSVType * to this object or null
  virtual BSVType * getMember (unsigned int idx) {
    // insure that payload is sized correctly
    setPayload();
    switch (idx) {
    case 0: return & m_header;
    case 1: return & m_payload;
    case 2: return & m_userdata;
    default:
      std::cerr << "Index error in getMember for class TLMResponse" << std::endl ;
    };
    return 0;
  };

  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const {
    // insure that payload is sized correctly
    setPayload();

    switch (idx) {
    case 0: return "header";
    case 1: return "payload" ;
    case 2: return "userdata" ;
    default:  std::cerr << "Index error in getMemberName for class TLMResponse" << std::endl ;
    };
    return 0;
  };

private:

  /// Utility function to raise x to the next bit boundary.
  /// note that next32(32) == 32.
  /// \param x -- initial value
  /// \return x increase
  static unsigned int next32(unsigned int x) {
    return 32*((x+31)/32);
  }

};
