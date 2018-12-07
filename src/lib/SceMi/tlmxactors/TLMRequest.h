//-*- C++ -*-x
#pragma once

#include "bsv_scemi.h"
#include "sized_types.h"
#include "TLMRequestParams.h"
#include "TLMBurstLong.h"
#include "TLMUtilities.h"
#include "TLMPayload.h"
#include "TLMResponse.h"
#include "../bsvxactors/XactorLog.h"
#include <vector>
#include <stdlib.h>


///  Data class representing a TLMRequest.
/// This class represents a TLMRequest.  A request consists of 4
/// fields, the header, the address, the burst length, and the payload.  This class is
/// templated on the address width and the data width, both must match
/// the widths of the underlying hardware xactor.
/// Valid address widths are 32 and 64
/// Valid data widths are 8, 16, 32, 64, 128, 256, 512, 1024.
template <unsigned int ADDRSIZE=32, unsigned int DATASIZE=32, unsigned int USERDSIZE=0>
class TLMRequest : public BSVType {
public:
  /// The request header
  TLMRequestParams m_header;
  /// The address for the request
  BitT<ADDRSIZE>   m_address;

  /// The userdata for the request
  BitT<USERDSIZE>   m_userdata;

  /// The payload (data plus byte enables)
  mutable TLMPayload<DATASIZE,USERDSIZE>   m_payload;
  /// The burst lenght
  unsigned         m_b_length;

  /// Empty constructor
  TLMRequest ()
    : m_header()
    , m_address()
    , m_userdata()
    , m_payload()
    , m_b_length(1)
  {
    m_header.m_b_size = getBSize();
  }

  ///  Constructor from Message stream.
  /// A TLMRequest object is constructed from the message buffer
  /// with data taken from the bit offset.
  /// \param msg -- pointer to message buffer class
  /// \param off -- initial bit offset into the message buffer
  /// (modified by call)
  TLMRequest ( const SceMiMessageDataInterface *msg, unsigned int &off )
    : m_header()
    , m_address()
    , m_userdata()
    , m_payload()
    , m_b_length(1)
  {
    off = unpackHeaderAddress(msg, off);
    off = next32(off);

    unpackPayload(msg, off);

    if ((int) m_header.m_b_size.m_val > (int) getBSize()) {
      XactorLog logfile;
      logfile.Warn ("TLMRequest constructed with incorrect bsize");
    }

  }

  /// Piece-wise constructor for header and address from
  /// message buffer.
  /// This method unpacks the message data populating the header and
  /// address fields.   It is not expected that will be be called
  /// outside of the class.
  /// \param msg -- pointer to message buffer class
  /// \param off -- initial bit offset into the message buffer
  /// (modified by call)
  /// \return the bit off for the next item in message buffer.
  unsigned int unpackHeaderAddress (const SceMiMessageDataInterface *msg, unsigned int &off) {
    unsigned int addroff = off + 64;
    m_header = TLMRequestParams(msg, off);

    TLMBurstLong blength(msg, off);
    m_b_length = 1 + (unsigned) blength;

    m_address = BitT<ADDRSIZE>(msg, addroff);
    setPayload();

    if (USERDSIZE != 0) {
      m_userdata = BitT<USERDSIZE> (msg, addroff);
    }

    return addroff;
  }


  ///  Piece-wise constructor for payload from message buffer.
  /// This method unpacks the message data populating payload field.
  /// It is not expected that will be be called
  /// outside of the class.
  /// \param msg -- pointer to message buffer class
  /// \param off -- initial bit offset into the message buffer
  /// (modified by call)
  /// \return the bit off for the next item in message buffer.
  unsigned int unpackPayload (const SceMiMessageDataInterface *msg, unsigned int &off) {
    setPayload();
    off = m_payload.unpackPayload(msg, off);
    return off;
  }

  /// Populates message buffer from object.
  /// A portion of the message buffer is populated from the contents
  /// of the class object.
  /// \param msg -- message buffer to be populated
  /// \param off -- initial bit offset into the message buffer
  /// \return the bit off for the next item in message buffer.
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    // insure that payload is sized correctly
    setPayload();
    unsigned int running = off;

    TLMBurstLong blength = (int) (m_b_length-1);

    running = m_header.setMessageData  ( msg, running );
    running = blength.setMessageData   ( msg, running );

    running = m_address.setMessageData ( msg, off+64 );
    running = m_userdata.setMessageData (msg, running );
    running = m_payload.setMessageData ( msg, running);

    return running;
  }

  ///  Get the payload size in words.
  /// Returns the size of the payload, in "datasize" words for the
  /// TLMRequest based on the command and burst length set in the header.
  /// \return size of data words for the request
  unsigned int getPayloadSize() const {
    unsigned int burstl = m_b_length ;
    return (m_header.m_command == TLMCommand::e_WRITE) ? (burstl) : 0;
  }

  ///  Resizes the payload to match the header fields.
  /// Resizes the payload field according to the command and burst
  /// lenght information in the header field.   Payload data may be
  /// dropped; new data is set to 0.
  void setPayload() const {
    // insure that payload is sized correctly
    m_payload.resize(getPayloadSize(), (bool) m_header.m_spec_byte_enable);
  }

  ///  Determine the payload size for the reponse.
  /// Returns the expected response payload size for this request.
  /// \return the payload size in "datasize" words for the response
  unsigned int determineResponseSize() const {
    unsigned int burstl = m_b_length ;
    return (m_header.m_command == TLMCommand::e_READ) ? (burstl) : 0;
  }

  /// PutTo operation, tcl friendly.
  friend std::ostream & operator<< (std::ostream &os, const TLMRequest &obj) {
    // insure that payload is sized correctly
    obj.setPayload();

    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "header " << obj.m_header ;os << " " ;
    os << "b_length " << std::dec << obj.m_b_length ; os << " " ;
    os << "address " << obj.m_address ;os << " " ;
    if (USERDSIZE != 0) {
      os << "userdata " << obj.m_userdata << " " ;
    }
    os << "payload " << obj.m_payload ;
    os << "}" ;
    return os;
  }

  /// returns the bit stream as series of bits
  virtual std::ostream & getBitString (std::ostream & os) const {
    // insure that payload is sized correctly
    setPayload();
    std::string pad;

    // Payload to header -- this is backwards...
    m_payload.getBitString(os);

    m_userdata.getBitString(os);
    m_address.getBitString (os);

    // Pad string out to 64 bits
    unsigned int hsize = m_header.getBitSize() + 8 + 1; // XXX
    pad.resize(64-hsize,'_');
    os << pad ;

    TLMBurstLong blength = (int) (m_b_length - 1);
    blength.getBitString(os);

    m_header.getBitString (os);
    // Pad string out to next word boundary

    return os;
  }


  /// generates an ostream listing the type of the hardware xactor
  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << std::dec << "TLMRequest #(" << ADDRSIZE << "," << DATASIZE << "," << USERDSIZE << " )";
    return os;
  }

  /// returns the size in bit of the packed object
  virtual unsigned int getBitSize () const {
    // data than 32 bits in not packed.
    return getHeaderBitSize() + m_payload.getBitSize();
  }
  /// returns the size in 32-bit words of the packed object
  virtual unsigned int getWordSize () const {
    return getHeaderWordSize() + getPayloadWordSize();
  }

  ///  returns the size in bits of the packed header and address field
  unsigned int getHeaderBitSize() const {
    return (64 + ADDRSIZE  + USERDSIZE);
  }
  ///  returns the size in 32-bit words of the packed header and address field
  unsigned int getHeaderWordSize() const {
    return  bits_to_words(getHeaderBitSize());
  }

  /// returns the size in bits of the packed payload field
  unsigned int getPayloadBitSize() const {
    // insure that payload is sized correctly
    setPayload();
    return m_payload.getBitSize();
  }
  ///  returns the size in 32-bit words) of the packed payload field
  unsigned int getPayloadWordSize() const {
    return  bits_to_words(getPayloadBitSize());
  }

  /// returns the class name
  virtual const char * getClassName() const {
    return "TLMRequest";
    //os << std::dec << "TLMRequest<" << ADDRSIZE << "," << DATASIZE << "," << USERDSIZE << " >";
  }

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const {
    return BSV_Struct ;
  }

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount() const {
    return 4;
  };

  virtual BSVType * getMember (unsigned int idx) {
    // insure that payload is sized correctly
    setPayload();
    switch (idx) {
    case 0: return & m_header;
    case 1: return & m_address;
    case 2: return & m_userdata;
    case 3: return & m_payload;
    default:
        std::cerr << "Index error in getMember for class TLMRequest" << std::endl ;
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
    case 1: return "address";
    case 2: return "userdata";
    default:       if (idx - 3 > m_payload.size()) {
        return "payload";
      }
      else  {
        std::cerr << "Index error in getMemberName for class TLMRequest" << std::endl ;
      }
    };
    return 0;
  };

  ///  Returns the TLMBSize enum for the DATASIZE.
  /// Used to set the bsize field in the header.
  TLMBSize::E_TLMBSize getBSize() const {
    return TLMUtilities::getBSize(DATASIZE);
  }

  // Populate fields of a TLMResponse object based on this request
  void populateResponse (TLMResponse<DATASIZE, USERDSIZE> &resp ) const {

    resp.m_header.m_error          = TLMErrorCode::e_NONE;
    resp.m_header.m_transaction_id = m_header.m_transaction_id;
    resp.m_header.m_status         = TLMStatus::e_SUCCESS;
    resp.m_header.m_command        = m_header.m_command;

    resp.m_b_length                = m_b_length;
    resp.m_userdata                = m_userdata;

    resp.setPayload();
  }


private:

  /// Utility function to raise x to the next bit boundary.
  /// note that next32(32) == 32.
  /// \param x -- initial value
  /// \return x increase
  static unsigned int next32(unsigned int x) {
    return 32*((x+31)/32);
  }

  /// Utility function to get the bit index of the data in the request
  /// \return bit index of the data payload
  unsigned int dataOffSet () {
    return 64 + next32(ADDRSIZE);
  }

};
