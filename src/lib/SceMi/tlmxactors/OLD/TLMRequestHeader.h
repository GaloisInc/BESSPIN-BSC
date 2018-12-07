// Automatically generated by: ::SceMiMsg
// DO NOT EDIT
// C++ Class with SceMi Message passing for type:  TLM3Api::TLMRequestHeader

#pragma once

#include "bsv_scemi.h"
#include "TLMRequestParams.h"

/// C++ class representing the hardware structure TLM3Api::TLMRequestHeader
/// This class has been automatically generated.
class TLMRequestHeader : public BSVType {
 public:
  TLMRequestParams m_params ;
  BitT<15> m_b_length ;

  /// A default constructor
  TLMRequestHeader ()
    :  m_params()
    , m_b_length()
  {}

  /// Constructor for object from a SceMiMessageData object
  /// @param msg -- the scemi message object
  /// @param off -- the starting bit offset, updated to next bit position
  TLMRequestHeader ( const SceMiMessageDataInterface *msg, unsigned int &off )
    : m_params(msg, off)
    , m_b_length(msg, off)
  {}

  /// Converts this object into its bit representation for sending as a SceMi message
  /// @param msg -- the message object written into
  /// @param off -- bit position off set in message
  /// @return next free bit position for writing
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_params.setMessageData( msg, running );
    running = m_b_length.setMessageData( msg, running );
    if (running != off + 51 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 51) << std::endl;
    }
    return running;
  }

  /// overload the put-to operator for TLMRequestHeader
  friend std::ostream & operator<< (std::ostream &os, const TLMRequestHeader &obj) {
    BSVType::PutTo * override = lookupPutToOverride ( obj.getClassName() );
    if ( override != 0 ) {
       return override(os, obj );
    }
    os << "{" ;
    os << "b_length " << obj.m_b_length ;os << " " ;
    os << "params " << obj.m_params ;os << "}" ;
    return os;
  }

  /// Adds to the stream the bit representation of this structure object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream & os) const {
    m_b_length.getBitString (os);
    m_params.getBitString (os);
  return os;
  }
  

  /// Accessor for the BSVType name for this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "TLM3Api::TLMRequestHeader" ;
    return os;
  }

  /// Accessor on the size of the object in bits
  /// @return the bit size
  virtual unsigned int getBitSize () const {
    return 51;
  }

  /// returns the class name for this object
  virtual const char * getClassName() const {
    return "TLMRequestHeader" ;
  }

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const {
    return BSV_Struct ;
  }

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount() const {
    return 2;
  };
  
  /// Accessor to member objects
  /// @param idx -- member index
  /// @return BSVType * to this object or null
  virtual BSVType * getMember (unsigned int idx) {
    switch (idx) {
      case 0: return & m_b_length;
      case 1: return & m_params;
      default: std::cerr << "Index error in getMember for class TLMRequestHeader" << std::endl ;
    };
    return 0;
  };
  
  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const {
    switch (idx) {
      case 0: return "b_length";
      case 1: return "params";
      default: std::cerr << "Index error in getMemberName for class TLMRequestHeader" << std::endl ;
    };
    return 0;
  };
  
  
};

