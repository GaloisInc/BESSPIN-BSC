// Automatically generated by: ::SceMiMsg
// DO NOT EDIT
// C++ Class with SceMi Message passing for type:  TLM3Defines::TLMBSize

#pragma once

#include "bsv_scemi.h"

/// C++ class representing the hardware enum TLM3Defines::TLMBSize.
/// This class has been automatically generated.
class TLMBSize : public BSVType {
 public:

  enum E_TLMBSize {
      e_BITS8,
      e_BITS16,
      e_BITS32,
      e_BITS64,
      e_BITS128,
      e_BITS256,
      e_BITS512,
      e_BITS1024
    } ;
  
E_TLMBSize m_val;

  /// Default constructor for enumeration
  TLMBSize (E_TLMBSize v = e_BITS8)
  : m_val(v)
  {}

  /// Constructor for enumeration from a SceMiMessageData object
  /// @param msg -- the scemi message object
  /// @param off -- the starting bit offset, updated to next bit position
  TLMBSize (const SceMiMessageDataInterface *msg, unsigned int & off) {
    m_val = (E_TLMBSize) msg->GetBitRange ( off, 2 );
    off = off + 3;
  }

  /// Converts enumeration into its bit representation for sending as a SceMi message
  /// @param msg -- the message object written into
  /// @param off -- bit position offset in message
  /// @return next free bit position in the message for writing
  unsigned int setMessageData( SceMiMessageDataInterface &msg, const unsigned int off) const {
    msg.SetBitRange ( off, 2, (SceMiU32) m_val );
    return off + 3;
  }

  /// overload operator == for enumeration classes with values
  bool operator== (const E_TLMBSize &x) const { return m_val == x ;}
  /// overload operator != for enumeration classes with values
  bool operator!= (const E_TLMBSize &x) const { return m_val != x ;}
  /// overload operator == for enumeration classes
  bool operator== (const TLMBSize &x) const { return m_val == x.m_val ;}
  /// overload operator != for enumeration classes
  bool operator!= (const TLMBSize &x) const { return m_val != x.m_val ;}
  

  /// overload the put-to operator for TLMBSize
  friend std::ostream & operator<< (std::ostream &os, const TLMBSize & e) {
    BSVType::PutTo * override = lookupPutToOverride ( e.getClassName() );
    if ( override != 0 ) {
       return override(os, e );
    }
    switch (e.m_val) {
      case e_BITS8: os << "BITS8" ; break ;
      case e_BITS16: os << "BITS16" ; break ;
      case e_BITS32: os << "BITS32" ; break ;
      case e_BITS64: os << "BITS64" ; break ;
      case e_BITS128: os << "BITS128" ; break ;
      case e_BITS256: os << "BITS256" ; break ;
      case e_BITS512: os << "BITS512" ; break ;
      case e_BITS1024: os << "BITS1024" ; break ;
      default: os << "Enum value error for TLMBSize: " << (int) e.m_val << "." ;
    }
    return os;
  };

  /// Accessor for symbolic name of the class' enumeration value
  /// @return the name as a char *
  const char * getName() const {
    const char *ret = "" ;
    switch (m_val) {
      case e_BITS8: ret = "BITS8" ; break ;
      case e_BITS16: ret = "BITS16" ; break ;
      case e_BITS32: ret = "BITS32" ; break ;
      case e_BITS64: ret = "BITS64" ; break ;
      case e_BITS128: ret = "BITS128" ; break ;
      case e_BITS256: ret = "BITS256" ; break ;
      case e_BITS512: ret = "BITS512" ; break ;
      case e_BITS1024: ret = "BITS1024" ; break ;
      default: std::cerr << "Enum value error for TLMBSize: " << (int) m_val << "." ;
    }
    return ret;
  };

  /// Adds to the stream the bit representation of this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream &os) const {
    unsigned int data = (unsigned int) m_val;
    for ( unsigned int i = 3; i > 0; --i) {
      unsigned int bitidx = i - 1;
      bool v = 0 != (data & (0x1 << bitidx));
      os << (v ? '1' : '0') ;
    }
  return os;
  }

  /// Accessor for the BSVType name for this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBSVType (std::ostream & os) const {
    os << "TLM3Defines::TLMBSize" ;
    return os;
  }

  /// Accessor on the size of the object in bits
  /// @return the bit size
  virtual unsigned int getBitSize () const {
    return 3;
  }

  /// returns the class name for this object
  virtual const char * getClassName() const {
    return "TLMBSize" ;
  }

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const {
    return BSV_Enum ;
  }

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount() const {return 0;};
  /// Accessor to member objects
  /// @param idx -- member index
  /// @return BSVType * to this object or null
  virtual BSVType * getMember (unsigned int idx) {return 0;};
  
  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const {return 0;};
  
};

