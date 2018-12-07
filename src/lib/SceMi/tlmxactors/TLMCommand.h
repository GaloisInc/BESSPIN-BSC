// Automatically generated by: ::SceMiMsg
// DO NOT EDIT
// C++ Class with SceMi Message passing for type:  TLM3Defines::TLMCommand

#pragma once

#include "bsv_scemi.h"

/// C++ class representing the hardware enum TLM3Defines::TLMCommand.
/// This class has been automatically generated.
class TLMCommand : public BSVType {
 public:

  enum E_TLMCommand {
      e_READ,
      e_WRITE,
      e_UNKNOWN
    } ;
  
E_TLMCommand m_val;

  /// Default constructor for enumeration
  TLMCommand (E_TLMCommand v = e_READ)
  : m_val(v)
  {}

  /// Constructor for enumeration from a SceMiMessageData object
  /// @param msg -- the scemi message object
  /// @param off -- the starting bit offset, updated to next bit position
  TLMCommand (const SceMiMessageDataInterface *msg, unsigned int & off) {
    m_val = (E_TLMCommand) msg->GetBitRange ( off, 1 );
    off = off + 2;
  }

  /// Converts enumeration into its bit representation for sending as a SceMi message
  /// @param msg -- the message object written into
  /// @param off -- bit position offset in message
  /// @return next free bit position in the message for writing
  unsigned int setMessageData( SceMiMessageDataInterface &msg, const unsigned int off) const {
    msg.SetBitRange ( off, 1, (SceMiU32) m_val );
    return off + 2;
  }

  /// overload operator == for enumeration classes with values
  bool operator== (const E_TLMCommand &x) const { return m_val == x ;}
  /// overload operator != for enumeration classes with values
  bool operator!= (const E_TLMCommand &x) const { return m_val != x ;}
  /// overload operator == for enumeration classes
  bool operator== (const TLMCommand &x) const { return m_val == x.m_val ;}
  /// overload operator != for enumeration classes
  bool operator!= (const TLMCommand &x) const { return m_val != x.m_val ;}
  

  /// overload the put-to operator for TLMCommand
  friend std::ostream & operator<< (std::ostream &os, const TLMCommand & e) {
    BSVType::PutTo * override = lookupPutToOverride ( e.getClassName() );
    if ( override != 0 ) {
       return override(os, e );
    }
    switch (e.m_val) {
      case e_READ: os << "READ" ; break ;
      case e_WRITE: os << "WRITE" ; break ;
      case e_UNKNOWN: os << "UNKNOWN" ; break ;
      default: os << "Enum value error for TLMCommand: " << (int) e.m_val << "." ;
    }
    return os;
  };

  /// Accessor for symbolic name of the class' enumeration value
  /// @return the name as a char *
  const char * getName() const {
    const char *ret = "" ;
    switch (m_val) {
      case e_READ: ret = "READ" ; break ;
      case e_WRITE: ret = "WRITE" ; break ;
      case e_UNKNOWN: ret = "UNKNOWN" ; break ;
      default: std::cerr << "Enum value error for TLMCommand: " << (int) m_val << "." ;
    }
    return ret;
  };

  /// Adds to the stream the bit representation of this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream &os) const {
    unsigned int data = (unsigned int) m_val;
    for ( unsigned int i = 2; i > 0; --i) {
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
    os << "TLM3Defines::TLMCommand" ;
    return os;
  }

  /// Accessor on the size of the object in bits
  /// @return the bit size
  virtual unsigned int getBitSize () const {
    return 2;
  }

  /// returns the class name for this object
  virtual const char * getClassName() const {
    return "TLMCommand" ;
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

