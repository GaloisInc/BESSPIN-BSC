// Copyright (c) 2009 -- 2012, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <map>
#include <vector>
#include <iostream>
#include <stdexcept>
#include "scemi.h"


///  Abstraction Virtual class for BSV objects.
///  Pure virtual base class for an object derived from a BSV type.
///  Static members within this class allow for users to customize the
///  put-to (operator<<) for custom display, since most of the put-to
///  functions are automatically generated.
class BSVType {
public:

  /// A listing of types for the various concrete instances
  enum BSVKind { BSV_Primitive, BSV_Struct, BSV_TaggedUnion, BSV_Enum, BSV_Vector, BSV_Payload };

  typedef std::ostream & PutTo ( std::ostream &os, const BSVType & x);
private:
  typedef std::map<const char *, PutTo *>  MapType;

private:
  /// Static container to hold customer functions which override the put-to (operator<<)
  /// since most deriived classes are automatically generated.
  static MapType *s_pPutToOverRides ;

protected:
  /// Lookup function for overridden put-to function
  /// \param cname -- the class name
  /// \return the custom display function or null
  static PutTo * lookupPutToOverride ( const char *cname) ;
  /// Sets the override put-to function for the class
  /// \param cname -- the class name
  /// \param putToFunc  the custom display function for overriding
  static void setPutToOverRide ( const char *cname, PutTo *putToFunc);

public:
  /// Destructor
  virtual ~BSVType() {};

  /// Sets the override put-to function for this class
  /// \param func --  the custom display function for overriding
  virtual void setPutToOverRide (PutTo *func) const;

  /// Accessor for the BSVType name for this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBSVType (std::ostream & os) const = 0;

  /// Accessor on the size of the object is bits
  /// @return the bit size of course!
  virtual unsigned int getBitSize () const = 0;


  /// Adds to the stream the bit representation of this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream &os) const = 0;
  virtual std::ostream & getBitStringRange (std::ostream &os,
                                            unsigned int from, unsigned int to) const
    { os << "Not implemented"; return os; }

  /// returns the class name for this object
  virtual const char * getClassName() const = 0;

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const =  0;
  /// Accessor to the \"tag\" portion of the tagged union
  virtual SceMiU32 getTaggedUnionTag () const { return 0;}
  /// returns the bit width of the tag portion of the tagged union
  virtual unsigned int getTaggedUnionTagWidth () const { return 0;}

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount () const = 0;
  /// Accessor to member objects
  /// @param idx -- member index
  /// @return BSVType * to this object or null
  virtual BSVType * getMember (unsigned int idx) = 0;

  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const = 0;

  /// Converts this object into its bit representation for sending as a SceMi message
  /// @param msg -- the message object written into
  /// @param off -- bit position off set in message
  /// @return next free bit position for writing
  virtual unsigned int setMessageData ( SceMiMessageDataInterface &msg, const unsigned int off=0) const = 0;

  /// overload equality test
  bool operator== ( const BSVType &that) const;
  /// overload inequality test
  bool operator!= ( const BSVType &that) const;

};
