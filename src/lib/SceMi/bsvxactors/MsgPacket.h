//-*- C++ -*-x
// Copyright (c) 2011 -- 2012, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include "scemi.h"
#include <iostream>
#include "DBuffer.h"

///  A Data container class implementing SceMiMessageDataInterface.
/// This class implements a SceMiMessageDataInterface interface for
/// marshalling data between SceMi and C++ objects.
class MsgPacket : public SceMiMessageDataInterface {
public:
  typedef unsigned char                MsgPacketData_t;
  // Do not use size_t here because scemi_pipe_c_try_send is limited to int
  typedef unsigned                     size_type;

private:
  /// The main data container a vector of 32-bit data
  DBuffer<MsgPacketData_t>             m_data;

public:
  /// Simple constructor without data
  MsgPacket();

  ///  Initializing contructor, all words are zeroed
  /// param size_in_bytes -- number of 8-bit words to allocate
  MsgPacket(unsigned int size_in_bytes);

  /// copy constructor
  MsgPacket(const MsgPacket &p);

  /// Destructor
  ~MsgPacket();

  // Allow access to some vector-like members.

  ///  Returns the number of bytes in the MsgPacket.
  size_type size() const ;
  /// returns true if the packet size is 0
  bool empty () const ;
  /// Resize the MsgPacket to hold x words of data.
  /// param x -- number of 32-bit words to initialize
  void resize( size_type x ) ;

  /// Add an element to the end
  void push_back( const MsgPacketData_t &x);

  /// Reserve storage in the vector
  void reserve( const size_type n) ;

  /// Clear the data, size is set to 0
  void clear ();

  /// access word at n
  MsgPacketData_t & operator[] (size_type n) ;
  /// access word at n
  const MsgPacketData_t &  operator[] (size_type n) const ;

  /// equality test
  bool operator== (const MsgPacket &p) const ;
  /// inequality test
  bool operator!= (const MsgPacket &p) const ;

  ///  Set the bits from i to i (i+range) with bits within the packet.
  /// \param i -- first bit to set
  /// \param range -- number of bits following i to be set
  /// \param bits -- its to set
  /// \param ec --  SceMi Error Handling class
  virtual void SetBitRange(unsigned int i, unsigned int range, SceMiU32 bits, SceMiEC* ec = 0);

  ///  Accessor for message data.
  ///  Set the bits from i to i (i+range) with bits within the packet
  /// \param i -- first bit to set
  /// \param range -- number of bits following i to be set
  /// \param ec --  SceMi Error Handling class
  /// \return 32 bit data word of the bits at i
  SceMiU32 GetBitRange(unsigned int i, unsigned int range, SceMiEC* ec = 0) const;

  ///  Memory block set method.
  ///  This copies data from data to this object starting at bit i, and upto range.
  ///  This is more efficient for large block provided the data is aligned.
  /// that is, i is a multiple of 8
  /// \param i -- the first bit to set.
  /// \param range --  number of bits following i to be set
  /// \param data -- the data source
  virtual void SetBlockRange( unsigned i, unsigned range, const char *data);

  ///  Memory block set method.
  ///  This copies data from data to this object starting at bit i, and upto range.
  ///  This is more efficient for large block provided the data is aligned.
  /// that is, i is a multiple of 8
  /// \param i -- the first bit to set.
  /// \param range --  number of bits following i to be set
  /// \param data -- the data source
  virtual void GetBlockRange (unsigned i, unsigned range, char *data) const;

  /// Block copy to a standard SceMiMessageData
  /// \param msg -- target of copy
  /// \return number of 32-bit words copied
  unsigned copy (SceMiMessageData &msg) const ;

  /// access the byte (8-bit)  at idx
  unsigned char getByte(size_type idx) const ;
  /// set the byte at idx to val
  void setByte(size_type idx, unsigned char val) ;

  /// Overload the put-to operator for packets
  friend std::ostream & operator<< (std::ostream &os, const MsgPacket &p) ;

};
