//-*- C++ -*-x
// Copyright (c) 2009 -- 2012, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <limits>
#include <typeinfo>
#include <cstring>
#include "BSVType.h"

/// Class representing an arbitrarily wide bit vector with the ability
/// to marshall the data to and from transactors.
/// That is, a  Bluespec Bit#(n) type.
/// template N refers to the number of bits in the word.  N >= 0
template <unsigned int N>
class BitT : public BSVType {
public:
  typedef std::ostream & PutTo ( std::ostream &os, const BitT<N> & x) ;

public:
  // Some versions of CXX  do not allow 0 sized arrays!
  SceMiU32 m_data[(N==0)? 1: (N+31)/32];

public:
  /// accessor on bits per word for class.
  static inline unsigned int BITSPERWORD() { return 32 ; }
  /// accessors on number of works in the class
  static inline unsigned int WORDS() { return (N+31)/32; }
  /// accessors on bytes in the class
  static inline unsigned int BYTES() { return (N+7)/8; }

protected:
  /// Generic constructor template function for building a BitT from a
  /// IT object, where IT objects are native c types.
  template<class IT>
  void buildBitT(const IT & din, const char * ptypename) {
    IT d = din;
    unsigned int ws = 8 * static_cast<unsigned int>(sizeof(IT));
    if ((N == 0) && (din != 0)) {
        /* Warn if high order bits are non-zero */
      std::cerr << "Warning: " << showClassName()  << " cannot represent (" << ptypename << ") 0x"
                << std::hex << din
                << " as used in constructor, data truncated." << std::endl;
    }
    if (N < ws) {
      bool issigned = std::numeric_limits<IT>::is_signed ;
      IT mask =  ( ~ ((IT) 0)) << (std::min) (ws,N);
      d = din & ~mask;
      IT dropped = din & mask;
      if (dropped != 0 && (!issigned || (dropped != mask))) {
        /* Warn if high order bits are non-zero */
        std::cerr << "Warning: " << showClassName()  << " cannot represent (" << ptypename << ") 0x"
                  << std::hex << din
                  << " as used in constructor, data truncated." << std::endl;
      }
    }
    if (N != 0) {
      unsigned int i;
      for (i  = 0; i < WORDS(); ++i ) {
        m_data[i] = static_cast<SceMiU32>(d) ;
        d = d >> BITSPERWORD()/2; /* compiler bug */
        d = d >> BITSPERWORD()/2;
      }
    }
  }

public:
  // Generic constructors for common C types -- see template above
  /// Constructor from short
  BitT(short din) {buildBitT(din, "short");}
  /// Constructor from unsigned short
  BitT(unsigned short din) {buildBitT(din, "unsigned short");}
  /// Constructor from int
  BitT(int din) {buildBitT(din, "int");}
  /// Constructor from unsigned int
  BitT(unsigned int din) {buildBitT(din, "unsigned int");}

  /// Constructor from long
  BitT(long din) {buildBitT(din, "long");}
  /// Constructor from unsigned long
  BitT(unsigned long din) {buildBitT(din, "unsigned long");}
  /// Constructor from long long
  BitT(long long din) {buildBitT(din, "long long");}
  /// Constructor from unsigned long long
  BitT(unsigned long long din) {buildBitT(din, "unsigned long long");}

  /// Constructor from Boolean and Null constructor
  BitT (bool din=false) {
    if ((N == 0) && din) {
      std::cerr << "Warning: " << showClassName()  << " cannot represent (bool) true"
                << " as used in constructor, data truncated." << std::endl;
    }
    else {
      m_data[0] = din ? 1 : 0;
      unsigned int i;
      for (i  = 1; i < WORDS(); ++i ) {
        m_data[i] = 0 ;
      }
    }
  }


  /// Constructor from memory chunk.  Creates a BitT<N> object from a chunk of memory.  It is assumed
  /// that there the data pointer contains sufficient memory to fill all
  /// N bits.
  BitT (const char *data) {
    if (N != 0) {
      // The last word must be cleared as the memcpy will stop at the specific byte
      m_data[WORDS()-1]  = 0;
      memcpy( (char *) &m_data[0], data, BYTES() );
    }
  }


  /// Constructor from Message Stream.  A BitT object is constructed from the message buffer
  /// with data taken from the bit offset.
  /// \param msg -- pointer to message buffer class
  /// \param off -- initial bit offset into the message buffer
  /// (modified by call)
  BitT (const SceMiMessageDataInterface *msg, unsigned int &off) {
    if ((N == 0)) {
      // nothing to do
    }
    else {
      unsigned int i;
      int remaining; // must be signed
      for ( i = 0, remaining = N ; (i < WORDS()) && (remaining > 0) ; remaining -= BITSPERWORD(), ++i) {
        m_data[i] = msg->GetBitRange ( off+(i*BITSPERWORD()),  (std::min)(BITSPERWORD(), (unsigned int) remaining) -1  );
      }
    }
    off = off+N;
  };

  /// Constructor from string of '1' or '0'.
  /// Error if one of the char is not either '1' or '0'
  BitT (std::string &st) {

    int stidx = st.size();

    // First initialize all bits to zero
    for (int i = WORDS()-1; (i>=0) ; --i )
      m_data[i] = 0;
    // Set each bit one at a time
    for (int i = 0; (i<N && stidx>0) ; i++ ) {
      unsigned int bitidx = i;
      unsigned int wrdidx = bitidx/32;
      --stidx;
      bitidx = bitidx & 0x01f;  // bitidx % 32
      if (stidx >= 0 && st[stidx] == '1')
	m_data[wrdidx] |= (0x1 << bitidx);
    }
  }

  /// Returns the low 32-bits of the data.
  /// N is not checked so date may be truncated
  /// \deprecated  Recommned to use the cast operators instead e.g.
  /// BitT<N> bitobj;
  /// ctype cobj  = (ctype) bitobj;
  SceMiU32 get () const {
    if (N == 0) return 0;
    return m_data[0] ;
  }

  /// Returns the low 64-bits of the data
  /// N is not checked
  /// \deprecated  Recommned to use the cast operators instead e.g.
  /// BitT<N> bitobj;
  /// ctype cobj  = (ctype) bitobj;
  SceMiU64 get64 () const {
    if (N == 0) return 0;
    SceMiU64 tmp = 0;
    if (WORDS() >= 2) {
      tmp = getWord(1);
      tmp = tmp << 32;
    }
    tmp |= getWord(0);
    return tmp;
  }

  ///////////////////////////////////////////////////
  // Methods to get and set specific WORDS of the data.
  /// Accessor method to access word
  SceMiU32 getWord (unsigned int idx) const {
    if (idx >= WORDS()) {
      std::cerr << "illegal index, " << idx << " in method getWord of class " << showClassName() << "." << std::endl;
      throw std::out_of_range( "BitT::getWord()" );
    }
    return m_data[idx] ;
 }

  /// Accessor method set a word
  /// \param idx -- index into 32 bit word
  /// \param d -- the data to set at word idx.
  void setWord ( unsigned int idx, SceMiU32 d) {
   if (idx >= WORDS()) {
     std::cerr << "illegal index, " << idx << " in method setWord of class " << showClassName() << "." << std::endl;
     throw std::out_of_range( "BitT::setWord()" );
   }
   // Check that no data is lost, mask data!
   unsigned int remaining = N - (BITSPERWORD() * idx);
   if (remaining < 32u) {
     SceMiU32 pat =  ~(0xFFFFFFFF << remaining);
     SceMiU32 newd = d & pat;
     if ( newd != d ) {
       // Warn if high order bits are non-zero
       std::cerr << "Warning: " << showClassName()  << " cannot represent 0x"  << std::hex << d
                 << " as used in setWord method, data truncated." << std::endl;
       d = newd;
     }
   }
   m_data[idx] = d ;
 }

  ///////////////////////////////////////////////////
protected:
  /// Helper function to convert from object to native C type
  template<class IT>
  void fromBitT (IT &dout, const char *ptypename) const {
    dout = 0;
    bool issigned = std::numeric_limits<IT>::is_signed;
    unsigned int outbits = std::numeric_limits<IT>::digits + (issigned ? 1 : 0);
    if (outbits < N) {
      std::cerr << "Warning: " << showClassName()  << " cannot convert to an integral type "
                << ptypename  << " (" << std::dec << outbits
                << " bits) without loss of information. Data will be truncated." << std::endl;
    }
    unsigned outwords = (outbits+31)/32;
    IT tmp;
    for (unsigned i = 0 ; (i < outwords) && (i < WORDS()) ; ++ i) {
      tmp = static_cast<IT> (getWord(i));
      dout = dout | static_cast<IT> (tmp << (32*i)) ;
    }

  }


public:
  /// cast operators overload
  /// Convert BitT to bool
  /// This captures the C/C++ semantics where any nonzero value is true,
  /// in contrast to casting it to BitT<1>, which would extract the LSB.
  /// For the LSB, convert BitT to a numeric type, than x&1.
  operator bool() const {
    bool tmp=false;
    for(unsigned int i = 0 ; i < WORDS() ; ++i) {
      tmp = tmp || getWord(i);
    }
    return tmp;
  }

  /// Convert BitT to char
  operator char() const 	{ char x; fromBitT(x, "char"); return x;}
  /// Convert BitT to signed char
  operator signed char() const 	{ signed char x; fromBitT(x, "signed char"); return x;}
  /// Convert BitT to short
  operator short() const	{ short x; fromBitT(x, "short"); return x;}
  /// Convert BitT to int
  operator int() const  	{ int x; fromBitT(x, "int"); return x;}
  /// Convert BitT to long
  operator long() const 	{ long x; fromBitT(x, "long"); return x;}
  /// Convert BitT to long long
  operator long long() const 	{ long long x; fromBitT(x, "long long"); return x;}

  /// Convert BitT to unsigned char
  operator unsigned char() const	{ unsigned char x; fromBitT(x, "unsigned char"); return x;}
  /// Convert BitT to unsigned short
  operator unsigned short() const	{ unsigned short x; fromBitT(x, "unsigned short"); return x;}
  /// Convert BitT to unsigned int
  operator unsigned int() const		{ unsigned int x; fromBitT(x, "unsigned int"); return x;}
  /// Convert BitT to unsigned long
  operator unsigned long() const 	{ unsigned long x; fromBitT(x, "unsigned long"); return x;}
  /// Convert BitT to unsigned long long
  operator unsigned long long() const	{ unsigned long long x; fromBitT(x, "unsigned long long"); return x;}

  ///////////////////////////////////////////////////
  /// Accessor method to access byte
  /// \param byteidx -- the byte index which to set
  /// \param d -- the data to be assigned to byte byteidx
  void setByte( unsigned int byteidx, SceMiU32 d ) {
    if (byteidx >= BYTES()) {
      std::cerr << "illegal index, " << byteidx << " in method setByte of class " << showClassName() << "." << std::endl;
      throw std::out_of_range( "BitT::setByte()" );
    }
    unsigned char *data = reinterpret_cast<unsigned char*>(m_data);
    // Check tha no data is lost, mask data!
    unsigned int remaining = N - (BITSPERWORD() * (byteidx/4));
    if (remaining < 8u) {
      SceMiU32 pat  = (~(0xFF << remaining)) & 0xFF;
      SceMiU32 newd = d & pat;
      if (newd != d) {
	// Warn if high order bits are non-zero
	std::cerr << "Warning: " << showClassName() << " cannot represent 0x" << std::hex << d
		  << " as used in a setByte method, data truncated." << std::endl;
	d = newd;
      }
    }
    data[byteidx] = d & 0xFF;
  }

  /// Accessor method to access byte
  /// \param byteidx -- the byte index which to get
  /// \return the data from byteidx
  SceMiU32 getByte( unsigned int byteidx ) const {
    if (byteidx >= BYTES()) {
      std::cerr << "illegal index, " << byteidx << " in method getByte of class " << showClassName() << "." << std::endl;
      throw std::out_of_range( "BitT::getByte()" );
    }
    unsigned char *data = reinterpret_cast<unsigned char*>(const_cast<SceMiU32*>(m_data));
    return data[byteidx];
  }

  ///////////////////////////////////////////////////
  // Methods to get and set specific BITS of the data.
  /// Accessor method to access bits.
  /// \param bitidx -- the bit index which to set
  /// \param d -- the data bit to be assigned at bitidx
  /// Only the least significant bit is extracted.
 void setBit( unsigned int bitidx, SceMiU32 d ) {
    if (bitidx >= N) {
      std::cerr << "illegal index. " << bitidx << " in method setBit of class " << showClassName() << "." << std::endl;
      throw std::out_of_range( "BitT::setBit()" );
    }
    unsigned int idx = bitidx / BITSPERWORD();
    unsigned int off = bitidx - (idx * BITSPERWORD());
    unsigned int mask = ~(1 << off);
    unsigned int data = (d == 0) ? 0 : (1 << off);

    m_data[idx] = (mask & m_data[idx]) | data;
  }

  /// Accessor method to access bits
  /// \param bitidx -- the bit index which to set
  /// \return -- the bit from bitidx
  SceMiU32 getBit( unsigned int bitidx ) const {
    if (bitidx >= N) {
      std::cerr << "illegal index. " << bitidx << " in method setBit of class " << showClassName() << "." << std::endl;
      throw std::out_of_range( "BitT::getBit()" );
    }
    unsigned int idx = bitidx / BITSPERWORD();
    unsigned int off = bitidx - (idx * BITSPERWORD());

    return (m_data[idx] >> off) & 0x1;
  }


  /// Populates message buffer from object.
  /// A portion of the message buffer is populated from the contents
  /// of the class object.
  /// \param msg -- message buffer to be populated
  /// \param off -- initial bit offset into the message buffer
  /// \return the bit off for the next item in message buffer.
  unsigned int setMessageData ( SceMiMessageDataInterface &msg, const unsigned int off=0) const {
    unsigned int i, off2;
    for ( i = 0, off2 = 0 ; (i < WORDS()) && (off2 < N) ; off2 += BITSPERWORD(), ++i) {
      msg.SetBitRange(off+off2, (std::min)(N-off2, BITSPERWORD())-1,  m_data[i]);
    }
    return off + N;
  };

  /// overload the put-to operator for this class
  friend std::ostream & operator<< (std::ostream &os, const BitT &d) {
    BSVType::PutTo * override = lookupPutToOverride ( d.getClassName() ) ;
    if ( override != 0 ) {
      return override(os, d);
    }
    if ( N == 0) {
      os << std::dec << N << "'h0" << std::hex  ;
    }
    if ( N > 3 ) {
      os << std::dec << N << "'h" << std::hex  ;
    }
    unsigned int i, off2;
    for ( i = WORDS(), off2 = 0 ; (i > 0) && (off2 < N) ; off2 += BITSPERWORD(), --i) {
      if ( i == WORDS() ) {
        unsigned int nibbles = ((N+3)/4) - ( (BITSPERWORD()/4)*(WORDS()-1) ) ;
        os << std::setw(nibbles) ;
      } else {
        os << "_" << std::setw(BITSPERWORD()/4);
      }
      os << std::setfill('0') << d.m_data[i-1] ;
    }
    return os;
  };

  /// Adds to the stream the bit representation of this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  virtual std::ostream & getBitString (std::ostream &os) const {
    unsigned int i;

    for (i = N; (i>0) ; --i ) {
      unsigned int bitidx = i - 1;
      unsigned int wrdidx = bitidx/32;
      bitidx = bitidx & 0x01f;  // bitidx % 32
      SceMiU32 w = m_data[wrdidx];
      bool v = 0 != (w & (0x1 << bitidx));
      os << (v ? '1' : '0') ;
    }
    return os;
  }

  /// returns the bit representation as series of bits
  virtual std::ostream & getBitStringRange (std::ostream &os, unsigned int from,
					    unsigned int to) const {
    int i, f;

    f = from;
    if (to > N-1) {
      os << "BitT getBitStringRange(): to index is greater than number of bits.\n";
      return os;
    }
    for (i = to; (i>=f) ; i--) {
      unsigned int bitidx = i;
      unsigned int wrdidx = bitidx/32;
      bitidx = bitidx & 0x01f;  // bitidx % 32
      SceMiU32 w = m_data[wrdidx];
      bool v = 0 != (w & (0x1 << bitidx));
      os << (v ? '1' : '0') ;
    }
    return os;
  }


  /// returns the class name for this class
 static std::string showClassName () {
    std::ostringstream os;
    os  << "BitT<" << std::dec << N << ">" ;
    return os.str();
  }

  /// returns the class name for this object
  virtual const char * getClassName () const {
    return "BitT";
  }

  /// Accessor for the BSVType name for this object
  /// @param os -- the ostream object which to append
  /// @return the ostream object
  std::ostream & getBSVType (std::ostream &os) const {
    os << "Bit#(" << std::dec << N << ")" ;
    return os;
  }

  /// returns the size in bit of the packed object
  unsigned int getBitSize ()  const  {
    return N;
  }

  /// returns the BSVKind for this object
  virtual BSVKind getKind() const {
    return BSV_Primitive ;
  }

  /// Accessor for the count of members in object
  virtual unsigned int getMemberCount () const                { return 0; }

  /// Accessor to member objects
  /// @param idx -- member index
  /// @return BSVType * to this object or null
  virtual BSVType * getMember (unsigned int idx)              { return 0; };


  /// Accessor for symbolic member names
  /// @param idx -- member index
  /// @return char* to this name or null
  virtual const char * getMemberName (unsigned int idx) const { return 0; };
};
