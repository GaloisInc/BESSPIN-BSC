//-*- C++ -*-x
#pragma once

#include "bsv_scemi.h"

#include "../bsvxactors/DBuffer.h"
#include <iostream>
#include "dllexport.h"

///  Data storage class represeting a TLMPayload holding data of
/// a particular size DATASIZE.
/// The underlying model is a dynamically sized vector of BitT<D>
/// objects which can be used as a data payload.   The implementation
/// is simply a vector of char to allow memcpy operations to and from
/// the payload object.
template <unsigned int DATASIZE, unsigned int USERDSIZE=0>
class DLLEXPORT TLMPayload : public BSVType {
private:
  /// The size in DATASIZE words of the payload
  unsigned m_size;
  /// container to store payload data
  DBuffer< char > m_localData;
  /// Alternate to store data as pointer to outside memory to avoid copy
  char *  m_externalData;

  /// container to store payload byte enable
  DBuffer< char > m_localBE;
  /// Alternate to store data as pointer to outside memory to avoid copy
  char *  m_externalBE;

  /// container for user data
  DBuffer< char > m_localUD;
  /// optional external pointer for user data
  char *  m_externalUD;


  /// indicator is payload is to have byte enables
  bool                 m_hasBE;

#ifdef _WIN32
  //This is a workaround for VC++ 2005 in which friend functions
  //seem not to be able to see protected static inherited
  static PutTo* BSVType_lookupPutToOverride(const char*cname) {
    return BSVType::lookupPutToOverride(cname);
  }
#endif

public:

  /// Empty constructor, building n size data
  /// \param n -- size of the payload
  /// \param hasByteEnables -- true to send byte enables
  TLMPayload(unsigned n=0, bool hasByteEnables=false);

  /// Constructor using external memory reference
  /// \param n -- size of the payload
  /// \param data -- pointer to block of data for payload
  /// \param beData -- pointer to block of data for payload
  TLMPayload(unsigned n, char * data, char *beData=0);


  ///  Constructor from Message stream.
  /// As this class is variable sized, it cannot be constructed like this,  the size
  /// must be known apriori
  /// TLMPayload ( const SceMiMessageDataInterface *msg, unsigned int &off ) ;


  ///  Move the data from the msg to this object starting at bit i.
  /// Populate the payload object from the msg object data starting at bit off.
  /// \param msg -- the message object
  /// \param off -- the initial bit offset into the message
  /// \return the next bit position in the messag buffer
  unsigned int unpackPayload (const SceMiMessageDataInterface *msg, unsigned int &off) ;


  ///  Populates message buffer from object's payload field.
  /// A portion of the message buffer is populated from the payload
  /// field of the class object.
  /// \param msg -- message buffer to be populated
  /// \param off -- initial bit offset into the message buffer
  /// \return the bit off for the next item in message buffer.
  unsigned int setMessageData (SceMiMessageDataInterface &msg, const unsigned int off=0) const ;

  ////////////////////////////////////////////////////////////////////////////////////
  /// Copy to the payload object from a memory pointer.
  /// Copy upto sz bytes of data from src into the payload object.  Not that if the
  /// payload is smaller than sz, fewer bytes are copied.  This does
  /// not change the size of the payload.
  /// \param src -- the source pointer for the data
  /// \param sz -- the max
  /// \return returns the number of bytes copied
  unsigned fillPayload (const char *src, unsigned sz) ;

  /// Set the payload object from a memory pointer.
  /// This method stores the pointer to the original data avoiding a copy of the data into
  /// the payload object.  This does
  /// not change the size of the payload.
  /// \param src -- the source pointer for the data
  void setPayloadData (char *src) ;

  ///  Copies data from the payload to a memory buffer.
  /// \param dst -- the destination memory location
  /// \param sz -- maximum number of bytes to copy
  /// \return the number of bytes copies to dst.
  unsigned fillMemory (char *dst, unsigned sz) const ;


  ///  Load the byte enable portion of payload object from a memory pointer.
  /// Copy upto sz bytes of data from src into the payload object.  Not that if the
  /// payload is smaller than sz, fewer bytes are copied.  This does
  /// not change the size of the payload.
  /// if byte enables are not expected 0 is returned
  /// \param src -- the source pointer for the data
  /// \param sz -- the max
  /// \return returns the number of bytes copied
  unsigned fillByteEnables (const char *src, unsigned sz) ;

  /// Set the payload object from a memory pointer.
  /// This method stores the pointer to the original data avoiding a copy of the data into
  /// the payload object.  This does
  /// not change the size of the payload.
  /// \param src -- the source pointer for the data
  void setByteEnables (char *src) ;

  ///  Copies data from the payload byte enables to a memory buffer.
  /// \param dst -- the destination memory location
  /// \param sz -- maximum number of bytes to copy
  /// \return the number of bytes copies to dst.
  unsigned fillMemoryFromByteEnables (char *dst, unsigned sz) const ;

  //////////////////////////////////////////////////////////////////////////////////////
  /// Utility member function
  //////////////////////////////////////////////////////////////////////////////////////

  /// returns the number of D-sized words in the payload.
  unsigned size() const  ;
  /// returns the number of byte enable bit
  unsigned byteEnableBits() const;
  /// returns the number of byte enable bytes
  unsigned byteEnableBytes() const;
  /// returns true if the payload size is 0
  bool empty() const ;
  /// clears the data in the payload --setting the size to 0
  void clear() ;
  /// resizes the payload to the sz words each D bits wide
  void resize (unsigned sz, bool useByteEnables) ;
  /// return true if the data payload included byte enables
  bool hasByteEnables() const ;

  /// returns the size of the data payload in bytes
  unsigned getDataSizeInBytes() const;

  /// returns the size of the BE payload in true (unpacked) bytes
  unsigned getBESizeInBytes() const;

  /// returns the size of the BE payload in true (unpacked) bytes
  unsigned getUDSizeInBytes() const;

  /// Get a pointer to the payload data at element idx
  char *getDataPtr (unsigned idx) ;
  /// Get a pointer to the payload data at element idx
  const char *getDataPtr (unsigned idx) const ;

  /// Get a pointer to the byte enable data at element 0
  const char *getBEPtr () const ;

  /// Get a pointer to the byte enable data at element 0
  const char *getUDPtr () const ;


  /// sets the idx word with d
  void setFromBitT(unsigned idx, const BitT<DATASIZE> & d) ;
  ///  Create a BitT object from the idx position in the payload.
  /// Note that an object is returned, and not a reference, since the
  /// payload class does not store BitT objects, only their data.
  BitT<DATASIZE> getBitT(unsigned idx) const;

  /// \brief sets the byte enable for the idx word with d
  void setBEFromBitT(unsigned idx, const BitT<DATASIZE/8> & d) ;

  ///  Create a BitT object from the idx position of the payload byte enable
  /// Note that an object is returned, and not a reference, since the
  /// payload class does not store BitT objects, only their data.
  BitT<DATASIZE/8> getBEBitT(unsigned idx) const;

  BitT<USERDSIZE> getUDBitT(unsigned idx) const ;
  void setUDFromBitT(unsigned idx, const BitT<USERDSIZE> & d) ;

  ///////////////////////////////////////////////////////////////////////////////////
  template<unsigned int D, unsigned int U>
    friend DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<D,U> &obj) ;

  virtual std::ostream & getBitString (std::ostream & os) const ;
  virtual std::ostream & getBSVType (std::ostream & os) const ;
  virtual unsigned int getBitSize () const ;
  virtual const char * getClassName() const ;
  virtual BSVKind getKind() const ;
  virtual unsigned int getMemberCount() const ;
  virtual BSVType * getMember (unsigned int idx) ;
  virtual const char * getMemberName (unsigned int idx) const ;

  /// prints the payload to the log file
  void debug () const;

};
