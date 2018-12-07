
#include "TLMPayload.h"
#include "../bsvxactors/XactorLog.h"
#include "dllexport.h"

static unsigned int next32(unsigned int x) {
  return 32*((x+31)/32);
}

///////////////////////////////////////////////////////////////////////////
/// \brief Empty constructor
template <unsigned int D, unsigned int U>
TLMPayload<D,U>::TLMPayload(unsigned n, bool hasByteEnable)
  : m_size(n)
  , m_localData(0)
  , m_externalData(0)
  , m_localBE(0)
  , m_externalBE(0)
  , m_localUD(0)
  , m_externalUD(0)
  , m_hasBE(hasByteEnable)
{
  resize(n, hasByteEnable);
}

template <unsigned int D, unsigned int U>
TLMPayload<D,U>::TLMPayload(unsigned n, char * data, char *beData)
  : m_size(n)
  , m_localData(0)
  , m_externalData(data)
  , m_localBE(0)
  , m_externalBE(beData)
  , m_localUD(0)
  , m_externalUD(0)
  , m_hasBE(0)
{
  resize(n, beData!=0);
}


/// \brief Constructor from Message stream
///
/// As this class is variable sized, it cannot be constructed like this,  the size
/// must be known apriori
/// TLMPayload ( const SceMiMessageDataInterface *msg, unsigned int &off ) ;


/// \brief Move the data from the msg to this object starting at bit i
template <unsigned int D, unsigned int U>
unsigned int TLMPayload<D,U>::unpackPayload (const SceMiMessageDataInterface *msg, unsigned int &off) {
  if (empty()) return off;

  m_externalData = 0;
  m_externalBE   = 0;
  m_externalUD   = 0;
  
  resize(m_size, m_hasBE);

  bool aligned = 0 == (off & 0x07);

  if (m_hasBE || (U != 0)) {
    for (unsigned i = 0; i < size(); ++i ) {
      if (U != 0) {
        BitT<U> ud(msg, off);
        setUDFromBitT(i, ud);
        off = next32(off);
      }

      BitT<D> x(msg,off);
      setFromBitT(i,x);

      if (m_hasBE) {
        BitT<D/8> be(msg,off);
        setBEFromBitT(i,be);
      }
      off = next32(off);

    }

  }
  else if (D < 32) {                 // UNPACKED 8/16
    for (unsigned i = 0; i < size(); ++i ) {
      BitT<D> x(msg,off);
      setFromBitT(i,x);
      off = next32(off);
    }
  }
  else if (aligned) {
    unsigned bitend = size() * D;

    msg->GetBlockRange(off, bitend-1, getDataPtr(0));
    off += bitend;
  }
  else {
    XactorLog logfile;
    logfile.Error ( "impossible! unaligned transfer in TLMPayload::unpackPayload") ;
  }
  off = next32(off);
  return off;
}

/// \brief Set msg with the payload
/// \brief Populates message buffer from object's payload field
///
/// A portion of the message buffer is populated from the payload
/// field of the class object.
/// \param msg -- message buffer to be populated
/// \param off -- initial bit offset into the message buffer
/// \return the bit off for the next item in message buffer.
template <unsigned int D, unsigned int U>
unsigned int TLMPayload<D,U>::setMessageData (SceMiMessageDataInterface &msg, const unsigned int off) const {
  if (empty()) return next32(off);

  unsigned running = off;
  bool aligned = 0 == (off & 0x07);
  if (m_hasBE || (U != 0)) {
      for (unsigned i = 0; i < size(); ++i ) {
        if (U != 0) {
          BitT<U> ud = getUDBitT(i);
          running = ud.setMessageData(msg, running);
          running = next32(running);
        }

        BitT<D> x(getDataPtr(i));
        running = x.setMessageData(msg, running);

        if (m_hasBE) {
          BitT<D/8> be = getBEBitT(i);
          running = be.setMessageData(msg, running);
        }
        running = next32(running);

      }
  }
  else if (D < 32) {                 // UNPACKED 8/16
      for (unsigned i = 0; i < size(); ++i ) {
        BitT<D> x(getDataPtr(i));
        running = x.setMessageData(msg, running);
        running = next32(running);
      }
  }
  else if (aligned) {
    // packed data
    unsigned numbits = size() * D;
    msg.SetBlockRange(off, numbits-1, getDataPtr(0));
    running += numbits;
  }
  else {
    XactorLog logfile;
    logfile.Error ( "impossible! unaligned transfer in TLMPayload::setMessageData") ;
  }
  return next32(running);
}

////////////////////////////////////////////////////////////////////////////////////
/// \brief load the payload via a memcopy
///
template <unsigned int D, unsigned int U>
unsigned TLMPayload<D,U>::fillPayload (const char *src, unsigned sz) {
  m_externalData = 0;
  resize(m_size, m_hasBE);

  unsigned maxsz = (std::min)( sz, size() * (D/8));
  memcpy ( &m_localData[0], src, maxsz );
  return maxsz;
}

template <unsigned int D, unsigned int U>
void TLMPayload<D,U>::setPayloadData (char *src) {
  resize(m_size, m_hasBE);

  m_externalData = src;
}

template <unsigned int D, unsigned int U>
unsigned TLMPayload<D,U>::fillMemory (char *dst, unsigned sz) const {
  unsigned maxsz = (std::min)( sz, size() * (D/8));
  memcpy (dst, getDataPtr(0), maxsz );
  return maxsz;
}

template <unsigned int D, unsigned int U>
unsigned TLMPayload<D,U>::fillByteEnables (const char *src, unsigned sz) {
  m_externalBE = 0;
  resize(m_size, m_hasBE);
  unsigned maxsz = (std::min)( sz, byteEnableBytes() );
  if (maxsz != 0) {
    memcpy ( &m_localBE[0], src, maxsz );
  }
  return maxsz;
}


template <unsigned int D, unsigned int U>
void TLMPayload<D,U>::setByteEnables (char *src) {
  resize(m_size, m_hasBE);

  m_externalBE = src;
}


template <unsigned int D, unsigned int U>
unsigned TLMPayload<D,U>::fillMemoryFromByteEnables (char *dst, unsigned sz) const {
  unsigned maxsz = (std::min)( sz, byteEnableBytes() );
  if (maxsz > 0 ) {
    memcpy (dst, getBEPtr(), maxsz );
  }
  return maxsz;
}


template <unsigned int D, unsigned int U>
void TLMPayload<D,U>::setFromBitT(unsigned idx, const BitT<D> & d) {
  if (m_externalData) {
    // Change data mode !!
    const char * xptr =  m_externalData;
    m_externalData = 0;
    resize(m_size, m_hasBE);
    memcpy(&m_localData[0], xptr, getDataSizeInBytes());
  }
  char * pdata = getDataPtr(idx);
  memcpy(pdata, (const char *) & d.m_data[0], D/8);
}

template <unsigned int D, unsigned int U>
BitT<D> TLMPayload<D,U>::getBitT(unsigned idx) const {
  BitT<D> x(getDataPtr(idx));
  return x;
}


template <unsigned int D, unsigned int U>
void TLMPayload<D,U>::setBEFromBitT(unsigned idx, const BitT<D/8> & d) {
  if (!m_hasBE) return ;

  if (m_externalBE) {
    // Change data mode !!
    const char * xptr =  m_externalBE;
    m_externalBE = 0;
    resize(m_size, m_hasBE);
    memcpy(&m_localBE[0], xptr, getBESizeInBytes());
  }

  // Byte enables are not aligned when data < 64 bits
  if (D < 64) {
    unsigned DDDD = (std::min)(D, 32u);
    unsigned mod = 64/D;          // 8, 4, 2.
    unsigned word = idx / mod;
    unsigned bit  = (idx % mod) * (D/8);
    unsigned  maskX = (1 << (DDDD/8))  - 1;
    char mask = (char) (maskX << bit) ;
    char x  = m_localBE[word];
    x = static_cast<char> ((x & ~mask) | (((char) d) << bit));
    m_localBE[word] = x;
  }
  else {
    char * pdata = &m_localBE[idx * (D/64)] ;
    memcpy(pdata, (const char *) & d.m_data[0], D/64);
  }
}

template <unsigned int D, unsigned int U>
BitT<D/8> TLMPayload<D,U>::getBEBitT(unsigned idx) const {
  BitT<D/8> ret = 0;
  if (!m_hasBE) return ret ;

  // Byte enables are not aligned when data < 64 bits
  if (D < 64) {
    unsigned DDDD = (std::min)(D, 32u);
    unsigned mod = 64/D;          // 8, 4, 2.
    unsigned word = idx / mod;
    unsigned bit  = (idx % mod) * (D/8);
    unsigned maskX = (1 << DDDD/8) - 1;
    char mask = (char) (maskX << bit) ;
    char x  = *(getBEPtr() + word);
    x = static_cast<char> ((x & mask) >> bit);
    ret = BitT<D/8>(x);
  }
  else {
    const char * pdata = getBEPtr() + (idx * (D/64)) ;
    ret = BitT<D/8> (pdata);
  }
  return ret;
}

template <unsigned int D, unsigned int U>
void TLMPayload<D,U>::setUDFromBitT(unsigned idx, const BitT<U> & d) {
  if (U == 0) return ;

  if (m_externalUD) {
    // Change data mode !!
    const char * xptr =  m_externalUD;
    m_externalUD = 0;
    resize(m_size, m_hasBE);
    memcpy(&m_localUD[0], xptr, getUDSizeInBytes());
  }

  char * pdata = &m_localUD[idx * (U/8)] ;
  memcpy(pdata, (const char *) & d.m_data[0], U/8);

}

template <unsigned int D, unsigned int U>
BitT<U> TLMPayload<D,U>::getUDBitT(unsigned idx) const {
  BitT<U> ret = 0;
  if (U == 0) return ret ;

  const char * pdata = getUDPtr() + (idx * (U/8)) ;
  ret = BitT<U> (pdata);

  return ret;
}



template <unsigned int D, unsigned int U>
char * TLMPayload<D,U>::getDataPtr (unsigned idx) {
  char * xptr = 0;
  if (m_externalData) {
    xptr = (idx*(D/8)) + (char *) m_externalData;
  }
  else{
    xptr = & m_localData[idx*(D/8)];
  }
  return xptr;
}

template <unsigned int D, unsigned int U>
const char * TLMPayload<D,U>::getDataPtr (unsigned idx) const {
  const char * xptr = 0;
  if (m_externalData) {
    xptr = (idx*(D/8)) + m_externalData;
  }
  else{
    xptr = & m_localData[idx*(D/8)];
  }
  return xptr;
}

/// Get a pointer to the byte enable data at element 0
template <unsigned int D, unsigned int U>
const char * TLMPayload<D,U>::getBEPtr () const {
  const char * xptr = 0;
  if (m_hasBE) {
    if (m_externalBE) {
      xptr = m_externalBE;
    }
    else{
      xptr = & m_localBE[0];
    }
  }
  return xptr;
}


/// Get a pointer to the byte enable data at element 0
template <unsigned int D, unsigned int U>
const char * TLMPayload<D,U>::getUDPtr () const {
  const char * xptr = 0;
  if (U != 0) {
    if (m_externalUD) {
      xptr = m_externalUD;
    }
    else{
      xptr = & m_localUD[0];
    }
  }
  return xptr;
}


template <unsigned int D, unsigned int U>
unsigned TLMPayload<D,U>::size() const  {
  return (m_size);
}

/// returns the number of byte enable bit
template <unsigned int D, unsigned int U>
unsigned TLMPayload<D,U>::byteEnableBits() const {
  return m_hasBE ? size() * (D/8) : 0;
}

/// returns the number of byte enable bytes
template <unsigned int D, unsigned int U>
unsigned TLMPayload<D,U>::byteEnableBytes() const {
  return (byteEnableBits()+7)/8;
}


template <unsigned int D, unsigned int U>
void TLMPayload<D,U>::clear()       {
  m_size = 0;
  m_localData.clear(); 
  m_localBE.clear();
}


template <unsigned int D, unsigned int U>
bool TLMPayload<D,U>::hasByteEnables() const { return m_hasBE; }

template <unsigned int D, unsigned int U>
bool   TLMPayload<D,U>::empty() const { return m_size == 0; }

template <unsigned int D, unsigned int U>
void TLMPayload<D,U>::resize (unsigned sz, bool useByteEnables) {
  m_size = sz;
  m_hasBE = useByteEnables;
  if (0 == m_externalData) {
    m_localData.resize(getDataSizeInBytes());
  }
  if (m_hasBE && (0 == m_externalBE)) {
    m_localBE.resize (getBESizeInBytes());
  }
  if (U != 0 && (0 == m_externalUD)) {
    m_localUD.resize (getUDSizeInBytes());
  }
}


template <unsigned int D, unsigned int U>
std::ostream & TLMPayload<D,U>::getBitString (std::ostream & os) const {
  // UNPACKED 8/16
  std::string pad;
  if (D < 32) pad.resize(32-D,'_');

  std::string padBE;
  if (D < 64) padBE.resize(32-(D/8),'_');

  // this is backwards  higher index first...
  for (int i= size() - 1 ; i >= 0; --i) {
    if (D < 32) os << pad;
    BitT<D> x(getDataPtr(i));
    x.getBitString(os);

    if (m_hasBE) {
      if (D < 64) os << padBE;
      BitT<D/8> be = getBEBitT(i);
      be.getBitString(os);
    }

    if (U != 0) {
      BitT<U> ud = getUDBitT(i);
      ud.getBitString(os);
    }

  }
  return os;
}
template <unsigned int D, unsigned int U>
std::ostream & TLMPayload<D,U>::getBSVType (std::ostream & os) const {
  os << std::dec << "TLMPayload #(" <<  D << " )";
  return os;
}

template <unsigned int D, unsigned int U>
unsigned int TLMPayload<D,U>::getBitSize () const {
  // UNPACKED 8/16
  unsigned bes = (m_hasBE) ? D/8 : 0;
  unsigned Dx = next32(D + bes) + U ;
  return Dx * size();
}
template <unsigned int D, unsigned int U>
const char * TLMPayload<D,U>::getClassName() const {
  return "TLMPayload";
}

template <unsigned int D, unsigned int U>
BSVType::BSVKind TLMPayload<D,U>::getKind() const {
  return BSVType::BSV_Payload ;
}

template <unsigned int D, unsigned int U>
unsigned int TLMPayload<D,U>::getMemberCount() const {
  return size() ;
};
template <unsigned int D, unsigned int U>
BSVType * TLMPayload<D,U>::getMember (unsigned int idx) {
  // use a static here so we can return a point to a real object...
  static BitT<D> staticX;
  staticX = getBitT(idx);
  return & staticX;
}
template <unsigned int D, unsigned int U>
const char * TLMPayload<D,U>::getMemberName (unsigned int idx) const {
  return "payload";
}
template <unsigned int D, unsigned int U>
void TLMPayload<D,U>::debug () const {
  std::stringstream oss;
#if defined(_WIN32) && _MSC_VER<1500
  //error C2593: 'operator <<' is ambiguous
  //bug in Microsoft Visual Studio 2005, fixed in MS VS 2008
  oss << "[Payload]";
#else
  oss << *this;
#endif
  XactorLog logfile;
  logfile.Debug("%s", oss.str().c_str() );
}

// ///////////////////////////////////////////////////////////////////////////////////
template <unsigned int D, unsigned int U>
DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<D,U> &obj) {
#ifdef _WIN32
  BSVType::PutTo * override = TLMPayload<D,U>::BSVType_lookupPutToOverride ( obj.getClassName() );
#else
  BSVType::PutTo * override = BSVType::lookupPutToOverride ( obj.getClassName() );
#endif

  if ( override != 0 ) {
    return override(os, obj );
  }
  os << "{" ;
  for (unsigned int i=0; i < obj.size(); ++i ) {
    if (i!= 0) os << " ";
    BitT<D> x = obj.getBitT(i);
    os << std::dec << "V[" << i << "] " << x ;
    if (obj.hasByteEnables()) {
      BitT<D/8> be = obj.getBEBitT(i);
      os << std::dec << " BE[" << i << "] " << be ;
    }

    if (U != 0) {
      BitT<U> ud = obj.getUDBitT(i);
      os << std::dec << " UD[" << i << "] " << ud ;
    }
  }
  os << "}" ;
  return os;
}



/// returns the size of the data payload in bytes
template <unsigned int D, unsigned int U>
unsigned  TLMPayload<D,U>::getDataSizeInBytes() const {
  return m_size * (D/8u);
}

/// returns the size of the data payload in bytes
template <unsigned int D, unsigned int U>
unsigned  TLMPayload<D,U>::getUDSizeInBytes() const {
  return m_size * (U/8u);
}

/// returns the size of the BE payload in bytes
template <unsigned int D, unsigned int U>
unsigned  TLMPayload<D,U>::getBESizeInBytes() const {
  return byteEnableBytes();
}

//order matters.  The operator<< must be defined before TLMPayload for VC++2005.
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<8,0> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<16,0> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<32,0> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<64,0> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<128,0> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<256,0> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<512,0> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<1024,0> &obj);

template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<8,32> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<16,32> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<32,32> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<64,32> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<128,32> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<256,32> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<512,32> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<1024,32> &obj);

template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<8,64> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<16,64> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<32,64> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<64,64> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<128,64> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<256,64> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<512,64> &obj);
template DLLEXPORT std::ostream & operator<< (std::ostream &os, const TLMPayload<1024,64> &obj);

template class DLLEXPORT TLMPayload<8,0>;
template class DLLEXPORT TLMPayload<16,0>;
template class DLLEXPORT TLMPayload<32,0>;
template class DLLEXPORT TLMPayload<64,0>;
template class DLLEXPORT TLMPayload<128,0>;
template class DLLEXPORT TLMPayload<256,0>;
template class DLLEXPORT TLMPayload<512,0>;
template class DLLEXPORT TLMPayload<1024,0>;

template class DLLEXPORT TLMPayload<8,32>;
template class DLLEXPORT TLMPayload<16,32>;
template class DLLEXPORT TLMPayload<32,32>;
template class DLLEXPORT TLMPayload<64,32>;
template class DLLEXPORT TLMPayload<128,32>;
template class DLLEXPORT TLMPayload<256,32>;
template class DLLEXPORT TLMPayload<512,32>;
template class DLLEXPORT TLMPayload<1024,32>;

template class DLLEXPORT TLMPayload<8,64>;
template class DLLEXPORT TLMPayload<16,64>;
template class DLLEXPORT TLMPayload<32,64>;
template class DLLEXPORT TLMPayload<64,64>;
template class DLLEXPORT TLMPayload<128,64>;
template class DLLEXPORT TLMPayload<256,64>;
template class DLLEXPORT TLMPayload<512,64>;
template class DLLEXPORT TLMPayload<1024,64>;
