//-*- C++ -*-x

#include "MsgPacket.h"


MsgPacket::MsgPacket()
  : m_data(0)
{}

MsgPacket::MsgPacket(unsigned int size_in_bytes)
  : m_data(size_in_bytes)
{
}

MsgPacket::MsgPacket(const MsgPacket &p)
  : m_data(p.m_data)
{
}

MsgPacket::~MsgPacket() {
}


// Allow access to some vector members
MsgPacket::size_type MsgPacket::size() const  {
  return m_data.size();
}

bool MsgPacket::empty () const {
  return m_data.empty();
}

void MsgPacket::clear () {
  m_data.clear();
}


void MsgPacket::resize (size_type n) {
  m_data.resize(n);
}

MsgPacket::MsgPacketData_t & MsgPacket::operator[] (size_type n) {
  return m_data[n];
}

const MsgPacket::MsgPacketData_t &  MsgPacket::operator[] (size_type n) const {
  return m_data[n];
}

bool MsgPacket::operator== (const MsgPacket &p) const {
  return (m_data == p.m_data);
}

bool MsgPacket::operator!= (const MsgPacket &p) const {
  return ! (*this == p);
}


void MsgPacket::SetBitRange(unsigned int i, unsigned int range, SceMiU32 bits, SceMiEC* ec){
  uncheckedSetBitRange(i, range,bits, (SceMiU32 *) &m_data[0]);
  if ((i+range) >= 8 * size() ) {
    std::cerr << "MsgPacket::SetBitRange out of bounds. "
              << std::dec << i << " + "
              << range << " >= 8 * " << size() << std::endl;
  }

}

SceMiU32 MsgPacket::GetBitRange(unsigned int i, unsigned int range, SceMiEC* ec) const{
  if ((i+range) >= 8 * size()) {
    std::cerr << "MsgPacket::SetBitRange out of bounds. "
              << std::dec << i << " + "
              << range << " >= 8 * " << size() << std::endl;
  }
  return uncheckedGetBitRange(i, range, reinterpret_cast<const SceMiU32 *>(&m_data[0]));
}

/*friend*/
std::ostream & operator<< (std::ostream &os, const MsgPacket &p) {
  os << "{MsgPacket {" ;
  for (unsigned int i = 0; i < p.size() ; ++ i) {
    if (i!=0) os << " " ;
    os << std::dec << "P[" << i << "] 0x" << std::hex << static_cast<unsigned> (p[i]) ;
  }
  os << "}}" ;
  return os;
}


unsigned char MsgPacket::getByte(size_type idx) const {
  MsgPacketData_t w = m_data[idx];
  return static_cast<unsigned char>(w);
}

void MsgPacket::setByte(size_type idx, unsigned char val) {
  m_data[idx] = val;
}


void MsgPacket::SetBlockRange( unsigned i, unsigned range, const char *data) {
  bool byteAligned = 0 == (i & 0x07);
  bool endAligned = (range != 0) && (0 == ((range+1) & 0x07));
  if (byteAligned && endAligned) {
    char *thisbase = (char *) & m_data[0];
    unsigned byte0 = i >> 3;
    unsigned byten = (range+1) >> 3;

    memcpy (thisbase + byte0, data, byten);
  } else {
    std::cerr << "MsgPacket::SetBlockRange only works with byte aligned offsets, "
              << std::dec << i << " " << range << " " << byteAligned
              << " " << endAligned << std::endl;
  }
}

void MsgPacket::GetBlockRange (unsigned i, unsigned range, char *data) const {
  bool byteAligned = 0 == (i & 0x07);
  bool endAligned = (range != 0) && (0 == ((range+1) & 0x07));
  if (byteAligned && endAligned) {
    const char *thisbase = reinterpret_cast<const char *> (& m_data[0]);
    unsigned byte0 = i >> 3;
    unsigned byten = (range+1) >> 3;
    memcpy (data, thisbase + byte0, byten);
  } else {
    std::cerr << "MsgPacket::GetBlockRange only works with byte aligned offsets, "
              << std::dec << i << " " << range << " " << byteAligned
              << " " << endAligned << std::endl;
  }
}

  /// Add an element to the end
void MsgPacket::push_back (const MsgPacketData_t &x) {
  m_data.push_back(x);
}

/// Reserve storage in the vector
void MsgPacket::reserve (const size_type n) {
  m_data.reserve(n);
}

// returns number of bytes copied
unsigned MsgPacket::copy (SceMiMessageData &msg) const {

  unsigned byteWidth  = (7+msg.WidthInBits())/8;

  for (unsigned i = 0; (4*i) < byteWidth; i++ ) {
    // Cast is safe as data is word aligned...
    const SceMiU32 *pdata = reinterpret_cast<const SceMiU32 *>( & m_data[4*i]);
    msg.Set (i, *pdata);
  }
  return byteWidth;
}
