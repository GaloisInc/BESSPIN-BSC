
#include "FastQueue.h"
#include <cassert>

  /// Constructs an empty queue
FastQueue::FastQueue()
  : m_data(0)
  , m_capacity(0)
  , m_head(0)
  , m_tail(0)
  , m_size(0)
{
  m_capacity = 4*2048;
  m_data = new FastQueue::FQData_t [m_capacity];
}
  /// Destructor
FastQueue::~FastQueue() {
  delete [] m_data;
}

  /// Remove all elements from the queue
void FastQueue::clear () {
  m_head = 0;
  m_tail = 0;
  m_size = 0;
}

/// Return a count of the number of elements in the Queue.
/// This method is based on one C integral data type and should be atomic in its
/// read operation, even without thread locks.
/// \return the size in elements of the queue
FastQueue::size_type FastQueue::size() const {
  //assert (m_tail - m_head == m_size);
  return m_size;
}

bool FastQueue::empty () const {
  return m_size == 0;
}

/// push the content of packet onto the Q
/// \param p -- the packet to be pushed
void FastQueue::push (const MsgPacket &p, size_type startFrom) {

  size_type adding = p.size() - startFrom ;
  push ( & p[startFrom], adding);
}

void FastQueue::push (const unsigned char *p, size_type adding ) {
  if (adding == 0) return;
  resize (adding);

  memcpy( & m_data[m_tail], p, adding);
  m_tail += adding;
  m_size += adding;
}

void FastQueue::push (const SceMiMessageData *data) {
  unsigned byteWidth = (data->WidthInBits()+7)/8;
  unsigned wordWidth = data->WidthInWords();
  resize(byteWidth);

  SceMiU32 *warray = new SceMiU32 [wordWidth];
  for (unsigned i = 0 ; i < wordWidth; ++i ) {
    SceMiU32 d = data->Get(i);
    warray[i] = d;
  }
  push ( reinterpret_cast<const unsigned char *> (warray), byteWidth);
  delete[] warray;
}


/// Copy n elements from the front of the queue to packet
/// \param p -- the destination packet
/// \param n -- the number of elements to copy
void FastQueue::copyFromFront (MsgPacket &p, size_type n) const {
  assert (n <= size() );
  MsgPacket::size_type ptail = p.size();
  p.resize(ptail + n);
  memcpy( &p[ptail], &m_data[m_head], n * sizeof(FastQueue::FQData_t));
}

/// Peek at the first element in the queue
const FastQueue::FQData_t & FastQueue::front() const {
  assert (m_size > 0);
  return m_data[m_head];
}


/// dequeue n elements from the queue
/// \param n -- number of elements to remove
void FastQueue::pop (size_type n) {
  assert (n <= size() );
  m_head += n;
  m_size -= n;
  assert (m_tail - m_head == m_size);
  compress();
}

bool FastQueue::take( SceMiMessageData &msg) {
  unsigned byteWidth = (msg.WidthInBits()+7)/8;
  unsigned wordWidth = msg.WidthInWords();

  bool taken = false;
  if (size() >= byteWidth) {
    SceMiU32 *warray = new SceMiU32 [wordWidth];
    memcpy ( warray, & m_data[m_head], wordWidth * 4);

    for (unsigned i = 0; i < wordWidth ; ++i ) {
      msg.Set(i, warray[i] );
    }
    m_head += byteWidth;
    m_size -= byteWidth;
    taken = true;
    delete[] warray;

  }
  return taken;
}


FastQueue::FQData_t * FastQueue::getFrontPtr () {
  return & m_data[m_head];
}

unsigned int* FastQueue::getFrontBitVec () {
  // A somewhat dangerous cast, but our scemi used this as byte
  return reinterpret_cast<unsigned int*> (& m_data[m_head]);
}


void FastQueue::compress () {
  if (m_head == m_tail) {
    clear();
  }
  else {
    size_type cap = m_capacity;
    if ( (m_head > (cap/2)) && ( m_head + m_size > (3*cap/4) )  ) {
      //fprintf(stderr, "Compressing Cap=%ld, Size=%ld, head=%ld\n", (long) cap, (long) m_size, (long) m_head);
      memcpy( &m_data[0], &m_data[m_head], m_size * sizeof(FQData_t));
      m_tail = m_size;
      m_head = 0;
    }
  }
}


void FastQueue::resize (size_type adding) {
  if (m_tail + adding > m_capacity) {
    // time to reallocate the buffer
    //fprintf(stderr, "Expanding Cap=%ld, Size=%ld, head=%ld tail=%ld Incr=%ld\n"
    //, (long) m_capacity, (long) m_size, (long) m_head, (long) m_tail, (long) p.size() );

    while (m_capacity < (m_tail + adding) ) {
      m_capacity *= 2;
    }
    FQData_t *newData = new FastQueue::FQData_t [m_capacity];

    memcpy( newData, & m_data[m_head] , m_size * sizeof(FastQueue::FQData_t));

    delete [] m_data;
    m_data = newData;
    m_tail = m_size;
    m_head = 0;
  }
}
