
#include "DataBufferQ.h"
#include <string.h>             // memcpy
#include <stdio.h>

/// Constructs an empty queue
DataBufferQ::DataBufferQ()
  : m_data(0)
  , m_capacity(0)
  , m_head(0)
  , m_tail(0)
  , m_size(0)
{
  m_capacity = 8;
  m_data = new DataBufferQ::FQData_t [m_capacity];
}
/// Destructor
DataBufferQ::~DataBufferQ() {
  delete [] m_data;
}

/// Remove all elements from the queue
void DataBufferQ::clear () {
  m_head = 0;
  m_tail = 0;
  m_size = 0;
}

/// Return a count of the number of elements in the Queue.
/// This method is based on one C integral data type and should be atomic in its
/// read operation, even without thread locks.
/// \return the size in elements of the queue
DataBufferQ::size_type DataBufferQ::size() const {
  //assert (m_tail - m_head == m_size);
  return m_size;
}

bool DataBufferQ::empty () const {
  return m_size == 0;
}

/// push the content of packet onto the Q
/// \param p -- the packet to be pushed
void DataBufferQ::push (const void *p, size_type byte_count) {

  size_type adding = byte_count ;

  if (m_tail + adding > m_capacity) {
    // time to reallocate the buffer
    //fprintf(stderr, "Expanding Cap=%ld, Size=%ld, head=%ld tail=%ld Incr=%ld\n"
    //        , (long) m_capacity, (long) m_size, (long) m_head, (long) m_tail, (long) byte_count );
    
    while (m_capacity < (m_tail + adding) ) {
      m_capacity *= 2;
    }
    FQData_t *newData = new DataBufferQ::FQData_t [m_capacity];
    memcpy( newData, m_data + m_head , m_size * sizeof(DataBufferQ::FQData_t));

    delete [] m_data;
    m_data = newData;
    m_tail = m_size;
    m_head = 0;
  }

  memcpy( m_data + m_tail, p, adding);
  m_tail += adding;
  m_size += adding;
}

 /// Copy n elements from the front of the queue to packet
/// \param p -- the destination packet
/// \param n -- the number of bytes to copy
void DataBufferQ::copyFromFront (void *p, size_type n) const {
  assert (n <= size() );
  memcpy( p, m_data + m_head, n);
}

/// Peek at the first element in the queue
const DataBufferQ::FQData_t & DataBufferQ::front() const {
  assert (m_size > 0);
  return m_data[m_head];
}


/// dequeue n elements from the queue
/// \param n -- number of elements to remove
void DataBufferQ::pop (size_type n) {
  assert (n <= size() );
  m_head += n;
  m_size -= n;
  assert (m_tail - m_head == m_size);
  compress();
}

DataBufferQ::FQData_t * DataBufferQ::getFrontPtr () {
  return & m_data[m_head];
}


void DataBufferQ::debug() {
  fprintf(stderr, "DataBuffer Dump head:%d tail:%d size:%d cap:%d\n",
          m_head, m_tail, m_size, m_capacity );
  for (unsigned i = 0; i < m_size ; ++i ) {
    unsigned x = i+1;
    char sep = (x % 8 == 0 || (x == (m_size)) ) ? '\n' : ' ';
    fprintf(stderr, "%02x%c", m_data[ m_head + i], sep );
  }
}


void DataBufferQ::compress () {
  if (m_head == m_tail) {
    clear();
  }
  else {
    size_type cap = m_capacity;
    if ( (m_head > (cap/2)) && ( m_head + m_size > (3*cap/4) )  ) {
      //fprintf(stderr, "Compressing Cap=%ld, Size=%ld, head=%ld\n", (long) cap, (long) m_size, (long) m_head);
      memcpy( m_data, m_data + m_head, m_size * sizeof(FQData_t));
      m_tail = m_size;
      m_head = 0;
    }
  }
}

