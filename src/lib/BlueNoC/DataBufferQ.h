//-*- C++ -*-x
#ifndef __DATA_BUFFER_Q_H__
#define __DATA_BUFFER_Q_H__

#include <list>
#include <cassert>


/// A Data container which implements a fast queue.
/// Optimized for inserting and dequeing data in memory blocks
/// Pushes are accomplished at the end of the memory, pops at the beginning
/// Data shifting is amortized over all pop operations.
class DataBufferQ {
public:
  typedef unsigned char                FQData_t;
  typedef unsigned                     size_type;

private:
  ///  The data container
  FQData_t       *m_data;
  /// The current size of the allocated buffer
  size_type      m_capacity;
  /// the head index
  size_type      m_head;
  /// the tail index
  size_type      m_tail;
  /// The number of elements in the Q.
  /// We track the size with an integral type to allow thread safer operations
  /// on size() calls;
  size_type      m_size;

public:
  /// Constructs an empty queue
  DataBufferQ();
  /// Destructor
  ~DataBufferQ();

  /// Remove all elements from the queue
  void clear ();

  /// Return a count of the number of elements inthe Q
  /// \return the size in elements of the queue
  size_type size() const ;
  /// Test for empty queue
  /// \return true of the queue is empty
  bool empty () const ;

  /// push the content of packet onto the Q
  /// \param src -- the data to be pushed
  /// \param byte_count -- the number of bytes to add to Q
  void push (const void *src, size_type byte_count);

  /// Copy n elements from the front of the queue to packet
  /// \param p -- the destination packet
  /// \param n -- the number of elements to copy
  void copyFromFront (void *p, size_type n) const;

  /// dequeue n elements from the queue
  /// \param n -- number of elements to remove
  void pop (size_type n);

  /// Peek at the first element in the queue
  const FQData_t & front() const ;

  /// Get a pointer to the data head of the queue
  /// \return point to the first element of the data
  FQData_t * getFrontPtr ();

  /// Dump some internal structures
  void debug();
private:
  /// Utility function to compress data to fron the of the queue
  void compress();
};

// end of message queue
class EomQ {
private:
  typedef struct {unsigned int cnt; bool eom; }  Data_t;

  std::list<Data_t>   m_queue;
public:
  EomQ () {};
  ~EomQ() {};
  void add_eom (bool eom, unsigned repeatCnt) {
    if (repeatCnt == 0) return;
    Data_t val;
    val.cnt = repeatCnt;
    val.eom = eom;
    if (m_queue.empty()) {
      m_queue.push_back(val);
    }
    else if (m_queue.back().eom == eom) {
     (m_queue.back().cnt) += repeatCnt;
    }
    else {
      m_queue.push_back(val);
    }
  }
  // Number of message available unto and including one with an EOM
  unsigned int next_eom () {
    if (m_queue.empty())      return 0;
    if (m_queue.front().eom)  return 1;
    unsigned int cnt = m_queue.front().cnt ;
    if ( (++(m_queue.begin())) != m_queue.end() ) ++cnt;
    return cnt;
  }

  bool take_eom (unsigned int n) {
    unsigned int take = n;
    bool ret = false;
    while (take != 0) {
      assert ( !m_queue.empty());
      unsigned int headcnt =  m_queue.front().cnt ;
      ret = m_queue.front().eom;
      if (headcnt <= take ) {
        take -= headcnt;
        m_queue.pop_front();
      }
      else {
        m_queue.front().cnt = headcnt - take;
        ret = m_queue.front().eom;
        take = 0;
      }
    }
    return ret;
  }
};


#endif
