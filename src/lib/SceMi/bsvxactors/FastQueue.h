//-*- C++ -*-x
#ifndef __FASTQUEUE_H__
#define __FASTQUEUE_H__

#include "scemi.h"
#include "MsgPacket.h"

/// A Data container which implements a fast queue.
/// Optimized for inserting and dequeing data in memory blocks, that is Packets.
/// Pushes are accomplished at the end of the memory, pops at the beginning
/// Data shifting is amortized over all pop operations.
class FastQueue {
public:
  typedef unsigned char                FQData_t;
  // Do not use size_t here because scemi_pipe_c_try_send is limited to int
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
  FastQueue();
  /// Destructor
  ~FastQueue();

  /// Remove all elements from the queue
  void clear ();

  /// Return a count of the number of elements inthe Q
  /// \return the size in elements of the queue
  size_type size() const ;
  /// Test for empty queue
  /// \return true of the queue is empty
  bool empty () const ;

  /// push the content of packet onto the Q
  /// \param p -- the packet to be pushed
  /// \param startFrom -- the first element of p which to take data
  void push (const MsgPacket &p, size_type startFrom = 0);

  /// push the content of the SceMiMessage onto the Q
  /// \param data -- the packet to be pushed
  void push (const SceMiMessageData *data);

  /// push raw byte data into the queue
  /// \param data -- data to be pushed
  /// \param n -- number of bytes
  void push (const unsigned char *data, size_type n);

  /// Copy n elements from the front of the queue to packet
  /// \param p -- the destination packet
  /// \param n -- the number of elements to copy
  void copyFromFront (MsgPacket &p, size_type n) const;

  /// dequeue n elements from the queue
  /// \param n -- number of elements to remove
  void pop (size_type n);

  /// take a msg worth of data from queue populating the message
  /// \param msg -- msg and width
  /// \return true is we created the message
  bool take (SceMiMessageData &msg);

  /// Peek at the first element in the queue
  const FQData_t & front() const ;

  /// Get a pointer to the data head of the queue
  /// \return point to the first element of the data
  FQData_t * getFrontPtr ();

  /// Get a pointer to the data head of the queue
  /// \return point to the first element of the data
  unsigned int* getFrontBitVec ();

private:
  /// Utility function to compress data to fron the of the queue
  void compress();
  /// Utility function to resize internal array
  void resize(size_type adding);
};

#endif //__FASTQUEUE_H__
