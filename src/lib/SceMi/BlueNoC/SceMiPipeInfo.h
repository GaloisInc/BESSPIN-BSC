#ifndef __SCEMI_PIPE_INFO_H__
#define __SCEMI_PIPE_INFO_H__

#include "scemi.h"
#include "scemi_pipes.h"
#include "SceMiPipe.h"
#include "DataBufferQ.h"
#include <sched.h>

class SceMiPipeInfo {
 protected:

  SceMiPipe*         m_pipe;
  int                m_depth;
  int                m_byte_width;
  DataBufferQ        m_recvDataQ;
  DataBufferQ        m_sendDataQ;
  svBitVecVal*       m_ptr;
  int                m_recvCount;
  int                m_sendCount;
  int                m_recvIdle;
  int                m_sendIdle;
  bool               m_sent;

  /* const unsigned int m_pipe_num; */
  /* const unsigned int m_pipe_width; */
  /* const unsigned int m_pipe_depth; */

 public:
  SceMiPipeInfo(SceMiPipe* pipe);
  ~SceMiPipeInfo() { 
    free (m_ptr);
  };

  SceMiPipe*   getPipe();
  svBitVecVal* getPtr();

  bool recv_empty () const ;
  int  recv_count () const ;
  void recv_push (const void *src, int n);
  void recv_pop (int n);
  void copyFromFront (void *p, int n) const;
  int getDepth() { return m_depth; }
  int getByteWidth() { return m_byte_width; }

  bool send_empty () const ;
  int  send_count () const ;
  void send_push (const void *src, int n);
  void send_pop (int n);
  void send_data();
  void send_data_immediate();

};

#endif // __SCEMI_PIPE_H__
