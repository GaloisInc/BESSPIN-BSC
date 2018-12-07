
#include "SceMiPipeInfo.h"

SceMiPipeInfo::SceMiPipeInfo(SceMiPipe* pipe) {
  int depth  = scemi_pipe_get_depth(pipe);
  int bwidth = scemi_pipe_get_bytes_per_element(pipe);

  m_pipe       = pipe;
  m_depth      = depth;
  m_byte_width = bwidth;
  m_recvCount  = 0;
  m_sendCount  = 0;

  m_recvIdle   = 0;
  m_sendIdle   = 0;
  m_sent       = false;

  m_ptr  = (svBitVecVal*) malloc((depth + 1) * bwidth);

}


SceMiPipe* SceMiPipeInfo::getPipe() {
  return m_pipe;
}


bool SceMiPipeInfo::recv_empty () const {
  return m_recvDataQ.empty();
}

int SceMiPipeInfo::recv_count () const {
  return m_recvCount;
}

void SceMiPipeInfo::recv_push(const void *src, int n) {
  //  fprintf(stderr, "RECV_PUSH: %d\n", n);
  m_recvDataQ.push(src, (n * m_byte_width));
  m_recvCount = m_recvCount + n;
}

void SceMiPipeInfo::recv_pop(int n) {
  m_recvDataQ.pop(n * m_byte_width);
  m_recvCount = m_recvCount - n;
}

void SceMiPipeInfo::copyFromFront(void *p, int n) const {
  m_recvDataQ.copyFromFront(p, (n * m_byte_width));
}

svBitVecVal* SceMiPipeInfo::getPtr() {
  return m_ptr;
}

bool SceMiPipeInfo::send_empty () const {
  return m_sendDataQ.empty();
}

int SceMiPipeInfo::send_count () const {
  return m_sendCount;
}

void SceMiPipeInfo::send_push(const void *src, int n) {
  m_sendDataQ.push(src, (n * m_byte_width));
  m_sendCount = m_sendCount + n;
  //  fprintf(stderr, "SEND_PUSH: %d\n", n);
  m_sendIdle = 0;
}

void SceMiPipeInfo::send_pop(int n) {
  m_sendDataQ.pop(n * m_byte_width);
  m_sendCount = m_sendCount - n;
}

void SceMiPipeInfo::send_data() {
  int32_t sent = 0;
  //  if (m_sent) { fprintf(stderr, "SEND DATA! %d\n", m_sendCount); }
  if (send_count() == getDepth() || m_sendIdle >= getDepth()) { 
    m_sendDataQ.copyFromFront(m_ptr, send_count()*m_byte_width);
    sent = scemi_pipe_c_try_send(m_pipe, 0, send_count(), (const svBitVecVal*)m_ptr, 0);
    if (sent) {
      //      fprintf(stderr, "SENT %d %d %d\n", sent, m_sendCount, m_sendIdle);
      m_sent = true;
      send_pop(sent);
    } else {
      m_sendIdle++;
      if (m_sent) {
	//	fprintf(stderr, "IDLE %d %d\n", m_sendIdle, m_sendCount);
      }
    }
  } else {
    m_sendIdle++;
  }
}

void SceMiPipeInfo::send_data_immediate() {
  int32_t sent = 0;
  if (!send_empty()) {
    m_sendDataQ.copyFromFront(m_ptr, send_count()*m_byte_width);
    sent = scemi_pipe_c_try_send(m_pipe, 0, send_count(), (const svBitVecVal*)m_ptr, 0);
    while (true) {
      if (sent) {
	//	fprintf(stderr, "SENT! %d\n", sent);
	send_pop(sent);
	break;
      }
      sched_yield();
      sent = scemi_pipe_c_try_send(m_pipe, 0, send_count(), (const svBitVecVal*)m_ptr, 0);
    }
  }
  m_sendIdle = 0;
}
