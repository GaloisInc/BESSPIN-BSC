// Copyright (c) 2013-2016, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// C++ transactor side for a LuminaPlus control module accessed via socket

#include <deque>
#include <pthread.h>

#include "RdBackControl.hpp"

struct RdBackStatus {
  uint64_t cycle;
  bool rdback_on;
  bool running;
  bool free_running;
  unsigned int edges;
};

class LuminaPlusControl : public RdBackControl
{
private:
  int m_sockfd;

  // Queue for incoming status messages
  std::deque<RdBackStatus> m_status_queue;
  // Mutex for the shared queue
  pthread_mutex_t m_status_queue_lock;
  // Signal to wakeup a waiting receiver
  mutable pthread_cond_t m_status_queue_cond;

  // Storage for incoming status words
  // until a full packet is received
  std::deque<unsigned int> m_status_words;

  // Service thread state
  bool m_service_thread_initialized;
  int m_service_sockets[2];
  pthread_t m_service_threadid;

  void serviceLoop();

  // static member function for pthread_create call
  static void *startServiceThread( void * obj ) {
    LuminaPlusControl *ptr = (LuminaPlusControl *) obj;
    ptr->serviceLoop();
    return 0;
  }

public:
  LuminaPlusControl(const unsigned int port);
  ~LuminaPlusControl();

  void startServiceLoop();
  void stopServiceLoop();

private:
  // Disallow default and copy constructors
  LuminaPlusControl & operator= (const LuminaPlusControl &);
  LuminaPlusControl( const LuminaPlusControl &);

  void write_word(uint32_t w);

  void dispatchWord(uint32_t w);

  void putStatus(RdBackStatus s);

public:
  bool sendEmuEdges(unsigned int count);
  bool sendEmuQuery();
  bool sendEmuStop();
  bool sendEmuResume();
  bool sendRdBackOn();
  bool sendRdBackOff();

  RdBackStatus getStatusBlocking();
  bool getStatusNonBlocking(RdBackStatus &s);
  // Timer specified as a delay relative to the current time
  bool getStatusTimed(RdBackStatus &s, const time_t & delta_seconds, const long & delta_microseconds=0);
  // Timer specified with absolute time
  bool getStatusTimed(RdBackStatus &s, struct timespec *expiration);

  bool sendRdBackClear();
  bool sendRdBackStore(unsigned int code);
  bool sendRdBackFinish(unsigned int config);
  bool sendRdBackBreakCode(unsigned int code);
};
