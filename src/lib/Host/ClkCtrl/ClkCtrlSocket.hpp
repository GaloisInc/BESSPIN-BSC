// Copyright (c) 2013-2016, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// C++ transactor side for the ClkCtrlServer module in BSVSource
// which connects via a TCP socket

#include "ClkCtrl.hpp"

class ClkCtrlSocket: public ClkCtrl
{
private:
  int m_sockfd;

  // Mutex for the shared queue
  pthread_mutex_t m_status_queue_lock;
  // Signal to wakeup a waiting receiver
  mutable pthread_cond_t m_status_queue_cond;

  // Service thread state
  bool m_service_thread_initialized;
  int m_service_sockets[2];
  pthread_t m_service_threadid;

  void serviceLoop();

  // static member function for pthread_create call
  static void *startServiceThread( void * obj ) {
    ClkCtrlSocket *ptr = (ClkCtrlSocket *) obj;
    ptr->serviceLoop();
    return 0;
  }

public:
  ClkCtrlSocket(const unsigned int port);
  ~ClkCtrlSocket();

  void startServiceLoop();
  void stopServiceLoop();

private:
  // Disallow default and copy constructors
  ClkCtrlSocket & operator= (const ClkCtrlSocket &);
  ClkCtrlSocket( const ClkCtrlSocket &);

  void write_word(uint32_t w);

  void putStatusWord(unsigned int);
  void putStatus(ClkCtrlStatus s);

public:
  bool sendRun();
  bool sendRunN(unsigned int count);
  bool sendQuery();
  bool sendStop();
  bool sendResume();

  ClkCtrlStatus getStatusBlocking();
  bool getStatusNonBlocking(ClkCtrlStatus &s);
  // Timer specified as a delay relative to the current time
  bool getStatusTimed(ClkCtrlStatus &s, const time_t & delta_seconds, const long & delta_microseconds=0);
  // Timer specified with absolute time
  bool getStatusTimed(ClkCtrlStatus &s, struct timespec *expiration);
};
