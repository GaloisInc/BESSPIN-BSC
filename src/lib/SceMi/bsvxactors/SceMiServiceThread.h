// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// SceMi Service Thread class
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <sched.h>
#include <scemi.h>
#include "Thread.h"

class SceMiServiceThread : public Thread
{
 private:
  SceMi      *m_scemi;
  bool       m_scemilooprunning;
  bool       m_stopping;
  
  static unsigned m_thread_count;
  
 public:
  // Constructor registers the scemi pointer and starts the SceMi
  // service loop in a separate thread
  SceMiServiceThread( SceMi *pSceMi )
    : Thread ()
    , m_scemi(pSceMi)
  {
    if (m_thread_count > 0) {
      std::cerr << "SceMiServiceThread new(): instantiating more than one thread is not allowed"
		<< std::endl;
      exit(1);
    }
    m_thread_count++;
    start();
  }
 private:
  SceMiServiceThread();
  SceMiServiceThread( const SceMiServiceThread &);
  
 public:
  ~SceMiServiceThread() {
    if (m_running) {
      stop();
      join();
    }
    m_running = false;
  };

 protected:
  virtual void  main() {
    int alive;                  /* heartbeat useful for debug */
    while( ! m_stop && ! m_scemi->ServiceThreadStop()) {
      ++alive;
      m_scemi->ServiceLoop();
      sched_yield();
    }
    m_scemi->ServiceThreadStop(false);
    m_running = false;
    std::cerr << "SceMi Service thread finished!" << std::endl;
  }
};
