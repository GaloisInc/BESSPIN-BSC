#ifndef __BLUENOC_THREAD_H__
#define __BLUENOC_THREAD_H__

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sched.h>
#include <bluenoc.h>
#include "thread.h"

class bluenoc_service_thread : public thread
{
private:
  bluenoc        *m_bluenoc;
  bool            m_bluenoc_loop_running;
  bool            m_stopping;

  static unsigned m_thread_count;

public:
  // Constructor registers the bluenoc pointer and starts the BlueNoC 
  // service loop in a separate thread.
  bluenoc_service_thread(bluenoc *pBlueNoC)
    : thread()
    , m_bluenoc(pBlueNoC)
  {
    if (m_thread_count > 0) {
      std::cerr << "bluenoc_service_thread new(): instancing more than one thread is not allowed!" << std::endl;
      exit(EXIT_FAILURE);
    }

    m_thread_count++;
    start();
  }

private:
  bluenoc_service_thread();
  bluenoc_service_thread( const bluenoc_service_thread &);

public:
  ~bluenoc_service_thread() 
  {
    if (m_running) {
      stop();
      join();
    }
    m_running = false;
  }

protected:
  virtual void main() 
  {
    int alive;
    while(!m_stop && !m_bluenoc->ServiceThreadStop()) {
      ++alive;
      m_bluenoc->ServiceLoop();
      sched_yield();
    }
    m_bluenoc->ServiceThreadStop(false);
    m_running = false;
    std::cerr << "BlueNoC service thread finished!" << std::endl;
  }
};

#endif // __BLUENOC_THREAD_H__
