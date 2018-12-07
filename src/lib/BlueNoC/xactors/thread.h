// Copyright (c) 2014, Bluespec, Inc.  ALL RIGHTS RESERVED

#ifndef __THREAD_H__
#define __THREAD_H__

#include <string>
#include <pthread.h>
#include <signal.h>

/// Pure virtual thread class.  Abstraction of a thread to a C++ object
class thread
{
protected:
  pthread_t      m_threadid;
  bool           m_stop;
  bool           m_running;

protected:
  thread()
    : m_threadid(0)
    , m_stop(false)
    , m_running(false)
  {
  }

public:
  virtual ~thread()
  {
  }

protected:
  virtual void main() = 0;
public:

  /// Start the thread based on the specific implementation.  Note that
  /// some implementations may start the thread in the constructor
  void start()
  {
    if (m_running) {
      throw std::string("thread is already running!");
    }
    m_running = true;
    if ( pthread_create( &m_threadid, 0, thread::start_thread, (void*)this ) ) {
      m_running = false;
      throw std::string("failed to create thread!");
    }
  }

  /// Set the stop flag to signal the thread to stop.  It is the 
  /// caller responsibility to periodically check this flag and
  /// ends its execution
  virtual void stop() 
  {
    m_stop = true;
  }

  int cancel() 
  {
    return pthread_cancel(m_threadid);
  }

  void testcancel() 
  {
    pthread_testcancel();
  }

#ifndef _WIN32
  /// Send a unix signal to this thread.  See man kill for possible signals.
  /// \param sig -- unix signal
  /// This member function is not supported on windows platforms
  int signal(int sig) 
  {
    m_running = false;
    return pthread_kill( m_threadid, sig );
  }
#endif

  /// Block until this thread has completed. Be sure that you have
  /// stopped the thread.
  void join() 
  {
    if (m_running) {
      pthread_join( m_threadid, 0 );
      m_running = false;
    }
  }

protected:
  // static member function for pthread_create call
  static void *start_thread( void * obj ) 
  {
    thread *ptr = (thread *) obj;
    ptr->main();
    return 0;
  }
};

#endif //__THREAD_H__
