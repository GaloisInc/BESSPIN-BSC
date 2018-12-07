// Copyright Bluespec Inc. 2011-2013

#pragma once

using namespace std;

// This is a simple template and example of the generated api calls
// for contructing testbench to interact with the user defined DUT

// Bluespec common code
#include "bsdebug_common.h"

// Forward declare the actual user testbench function
int do_test();

class UserTBServiceThread : public Thread
{
 private:

  static unsigned m_thread_count;

 public:
  // Constructor registers the scemi pointer and starts the SceMi
  // service loop in a separate thread
  UserTBServiceThread()
    : Thread ()
  {
    if (m_thread_count > 0) {
      std::cerr << "UserTBServiceThread new(): instantiating more than one thread is not allowed"
                << std::endl;
      exit(1);
    }
    m_thread_count++;
    start();
  }
 private:
  UserTBServiceThread( const UserTBServiceThread &);

 public:
  ~UserTBServiceThread() {
    if (m_running) {
      stop();
      join();
    }
    m_thread_count--;
    m_running = false;
  };

  bool isStop() { return m_stop; }

 protected:
  virtual void  main();
};

