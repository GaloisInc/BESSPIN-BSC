// Copyright Bluespec Inc. 2011-2013

#include <signal.h>

#include "usertb.h"

// This is a simple template and example of the generated api calls
// for contructing testbench to interact with the user defined DUT

using namespace std;

unsigned UserTBServiceThread::m_thread_count = 0;
static UserTBServiceThread *theUserTBServiceThread = NULL;

static bool can_start_test = true;
static bool first_test     = true;

// Is restarting the test again and again allowed?
static bool redo_testing   = false;

// Set redo_testing value
void set_redo_testing(bool v) { redo_testing = v; }

// The is the main function of the thread
void UserTBServiceThread::main() {
  while (!m_stop) {
    if (can_start_test && (first_test || redo_testing)) {
      can_start_test = false;
      first_test     = false;
      do_test();
      can_start_test = true;
    } else
      sched_yield();
  }
}

// This function starts the user testbench thread
int test_usertb() {

  // Start the thread if needed
  if (theUserTBServiceThread == NULL) {

    theUserTBServiceThread = new UserTBServiceThread();
    //cout << "UserTB Service thread started!" << endl;
  }

  return 0;
}

// This function is called when the testbench is finished and exits
void destroy_usertb()
{
  if (theUserTBServiceThread) {

    // Stop the thread first
    theUserTBServiceThread->stop();

    // Kill the thread
    //theUserTBServiceThread->signal(SIGKILL);
    //theUserTBServiceThread->signal(SIGTERM);
    theUserTBServiceThread->signal(0);
    // This doesn't work, because the thread doesn't enter a cancellation point
    //theUserTBServiceThread->cancel();
    //theUserTBServiceThread->join();

    // Delete the thread
    delete theUserTBServiceThread;
    theUserTBServiceThread = NULL;

    //cout << "UserTB Service thread finished!" << endl;
  }
}
// This function reset the user testbench
int reset_usertb()
{
  first_test = true;
  can_start_test = true;

  return 1;
}

