// Copyright (c) 2013-2016, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// C++ transactor side for the ClkCtrlServer module in BSVSource

#include <inttypes.h>
#include <deque>

// -----

struct ClkCtrlStatus {
  uint64_t cycle;
  bool running;
  bool free_running;
  unsigned int edges;
};

// -----

class ClkCtrl
{
protected:
  // Queue for incoming status messages
  std::deque<ClkCtrlStatus> m_status_queue;

  // Storage for incoming status words
  // until a full packet is received
  std::deque<unsigned int> m_status_words;

  // Add a status word
  virtual void putStatusWord(unsigned int) = 0;

public:
  ClkCtrl() { };
  ~ClkCtrl() { };

private:
  // Disallow default and copy constructors
  ClkCtrl & operator= (const ClkCtrl &);
  ClkCtrl( const ClkCtrl &);

public:
  virtual bool sendRun() = 0;
  virtual bool sendRunN(unsigned int count) = 0;
  virtual bool sendQuery() = 0;
  virtual bool sendStop() = 0;
  virtual bool sendResume() = 0;

  virtual ClkCtrlStatus getStatusBlocking() = 0;
  virtual bool getStatusNonBlocking(ClkCtrlStatus &s) = 0;
  // Timer specified as a delay relative to the current time
  virtual bool getStatusTimed(ClkCtrlStatus &s, const time_t & delta_seconds, const long & delta_microseconds=0) = 0;
  // Timer specified with absolute time
  virtual bool getStatusTimed(ClkCtrlStatus &s, struct timespec *expiration) = 0;
};

// -----
