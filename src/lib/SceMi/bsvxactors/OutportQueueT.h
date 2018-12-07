// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// Combined OutportProxyT and WaitQueueT class to use a SceMi Outport proxy

#include <string>

#include "OutportProxyT.h"
#include "WaitQueueT.h"

#ifndef BS_SCEMI_DEBUG
#define BS_SCEMI_DEBUG false
#endif

// requirements on T
// constructor T(const SceMiMessageData *)
// friend std::ostream operator<< (std::ostream &, const T &)

template <typename T>
class OutportQueueT {
 private:
  OutportProxyT<T> m_proxy;
  WaitQueueT<T> m_queue;

  // Debug
  bool m_debug;

 public:
  OutportQueueT (const std::string & hier, const std::string & instname, SceMi *scemi) 
    : m_proxy(hier, instname, scemi)
    , m_queue()
    , m_debug(BS_SCEMI_DEBUG)
  {
    m_proxy.setDebug(BS_SCEMI_DEBUG);
    m_proxy.setCallBack(&SreceiveCallBackT, (void*) this);
  }

  ~OutportQueueT () {}

 private:
  // Copy constructor are disabled.
  OutportQueueT & operator= (const OutportQueueT &);
  OutportQueueT ( const OutportQueueT &);

 public:
  // Accessors to proxy
  unsigned int PortWidth () const { return m_proxy->PortWidth() ; }
  const char*  PortName ()  const { return m_proxy->PortName() ; }
  const char*  TransactorName () const { return m_proxy->TransactorName() ; }

  void setDebug(bool val) {
    m_debug = val;
    m_proxy.setDebug(val);
  }

  // Note that this function blocks if there is no message to receive.
  // This function should NOT be called from a scemi call back as
  // deadlock will occur.
  T getMessage () { return m_queue.get() ; }
  // This version doesn't block, returning true if data is received
  bool getMessageNonBlocking (T &t) { return m_queue.getNonBlocking(t) ; }
  // This version blocks, but with a timeout
  bool getMessageTimed (T &t, struct timespec *expiration)
  { return m_queue.getTimed(t, expiration) ; }
  bool getMessageTimed (T &t, const time_t & delta_seconds,  const long & delta_microseconds=0)
  { return m_queue.getTimed(t, delta_seconds, delta_microseconds); }

  // Status of the queue
  bool isEmpty () const {
    return ! m_queue.isDataAvailable();
  }
  unsigned size () const {
    return m_queue.size();
  }

 private:
  // This call back is executed when the scemi transactor has data.
  static void SreceiveCallBackT(void *x, const T& t) {
    OutportQueueT<T> *tx = (OutportQueueT<T> *) x;
    tx->m_queue.put(t);
  }

};

