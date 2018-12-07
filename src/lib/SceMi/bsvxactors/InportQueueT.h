// Copyright (c) 2009-2010, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// Similar to InportProxyT, but with a queue to allow non-blocking send

#include <string>
#include <deque>
#include <pthread.h>
#include <time.h>
#include <errno.h>
#include <stdexcept>

#include "scemi.h"
#include <iostream>

#ifndef BS_SCEMI_DEBUG
#define BS_SCEMI_DEBUG false
#endif

// Requirements on Type T
// unsigned int setMessage ( SecMiMessageData *, unsigned int offset) const ;
template <typename T>
class InportQueueT {
 private:
  // SceMi port
  SceMiMessageInPortProxy * m_inport ;
  // Shared state
  bool       m_readyToSend;
  std::deque<T> m_queue;
  // Mutex for gaining control of the shared state
  pthread_mutex_t m_lock;

  // Debug
  bool       m_debug;

 public:
  InportQueueT (const std::string & hier, const std::string & instname, SceMi *scemi) 
    : m_readyToSend(false)
    , m_queue()
    , m_debug(BS_SCEMI_DEBUG)
    {
      // Initialize the lock
      pthread_mutex_init(&m_lock, NULL);

      // Initialize the SceMi port
      m_inport = scemi->BindMessageInPort (hier.c_str(), instname.c_str() );
      if (m_inport ==  0) throw std::runtime_error ("Could not bind message inport: " + hier + " " + instname);

      SceMiMessageInPortBinding inPortBinding ;
      inPortBinding.Context = this;
      inPortBinding.IsReady = SsendCallBackT;
      inPortBinding.Close   = NULL;
      m_inport->ReplaceBinding (&inPortBinding);
    }
  // Destructor
  ~InportQueueT () {
    // use a null binding to stop any callbacks.
    m_inport->ReplaceBinding ();
    // Destroy the lock
    pthread_mutex_destroy(&m_lock);
  }

 private:
  // Copy constructor are disabled.
  InportQueueT & operator= (const InportQueueT &);
  InportQueueT ( const InportQueueT &);

 public:
  // Accessors to proxy
  unsigned int PortWidth () const { return m_inport->PortWidth() ; }
  const char*  PortName ()  const { return m_inport->PortName() ; }
  const char* TransactorName() const { return m_inport->TransactorName(); }

  void setDebug( bool val) { m_debug = val ; } ;

 private:
  // The core send function, used by the public function and the callback.
  // This should only be called when holding the lock.
  void doSend (const T & t) {
    // send the message
    unsigned int off = 0;
    SceMiMessageData msg (*m_inport);
    t.setMessageData(msg, off);
    m_inport->Send(msg);

    // update the state
    m_readyToSend = false;
    if (m_debug) {
      std::cout <<  m_inport->TransactorName() << "." << PortName() << " sent: "
           << t << std::endl;
    }
  }

 public:
  // This function will block for control of the state, but will not
  // otherwise wait for some action from the SceMi service thread,
  // so it is safe to use inside the SceMi service thread.
  void sendMessage (const T & t) {
    // grab the lock
    pthread_mutex_lock(&m_lock);

    // is the port ready?
    if (m_readyToSend) {
      doSend(t);
    } else {
      m_queue.push_back(t);
    }

    // release the lock
    pthread_mutex_unlock(&m_lock);
  }
  // Send multiple messages
  void sendMessage (const T ts[], unsigned count ) {
    // grab the lock
    pthread_mutex_lock(&m_lock);

    // Queue up every thing then send the head if you can
    for (unsigned i = 0 ; i < count ; ++ i) {
      m_queue.push_back(ts[i]);
    }
    if (m_readyToSend) {
      const T & res = m_queue.front();
      doSend(res);
      m_queue.pop_front();
    }
    // release the lock
    pthread_mutex_unlock(&m_lock);
  }
  // get the status of queue
  bool isEmpty () const {
    return m_queue.empty();
  }
  unsigned size () const {
    return m_queue.size();
  }

 private:
  // The function called when the port is ready to send
  static void SsendCallBackT (void *x) {
    InportQueueT * tx = (InportQueueT *) x;

    // grab the lock
    pthread_mutex_lock(&(tx->m_lock));

    // is data waiting?
    if (tx->m_queue.empty()) {
      tx->m_readyToSend = true;
    } else {
      const T & res = tx->m_queue.front();
      tx->doSend(res);
      tx->m_queue.pop_front();
    }

    // release the lock
    pthread_mutex_unlock(&(tx->m_lock));
  }

};

