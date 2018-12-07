//-*- C++ -*-x
// Copyright (c) 2009-2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// SceMi C++ model for a SceMi inport proxy template class

#include <string>
#include <stdexcept>

#include <pthread.h>
#include <time.h>
#include <errno.h>
#include <sstream>
#include <iostream>
#include "scemi.h"

#ifdef __APPLE__
#include <sys/time.h>
#endif

#ifndef BS_SCEMI_DEBUG
#define BS_SCEMI_DEBUG false
#endif

// Requirements on Type T
// unsigned int setMessage ( SecMiMessageData *, unsigned int offset) const ;
template <typename T>
class InportProxyT {
public:
  typedef void (*CallBackFunc) (void *context);

private:
  // SceMi port
  SceMiMessageInPortProxy * m_inport ;
  // Shared state
  bool       m_readyToSend;
  // Mutex for gaining control of the shared state
  pthread_mutex_t m_lock;
  // Signal to wakeup the waiting sender
  pthread_cond_t m_cond;

  // User callback
  CallBackFunc m_callbackFunction;
  void         *m_callbackContext;

  // Debug
  bool       m_debug;

 public:
  InportProxyT (const std::string & hier, const std::string & instname, SceMi *scemi)
    : m_readyToSend(false)
    , m_callbackFunction(NULL)
    , m_callbackContext(NULL)
    , m_debug(BS_SCEMI_DEBUG)
    {
      // Initialize the signals
      pthread_mutex_init(&m_lock, NULL);
      pthread_cond_init(&m_cond, NULL);

      // Initialize the SceMi port
      m_inport = scemi->BindMessageInPort (hier.c_str(), instname.c_str() );
      if (m_inport ==  0) throw std::runtime_error ("Could not bind message inport: " + hier + " " + instname);

      // Check sizes
      T obj;
      if (PortWidth() != obj.getBitSize() ) {
        std::ostringstream oss("");
        oss << "Port width (" << PortWidth() << ") does not match Object width ("
            << obj.getBitSize() << ")  in inport: " << hier << " " << instname;
        throw std::runtime_error (oss.str());
      }

      SceMiMessageInPortBinding inPortBinding ;
      inPortBinding.Context = this;
      inPortBinding.IsReady = SsendCallBackT;
      inPortBinding.Close   = NULL;
      m_inport->ReplaceBinding (&inPortBinding);
    }
  // Destructor
  ~InportProxyT () {
    // Disable user callback
    setCallBack();
    // use a null binding to stop any callbacks.
    if (SceMi::Pointer() != NULL)
      m_inport->ReplaceBinding();

    // Destroy the signals
    pthread_cond_destroy(&m_cond);
    pthread_mutex_destroy(&m_lock);
  }

 private:
  // Copy constructor are disabled.
  InportProxyT & operator= (const InportProxyT &);
  InportProxyT ( const InportProxyT &);

 public:
  // Accessors to proxy
  unsigned int PortWidth () const { return m_inport->PortWidth() ; }
  const char*  PortName ()  const { return m_inport->PortName() ; }
  const char* TransactorName() const { return m_inport->TransactorName(); }

  void setDebug( bool val) { m_debug = val ; } ;

 private:
  // The core send function used by the public functions below.
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
  // returns the current status of the port
  bool readyToSend () const {
    return m_readyToSend;
  }
  // Note that this function blocks until the message is sent.
  // This function should NOT be called from a scemi call back as
  // deadlock will occur.
  void sendMessage (const T & t) {
    // grab the lock
    pthread_mutex_lock(&m_lock);

    // is the port ready?
    while (!m_readyToSend) {
      // this wait will release the lock atomically with the wait
      pthread_cond_wait(&m_cond, &m_lock);
      // and will give back the lock when the wait is over
    }

    doSend(t);

    // release the lock
    pthread_mutex_unlock(&m_lock);
  }


  // Note that this method blocks until the message is sent and a ready to send
  // message is returned from the DUT. I.e., the message has been received by the
  // DUT
  // This function should NOT be called from a scemi call back as
  // deadlock will occur.
  void sendMessageAck (const T & t) {
    // grab the lock
    pthread_mutex_lock(&m_lock);

    // is the port ready?
    while (!m_readyToSend) {
      // this wait will release the lock atomically with the wait
      pthread_cond_wait(&m_cond, &m_lock);
      // and will give back the lock when the wait is over
    }

    doSend(t);

    // Wait for the port to be ready again
    while (!m_readyToSend) {
      // this wait will release the lock atomically with the wait
      pthread_cond_wait(&m_cond, &m_lock);
      // and will give back the lock when the wait is over
    }

    // release the lock
    pthread_mutex_unlock(&m_lock);
  }

  // This version doesn't block, returning true if the data is sent
  bool sendMessageNonBlocking (const T & t) {
    bool res = false;

    // grab the lock
    pthread_mutex_lock(&m_lock);

    // is the port ready?
    if (m_readyToSend) {
      doSend(t);
      res = true;
    }

    // release the lock
    pthread_mutex_unlock(&m_lock);

    return res;
  }

  // This version blocks, but with a timeout
  bool sendMessageTimed (const T & t, const time_t & delta_seconds,  const long & delta_microseconds=0) {
    struct timespec ts;

#ifdef __APPLE__
    struct timeval tv;
    gettimeofday(&tv, NULL);
    ts.tv_sec = tv.tv_sec;
    ts.tv_nsec = tv.tv_usec*1000;
#else
    clock_gettime(CLOCK_REALTIME, &ts);
#endif
    time_t overflow_s  = delta_microseconds / 1000000 ;
    long corrected_us  = delta_microseconds % 1000000 ;

    ts.tv_nsec = ts.tv_nsec + (corrected_us * 1000);
    if (ts.tv_nsec >= 1000000000) {
      ts.tv_nsec = ts.tv_nsec - 1000000000;
      overflow_s ++;
    }
    ts.tv_sec = ts.tv_sec + delta_seconds + overflow_s;
    return sendMessageTimed(t, &ts);
  }
  bool sendMessageTimed (const T & t, struct timespec *expiration) {
    bool res = false;
    int status;

    // grab the lock
    pthread_mutex_lock(&m_lock);

    // is the port ready?
    while (!m_readyToSend) {
      // this wait will release the lock atomically with the wait
      status = pthread_cond_timedwait(&m_cond, &m_lock, expiration);
      // and will give up back the lock when the wait is over
      if (status == ETIMEDOUT) {
        break;
      }
    }

    // Even if the timer timed out, it's still possible that
    // the ready callback happened at the last moment
    if (m_readyToSend) {
      doSend(t);
      res = true;
    }

    // release the lock
    pthread_mutex_unlock(&m_lock);

    return res;
  }

  // Wait (Block) until the port is ready to send data.
  // implying that all previous data has been recevied.
  // It is possible that some other thread can grab the lock and the port
  // will become unready immediately after this command returns
  bool waitUntilReady () {
    // grab the lock
    pthread_mutex_lock(&m_lock);

    while (!m_readyToSend) {
      // this wait will release the lock atomically with the wait
      pthread_cond_wait(&m_cond, &m_lock);
      // and will give back the lock when the wait is over
    }

    // release the lock
    pthread_mutex_unlock(&m_lock);
    return m_readyToSend;
  }

  // Wait (Block) until the port is ready to send data or expiration has been met
  // implying that all previous data has been recevied.
  // It is possible that some other thread can grab the lock and the port
  // will become unready immediately after this command returns
  bool waitUntilReady ( const time_t & delta_seconds,  const long & delta_microseconds=0) {
    struct timespec ts;

#ifdef __APPLE__
    struct timeval tv;
    gettimeofday(&tv, NULL);
    ts.tv_sec = tv.tv_sec;
    ts.tv_nsec = tv.tv_usec*1000;
#else
    clock_gettime(CLOCK_REALTIME, &ts);
#endif
    time_t overflow_s  = delta_microseconds / 1000000 ;
    long corrected_us  = delta_microseconds % 1000000 ;

    ts.tv_nsec = ts.tv_nsec + (corrected_us * 1000);
    if (ts.tv_nsec >= 1000000000) {
      ts.tv_nsec = ts.tv_nsec - 1000000000;
      overflow_s ++;
    }
    ts.tv_sec = ts.tv_sec + delta_seconds + overflow_s;
    return waitUntilReady(&ts);
  }


  // Wait (Block) until the port is ready to send data or expiration has been met
  // implying that all previous data has been recevied.
  // It is possible that some other thread can grab the lock and the port
  // will become unready immediately after this command returns
  bool waitUntilReady ( struct timespec *expiration) {
    int status;
    // grab the lock
    pthread_mutex_lock(&m_lock);

    while (!m_readyToSend) {
      // this wait will release the lock atomically with the wait
      status = pthread_cond_timedwait(&m_cond, &m_lock, expiration);
      // and will give up back the lock when the wait is over
      if (status == ETIMEDOUT) {
        break;
      }
    }

    // release the lock
    pthread_mutex_unlock(&m_lock);
    return m_readyToSend;
  }

  

  // Callback occurs when the port becomes ready to send
  void setCallBack (CallBackFunc cbfunc = 0, void *context = 0) {
    m_callbackFunction = cbfunc;
    m_callbackContext  = context;
  }

 private:

  void executeCallBack () {
    if (m_callbackFunction != 0) {
      m_callbackFunction (m_callbackContext);
    }
  }
  static void SsendCallBackT (void *x) {
    InportProxyT * tx = (InportProxyT *) x;
    pthread_mutex_lock(&(tx->m_lock));
    tx->m_readyToSend = true;
    pthread_cond_signal(&(tx->m_cond));
    pthread_mutex_unlock(&(tx->m_lock));
    tx->executeCallBack();
  }

};
