// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED

// SceMi C++ model for a SceMi outport proxy class

#pragma once

#include <string>
#include <stdexcept>
#include "scemi.h"
#include <iostream>

#ifndef BS_SCEMI_DEBUG
#define BS_SCEMI_DEBUG false
#endif

// requirements on T
// constructor T(const SceMiMessageData *)
// friend std::ostream operator<< (std::ostream &, const T &)

template <typename T>
class OutportProxyT {
 private:
  // SceMi port
  SceMiMessageOutPortProxy * m_outport;

  // Debug
  bool m_debug;

  // call back for message conversion to Proxy data type
  //  T (* m_toProxyType) (const SceMiMessageData *msg);
  // user call back functions when data arrives
  void (*m_callback) (void *, const T &);
  void * m_callbackPtr;

 public:
  OutportProxyT (const std::string & hier, const std::string & instname, SceMi *scemi)
    : m_debug(BS_SCEMI_DEBUG)
    , m_callback(NULL)
    , m_callbackPtr(NULL)
    {
      // Initialize the SceMi port
      m_outport = scemi->BindMessageOutPort (hier.c_str(), instname.c_str() );
      if (m_outport ==  0) throw std::runtime_error ("Could not bind message outport: " + hier + " " + instname);
      SceMiMessageOutPortBinding outPortBinding ;
      outPortBinding.Context = this;
      outPortBinding.Receive = SreceiveCallBackT;
      outPortBinding.Close   = NULL;
      m_outport->ReplaceBinding (&outPortBinding);
    }

  // Destructor
  ~OutportProxyT () {
    // use a null binding to stop any callbacks.
    if (SceMi::Pointer() != NULL)
      m_outport->ReplaceBinding ();
  }

 private:
  // Copy constructor are disabled.
  OutportProxyT & operator= (const OutportProxyT &);
  OutportProxyT ( const OutportProxyT &);

 public:
  // Accessors to proxy
  unsigned int PortWidth () const     { return m_outport->PortWidth() ; }
  const char*  PortName ()  const     { return m_outport->PortName() ; }
  const char*  TransactorName() const { return m_outport->TransactorName() ; }

  // The callback function must follow the guidelines listed in the
  // scemi spec.  That is, the call back should return immediately
  // without blocking.
  void setCallBack( void func (void *, const T &), void * ptr){
    m_callback = func;
    m_callbackPtr = ptr;
  }
  void setDebug( bool val) { m_debug = val ; } ;

 private:

  // This call back is executed when the scemi transactor has data.
  void receiveCallBackT(const SceMiMessageData *msg) {
    unsigned int off = 0;
    // Execute user call back
    if (m_callback != NULL) {
      T data(msg, off);
      if ( m_debug ) {
        std::cout << TransactorName() << "." << PortName() << " received: "
             << data << std::endl;
      }
      m_callback (m_callbackPtr, data);
    }
  }

  static void  SreceiveCallBackT(void *x, const SceMiMessageData *msg) {
    OutportProxyT<T> *tx = (OutportProxyT<T> *) x;
    tx->receiveCallBackT (msg) ;
  };

};
