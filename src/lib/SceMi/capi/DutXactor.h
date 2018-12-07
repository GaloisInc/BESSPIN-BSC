// Copyright Bluespec Inc. 2012-2013

#pragma once

// Include Bluespec's SceMi C++ api
#include "bsv_scemi.h"

// Define a class for the top-level transactor
class DutXactor {

 protected:

  // Shutdown Xactor
  ShutdownXactor	 *m_shutdown;

  // Reset control
  InportProxyT < BitT<16> >  *m_reset;
  OutportQueueT < Bool > *m_reset_ack;


 public:

  DutXactor(SceMi *scemi);

  // Destructor
  ~DutXactor();

  // Reset
  bool assertReset(unsigned short cycles);

  // Shutdown
  void shutdown();

};
