// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// SceMi C++ side Shutdown transactor

#include <string>
#include <iostream>

#include "InportProxyT.h"
#include "OutportQueueT.h"
#include "BitT.h"

class ShutdownXactor
{
 private:
  InportProxyT<BitT<1> >  m_in_proxy;
  OutportQueueT<BitT<1> > m_out_proxy;

  bool m_is_finished;

 public:

  ShutdownXactor (const std::string hier, const std::string instname, SceMi *scemi)
    : m_in_proxy (hier, instname + "_ctrl_in", scemi)
    , m_out_proxy (hier, instname + "_ctrl_out", scemi)
    , m_is_finished (false)
  {
  }

 private:
  // Disallow default and copy constructors
  ShutdownXactor & operator= (const ShutdownXactor &);
  ShutdownXactor( const ShutdownXactor &);

 public:
  void blocking_send_finish() {
    BitT<1> out_msg;
    out_msg.setBit(0,1);
    m_in_proxy.sendMessage(out_msg);  //  Blocking
    BitT<1> in_msg;
    bool stat = m_out_proxy.getMessageTimed(in_msg, 10);
    if (!stat) {
      std::cerr << "No acknowledge message received from dut during Shutdown." << std::endl;
    }
    m_is_finished = true;
  }
  bool is_finished() {
    return m_is_finished;
  }
};
