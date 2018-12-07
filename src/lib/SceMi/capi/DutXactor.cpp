// Copyright Bluespec Inc. 2012-2013

#include "DutXactor.h"
#include "semu_capi.h"

using namespace std;

DutXactor::DutXactor(SceMi *scemi)
  : m_shutdown(0)
  , m_reset(0)
  , m_reset_ack(0)
{
  m_shutdown = new ShutdownXactor("", "scemi_control", scemi);
  m_reset = new InportProxyT< BitT<16> >("", "scemi_reset_ctrl_in", scemi);
  m_reset_ack = new OutportQueueT<Bool>("", "scemi_reset_ctrl_out", scemi);
}

DutXactor::~DutXactor()
{
  delete m_shutdown;
  delete m_reset;
  delete m_reset_ack;
}

bool DutXactor::assertReset(unsigned short cycles)
{
  BitT<16> c = cycles;

  m_reset->sendMessage(c);
  bool ack = m_reset_ack->getMessage();

  return ack;
}

void DutXactor::shutdown()
{
  m_shutdown->blocking_send_finish();
}
