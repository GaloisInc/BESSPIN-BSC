//-*- C++ -*-x
// Copyright (c) 2011 - 2013, Bluespec, Inc.  ALL RIGHTS RESERVED

#include <ctime>
#include "XactorCore.h"
#include "SceMiAdapter.h"
#include "SceMiPortAdapter.h"

#ifdef __APPLE__
#include <Availability.h>
#if defined(__MAC_OS_X_VERSION_MIN_REQUIRED) && (__MAC_OS_X_VERSION_MIN_REQUIRED < 101200)
#include <sys/time.h>
//clock_gettime is not implemented on OSX
int clock_gettime(int /*clk_id*/, struct timespec* t) {
    struct timeval now;
    int rv = gettimeofday(&now, NULL);
    if (rv) return rv;
    t->tv_sec  = now.tv_sec;
    t->tv_nsec = now.tv_usec * 1000;
    return 0;
}
#define CLOCK_REALTIME (0)
#endif
#endif

void gettimeofday1(struct timespec *ts) {
  clock_gettime(CLOCK_REALTIME, ts);
}

XactorCore::XactorCore (const std::string &name, const std::string &path)
  : m_sendCallBack(0)
  , m_recvCallBack(0)
  , m_sendCBContext(0)
  , m_recvCBContext(0)
{
  m_padapter = new SceMiAdapter(name, path);

  m_padapter->setSendCallBack(XactorCore::signalCanSend, this);
  m_padapter->setRecvCallBack(XactorCore::signalCanReceive, this);
  signalCanSend();
}

XactorCore::XactorCore (const std::string &name, const std::string &path, XactorAdapter::SPipes type)
  : m_sendCallBack(0)
  , m_recvCallBack(0)
  , m_sendCBContext(0)
  , m_recvCBContext(0)
{
  m_padapter = new SceMiAdapter(name, path);

  m_padapter->setSendCallBack(XactorCore::signalCanSend, this);
  m_padapter->setRecvCallBack(XactorCore::signalCanReceive, this);
  signalCanSend();
}

XactorCore::XactorCore (const std::string &name, const std::string &path, XactorAdapter::SPorts type)
  : m_sendCallBack(0)
  , m_recvCallBack(0)
  , m_sendCBContext(0)
  , m_recvCBContext(0)
{
  m_padapter = new SceMiPortAdapter(name, path);

  m_padapter->setSendCallBack(XactorCore::signalCanSend, this);
  m_padapter->setRecvCallBack(XactorCore::signalCanReceive, this);
  signalCanSend();
}


XactorCore::XactorCore (const std::string &name, const std::string &path, XactorAdapter::SInpipe restrict)
  : m_sendCallBack(0)
  , m_recvCallBack(0)
  , m_sendCBContext(0)
  , m_recvCBContext(0)
{
  m_padapter = new SceMiAdapter(name, path, restrict);

  m_padapter->setSendCallBack(XactorCore::signalCanSend, this);
  signalCanSend();
}

XactorCore::XactorCore (const std::string &name, const std::string &path, XactorAdapter::SOutpipe restrict)
  : m_sendCallBack(0)
  , m_recvCallBack(0)
  , m_sendCBContext(0)
  , m_recvCBContext(0)
{
  m_padapter = new SceMiAdapter(name, path, restrict);

  m_padapter->setRecvCallBack(XactorCore::signalCanReceive, this);
}

XactorCore::XactorCore (const std::string &name, const std::string &path, XactorAdapter::SInport restrict)
  : m_sendCallBack(0)
  , m_recvCallBack(0)
  , m_sendCBContext(0)
  , m_recvCBContext(0)
{
  m_padapter = new SceMiPortAdapter(name, path, restrict);

  m_padapter->setSendCallBack(XactorCore::signalCanSend, this);
  signalCanSend();
}

XactorCore::XactorCore (const std::string &name, const std::string &path, XactorAdapter::SOutport restrict)
  : m_sendCallBack(0)
  , m_recvCallBack(0)
  , m_sendCBContext(0)
  , m_recvCBContext(0)
{
  m_padapter = new SceMiPortAdapter(name, path, restrict);

  m_padapter->setRecvCallBack(XactorCore::signalCanReceive, this);
}


XactorCore::~XactorCore ()
{
  delete m_padapter;
  m_sendCallBack = 0;
  m_recvCallBack = 0;
}

const char *XactorCore::getNameStr () const 
{
  return m_padapter->getNameStr();
}

const std::string &XactorCore::getName () const
{
  return m_padapter->getName();
}

uint32_t XactorCore::setAdapterDebug (uint32_t mask)
{
  return m_padapter->setDebug(mask);
}

bool XactorCore::canSend() const 
{
  return m_padapter->canSend();
}

unsigned XactorCore::canReceive(const unsigned req) const
{
  return m_padapter->canReceive(req);
}

void XactorCore::send (const MsgPacket &sendObj)
{
  m_padapter->lockSend();
  while(!canSend()) {
    m_padapter->send_cond_wait();
  }
  doSend(sendObj);
  m_padapter->unlockSend();
}

bool XactorCore::sendNB (const MsgPacket &sendObj)
{
  bool res = false;
  m_padapter->lockSend();
  if (canSend()) {
    res = doSend(sendObj);
  }
  m_padapter->unlockSend();
  return res;
}

bool XactorCore::sendT (const MsgPacket &sendObj, struct timespec &expiration)
{
  return sendT (sendObj, &expiration);
}

bool XactorCore::sendT (const MsgPacket &sendObj, struct timespec *expiration)
{
  bool res = false;
  int32_t status;

  m_padapter->lockSend();

  while(!canSend()) {
    status = m_padapter->send_cond_timedwait(expiration);
    if (status == ETIMEDOUT) {
      break;
    }
  }

  if (canSend()) {
    res = doSend(sendObj);
  }

  m_padapter->unlockSend();
  return res;
}

bool XactorCore::sendT (const MsgPacket &sendObj, const time_t seconds, const long microseconds)
{
  struct timespec expiration;
  setTimeout(expiration, seconds, microseconds);
  return sendT(sendObj, &expiration);
}


void XactorCore::waitSendAck ()
{
  m_padapter->lockSend();

  while(! m_padapter->sendAcknowledged()) {
    m_padapter->send_cond_wait();
  }
  m_padapter->unlockSend();

  // Chack the the data is flushed for pipe operation.
  m_padapter->flushSend();

}

////////////////////////////////////////////////////////
void XactorCore::receive (MsgPacket &recvObj, unsigned maxread)
{
  m_padapter->lockRecv();

  while(XactorCore::canReceive(maxread) < maxread) {
    m_padapter->recv_cond_wait();
  }

  doReceive(recvObj, maxread);

  m_padapter->unlockRecv();
}

bool XactorCore::receiveNB (MsgPacket &recvObj, unsigned maxread)
{
  bool res = false;
  m_padapter->lockRecv();

  if (XactorCore::canReceive(maxread) >= maxread) {
    res = doReceive(recvObj, maxread);
  }

  m_padapter->unlockRecv();
  return res;
}

bool XactorCore::receiveT (MsgPacket &recvObj, unsigned maxread, struct timespec &expiration)
{
  return receiveT (recvObj, maxread, &expiration);
}

bool XactorCore::receiveT (MsgPacket &recvObj, unsigned maxread, struct timespec *expiration)
{
  bool res = false;
  int32_t status;

  m_padapter->lockRecv();

  while (XactorCore::canReceive(maxread) < maxread) {
    status = m_padapter->recv_cond_timedwait(expiration);
    if (status == ETIMEDOUT) {
      break;
    }
  }

  if (XactorCore::canReceive(maxread) >= maxread) {
    res = doReceive(recvObj, maxread);
  }

  m_padapter->unlockRecv();
  return res;
}

bool XactorCore::receiveT (MsgPacket &recvObj, unsigned maxread, uint32_t seconds, uint32_t microseconds)
{
  struct timespec expiration;
  setTimeout(expiration, seconds, microseconds);
  return receiveT(recvObj, maxread, &expiration);
}

bool XactorCore::peek (MsgPacket &recvObj, unsigned maxread) const
{
  bool res = false;
  m_padapter->lockRecv();

  if (XactorCore::canReceive(maxread) >= maxread) {
    res = doPeek(recvObj, maxread);
  }

  m_padapter->unlockRecv();
  return res;
}

bool XactorCore::peekNB (MsgPacket &recvObj, unsigned maxread) const
{
  bool res = false;
  m_padapter->lockRecv();

  if (XactorCore::canReceive(maxread) >= maxread) {
    res = doPeek(recvObj, maxread);
  }

  m_padapter->unlockRecv();
  return res;
}

bool XactorCore::peekT (MsgPacket &recvObj, unsigned maxread, struct timespec &expiration) const
{
  return peekT (recvObj, maxread, &expiration);
}

bool XactorCore::peekT (MsgPacket &recvObj, unsigned maxread, struct timespec *expiration) const
{
  bool res = false;
  int32_t status;

  m_padapter->lockRecv();
  
  while(XactorCore::canReceive(maxread) < maxread) {
    status = m_padapter->recv_cond_timedwait(expiration);
    if (status == ETIMEDOUT) {
      break;
    }
  }

  if (XactorCore::canReceive(maxread) >= maxread) {
    res = doPeek(recvObj, maxread);
  }

  m_padapter->unlockRecv();
  return res;
}

bool XactorCore::peekT (MsgPacket &recvObj, unsigned maxread, uint32_t seconds, uint32_t microseconds) const
{
  bool res = false;
  struct timespec expiration;
  setTimeout(expiration, seconds, microseconds);
  res = peekT(recvObj, maxread, &expiration);
  return res;
}

void XactorCore::setCanSendNotifyCallBack (XactorCallBack cbfunc, void *context) 
{
  m_sendCBContext = context;
  m_sendCallBack = cbfunc;
}

void XactorCore::setCanRecvNotifyCallBack(XactorCallBack cbfunc, void *context)
{
  m_recvCBContext = context;
  m_recvCallBack = cbfunc;
}

void XactorCore::executeSendCallBack()
{
  if (m_sendCallBack != NULL) {
    (*m_sendCallBack)(m_sendCBContext);
  }
}

void XactorCore::executeRecvCallBack()
{
  if (m_recvCallBack != NULL) {
    (*m_recvCallBack)(m_recvCBContext);
  }
}

bool XactorCore::doSend (const MsgPacket &p)
{
  bool stat = false;
  if (!p.empty()) {
    stat = m_padapter->doSend(p);
  }
  return stat;
}

bool XactorCore::doReceive (MsgPacket &p, unsigned maxread)
{
  bool stat = false;
  if (maxread != 0) {
    p.clear();
    stat = m_padapter->doRecv(p, maxread);
  }
  return stat;
}

bool XactorCore::doPeek (MsgPacket &p, unsigned maxread) const
{
  p.clear();
  return m_padapter->doPeek(p, maxread);
}

void XactorCore::signalCanSend () 
{
  executeSendCallBack();
}

void XactorCore::signalCanReceive ()
{
  executeRecvCallBack();
}

void XactorCore::signalCanSend (void *pv)
{
  XactorCore *px = reinterpret_cast<XactorCore*>(pv);
  px->signalCanSend();
}

void XactorCore::signalCanReceive (void *pv)
{
  XactorCore *px = reinterpret_cast<XactorCore*>(pv);
  px->signalCanReceive();
}

void XactorCore::debug () const
{
  m_padapter->debug();
}

void XactorCore::setTimeout(struct timespec &ts, const time_t delta_seconds, const long delta_microseconds)
{
  gettimeofday1(&ts);

  const long usPerSec = 1000000L;
  const long nsPerSec = 1000000000L;

  long overflow_s    = delta_microseconds / usPerSec ;
  long corrected_us  = delta_microseconds % usPerSec ;

  ts.tv_nsec = ts.tv_nsec + (corrected_us * 1000L);
  while (ts.tv_nsec >= nsPerSec ) {
    ts.tv_nsec = ts.tv_nsec - nsPerSec;
    overflow_s ++;
  }

  ts.tv_sec = ts.tv_sec + delta_seconds + overflow_s;
}

void XactorCore::xdebug ()
{
  XactorAdapter::xdebug();
  fflush(stdout);
}
