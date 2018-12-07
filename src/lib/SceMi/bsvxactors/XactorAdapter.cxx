//-*- C++ -*-x

#include "XactorAdapter.h"

std::list<XactorAdapter const *> XactorAdapter::s_adapters;

XactorAdapter::XactorAdapter(const std::string & name)
  : m_canSendCB(0)
  , m_canRecvCB(0)
  , m_canSendCBContext(0)
  , m_canRecvCBContext(0)
  , m_name(name)
  , m_debug(false)
  , m_debugL(false)
  , m_isopen(false)
{
  pthread_mutex_init(&m_send_mutex, 0);
  pthread_mutex_init(&m_recv_mutex, 0);
  pthread_cond_init(&m_sendCond, NULL);
  pthread_cond_init(&m_recvCond, NULL);
  s_adapters.push_back(this);
}

XactorAdapter::~XactorAdapter()
{
  s_adapters.remove(this);
  pthread_cond_destroy(&m_recvCond);
  pthread_cond_destroy(&m_sendCond);
  pthread_mutex_destroy(&m_recv_mutex);
  pthread_mutex_destroy(&m_send_mutex);
}

void XactorAdapter::setSendCallBack(AdapterCallBack cbfunc, void *context)
{
  m_canSendCBContext = context;
  m_canSendCB = cbfunc;
}

void XactorAdapter::setRecvCallBack(AdapterCallBack cbfunc, void *context)
{
  m_canRecvCBContext = context;
  m_canRecvCB = cbfunc;
}

void XactorAdapter::executeSendCallBack()
{
  if (m_canSendCB != 0) {
    (*m_canSendCB)(m_canSendCBContext);
  }
}

void XactorAdapter::executeRecvCallBack()
{
  if (m_canRecvCB != 0) {
    (*m_canRecvCB)(m_canRecvCBContext);
  }
}

void XactorAdapter::lockSend() const
{
  if (m_debugL) {
    //    m_logfile.Debug("XactorAdapter %s Locking Send Data", m_name.c_str());
  }
  int32_t error = pthread_mutex_lock(&m_send_mutex);
  assert(!error);
}

void XactorAdapter::unlockSend() const
{
  if (m_debugL) {
    //    m_logfile.Debug("XactorAdapter %s Unlocking Send Data", m_name.c_str());
  }
  int32_t error = pthread_mutex_unlock(&m_send_mutex);
  assert(!error);
}

void XactorAdapter::lockRecv() const
{
  if (m_debugL) {
    //    m_logfile.Debug("XactorAdapter %s Locking Recv Data", m_name.c_str());
  }
  int32_t error = pthread_mutex_lock(&m_recv_mutex);
  assert(!error);
}

void XactorAdapter::unlockRecv() const
{
  if (m_debugL) {
    //    m_logfile.Debug("XactorAdapter %s Unlocking Recv Data", m_name.c_str());
  }
  int32_t error = pthread_mutex_unlock(&m_recv_mutex);
  assert(!error);
}

void XactorAdapter::signalSendCondition()
{
  pthread_cond_signal(&m_sendCond);
}

void XactorAdapter::signalRecvCondition()
{
  pthread_cond_signal(&m_recvCond);
}

void XactorAdapter::send_cond_wait()
{
  if (m_debugL) {
    m_logfile.Debug("XactorAdapter %s waiting for can send", m_name.c_str());
  }
  pthread_cond_wait(&m_sendCond, &m_send_mutex);
  if (m_debugL) {
    m_logfile.Debug("XactorAdapter %s DONE waiting for can send", m_name.c_str());
  }
}

int32_t XactorAdapter::send_cond_timedwait(const struct timespec *expiration)
{
  if (m_debugL) {
    m_logfile.Debug("XactorAdapter %s timed waiting for can send", m_name.c_str());
  }
  int32_t status = pthread_cond_timedwait(&m_sendCond, &m_send_mutex, expiration);
  if (m_debugL) {
    m_logfile.Debug("XactorAdapter %s DONE timed waiting for can send", m_name.c_str());
  }  
  return status;
}

void XactorAdapter::recv_cond_wait()
{
  if (m_debugL) {
    m_logfile.Debug("XactorAdapter %s waiting for can recv", m_name.c_str());
  }
  pthread_cond_wait(&m_recvCond, &m_recv_mutex);
  if (m_debugL) {
    m_logfile.Debug("XactorAdapter %s DONE waiting for can recv", m_name.c_str());
  }
}

int32_t XactorAdapter::recv_cond_timedwait(const struct timespec *expiration)
{
  if (m_debugL) {
    m_logfile.Debug("XactorAdapter %s timed waiting for can recv", m_name.c_str());
  }
  int32_t status = pthread_cond_timedwait(&m_recvCond, &m_recv_mutex, expiration);
  if (m_debugL) {
    m_logfile.Debug("XactorAdapter %s DONE timed waiting for can recv", m_name.c_str());
  }  
  return status;
}

bool XactorAdapter::setDebug(bool val)
{
  bool stat = m_debug;
  m_debug = val;
  return stat;
}

bool XactorAdapter::setLDebug(bool val)
{
  bool stat = m_debugL;
  m_debugL = val;
  return stat;
}

uint32_t XactorAdapter::setDebug(uint32_t mask)
{
  uint32_t x = 0;

  bool b0 = setDebug((mask & 0x01) != 0);
  if (b0) x |= 0x01;

  bool b1 = setLDebug((mask & 0x02) != 0);
  if (b1) x |= 0x02;

  return x;
}

static int32_t xxdebug(const XactorAdapter *x)
{
  x->debug();
  return 0;
}

void XactorAdapter::xdebug()
{
  for_each(s_adapters.begin(), s_adapters.end(), xxdebug);
}
