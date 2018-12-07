#include "SceMiAdapter.h"
#include "scemi_pipes.h"
#include "iostream"

pthread_mutex_t SceMiAdapter::s_cmd_mutex = PTHREAD_MUTEX_INITIALIZER;

SceMiAdapter::SceMiAdapter(const std::string &name, const std::string &path)
  : XactorAdapter(name + "." + path)
  , m_putHandle(0)
  , m_getHandle(0)
  , m_get_callback_handle(0)
  , m_put_callback_handle(0)
  , m_sendDataQ()
  , m_sendDataQLimit(0)
  , m_recvDataQ()
  , m_recvDataQLimit(0)
  , m_putPipeDepth(0)
  , m_putPipeWidth(0)
  , m_getPipeDepth(0)
  , m_getPipeWidth(0)
{
  // Initialize the pipe location in scemi
  std::string inPipeName(name== "" ? path : (name+"."+path));
  std::string outPipeName(name== "" ? path : (name+"."+path));

  inPipeName  += "_instream";
  outPipeName += "_outstream";

  //std::cerr << "inPipeName: " << inPipeName << std::endl;
  //std::cerr << "outPipeName: " << outPipeName << std::endl;

  m_putHandle = scemi_pipe_c_handle(inPipeName.c_str());
  m_getHandle = scemi_pipe_c_handle(outPipeName.c_str());
  if (m_putHandle == 0) {
    throw std::runtime_error("Unable to open scemi input pipe " + inPipeName);
    return;
  }
  if (m_getHandle == 0) {
    throw std::runtime_error("Unable to open scemi output pipe " + outPipeName);
    return;
  }

  m_putPipeDepth = scemi_pipe_get_depth(m_putHandle);
  m_putPipeWidth = (7+scemi_pipe_get_bits_per_element(m_putHandle))/8;
  m_sendDataQLimit = 4 * m_putPipeDepth * m_putPipeWidth;

  m_getPipeDepth = scemi_pipe_get_depth(m_getHandle);
  m_getPipeWidth = (7+scemi_pipe_get_bits_per_element(m_getHandle))/8;
  m_recvDataQLimit = 4 * m_getPipeDepth * m_getPipeWidth;

  // local callback
  m_put_callback_handle = scemi_pipe_set_notify_callback(m_putHandle, SceMiAdapter::canSendCB, this);
  m_get_callback_handle = scemi_pipe_set_notify_callback(m_getHandle, SceMiAdapter::canRecvCB, this);

  m_isopen = true;

  // start pulling in data since scemi does not notify unless there is a request
  canRecvCB();

}


SceMiAdapter::SceMiAdapter(const std::string &name, const std::string &path, SInpipe restrict)
  : XactorAdapter(name + "." + path)
  , m_putHandle(0)
  , m_getHandle(0)
  , m_get_callback_handle(0)
  , m_put_callback_handle(0)
  , m_sendDataQ()
  , m_sendDataQLimit(0)
  , m_recvDataQ()
  , m_recvDataQLimit(0)
  , m_putPipeDepth(0)
  , m_putPipeWidth(0)
  , m_getPipeDepth(0)
  , m_getPipeWidth(0)
{

  // Initialize the pipe location in scemi
  std::string inPipeName(name== "" ? path : (name+"."+path));
  m_putHandle = scemi_pipe_c_handle(inPipeName.c_str());
  if (m_putHandle == 0) {
    throw std::runtime_error("Unable to open scemi input pipe " + path);
    return;
  }

  m_putPipeDepth = scemi_pipe_get_depth(m_putHandle);
  m_putPipeWidth = (7+scemi_pipe_get_bits_per_element(m_putHandle))/8;
  m_sendDataQLimit = 4 * m_putPipeDepth * m_putPipeWidth;

  // local callback
  m_put_callback_handle = scemi_pipe_set_notify_callback(m_putHandle, SceMiAdapter::canSendCB, this);

  m_isopen = true;
}

SceMiAdapter::SceMiAdapter(const std::string &name, const std::string &path, SOutpipe restrict)
  : XactorAdapter(name + "." + path)
  , m_putHandle(0)
  , m_getHandle(0)
  , m_get_callback_handle(0)
  , m_put_callback_handle(0)
  , m_sendDataQ()
  , m_sendDataQLimit(0)
  , m_recvDataQ()
  , m_recvDataQLimit(0)
  , m_putPipeDepth(0)
  , m_putPipeWidth(0)
  , m_getPipeDepth(0)
  , m_getPipeWidth(0)
{
  // Initialize the pipe location in scemi
  std::string outPipeName(name== "" ? path : (name+"."+path));
  m_getHandle = scemi_pipe_c_handle(outPipeName.c_str());
  if (m_getHandle == 0) {
    throw std::runtime_error("Unable to open scemi output pipe " + path);
    return;
  }

  m_getPipeDepth = scemi_pipe_get_depth(m_getHandle);
  m_getPipeWidth = (7+scemi_pipe_get_bits_per_element(m_getHandle))/8;
  m_recvDataQLimit = 4 * m_getPipeDepth * m_getPipeWidth;

  // local callback
  m_get_callback_handle = scemi_pipe_set_notify_callback(m_getHandle, SceMiAdapter::canRecvCB, this);
  m_isopen = true;

  // start pulling in data since scemi does not notify unless there is a request
  canRecvCB();
}


SceMiAdapter::~SceMiAdapter()
{
  // clear out the callback function to stop notifications
  if (m_get_callback_handle != 0)
    scemi_pipe_clear_notify_callback(m_get_callback_handle);
  if (m_put_callback_handle)
    scemi_pipe_clear_notify_callback(m_put_callback_handle);


  // clear out queue
  lockSend();
  setSendCallBack(NULL, NULL);
  m_sendDataQ.clear();
  unlockSend();

  lockRecv();
  setRecvCallBack(NULL, NULL);
  m_recvDataQ.clear();
  unlockRecv();
}

void SceMiAdapter::debug() const
{
  int sendPipe = (m_put_callback_handle) ? scemi_pipe_c_can_send(m_putHandle) : -1;
  fprintf (stderr, "SceMiAdapter %s: SendQ=%u (%d) RecvQ=%u ", getNameStr(),
           m_sendDataQ.size(), sendPipe,
           m_recvDataQ.size());
}

void SceMiAdapter::lockCmds(const char *msg)
{
  return ;
  // if (m_debugL) {
  //   m_logfile.Debug("Locking  %s %s -- ", msg, getNameStr());
  // }
  // int32_t error = pthread_mutex_lock(&s_cmd_mutex);
  // assert(!error);
}

void SceMiAdapter::unlockCmds(const char *msg)
{
  return;
  // if (m_debugL) {
  //   m_logfile.Debug("Unlocked  %s %s -- ", msg, getNameStr());
  // }
  // int32_t error = pthread_mutex_unlock(&s_cmd_mutex);
  // assert(!error);
}

bool SceMiAdapter::canSend() const
{
  if (m_putHandle == 0) return false;
  if ((m_sendDataQLimit != 0) && (m_sendDataQ.size() < m_sendDataQLimit)) {
      return true;
  }
  return false;
}

bool SceMiAdapter::sendAcknowledged()
{
  // there is no ready to send on the pipe interface except through calling flush
  return m_sendDataQ.empty();
}

void SceMiAdapter::flushSend()
{
  scemi_pipe_c_flush (m_putHandle);
}


bool SceMiAdapter::doSend(const MsgPacket &p)
{
  assert (m_putHandle != 0);
  if (m_debug) {
    std::stringstream oss;
    oss << getName() << " Sending: " << p;
    m_logfile.Debug("SA - %s", oss.str().c_str());
  }

  bool stat = true;
  int32_t sent = 0;

  // Do we have enough for 1 element
  int toSend =  p.size() / m_putPipeWidth;

  if ((toSend != 0) && m_sendDataQ.empty()) {
    lockCmds("send");
    sent = scemi_pipe_c_try_send(m_putHandle, 0, toSend, (const svBitVecVal*)&p[0], 0);
    sent *= m_putPipeWidth;
    unlockCmds("send");
  }

  if (sent < (int) p.size()) {
    // The pipe is in overflow state which means call-backs will occur.
    m_sendDataQ.push(p, sent);
    stat = true;
  }

  if (m_debug) {
    m_logfile.Debug("SA -- %s sending packet size=%ld sent=%ld QS=%ld %08lx", getNameStr(), (long)p.size(), sent, (long) m_sendDataQ.size(), (long)p[0]);
  }

  return stat;
}

unsigned SceMiAdapter::canReceive(const unsigned req) const
{
  assert (m_getHandle != 0);
  if ((m_recvDataQLimit !=0) && (m_recvDataQLimit < req)) {
    SceMiAdapter *ncthis = const_cast<SceMiAdapter *> (this);

    // looking for more data than Q limit,  increase limit.
    ncthis->m_logfile.Warn ("Increasing receive queue limit from %d to %d bytes on pipe `%s'",
                            m_recvDataQLimit, (2*req), getNameStr());
    // Queue size increase to 2x desired.
    m_recvDataQLimit = 2 * req;
    ncthis->receivePull();
  }

  return m_recvDataQ.size();
}

// recvData must be locked.
bool SceMiAdapter::doRecv(MsgPacket &p, unsigned count)
{
  assert (m_getHandle != 0);
  bool stat = false;
  bool recvFull = (m_recvDataQLimit != 0) && (m_recvDataQ.size() >= m_recvDataQLimit);
  if (count <= m_recvDataQ.size()) {
    m_recvDataQ.copyFromFront(p, count);
    m_recvDataQ.pop(count);
    stat = true;
  }

  bool stillFull = (m_recvDataQLimit != 0) && (m_recvDataQ.size() >= m_recvDataQLimit);
  if (recvFull && ! stillFull ) {
    receivePull();
  }

  return stat;
}

bool SceMiAdapter::doPeek(MsgPacket &p, unsigned count) const
{
  assert (m_getHandle != 0);
  bool stat = false;
  if (count <= m_recvDataQ.size()) {
    m_recvDataQ.copyFromFront(p, count);
    stat = true;
  }
  return stat;
}

void SceMiAdapter::canSendCB(void *pt)
{
  SceMiAdapter *obj = reinterpret_cast<SceMiAdapter*>(pt);
  obj->canSendCB();
}

void SceMiAdapter::canSendCB()
{
  int32_t sent = -1;
  lockSend();

  int toSend =  m_sendDataQ.size() / m_putPipeWidth;
  if (toSend != 0) {
    // send the rest of the data

    lockCmds("sendCB");
    sent = scemi_pipe_c_try_send(m_putHandle, 0, toSend, m_sendDataQ.getFrontBitVec(), 0);
    unlockCmds("sendCB");

    if (m_debug) {
      m_logfile.Debug("SA sendCB -- %s sent %ld %08x", getNameStr(), (long) sent,
                      *m_sendDataQ.getFrontBitVec() );
    }


    m_sendDataQ.pop(sent * m_putPipeWidth);


  }

  signalSendCondition();

  unlockSend();
  executeSendCallBack();
}

void SceMiAdapter::canRecvCB(void *pt)
{
  SceMiAdapter *obj = reinterpret_cast<SceMiAdapter*>(pt);
  obj->canRecvCB();
}

void SceMiAdapter::canRecvCB()
{
  lockRecv();

  bool doCallBack = receivePull();

  if (doCallBack) {
    signalRecvCondition();
  }
  unlockRecv();
  if (doCallBack) {
    executeRecvCallBack();
  }
}

// requires the recv side is locked
bool SceMiAdapter::receivePull ()
{
  if ((m_recvDataQLimit != 0) && (m_recvDataQ.size() >= m_recvDataQLimit)) {
    return false;
  }

  MsgPacket data((m_getPipeDepth* m_getPipeWidth) + 1);
  svBit eom;
  //  svBitVecVal *ptr = static_cast<svBitVecVal*>(&data[0]);
  svBitVecVal *ptr = (unsigned int *) (&data[0]);

  lockCmds("recvCB");
  int32_t received = 0;
  bool doCallBack = false;

  // we need to pull everything out of the pipe, since we will not get another notification
  // unless the pipe is empty.
  do {
    received = scemi_pipe_c_try_receive(m_getHandle, 0, m_getPipeDepth+1, ptr, &eom);
    received *= m_getPipeWidth;

    data.resize(received);
    m_recvDataQ.push(data);

    if (m_debug) {
      std::stringstream oss;
      oss << getNameStr() << " Receiving: " << data;
      m_logfile.Debug(oss.str().c_str());
    }

    doCallBack = doCallBack || (received > 0);
    if ((m_recvDataQLimit != 0) && (m_recvDataQ.size() >= m_recvDataQLimit)) break;
  } while ((received > 0) && (eom == sv_1));

  unlockCmds("recvCB");
  return doCallBack;
}
