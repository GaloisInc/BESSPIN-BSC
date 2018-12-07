
#include "SceMiPortAdapter.h"
#include <assert.h>
#include <string>
#include <sstream>
#include "scemi.h"
#include "XactorLog.h"
#include "MsgPacket.h"

// Hooks for licensing....
extern void xactors_checkin();
extern void xactors_checkout();

// Static variables
pthread_mutex_t  SceMiPortAdapter::s_cmd_mutex = PTHREAD_MUTEX_INITIALIZER;


// Constructor
SceMiPortAdapter::SceMiPortAdapter(const std::string & name, const std::string & path)
  : XactorAdapter(name + "." + path)
  , m_inRCB(false)
  , m_readyToSend(false)
{
  //  xactors_checkout();

  std::string inPipeName(path);
  std::string outPipeName(path);

  inPipeName  += "_instream";
  outPipeName += "_outstream";

  SceMi *scemi = SceMi::Pointer();

  initInPort (name, inPipeName, scemi );
  initOutPort (name, outPipeName, scemi );

  //m_debug = true;
  //m_debugL = true;
}


// Constructor
  SceMiPortAdapter::SceMiPortAdapter(const std::string & name, const std::string & path, XactorAdapter::SInport type )
  : XactorAdapter(name + "." + path)
  , m_poutport(0)
  , m_inRCB(false)
  , m_readyToSend(false)
{
  //  xactors_checkout();

  std::string inPipeName(path);

  SceMi *scemi = SceMi::Pointer();
  initInPort (name, inPipeName, scemi );

  //m_debug = true;
  //m_debugL = true;
}

// Constructor
SceMiPortAdapter::SceMiPortAdapter(const std::string & name, const std::string & path, XactorAdapter::SOutport type)
  : XactorAdapter(name + "." + path)
  , m_pinport(0)
  , m_inRCB(false)
  , m_readyToSend(false)
{
  //  xactors_checkout();

  std::string outPipeName(path);

  SceMi *scemi = SceMi::Pointer();
  initOutPort (name, outPipeName, scemi );

  //m_debug = true;
  //m_debugL = true;
}


void SceMiPortAdapter::initOutPort (const std::string &name, const std::string & outPipeName, SceMi *scemi ) {

  // Initialize the SceMi OUTPUT port
  m_poutport = scemi->BindMessageOutPort (name.c_str(), outPipeName.c_str() );
  if (m_poutport ==  0) {
    XactorLog logfile;
    logfile.Error( "Unable to open scemi output pipe %s %s.", name.c_str(), outPipeName.c_str() );
    return;
  }

  SceMiMessageOutPortBinding outPortBinding ;
  outPortBinding.Context = this;
  outPortBinding.Receive = SceMiPortAdapter::canRecvCB;
  outPortBinding.Close   = SceMiPortAdapter::closeOutport;
  m_poutport->ReplaceBinding (&outPortBinding);
}

// private static function
void SceMiPortAdapter::closeOutport (void *pt)  {
  SceMiPortAdapter *obj = (SceMiPortAdapter *) pt;
  obj->closeOutport();
}
void SceMiPortAdapter::closeOutport() {
  lockRecv();
  setRecvCallBack(0,0);
  unlockRecv();
  m_poutport->ReplaceBinding();
  m_poutport = NULL;

}


void SceMiPortAdapter::initInPort (const std::string &name, const std::string &inPipeName, SceMi *scemi ) {
  // Initialize the SceMi INPUT port
  m_pinport = scemi->BindMessageInPort (name.c_str(), inPipeName.c_str() );
  if (m_pinport ==  0) {
    XactorLog logfile;
    logfile.Error ( "Unable to open scemi input pipe %s %s.", name.c_str(), inPipeName.c_str() );
    return;
  }
  // set up the local callbacks
  SceMiMessageInPortBinding inPortBinding ;
  inPortBinding.Context = this;
  inPortBinding.IsReady = SceMiPortAdapter::canSendCB;
  inPortBinding.Close   = SceMiPortAdapter::closeInport;
  m_pinport->ReplaceBinding (&inPortBinding);
}

// private static function
void SceMiPortAdapter::closeInport (void *pt)  {
  SceMiPortAdapter *obj = (SceMiPortAdapter *) pt;
  obj->closeInport();
}
void SceMiPortAdapter::closeInport() {
  lockSend();
  setSendCallBack(0,0);
  unlockSend();
  m_pinport->ReplaceBinding();
  m_pinport = NULL;
}


SceMiPortAdapter::~SceMiPortAdapter() {
  // turn off call backs
  if (m_pinport) {
    closeInport();
  }

  if (m_poutport) {
    closeOutport();
  }


  //
  //xactors_checkin();
}


bool SceMiPortAdapter::canSend() const {
  // Always allow sends if we are in buffered mode
  return  m_sendDataQ.empty();
}

// All the data is gone, and we're ready to send.
bool SceMiPortAdapter::sendAcknowledged()
{
  return canSend() && m_readyToSend;
}

/// Main entry for send process.
/// assume lock on send data
bool SceMiPortAdapter::doSend (const MsgPacket &p) {

  if (m_debug) {
    std::stringstream oss;
    oss << getName() << " Sending: " << p << std::endl;
    XactorLog logfile;
    logfile.Debug("SPA - %s", oss.str().c_str());
    debug();
  }

  bool stat = true;
  unsigned sent = 0;

  if (m_readyToSend && m_sendDataQ.empty()) {

    // Standard SceMi message (limited constr)
    SceMiMessageData outMsg ( *m_pinport);
    unsigned byteWidth = (7+outMsg.WidthInBits())/8;

    if (p.size() >= byteWidth) {
      sent = byteWidth;
      p.copy (outMsg);
      m_pinport->Send( outMsg );
      m_readyToSend = false;
    }

  }
  // Queue the remaining data.
  if (sent < p.size()) {
    m_sendDataQ.push(p, sent);
    stat = true;
  }
  return stat;
}
// private static function
void SceMiPortAdapter::canSendCB (void *pt) {
  SceMiPortAdapter *obj = (SceMiPortAdapter *) pt;
  obj->canSendCB();
}

// Call back called when InportQueue finds itself empty
void SceMiPortAdapter::canSendCB () {

  lockSend();
  unsigned sent = 0;

  // Standard SceMi message (limited constr)
  SceMiMessageData outMsg ( *m_pinport);

  if (m_sendDataQ.take(outMsg)) {
    m_pinport->Send( outMsg );
    ++sent;
  }
  else {
    m_readyToSend = true;
  }

  // Notify the user after last call is sent,  proxy is not ready to send,
  // but we can queue more data.
  bool doCB = (m_readyToSend) || m_sendDataQ.empty();

  if (doCB) {
    signalSendCondition();
  }
  unlockSend();

  // Execute user callback without locks
  if (doCB) executeSendCallBack();
}


unsigned SceMiPortAdapter::canReceive(const unsigned req) const {
  // req is unused.  the m_recDataQ is unlimited since back pressure cannot cross to dut
  return m_recvDataQ.size();
}

bool SceMiPortAdapter::doRecv (MsgPacket &pm, unsigned count ) {
  bool stat = false;

  if (count <= m_recvDataQ.size()) {
    m_recvDataQ.copyFromFront(pm, count);
    m_recvDataQ.pop(count);
    stat = true;
  }
  if (m_debug) {
    std::stringstream oss;
    oss << getNameStr() << " Receiving: " << pm << std::endl;
    m_logfile.Debug(oss.str().c_str());
  }
  return stat;
}
bool SceMiPortAdapter::doPeek(MsgPacket &pm, unsigned count) const
{
  bool stat = false;
  if (count <= m_recvDataQ.size()) {
    m_recvDataQ.copyFromFront(pm, count);
    stat = true;
  }
  return stat;
}


// private static function
void SceMiPortAdapter::canRecvCB (void *pt, const SceMiMessageData *msg)  {
  SceMiPortAdapter *obj = (SceMiPortAdapter *) pt;
  obj->canRecvCB(msg);
}
// Callback from the outport proxy
void SceMiPortAdapter::canRecvCB(const SceMiMessageData *msg){

  if (m_inRCB) {
    XactorLog logfile;
    logfile.Error ( "SPA %s calling canRecvCB when already entered ....!", getNameStr());
    return;
  }

  lockRecv();
  m_inRCB=true;

  m_recvDataQ.push( msg );
  // Handle the scemi timestamp from the message.

  bool doCB = true;

  m_inRCB=false;
  if (doCB) {
    signalRecvCondition();
  }
  unlockRecv();
  // User call back with unlocked state
  if (doCB) {
    executeRecvCallBack();
  }
}

void SceMiPortAdapter::debug () const {
   XactorLog logfile;
   logfile.Debug("SceMiPortAdapter %s: canSend=%d, SendQ=%ld, canRecv=%ld RecvQ=%ld ",
                  getNameStr(), canSend(), (long) m_sendDataQ.size(), (long) canReceive(0u), (long) m_recvDataQ.size() );
}
