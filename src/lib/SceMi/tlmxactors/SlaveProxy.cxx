
#include "SlaveProxy.h"
#include "../bsvxactors/MsgPacket.h"
#include "../bsvxactors/XactorLog.h"
#include "TLMUtilities.h"

using namespace TLMUtilities ;

bool operator!= (const ResponseHandling &, const ResponseHandling &);

// Constructor for SceMi interface
template<unsigned int A, unsigned int D, unsigned int U>
SlaveProxy<A,D,U>::SlaveProxy(const std::string &name, const std::string &path)
  : XactorCore (name, path)
  , m_responseQ()
{
}

// Constructor for SceMi interface
template<unsigned int A, unsigned int D, unsigned int U>
SlaveProxy<A,D,U>::SlaveProxy(const std::string &name, const std::string &path,  const XactorAdapter::SPipes type)
  : XactorCore (name, path, type)
  , m_responseQ()
{
}

// Constructor for SceMi interface
template<unsigned int A, unsigned int D, unsigned int U>
SlaveProxy<A,D,U>::SlaveProxy(const std::string &name, const std::string &path,  const XactorAdapter::SPorts type)
  : XactorCore (name, path, type)
  , m_responseQ()
{
}


template<unsigned int A, unsigned int D, unsigned int U>
void SlaveProxy<A,D,U>::send (const TLMRequest<A,D,U> &req, bool discardResponse) {
  // Convert the request into a packet
  checkTLMRequest(req);
  MsgPacket p(4*req.getWordSize());

  queueExpectedResponse (req, discardResponse);
  req.setMessageData(p);
  XactorCore::send(p);
}


template<unsigned int A, unsigned int D, unsigned int U>
bool SlaveProxy<A,D,U>::sendNB (const TLMRequest<A,D,U> &req, bool discardResponse) {
  checkTLMRequest(req);
  bool stat = false;
  // Convert the request into a packet
  MsgPacket p(4*req.getWordSize());

  req.setMessageData(p);
  if (XactorCore::sendNB(p)) {
    queueExpectedResponse (req, discardResponse);
    stat = true;
  }
  return stat;
}

template<unsigned int A, unsigned int D, unsigned int U>
bool SlaveProxy<A,D,U>:: sendT (const TLMRequest<A,D,U> &req
                                , struct timespec &expiration
                                , bool discardResponse ) {
  checkTLMRequest(req);
  bool stat = false;
  // Convert the request into a packet
  MsgPacket p(4*req.getWordSize());

  req.setMessageData(p);
  if (XactorCore::sendT(p, expiration)) {
    stat = true;
    queueExpectedResponse (req,discardResponse);
  }
  return stat;
}

template<unsigned int A, unsigned int D, unsigned int U>
bool SlaveProxy<A,D,U>:: sendT (const TLMRequest<A,D,U> &req
                                , const time_t seconds, const long microseconds
                                , bool discardResponse ) {
  struct timespec expiration;
  setTimeout(expiration, seconds, microseconds);

  return sendT(req, expiration, discardResponse);
}

template<unsigned int A, unsigned int D, unsigned int U>
void SlaveProxy<A,D,U>::receive (TLMResponse<D,U> &resp) {
  bool gotOne = false;

  while (!gotOne) {
    // Get the response size (blocking call!)
    ResponseHandling rh = m_responseQ.get();
    unsigned int payloadsize = rh.m_payload;
    MsgPacket p;
    XactorCore::receive(p, 4*resp.getWordSizeByPayload(payloadsize));

    if (!rh.m_discard) {
      unsigned int off=0;
      resp = TLMResponse<D,U>(&p,off);
      gotOne = true;
    }
  }
}

template<unsigned int A, unsigned int D, unsigned int U>
bool SlaveProxy<A,D,U>::receiveNB (TLMResponse<D,U> &resp) {

  bool stat = false;
  ResponseHandling rh;
  MsgPacket p;

  while (!stat
         && m_responseQ.peekNB (rh)
         && XactorCore::receiveNB(p, 4*resp.getWordSizeByPayload(rh.m_payload))
         ) {

    if (rh != m_responseQ.get()) {
      XactorLog logfile;
      logfile.Error("SlaveProxy<A,D,U>::receivedNB response size changed Impossible! %d",
                    rh.m_payload);
    }

    if (!rh.m_discard) {
      unsigned int off=0;
      resp = TLMResponse<D,U>(&p,off);
      stat = true;
    }
  }
  return stat ;
}


template<unsigned int A, unsigned int D, unsigned int U>
bool SlaveProxy<A,D,U>:: receiveT (TLMResponse<D,U> &resp
                                   ,struct timespec &expiration) {
  bool stat = false;
  ResponseHandling rh;
  MsgPacket p;

  while (!stat
         && m_responseQ.peekT (rh, expiration)
         && XactorCore::receiveT(p, 4*resp.getWordSizeByPayload(rh.m_payload), expiration)
      ) {

    if (rh != m_responseQ.get()) {
      XactorLog logfile;
      logfile.Error ("SlaveProxy<A,D,U>::receivedT response size changed! Impossible! %d", rh.m_payload);
    }

    if (!rh.m_discard) {
      stat = true;
      unsigned int off=0;
      resp = TLMResponse<D,U>(&p,off);
    }
  }
  return stat ;
}

template<unsigned int A, unsigned int D, unsigned int U>
bool SlaveProxy<A,D,U>:: receiveT (TLMResponse<D,U> &resp
                                   , const time_t seconds, const long microseconds) {
  struct timespec expiration;
  setTimeout(expiration, seconds, microseconds);
  return receiveT (resp, expiration);
}

template<unsigned int A, unsigned int D, unsigned int U>
unsigned SlaveProxy<A,D,U>::canReceive () {
  unsigned ret = 0;
  TLMResponse<D,U> resp;
  MsgPacket p;
  ResponseHandling rh;
  bool keepGoing = true;

  while (keepGoing
         && m_responseQ.peekNB(rh)) {

    if (rh.m_discard) {
      // discard and continue
      if (XactorCore::receiveNB(p, 4*resp.getWordSizeByPayload(rh.m_payload)) ) {
        if (rh != m_responseQ.get()) {
          XactorLog logfile;
          logfile.Error("SlaveProxy<A,D,U>::canReceive response size changed Impossible! %d",
                        rh.m_payload);
        }
      }
      else {
        // there is not enough data in the xactor,
        keepGoing = false;
      }
    }
    else {
      keepGoing = false;
      unsigned required = (4u *  resp.getWordSizeByPayload(rh.m_payload));
      ret = (XactorCore::canReceive(required) >= required ) ? 1:0;
    }
  }
  return ret;
}

template<unsigned int A, unsigned int D, unsigned int U>
void SlaveProxy<A,D,U>::send (const TLMRequest<A,D,U> &req, TLMResponse<D,U> &resp) {
  send(req, false);
  receive(resp);
}

template<unsigned int A, unsigned int D, unsigned int U>
bool SlaveProxy<A,D,U>::sendT (const TLMRequest<A,D,U> &req, TLMResponse<D,U> &resp
                               , const time_t seconds, const long microseconds) {
  struct timespec expiration;
  setTimeout(expiration, seconds, microseconds);

  sendT(req, expiration, false);
  return receiveT(resp, expiration);
}

template<unsigned int A, unsigned int D, unsigned int U>
bool SlaveProxy<A,D,U>::checkTLMRequest (const TLMRequest<A,D,U> &req) {
  bool stat = true;
  // if (m_fabric != e_UNKNOWN) {

  //   TLMUtilities::ErrorList_t errs;
  //   unsigned ecount = isValid (m_fabric, req, errs);
  //   if (ecount != 0) {
  //     stat = false;
  //     XactorLog logfile;
  //     for (ErrorList_t::iterator iter = errs.begin(); iter != errs.end(); ++iter ) {
  //       logfile.Debug(m_reportLevel, "SlaveProxy `%s', %s", getNameStr(),
  //                     iter->c_str() );
  //     }
  //   }
  // }
  return stat;
}


template<unsigned int A, unsigned int D, unsigned int U>
void  SlaveProxy<A,D,U>::queueExpectedResponse( const TLMRequest<A,D,U> & req, bool discard ) {

  ResponseHandling rh;
  rh.m_payload = req.determineResponseSize();
  // Only discard write commands
  rh.m_discard = discard && (req.m_header.m_command == TLMCommand::e_WRITE);
  m_responseQ.put(rh);
}


bool operator!= (const ResponseHandling &l, const ResponseHandling &r) {

  return (l.m_payload != r.m_payload) || (l.m_discard != r.m_discard);
}


template class DLLEXPORT SlaveProxy<32,8,0>;
template class DLLEXPORT SlaveProxy<32,16,0>;
template class DLLEXPORT SlaveProxy<32,32,0>;
template class DLLEXPORT SlaveProxy<32,64,0>;
template class DLLEXPORT SlaveProxy<32,128,0>;
template class DLLEXPORT SlaveProxy<32,256,0>;
template class DLLEXPORT SlaveProxy<32,512,0>;
template class DLLEXPORT SlaveProxy<32,1024,0>;

template class DLLEXPORT SlaveProxy<64,8,0>;
template class DLLEXPORT SlaveProxy<64,16,0>;
template class DLLEXPORT SlaveProxy<64,32,0>;
template class DLLEXPORT SlaveProxy<64,64,0>;
template class DLLEXPORT SlaveProxy<64,128,0>;
template class DLLEXPORT SlaveProxy<64,256,0>;
template class DLLEXPORT SlaveProxy<64,512,0>;
template class DLLEXPORT SlaveProxy<64,1024,0>;


template class DLLEXPORT SlaveProxy<32,8,32>;
template class DLLEXPORT SlaveProxy<32,16,32>;
template class DLLEXPORT SlaveProxy<32,32,32>;
template class DLLEXPORT SlaveProxy<32,64,32>;
template class DLLEXPORT SlaveProxy<32,128,32>;
template class DLLEXPORT SlaveProxy<32,256,32>;
template class DLLEXPORT SlaveProxy<32,512,32>;
template class DLLEXPORT SlaveProxy<32,1024,32>;

template class DLLEXPORT SlaveProxy<64,8,32>;
template class DLLEXPORT SlaveProxy<64,16,32>;
template class DLLEXPORT SlaveProxy<64,32,32>;
template class DLLEXPORT SlaveProxy<64,64,32>;
template class DLLEXPORT SlaveProxy<64,128,32>;
template class DLLEXPORT SlaveProxy<64,256,32>;
template class DLLEXPORT SlaveProxy<64,512,32>;
template class DLLEXPORT SlaveProxy<64,1024,32>;


template class DLLEXPORT SlaveProxy<32,8,64>;
template class DLLEXPORT SlaveProxy<32,16,64>;
template class DLLEXPORT SlaveProxy<32,32,64>;
template class DLLEXPORT SlaveProxy<32,64,64>;
template class DLLEXPORT SlaveProxy<32,128,64>;
template class DLLEXPORT SlaveProxy<32,256,64>;
template class DLLEXPORT SlaveProxy<32,512,64>;
template class DLLEXPORT SlaveProxy<32,1024,64>;

template class DLLEXPORT SlaveProxy<64,8,64>;
template class DLLEXPORT SlaveProxy<64,16,64>;
template class DLLEXPORT SlaveProxy<64,32,64>;
template class DLLEXPORT SlaveProxy<64,64,64>;
template class DLLEXPORT SlaveProxy<64,128,64>;
template class DLLEXPORT SlaveProxy<64,256,64>;
template class DLLEXPORT SlaveProxy<64,512,64>;
template class DLLEXPORT SlaveProxy<64,1024,64>;
