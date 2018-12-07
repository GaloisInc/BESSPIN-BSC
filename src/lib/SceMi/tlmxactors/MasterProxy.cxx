//-*- C++ -*-x

#include "MasterProxy.h"
#include "../bsvxactors/MsgPacket.h"
#include "../bsvxactors/XactorLog.h"
#include "TLMUtilities.h"
#include "dllexport.h"

using namespace TLMUtilities ;

// Constructor for SceMi interface
template<unsigned int A, unsigned int D, unsigned int U>
MasterProxy<A,D,U>::MasterProxy(const std::string &name, const std::string &path)
  : XactorCore (name, path)
{
}

template<unsigned int A, unsigned int D, unsigned int U>
MasterProxy<A,D,U>::MasterProxy(const std::string &name, const std::string &path, const XactorAdapter::SPipes type)
  : XactorCore (name, path, type)
{
}

template<unsigned int A, unsigned int D, unsigned int U>
MasterProxy<A,D,U>::MasterProxy(const std::string &name, const std::string &path, const XactorAdapter::SPorts type)
  : XactorCore (name, path, type)
{
}

template<unsigned int A, unsigned int D, unsigned int U>
void MasterProxy<A,D,U>::send (const TLMResponse<D,U> &resp) {
  // Convert the request into a packet
  MsgPacket p(4*resp.getWordSize());

  resp.setMessageData(p);
  XactorCore::send(p);
}

template<unsigned int A, unsigned int D, unsigned int U>
bool MasterProxy<A,D,U>::sendNB (const TLMResponse<D,U> &resp) {
  // Convert the request into a packet
  MsgPacket p(4*resp.getWordSize());

  resp.setMessageData(p);
  return XactorCore::sendNB(p);
}

template<unsigned int A, unsigned int D, unsigned int U>
bool MasterProxy<A,D,U>:: sendT (const TLMResponse<D,U> &resp
                               , const time_t seconds, const long microseconds){
  // Convert the request into a packet
  MsgPacket p(4*resp.getWordSize());

  resp.setMessageData(p);
  return XactorCore::sendT(p, seconds, microseconds);
}

template<unsigned int A, unsigned int D, unsigned int U>
void MasterProxy<A,D,U>:: receive (TLMRequest<A,D,U> &req) {
  // Clear the request
  req = TLMRequest<A,D,U>();

  // Get the Header & address so we know the size
  unsigned int hasize = 4*req.getWordSize();

  MsgPacket p;
  XactorCore::receive(p, hasize);
  unsigned int offset=0;
  req.unpackHeaderAddress(&p,offset);

  // Receive the Payload
  unsigned int psize = (4*req.getWordSize()) - hasize;
  XactorCore::receive(p, psize);
  offset = 0;
  req.unpackPayload(&p, offset);

  checkTLMRequest(req);
}

template<unsigned int A, unsigned int D, unsigned int U>
bool MasterProxy<A,D,U>::receiveNB (TLMRequest<A,D,U> &req){
  bool stat  = false;
  // Set the request to be a read, with no payload.
  req = TLMRequest<A,D,U>();
  req.m_header.m_command =  TLMCommand::e_READ ;

  // Get size of the Header & address
  unsigned int hasize = 4*req.getWordSize();

  MsgPacket p;
  if (peekNB(p, hasize)) {
    unsigned int offset=0;
    req.unpackHeaderAddress(&p,offset);
    // Now see if we can get the full payload
    unsigned int psize = 4*req.getWordSize();

    if (XactorCore::receiveNB(p, psize)) {
      offset = 0;
      req = TLMRequest<A,D,U>(&p,offset);
      checkTLMRequest(req);
      stat = true;
    }
  }
  return stat ;
}

template<unsigned int A, unsigned int D, unsigned int U>
bool MasterProxy<A,D,U>::receiveT (TLMRequest<A,D,U> &req
                                 , const time_t seconds, const long microseconds) {
  bool stat  = false;
  // Set the request to be a read, with no payload.
  req = TLMRequest<A,D,U>();
  req.m_header.m_command =  TLMCommand::e_READ ;

  // Get size of the Header & address
  unsigned int hasize = 4*req.getWordSize();

  struct timespec expiration;
  // Use one timeout for entire operation
  setTimeout(expiration, seconds, microseconds);

  MsgPacket p;
  if (peekT(p, hasize, expiration)) {
    unsigned int offset=0;
    req.unpackHeaderAddress(&p,offset);
    // Now see if we can get the full payload
    unsigned int psize = 4*req.getWordSize();

    if (XactorCore::receiveT(p, psize, expiration)) {
      offset = 0;
      req = TLMRequest<A,D,U>(&p,offset);
      checkTLMRequest(req);
      stat = true;
    }
  }
  return stat ;

}

template<unsigned int A, unsigned int D, unsigned int U>
unsigned MasterProxy<A,D,U>::canReceive () {
  unsigned ret = 0;
  // Set the request to be a read, with no payload.
  TLMRequest<A,D,U> req;
  req.m_header.m_command =  TLMCommand::e_READ ;

  // Get size of the Header & address
  unsigned int hasize = 4*req.getWordSize();

  MsgPacket p;
  if (peekNB(p, hasize)) {
    unsigned int offset=0;
    req.unpackHeaderAddress(&p,offset);
    // Now see if we can get the full payload
    unsigned int psize = 4*req.getWordSize();

    ret = (XactorCore::canReceive(psize) >= psize) ? 1 : 0;
  }
  return ret;
}

template<unsigned int A, unsigned int D, unsigned int U>
bool MasterProxy<A,D,U>::checkTLMRequest (const TLMRequest<A,D,U> &req) {
  bool stat = true;
  // if (m_fabric != e_UNKNOWN) {

  //   TLMUtilities::ErrorList_t errs;
  //   unsigned ecount = isValid (m_fabric, req, errs);
  //   if (ecount != 0) {
  //     stat = false;
  //     XactorLog logfile;
  //     for (ErrorList_t::iterator iter = errs.begin(); iter != errs.end(); ++iter ) {
  //       logfile.Debug(m_reportLevel, "MasterProxy `%s', %s", getNameStr(),
  //                     iter->c_str() );
  //     }
  //   }
  // }
  return stat;
}

template class DLLEXPORT MasterProxy<32,8,0>;
template class DLLEXPORT MasterProxy<32,16,0>;
template class DLLEXPORT MasterProxy<32,32,0>;
template class DLLEXPORT MasterProxy<32,64,0>;
template class DLLEXPORT MasterProxy<32,128,0>;
template class DLLEXPORT MasterProxy<32,256,0>;
template class DLLEXPORT MasterProxy<32,512,0>;
template class DLLEXPORT MasterProxy<32,1024,0>;

template class DLLEXPORT MasterProxy<64,8,0>;
template class DLLEXPORT MasterProxy<64,16,0>;
template class DLLEXPORT MasterProxy<64,32,0>;
template class DLLEXPORT MasterProxy<64,64,0>;
template class DLLEXPORT MasterProxy<64,128,0>;
template class DLLEXPORT MasterProxy<64,256,0>;
template class DLLEXPORT MasterProxy<64,512,0>;
template class DLLEXPORT MasterProxy<64,1024,0>;



template class DLLEXPORT MasterProxy<32,8,32>;
template class DLLEXPORT MasterProxy<32,16,32>;
template class DLLEXPORT MasterProxy<32,32,32>;
template class DLLEXPORT MasterProxy<32,64,32>;
template class DLLEXPORT MasterProxy<32,128,32>;
template class DLLEXPORT MasterProxy<32,256,32>;
template class DLLEXPORT MasterProxy<32,512,32>;
template class DLLEXPORT MasterProxy<32,1024,32>;

template class DLLEXPORT MasterProxy<64,8,32>;
template class DLLEXPORT MasterProxy<64,16,32>;
template class DLLEXPORT MasterProxy<64,32,32>;
template class DLLEXPORT MasterProxy<64,64,32>;
template class DLLEXPORT MasterProxy<64,128,32>;
template class DLLEXPORT MasterProxy<64,256,32>;
template class DLLEXPORT MasterProxy<64,512,32>;
template class DLLEXPORT MasterProxy<64,1024,32>;



template class DLLEXPORT MasterProxy<32,8,64>;
template class DLLEXPORT MasterProxy<32,16,64>;
template class DLLEXPORT MasterProxy<32,32,64>;
template class DLLEXPORT MasterProxy<32,64,64>;
template class DLLEXPORT MasterProxy<32,128,64>;
template class DLLEXPORT MasterProxy<32,256,64>;
template class DLLEXPORT MasterProxy<32,512,64>;
template class DLLEXPORT MasterProxy<32,1024,64>;

template class DLLEXPORT MasterProxy<64,8,64>;
template class DLLEXPORT MasterProxy<64,16,64>;
template class DLLEXPORT MasterProxy<64,32,64>;
template class DLLEXPORT MasterProxy<64,64,64>;
template class DLLEXPORT MasterProxy<64,128,64>;
template class DLLEXPORT MasterProxy<64,256,64>;
template class DLLEXPORT MasterProxy<64,512,64>;
template class DLLEXPORT MasterProxy<64,1024,64>;
