//-*- C++ -*-x
#pragma once

#include "TLMRequest.h"
#include "TLMResponse.h"
#include "../bsvxactors/XactorCore.h"
#include "dllexport.h"

///  Slave Transactor: receives TLM requests; sends TLM responses.
/// This xactor is the host side of the client slave xactor.  It receives
/// TLMRequests to the client and sends TLMResponses.  Templates are
/// used to address width (bits) and data size width (bits)
template<unsigned int ADDRSIZE=32, unsigned int DATASIZE=32, unsigned int USERDSIZE=0>
class DLLEXPORT MasterProxy : public XactorCore {
private:

public:
  ///  Constructor for SceMi based client.
  /// \param name -- symbolic name
  /// \param path -- scemi path, in params file
  MasterProxy(const std::string &name, const std::string &path);

  ///  Constructor for SceMi based client. SceMi pipes
  /// \param name -- symbolic name
  /// \param path -- scemi path, in params file
  MasterProxy(const std::string &name, const std::string &path, const XactorAdapter::SPipes type);

  ///  Constructor for SceMi based client. SceMi ports
  /// \param name -- symbolic name
  /// \param path -- scemi path, in params file
  MasterProxy(const std::string &name, const std::string &path, const XactorAdapter::SPorts type);

  ///  Blocking send method.
  /// This methods blocks until the TLMResponse has been
  /// successfully passed to the xactor
  /// \param resp -- the TLMResponse to be sent
  void send   (const TLMResponse<DATASIZE,USERDSIZE > &resp) ;


  ///  Non-blocking send.
  /// This method sends the Response if the xactor can accept its.
  /// \param resp -- the TLMResponse to be sent
  /// \return  returns true if the Response has been sent
  bool sendNB (const TLMResponse<DATASIZE,USERDSIZE > &resp) ;


  ///  Timed-blocking send.
  /// This method tries to send the TLMResponse but will timeout if the
  /// request has not been sent within delta time.
  /// \param resp -- the TLMResponse to be sent
  /// \param seconds -- timeout delay in seconds
  /// \param microseconds -- delay in micro-seconds
  /// \return  returns true if the Request has been sent,  false if
  /// a timeout occurred.
  bool sendT  (const TLMResponse<DATASIZE,USERDSIZE > &resp
               , const time_t seconds, const long microseconds=0 );



  ///  Blocking receive.
  /// This method blocks until a request is received from the xactor.
  /// \param req -- the TLMRequest received (this is populated by
  /// this call.)
  void receive   (TLMRequest<ADDRSIZE,DATASIZE,USERDSIZE > &req);

  ///  Non-Blocking receive.
  /// This method checks if a request is available from the xactor
  /// and returns it if it is.
  /// \param req -- the TLMRequest received (this is populated by
  /// this call.)
  /// \return  returns true if the Request has been received
  bool receiveNB (TLMRequest<ADDRSIZE,DATASIZE,USERDSIZE > &req);

  ///  Timeout based receive.
  /// This method attempts to retrieve a message from the xactor,
  /// blocking until the message is received or the timeout expires.
  /// \param req -- the TLMRequest received (this is populated by
  /// this call.)
  /// \param seconds -- timeout delay in seconds
  /// \param microseconds -- delay in micro-seconds
  /// \return  returns true if the Request has been received,  false if
  /// a timeout occurred.
  bool receiveT  (TLMRequest<ADDRSIZE,DATASIZE,USERDSIZE > &req
                  , const time_t seconds, const long microseconds=0) ;

  ///  Indicates if a Response can be received.
  /// This method determines if a full TLMResponse is available and returns 1,
  /// 0 otherwise.  Unlike the XactorCore version, this does not return the full
  /// count of data
  virtual unsigned canReceive();

  /// Check the given request to see if it complies with the bus frabric of the
  /// transactor
  /// \param req -- the request to check
  /// \return returns true if the checks passed or were disabled
  bool checkTLMRequest (const TLMRequest<ADDRSIZE,DATASIZE,USERDSIZE > &req);

};
