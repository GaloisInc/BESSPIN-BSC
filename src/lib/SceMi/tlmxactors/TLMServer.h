//-*- C++ -*-x
// Copyright (c) 2012-2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// a TLM server model use to handle requests from a master transactor
// This is a shell code and should be extended to handle specific requests

#include "../bsvxactors/Thread.h"
#include "MasterProxy.h"

template<unsigned int ADDRSIZE=32, unsigned int DATASIZE=32, unsigned int USERDSIZE=0>
class TLMServer : public Thread
{
private:
  MasterProxy<ADDRSIZE, DATASIZE, USERDSIZE> & m_xactor;
public:
  // Construction automatically start the thread
  TLMServer (MasterProxy<ADDRSIZE, DATASIZE, USERDSIZE> & xactor) 
    : m_xactor(xactor)
  {
    start();
  }
  ~TLMServer() {
    stop();
    join();
  }

private:
  TLMServer( const TLMServer &);
  TLMServer & operator= (const TLMServer &);

public:
  virtual void main () {

    time_t seconds = 1;
    long   microSeconds = 0;

    bool responseNeeded = false;
    TLMRequest<ADDRSIZE,DATASIZE,USERDSIZE> request ;
    TLMResponse<DATASIZE,USERDSIZE>         response ;

    while (! m_stop ) {

      // Wait for a request from the xactor use timed request to handle timeout
      if ( ! responseNeeded && m_xactor.receiveT (request, seconds, microSeconds)) {
        responseNeeded = true;
        std::cout << "request recevied " << request << std::endl;
        request.populateResponse (response);

        // Fill in the payload for read request
        if ( request.m_header.m_command == TLMCommand::e_READ ) {
          long long data = static_cast<long long> ( request.m_address);
          for ( unsigned i = 0 ; i < request.m_b_length ; ++ i) {
            BitT<DATASIZE> bit_data(data);
            response.m_payload.setFromBitT (i, bit_data);
            data += DATASIZE;
          }
        }

      }

      // return the response
      if (responseNeeded && m_xactor.sendT (response, seconds, microSeconds)) {
        std::cout << "response sent " << response << std::endl;
        responseNeeded = false;
      }
    } // end while (! m_stop)
  }

};
