// Copyright 2012-2013 Bluespec Inc.  All rights reserved

#pragma once

#include "MasterProxy.h"
#include "tlm.h"
#include "tlm_utils/simple_initiator_socket.h"

template<class IP, unsigned int A, unsigned int D, unsigned int U>
  void bsc_service_initiator_proxy ( tlm_utils::simple_initiator_socket< IP >  & socket
                                     ,MasterProxy<A, D, U> & mproxy)
{
  TLMRequest<A,D,U>      request;
  TLMResponse<D,U>       response;

  tlm::tlm_generic_payload  payload;
  sc_core::sc_time delay(0, sc_core::SC_NS);

  //=============================================================================
  // Read FIFO to Get new transaction GP from the traffic generator
  //=============================================================================

  if (mproxy.receiveNB(request)) {
    std::cerr << "Request received: " << request << std::endl;
    // convert to generic payload
    bsv_tlm_sysc::convertRequestToGP(request, response, payload);

    // Send the payload out -- blocking....
    socket->b_transport(payload, delay);

    // Convert back to response both the common fields and others
    bsv_tlm_sysc::convertGPToResponse(request, payload, response);

    std::cerr << "Sending response: " << response << std::endl;
    mproxy.send(response);

  }

}
