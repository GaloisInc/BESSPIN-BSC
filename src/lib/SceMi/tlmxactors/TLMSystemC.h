// Copyright (c) 2012-2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include "tlm.h"

#include "MasterProxy.h"
#include "SlaveProxy.h"
#include "TLMUtilities.h"

namespace bsv_tlm_sysc {

  inline TLMCommand::E_TLMCommand cvtCommand( tlm::tlm_command tlmcmd )
  {
    TLMCommand::E_TLMCommand cmd;
    switch (tlmcmd) {
    case tlm::TLM_READ_COMMAND: cmd = TLMCommand::e_READ; break ;
    case tlm::TLM_WRITE_COMMAND: cmd = TLMCommand::e_WRITE; break ;
    case tlm::TLM_IGNORE_COMMAND: cmd = TLMCommand::e_UNKNOWN; break ;
    }
    return cmd;
  }

 inline tlm::tlm_command cvtCommand( TLMCommand cmd )
  {
    tlm::tlm_command sc_cmd;
    switch (cmd.m_val) {
    case TLMCommand::e_READ:    sc_cmd = tlm::TLM_READ_COMMAND   ; break ;
    case TLMCommand::e_WRITE:   sc_cmd = tlm::TLM_WRITE_COMMAND  ; break ;
    case TLMCommand::e_UNKNOWN: sc_cmd = tlm::TLM_IGNORE_COMMAND ; break ;
    }
    return sc_cmd;
  }

  inline tlm::tlm_response_status cvtResponse ( const TLMResponseParams & resp )
  {
    switch (resp.m_status.m_val) {
    case TLMStatus::e_SUCCESS: return tlm::TLM_OK_RESPONSE;
      // XXX How do these map to tlm?
      // case e_ERROR:   switch (resp.m_error.m_val) {
      //   case e_NONE:
      //   case e_SPLIT_CONTINUE:
      //   case e_RETRY:
      //   case e_SPLIT:
      //   case e_RW_ONLY:
      //   case e_UNMAPPED:
      //   case e_SLVERR:
      //   case e_DECERR:
      //    }
      // case e_EXOKAY,
      // case e_UNKNOWN
    default: break;
    }
    return tlm::TLM_GENERIC_ERROR_RESPONSE;
  }


  inline TLMStatus cvtResponse ( const tlm::tlm_response_status & status )
  {
    switch (status) {
    case tlm::TLM_OK_RESPONSE: return TLMStatus::e_SUCCESS;
    default: break;
    }
    return TLMStatus::e_ERROR;
  }


  template<unsigned D>
  unsigned bytesToPayloadSize (unsigned bc) {
    return std::max (1u, bc /(D/8u));
  }

  ///////////////////////////////////////////////////////
  // Conversion from a tlm_generic_payload to a TLMRequest
  template<unsigned A, unsigned D, unsigned U>
  void populate_tlm_request ( const tlm::tlm_generic_payload &payload, TLMRequest<A,D,U> &request)
  {
    request.m_header.m_command = cvtCommand (payload.get_command());
    // request.m_header.m_transaction_id = tid;
    request.m_address = payload.get_address() ;
    // Data size --
    unsigned int byte_len = payload.get_data_length();
    request.m_b_length = bytesToPayloadSize<D> (byte_len);

    if (request.m_header.m_command == TLMCommand::e_WRITE) {
      // convert data here
      request.setPayload();
      request.m_payload.setPayloadData ((char *) payload.get_data_ptr());
      // XXX todo  Byte enables
    }
  }

  ///////////////////////////////////////////////////////
  // Conversion from a TLMResponse to tlm_generic_payload
  template<unsigned D, unsigned U>
  void extract_from_tlm_response ( const TLMResponse<D,U> & response,  tlm::tlm_generic_payload &payload)
  {
    payload.set_dmi_allowed (false);
    payload.set_response_status ( cvtResponse(response.m_header) );
    if (payload.is_read()) {
      // convert payload
      response.m_payload.fillMemory ((char *) payload.get_data_ptr(), payload.get_data_length());
    }

  }

  // Execute a transaction in the transactor according to the payload.
  template<unsigned A, unsigned D, unsigned U>
  void send_request_response ( SlaveProxy<A,D,U> & mxactor,  tlm::tlm_generic_payload  &payload)
  {
    TLMRequest<A,D,U> request;
    TLMResponse<D,U>  response;

    populate_tlm_request ( payload, request);
    std::cerr << "REQ: " << request << std::endl;
    mxactor.send( request, response);
    extract_from_tlm_response ( response, payload );
    std::cerr << "RESP: " << response << std::endl;
  }

  // Generate the expected response shell from the request.
  template <unsigned A, unsigned D, unsigned U>
    void genResponseShell (const TLMRequest<A, D, U> &req
                           , TLMResponse<D,U> &resp)
    {

      resp.m_header.m_transaction_id = req.m_header.m_transaction_id;
      resp.m_b_length       = req.m_b_length;
      resp.m_header.m_command        = req.m_header.m_command;
      resp.m_header.m_status         = TLMStatus::e_SUCCESS;
      resp.m_header.m_error          = TLMErrorCode::e_NONE;

      resp.setPayload();
  }


  template<unsigned A, unsigned D, unsigned U>
    void convertRequestToGP (const TLMRequest<A,D,U> & request
                             , TLMResponse<D,U> &resp
                             , tlm::tlm_generic_payload  &payload )
  {
    genResponseShell (request, resp);

    payload.set_command (cvtCommand (request.m_header.m_command.m_val));
    payload.set_address (request.m_address);
    payload.set_data_length (request.m_b_length * D /8u);

    // Setup the payload.  If a read, then use the payload on the expecte write
    if (payload.is_read()) {
      payload.set_data_ptr (  reinterpret_cast<unsigned char *> (resp.m_payload.getDataPtr(0)) );
    }
    else {
      payload.set_data_ptr (  reinterpret_cast<unsigned char *> (request.m_payload.getDataPtr(0)) );
    }

    // TODO Byte enables
    // TODO extensions...
  }

  template<unsigned A, unsigned D, unsigned U>
    void convertGPToResponse (const TLMRequest<A,D,U> & request
                              , const tlm::tlm_generic_payload  &payload
			      , TLMResponse<D,U> &resp)
  {
    genResponseShell (request, resp);
    resp.m_header.m_status = cvtResponse (payload.get_response_status());
  }



} // namespace bsv_tlm_sysc
