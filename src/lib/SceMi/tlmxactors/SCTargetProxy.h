// Copyright 2012-2013 Bluespec Inc.  All rights reserved
#pragma once

#include "tlm.h"
#include "tlm_utils/simple_target_socket.h"

/// Representation of a SlaveProxy as a SystemC target model
template<unsigned int ADDRSIZE=32, unsigned int DATASIZE=32, unsigned int USERDSIZE=0>
  class SCTargetProxy : public sc_core::sc_module  {

public:
  // The C++ Slave proxy is left as a public member to allow user access and extensions
  SlaveProxy<ADDRSIZE, DATASIZE, USERDSIZE> & m_slavep;
  tlm_utils::simple_target_socket<SCTargetProxy<ADDRSIZE,DATASIZE,USERDSIZE> >  m_target_socket;

private:
  const sc_core::sc_time    m_read_response_delay;  ///< read response delay
  const sc_core::sc_time    m_write_response_delay; ///< write response delays


public:
  // Constructor
  // Assumes the SlaveProxy has been created.
  SCTargetProxy ( SlaveProxy<ADDRSIZE, DATASIZE, USERDSIZE> & slave
                  , const sc_core::sc_time read_delay
                  , const sc_core::sc_time write_delay )
    : m_slavep(slave)
    , m_read_response_delay(read_delay)
    , m_write_response_delay(write_delay)
  {
    m_target_socket.register_b_transport(this, custom_b_transport);

  }

  // Destructor
  ~SCTargetProxy() {}

private:
  /// b_transport() - Blocking Transport
  void                                                // returns nothing
  custom_b_transport ( tlm::tlm_generic_payload  &payload
                       , sc_core::sc_time        &delay_time
                       )
  {

    // m_target_memory.operation(payload, mem_op_time);
    bsv_tlm_sysc::send_request_response ( m_slavep, payload);

    sc_core::sc_time mem_op_time = payload.is_read() ? m_read_response_delay : m_write_response_delay;
    delay_time = delay_time + mem_op_time;
  }

};


// template<unsigned int ADDRSIZE=32, unsigned int DATASIZE=32, unsigned int USERDSIZE=0>
// void bsc_b_transport_common (SlaveProxy<ADDRSIZE, DATASIZE, USERDSIZE>  &proxy
//                              , tlm::tlm_generic_payload  &payload) {

//     // m_target_memory.operation(payload, mem_op_time);
//     bsv_tlm_sysc::send_request_response ( m_slavep, payload);
//   }

// }
