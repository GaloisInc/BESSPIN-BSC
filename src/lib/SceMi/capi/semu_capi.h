// Copyright Bluespec Inc. 2012-2013

#pragma once

#include "TBXactor.h"

using namespace std;
using namespace RdBack;

//
// This is Bluespec C API for communicating with the DUT
//

enum EmulationPortType { LooselyCoupled, TightlyCoupled };

#ifdef __cplusplus
extern "C" {
#endif

  // Initialization routines
  
  // Start scemi
  bool semu_start_scemi(const char *paramfile);

  // Start simulation control
  bool semu_start_simulation_control();

  // Start probe control
  bool semu_start_probe_control();
  
  // Start readback control
  bool semu_start_readback_control();
  
  // Start service thread
  bool start_service_thread();

  // Start vcd writer
  bool semu_start_vcd_writer(const char *dumpfile=0);
  
  // Start all the services available
  bool semu_start_services(const char *paramfile);
  
  // Stop and bring down all services
  bool semu_stop_services();

  
  // Controlled clock related API
  
  // Advance controlled clock (clock_name when not given, the default controlled clock is used)
  bool semu_advance_controlled_clock(unsigned int number_of_edges, const char *clock_name=NULL);
  
  // Advance controlled clock (blocking call) and
  // clock_name when not given, the default controlled clock is used
  bool semu_advance_controlled_clockB(unsigned int number_of_edges, const char *clock_name=NULL);
  
  // Get the cycle stamps of the controlled clock
  bool semu_get_current_controlled_clock_cycle(SceMiU64 &cycles, const char *clock_name=NULL);
  
  // Start controlled clock into free running mode
  bool semu_start_controlled_clock(const char *clock_name=NULL);
  
  // Stop controlled clock
  bool semu_stop_controlled_clock(const char *clock_name=NULL);
  

#ifdef __cplusplus
};
#endif

