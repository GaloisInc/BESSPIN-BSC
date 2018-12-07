// Copyright Bluespec Inc. 2012-2013

#include "semu_capi.h"

// Start scemi
bool semu_start_scemi(const char *paramfile)
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->startSceMi(paramfile);

  return false;
}

// Start simulation control
bool semu_start_simulation_control()
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->startSimulationControl();

  return false;
}

// Start probe control
bool semu_start_probe_control()
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->startProbe();

  return false;
}

// Start readback control
bool semu_start_readback_control()
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->startReadback();
  
  return false;
}

// Start service thread
bool semu_start_service_thread()
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->startServiceThread();

  return false;
}

// Start vcd writer
bool semu_start_vcd_writer(const char *dumpfile)
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->startVcdWriter();

  return false;
}

// Start all the services available
bool semu_start_services(const char *paramfile)
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->startAllServices(paramfile);

  return false;
}

// Stop and bring down all services
bool semu_stop_services()
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor) {
    xactor->destroy();
    return true;
  }

  return false;
}

// Controlled clock related API

// Advance controlled clock (clock_name when not given, the default controlled clock is used)
bool semu_advance_controlled_clock(unsigned int number_of_edges, const char *clock_name)
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->advanceControlledClock(number_of_edges);

  return false;
}

// Advance controlled clock (clock_name when not given, the default controlled clock is used)
bool semu_advance_controlled_clockB(unsigned int number_of_edges, const char *clock_name)
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->advanceControlledClockB(number_of_edges);

  return false;
}

// Get the cycle stamps of the controlled clock
bool semu_get_current_controlled_clock_cycle(SceMiU64 &cycles, const char *clock_name)
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor)
    return xactor->getControlledClock(cycles);

  return false;
}

// Start controlled clock into free running mode
bool semu_start_controlled_clock(const char *clock_name)
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor) {
    return xactor->startControlledClock();
  }
  return false;
}

// Stop controlled clock
bool semu_stop_controlled_clock(const char *clock_name)
{
  TBXactor *xactor = TBXactor::getOrCreate();
  if (xactor) {
    return xactor->stopControlledClock();
  }

  return false;
}

