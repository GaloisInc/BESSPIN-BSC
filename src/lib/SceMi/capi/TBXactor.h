// Copyright Bluespec Inc. 2012-2013

#pragma once

#include <iostream>
#include <stdexcept>
#include <string>
#include <cstdlib>
#include <cstring>
#include <pthread.h>

#include "Design.hpp"

using namespace std;

// Include Bluespec's SceMi C++ api
#include "bsv_scemi.h"
#include "ReadBackControl.h"

// Define a class for the top-level transactor
class TBXactor {

 private:

  static TBXactor           * m_tbXactor;

 protected:

  SceMi                     * m_scemi;
  SceMiServiceThread        * m_serviceThread;
  RdBack::SimulationControl * m_simControl;
  SimulationControl         * m_tbsimControl;
  ProbesXactor              * m_probeControl;
  Design                    * m_design;
  RdBack::VCDWriter         * m_vcdWriter;
  

  // Protected cannot be called by user
  TBXactor();

  // Destructor - protected
  ~TBXactor();

 public:

  static TBXactor *getOrCreate();
  static void destroy();

  // Activate routines
  bool startSceMi(const char *paramfile);
  bool startSimulationControl();
  bool startTbSimulationControl();
  bool startProbe();
  bool startReadback();
  bool startServiceThread();
  bool startVcdWriter(const char *dumpfile=0);
  bool startAllServices(const char *paramfile);
  bool startAllServicesExceptProbe(const char *paramfile);

  // Accessors 
  SceMi *getSceMi() { return m_scemi; }
  SceMiServiceThread* getServiceThread() { return m_serviceThread; }
  RdBack::SimulationControl* getSimulationControl() { return m_simControl; }
  SimulationControl* getTbSimulationControl() { return m_tbsimControl; }
  ProbesXactor* getProbeControl() { return m_probeControl; }
  Design* getDesign() { return m_design; }
  RdBack::VCDWriter* getVCDWriter() { return m_vcdWriter; }

  // Clock management (blocking calls)
  bool advanceControlledClock(unsigned int number_of_edges);
  bool advanceControlledClockB(unsigned int number_of_edges);
  bool getControlledClock(SceMiU64 &cycle);
  bool startControlledClock();
  bool stopControlledClock();

};
