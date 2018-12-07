// Copyright Bluespec Inc. 2012-2013

#include "TBXactor.h"

using namespace std;

TBXactor *TBXactor::m_tbXactor = NULL;

TBXactor *TBXactor::getOrCreate()
{
  if (m_tbXactor == NULL)
    m_tbXactor = new TBXactor();

  return m_tbXactor;
}

void TBXactor::destroy()
{
  delete m_tbXactor;
  m_tbXactor = NULL;
}

TBXactor::TBXactor()
  : m_scemi(0)
  , m_serviceThread(0)
  , m_simControl(0)
  , m_tbsimControl(0)
  , m_probeControl(0)
  , m_design(0)
  , m_vcdWriter(0)
{
}

TBXactor::~TBXactor()
{
  if (m_design) {
    delete m_design;
    m_design = NULL;
  }

  //Delete the vcd writer
  if (m_vcdWriter) {
    delete m_vcdWriter;
    m_vcdWriter = NULL;
  }

  // Stop and join with the service thread, then shut down scemi --
  if (m_serviceThread) {
    m_serviceThread->stop();
    m_serviceThread->join();
    delete m_serviceThread;
    m_serviceThread = 0;
  }

  if (m_tbsimControl) {
    delete m_tbsimControl;
    m_tbsimControl = 0;
  }

  // Delete the simulation control
  if (m_simControl) {
    delete m_simControl;
    m_simControl = 0;
  }

  // Shutdown the probes transactor
  ProbesXactor::shutdown();

  // Shutdown scemi
  SceMi::Shutdown(m_scemi);
}

// Start SceMi
bool TBXactor::startSceMi(const char *paramfile) {

  if (m_scemi) {
    throw std::runtime_error ("scemi is already initialized");
    return false;
  }

  // Initialize SceMi
  int sceMiVersion = SceMi::Version( SCEMI_VERSION_STRING );
  SceMiParameters params( paramfile );
  m_scemi = SceMi::Init( sceMiVersion, & params );
  if (! m_scemi) {
    throw std::runtime_error ("Could not initialize SceMi");
    return false;
  }

  return true;
}

// Start Simulation Control
bool TBXactor::startSimulationControl() {

  if (m_simControl == NULL) {
    m_simControl  = new RdBack::SimulationControl ("", "scemi_simControl", m_scemi);
  }

  return true;
}
  
// Start Testbench Simulation Control
bool TBXactor::startTbSimulationControl() {

  if (m_tbsimControl == NULL) {
    m_tbsimControl  = new SimulationControl ("", "scemi_tbsimControl", m_scemi);
  }

  return true;
}
  
bool TBXactor::startProbe() {

  if (m_probeControl == NULL) {
    m_probeControl = ProbesXactor::init("", "scemi_dut_prb_control", NULL, m_scemi);
  }
  
  return true;
}

bool TBXactor::startServiceThread() {
  
  // Start a SceMiService thread
  if (m_serviceThread == NULL) {
    m_serviceThread = new SceMiServiceThread  (m_scemi);
  }

  return true;
}

bool TBXactor::startVcdWriter(const char *dumpfile) {

  if (m_vcdWriter == NULL) {
    if (dumpfile == NULL) {
      m_vcdWriter = new RdBack::VCDWriter("dump1.vcd");
    }
    else
      m_vcdWriter = new RdBack::VCDWriter(dumpfile);
  }

  return true;
}

bool TBXactor::startReadback() {

  if (m_design == NULL) {

    m_design = new Design();
    m_design->setVCDWriter(m_vcdWriter);
    RdBackControl *simRdBackControl = m_simControl; // Cast to base class
    m_design->setControl(simRdBackControl);
  }

  return true;
}

bool TBXactor::startAllServices(const char *paramfile) {

  bool ret;

  ret = startSceMi(paramfile);
  //ret &= startProbe();
  ret &= startServiceThread();
  ret &= startSimulationControl();
  ret &= startTbSimulationControl();
  ret &= startVcdWriter();
  ret &= startReadback();

  return ret;
}
  

bool TBXactor::startAllServicesExceptProbe(const char *paramfile) {

  bool ret;

  ret = startSceMi(paramfile);
  ret &= startServiceThread();
  ret &= startSimulationControl();
  ret &= startTbSimulationControl();
  ret &= startVcdWriter();
  ret &= startReadback();

  return ret;
}
  
bool TBXactor::advanceControlledClock(unsigned int number_of_edges)
{
  if (m_scemi == NULL)
    return false;

  // Drain any accumulated simulation control messaages
  m_tbsimControl->drainStatusMessages();

  return m_tbsimControl->sendCommand(Edges, number_of_edges);
}

bool TBXactor::advanceControlledClockB(unsigned int number_of_edges)
{
  SimStatusResp resp;
  int status;

  if (m_scemi == NULL)
    return false;

  // Drain any accumulated simulation control messaages
  m_tbsimControl->drainStatusMessages();

  status = m_tbsimControl->sendCommand(Edges, number_of_edges);
  if (status == false)
    return false;

  resp = m_tbsimControl->getStatus();

  if (resp.cyclesRemaining() == 0)
    return true;

  return false;
}

bool TBXactor::getControlledClock(SceMiU64 &cycle)
{
  if (m_scemi == NULL)
    return false;
  
  // Drain any accumulated simulation control messaages
  m_tbsimControl->drainStatusMessages();

  if (m_tbsimControl->sendCommand(Query)) {
    
    StampedT<SimStatusResp> m;
    m = m_tbsimControl->getStatusStamped();
    cycle = m.getTimeStamp();
    return true;
  }
  return false;
}

bool TBXactor::startControlledClock()
{
  if (m_scemi == NULL)
    return false;

  // Drain any accumulated simulation control messaages
  m_tbsimControl->drainStatusMessages();

  unsigned int maxcount = 1 << 30;
  return m_tbsimControl->sendCommand(Edges, maxcount-1);
}

bool TBXactor::stopControlledClock()
{
  if (m_scemi == NULL)
    return false;

  // Drain any accumulated simulation control messaages
  m_tbsimControl->drainStatusMessages();

  return m_tbsimControl->sendCommand(Stop);
} 

