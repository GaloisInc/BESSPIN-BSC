// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED

#include "SerialProbeXactor.h"
#include "CycleCountStruct.h"
#include "PowerSwitchStruct.h"
#include "IsoFailureStruct.h"
#include "BitT.h"
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <iostream>
#include <sstream>
//
// REPLICATED CODE WARNING
// If these encodings change, the following files must be kept in sync:
//
// SceMiSerialProbe.bsv
//
// Verilog Probes (ProbeTrigger.v, ProbeCapture.v etc.)
//
// C++ Transactor code (src/lib/SceMi/bsvxactors/SerialProbeXactor.cxx)
//

#define INVALID_PROBE_NUM std::numeric_limits<unsigned int>::max()
#define TIMESTAMP_REQUEST 0xFFFF
#define TRIGGER_COMMAND 0xFFFB
#define DISABLE_COMMAND 0xFFF9
#define ENABLE_COMMAND 0xFFFA

#define MAX_LINE_SIZE 1023
#define SCEMI_CHANNEL_WIDTH 32

// A static bool specifying whether the rolling averager delivers a zero
// average until the window is complete.  Useful to prevent unexpected peaks
// which might upset gtkwave's automatic y-scaling.
static Bool inhibit_incomplete_averages = true;

// The static member pointer to the ProbesXactor
static ProbesXactor *theProbesXactor = NULL;

// The static map of all top level HierProbeDefNodes
std::map<std::string,HierProbeDefNode*> HierProbeDefNode::TopHierProbeDefNodes;

void RollingAverager::change_size(const size_t new_size) {
  complete_size = new_size;
  lst.clear();
}

float RollingAverager:: average(const float x) {
  if (complete_size==1) return x;

  if (lst.size()==complete_size) {
    float y = lst.back();
    lst.pop_back();
    sum = sum - y;
  }
  sum = sum + x;
  lst.push_front(x);
  size_t sz = lst.size();
  bool zero_average = (inhibit_incomplete_averages && sz!=complete_size);
  return (zero_average ? 0.0f : (sum/static_cast<float>(sz)));
}


/*
float lowPassFilter(float input, float prev_output)
{
  float alpha = 1.0;
  //float output = alpha * prev_output + (1.0 - alpha) * (input - prev_output);
  float output = alpha * input + (1.0 - alpha) * prev_output;
  //printf("input %f prev_output %f alpha %f (i-o) %f a(i-o) %f output %f\n", input, prev_output, alpha, (input - prev_output), alpha*(input - prev_output), output);
  return output;
}
*/

void eliminate_space(const char *name, std::string &return_name)
{
  return_name = name;

  size_t loc = return_name.find_first_of(' ');
  while (loc != std::string::npos) {

    return_name[loc] = '_';
    loc = return_name.find_first_of(' ', loc);
  }
}

// init() - static member for instantiating the ProbesXactor with debug printing
ProbesXactor *ProbesXactor::init(bool debug,
                                 const std::string & hier, const std::string & instname,
                                 const char *file_base, SceMi *scemi, bool useMaxScale)
{
  SceMi *sceMi;

  if (theProbesXactor)
    return theProbesXactor;

  if (scemi == NULL) {
    sceMi = SceMi::Pointer();
    if (sceMi == NULL) {
      std::cerr << "ProbesXactor Error: SceMi must be intialized before calling init()." << std::endl;
      return NULL;
    }
  } else
    sceMi = scemi;

  // Set the static pointer
  theProbesXactor = new ProbesXactor(debug, hier, instname, file_base, sceMi, useMaxScale);

  return theProbesXactor;
}

// init() - static member for instantiating the ProbesXactor
ProbesXactor *ProbesXactor::init(const std::string & hier, const std::string & instname,
                                 const char *file_base, SceMi *scemi, bool useMaxScale)
{
  SceMi *sceMi;

  if (theProbesXactor)
    return theProbesXactor;

  if (scemi == NULL) {
    sceMi = SceMi::Pointer();
    if (sceMi == NULL) {
      std::cerr << "ProbesXactor Error: SceMi must be intialized before calling init()." << std::endl;
      return NULL;
    }
  } else
    sceMi = scemi;

  // Set the static pointer
  theProbesXactor = new ProbesXactor(false, hier, instname, file_base, sceMi, useMaxScale);

  return theProbesXactor;
}


// Shutdown the transactor
void ProbesXactor::shutdown()
{
  delete theProbesXactor;

  theProbesXactor = NULL;
}


// Constructor
ProbesXactor::ProbesXactor(bool debug, const std::string & hier, const std::string & instname,
                           const char *file_base, SceMi *scemi, bool useMaxScale)
  : m_probedata(hier, instname + "_data_out", scemi)
  , m_probectrl(hier, instname + "_control_in", scemi)
  , m_debug(debug)
{
  // Get the Probe/Capture parameters from SceMi
  //  and then setup the xactors for receiving all
  //  the Probes and Captures.
  unsigned int samples, offset, prbNum, width;
  const char *path, *label, *type;
  SerialProbeDef *def;
  SerialProbeType kind;

  // Initialize max probe numbers
  max_power_probe_num = 0;
  has_power_probe = false;
  m_prev_meter_cycle = 0;
  m_last_calculated_meter_cycle = 0;
  scemi_clock_frequency = 100;  // 100 Mhz default

  max_scale = 0.0;
  use_max_scale = useMaxScale;

  //m_debug = true;
  theSceMiParams = scemi->parameter();
  unsigned int num = scemi->parameter()->NumberOfObjects("Serial");
  // XXX temp hack:
  m_is_first_zero = true;

  // Gather all the probes/captures definitions from param file
  for (unsigned int index = 0; index < num; ++index) {

    samples = theSceMiParams->AttributeIntegerValue("Serial", index, "Samples");
    offset = getParamOffset(index);
    path = getParamPath(index);
    label = getParamLabel(index);
    type = getParamBSVType(index);
    kind = getParamType(index);
    width = getParamWidth(index);

    // Create definition record
    std::string newlabel;
    eliminate_space(label, newlabel);
    def = new SerialProbeDef( samples, offset, width, path, newlabel.c_str(), type);

    def->setProbeType(kind);

    prbNum = getParamProbeNum(index);
    if (prbNum > max_power_probe_num)
      max_power_probe_num = prbNum;
    def->setProbeNum(prbNum);

    // Parameter index
    def->setID(index);

    // Used for assigning new power probe for display real values
    max_power_probe_num++;

    m_probedefs.push_back (def); // Array of valid probe/capture (throw away trigger)
    m_probedefs_map[prbNum] = def; // Also mapping table based on probeNum
    m_probedefs_name_map[newlabel] = def; // Also mapping table based on probeNum


    tSceMiAttrType t = theSceMiParams->GetAttributeType("Serial", index, "SubNets");
    if (t == SCEMI_NO_ATTR)
      continue;

    const char *subnetsString = getParamSubNets(index);
    if (subnetsString)
      createSubNetProbeDefs(def, subnetsString);

  }

  // Set some bookkeeping data
  m_current_probe = static_cast<unsigned int>(m_probedefs.size());
  m_max_bitsize = getMaxBitSize();
  m_remaining = 0;
  m_receiving_data = false;
  m_data = new SceMiU32[(m_max_bitsize+31)/32];
  m_receiving_data = false;
  m_current_scan  =  0;
  m_previous_scan  = -1;
  m_previous_probe = -1;

  // VCD file
  if (file_base == NULL) {
    vcd_filename = "scemi_test.vcd";
  } else {
    vcd_filename = file_base;
    vcd_filename += ".vcd";
  }

  vcd_writer = new VCDWriter(vcd_filename.c_str());
  vcd_writer->registerProbesXactor(this);

  std::string data_filename;
  if (file_base == NULL) {
    data_filename = "dump.scd";
  } else {
    data_filename = file_base;
    data_filename += ".scd";
  }

  data_writer = new DataWriter(data_filename.c_str());
  data_writer->startDataFile();

  // Set callback for watching probe message
  m_probedata.setCallBack(ProbesXactor::logProbe, (void*)this);

  // Set power probe num
  for (unsigned int i=0; i<m_probedefs.size(); i++) {
    def = m_probedefs[i];
    if (def->getProbeType() == PowerMeter) {
      prbNum = max_power_probe_num++;
      def->setPowerProbeNum(prbNum);
      m_probedefs_map[prbNum] = def; // Also mapping table based on probeNum
      has_power_probe = true;
      getOrCreatePowerCounter(def->getPowerProbeNum());
      //printf("PowerCounter Created %s prbNum %d\n", def->getLabel(), prbNum);
    }
    else if (def->getProbeType() == PowerChanges) {
      prbNum = max_power_probe_num++;
      m_probedefs_map[prbNum] = def; // Also mapping table based on probeNum
      if (def->getProbeNum() > max_power_probe_num)
        max_power_probe_num = def->getProbeNum();
      has_power_probe = true;
      //printf("PowerChanges Created %s prbNum %d\n", def->getLabel(), prbNum);
    }
  }

  // Now create a SerialProbeDef for Power Sources
  if (has_power_probe) {
    std::stringstream ss;
    std::string newlabel;

    // Create definition record for power source
    for (int i=0; i<4; i++) {
      ss.str("");
      prbNum = max_power_probe_num++;
      ss << i;
      newlabel = "PowerSource" + ss.str();
      def = new SerialProbeDef(0, 0, 32, "", newlabel.c_str(), "real");
      def->setProbeType(PowerSrc);
      def->setProbeNum(prbNum);
      def->setPowerProbeNum(i);
      def->setID(m_probedefs.size());
      def->setLabel(newlabel.c_str());
      //printf("PowerSource Created %s prbNum %d\n", newlabel.c_str(), prbNum);

      m_probedefs.push_back (def); // Array of valid probe/capture (throw away trigger)
      m_probedefs_name_map[newlabel] = def; // Also mapping table based on probeNum
      m_probedefs_map[prbNum] = def; // Also mapping table based on probeNum
      getOrCreatePowerSource(def);
    }

    // Now create a SerialProbeDef for TotalPower meter
    parsePowerRateFile("power.spec");

    // Create definition record for total power
    def = new SerialProbeDef(0, 0, 32, "", "TotalPower", "real");
    def->setProbeType(PowerMeter);
    def->setProbeNum(getTotalPowerProbeNum());
    def->setID(m_probedefs.size());

    m_probedefs.push_back (def); // Array of valid probe/capture (throw away trigger)
    m_probedefs_name_map["TotalPower"] = def; // Also mapping table based on probeNum
    m_probedefs_map[getTotalPowerProbeNum()] = def; // Also mapping table based on probeNum
  }

  // Initialize vcd file
  //if (m_debug) fprintf(stderr, "initialize vcd file\n");
  vcd_writer->startVCDFile();
}


// Destructor (private)
ProbesXactor::~ProbesXactor()
{
  SerialProbeDef *def;
  for (unsigned int i=0; i<m_probedefs.size(); i++) {
    def = m_probedefs[i];
    delete def;
  }

  delete [] m_data;
  m_data = NULL;

  delete vcd_writer;
  vcd_writer = NULL;

  data_writer->addFinish();

  delete data_writer;
  data_writer = NULL;
}

// Destructor
SerialProbeDef::~SerialProbeDef()
{
  SubNetProbeDef *def;
  SubNetProbeDefIterator itr;

  for (itr = subnets.begin(); itr != subnets.end(); itr++) {
    def = *itr;
    delete def;
  }
}

// Methods to create the subnets definition for each probedef
void ProbesXactor::createSubNetProbeDefs(SerialProbeDef *def, const char *subnetsString)
{
  int width, from, to;
  size_t colon, comma, firstchar;
  std::string name;
  std::string wstr;
  SubNetProbeDef *snDef;
  int bundleWidth = def->getWidth();  // This is aggregate width

  std::string snStr = subnetsString;
  to = bundleWidth-1;
  firstchar = 0;
  colon = snStr.find(':', firstchar);
  comma = snStr.find(',', firstchar);

  while ((colon != std::string::npos) && (comma != std::string::npos)) {

    if (colon > comma)
      if (m_debug) fprintf(stderr,"ProbesXactor::createSubNetProbeDefs() something wrong in the definition of SubNets of probe %s\n", def->getLabel());

    name = snStr.substr(firstchar, colon-firstchar);
    wstr = snStr.substr(colon+1, comma-colon+1);
    width = atoi(wstr.c_str());
    from = to - width + 1;
    snDef = new SubNetProbeDef(name.c_str(), width, from, to);
    def->addSubNet(snDef);

    firstchar = comma + 1;
    colon = snStr.find(':', firstchar);
    comma = snStr.find(',', firstchar);
    to = from - 1;
  }

  name = snStr.substr(firstchar, colon-firstchar);
  wstr = snStr.substr(colon+1);
  width = atoi(wstr.c_str());
  from = to - width + 1;
  snDef = new SubNetProbeDef(name.c_str(), width, from, to);
  def->addSubNet(snDef);
}

// Methods to enable or disable the Probe/Capture
void ProbesXactor::enable(unsigned int probeID)
{
  SerialProbeDef *def = m_probedefs[probeID];

  if (def == NULL)
    return;

  // Only trigger probe count needs to be incremented
  if (def->getProbeType()==TriggerProbe)
    def->setEnable(true);
  else if (!def->enabled())
    def->setEnable(true);

  m_control.m_enable = true;
  m_control.m_probe_num = def->getProbeNum();

  if (m_debug) fprintf(stderr,"ProbesXactor enable probeID %zu\n", def->getID());

  m_probectrl.sendMessage(m_control);
}

// Methods to enable or disable the Probe/Capture
void ProbesXactor::enableByProbeNum(unsigned int probeNum)
{
  SerialProbeDef *def = m_probedefs_map[probeNum];

  if (def == NULL)
    return;

  // Only trigger probe count needs to be incremented
  if (def->getProbeType()==TriggerProbe)
    def->setEnable(true);
  else if (!def->enabled())
    def->setEnable(true);

  m_control.m_enable = true;
  m_control.m_probe_num = def->getProbeNum();

  if (m_debug) fprintf(stderr,"ProbesXactor enable probeNum %d\n", def->getProbeNum());

  m_probectrl.sendMessage(m_control);
}

// Methods to enable or disable the Probe/Capture
void ProbesXactor::enableAll()
{
  SerialProbeDef *def;
  std::vector<SerialProbeDef*>::iterator defItr;
  for (defItr = m_probedefs.begin(); defItr != m_probedefs.end(); defItr++) {

    def = *defItr;

    // Only trigger probe count needs to be incremented
    if (def->getProbeType()==TriggerProbe)
      def->setEnable(true);
    else if (!def->enabled())
      def->setEnable(true);

    m_control.m_enable = true;
    m_control.m_probe_num = def->getProbeNum();

    if (m_debug) fprintf(stderr,"ProbesXactor enableAll probeNum %d\n", def->getProbeNum());

    m_probectrl.sendMessage(m_control);
  }
}

// Methods to enable or disable the Probe/Capture
void ProbesXactor::disableAll()
{
  SerialProbeDef *def;
  std::vector<SerialProbeDef*>::iterator defItr;
  for (defItr = m_probedefs.begin(); defItr != m_probedefs.end(); defItr++) {

    def = *defItr;

    if (def->enabled()) {
      // Only trigger probe count needs to be incremented
      def->setEnable(false);
      m_control.m_enable = false;
      m_control.m_probe_num = def->getProbeNum();
      if (m_debug) fprintf(stderr,"ProbesXactor disableAll probeNum %d\n", def->getProbeNum());
      m_probectrl.sendMessage(m_control);
    }
  }
}

bool ProbesXactor::isPowerProbe(unsigned int probeNum)
{
  SerialProbeDef *def = m_probedefs_map[probeNum];
  if (!def) return false;
  return def->isPowerProbe();
}

bool ProbesXactor::isPowerGroupProbe(unsigned int probeNum)
{
  SerialProbeDef *def = m_probedefs_map[probeNum];
  if (!def) return false;
  return def->isPowerGroupProbe();
}

bool ProbesXactor::isPowerSourceProbe(unsigned int probeNum)
{
  SerialProbeDef *def = m_probedefs_map[probeNum];
  if (!def) return false;
  return def->isPowerSourceProbe();
}

// Methods to enable or disable the Probe/Capture
void ProbesXactor::queryTimestamp()
{
  m_control.m_enable = true;
  m_control.m_probe_num = 0xFFFF;
  m_probectrl.sendMessage(m_control);
}

// Disable probe with the given probeNum
void ProbesXactor::disable(unsigned int probeID)
{
  if (probeID >= getNumberOfProbes())
    return;
  
  SerialProbeDef *def = m_probedefs[probeID];
  if (def->enabled()) {
    def->setEnable(false);
    m_control.m_enable = false;
    m_control.m_probe_num = def->getProbeNum();
    if (m_debug) fprintf(stderr,"ProbesXactor disable probeID %d\n", probeID);
    m_probectrl.sendMessage(m_control);
  }
}

// Disable probe with the given probeNum
void ProbesXactor::disableByProbeNum(unsigned int probeNum)
{
  SerialProbeDef *def = m_probedefs_map[probeNum];

  if (def->enabled()) {
    def->setEnable(false);
    m_control.m_enable = false;
    m_control.m_probe_num = def->getProbeNum();
    if (m_debug) fprintf(stderr,"ProbesXactor disable probeNum %d\n", probeNum);
    m_probectrl.sendMessage(m_control);
  }
}

// Methods to enable or disable the Probe/Capture
void ProbesXactor::associateBlockToPowerSource(unsigned int block_id, unsigned int source_id)
{
  unsigned int num;

  m_control.m_enable = true;
  num = 0x0;
  num |= (0x7 << 12);
  num |= (block_id << 7);
  num |= source_id;
  m_control.m_probe_num = num;
  if (m_debug) fprintf(stderr,"ProbesXactor associate block %d to source %d control %x\n",
                       block_id, source_id,
                       num);
  m_probectrl.sendMessage(m_control);

  /* PowerSource *ps =*/ getOrCreatePowerSource(source_id);
  // ps = NULL;
  //PowerCounter *pc = getOrCreatePowerCounter(block_id, 1.0);
  //associatePowerAndCounter(ps, pc);
}

// Methods to enable or disable the Probe/Capture
void ProbesXactor::enablePowerIsolators()
{
  SerialProbeDef *def;
  for (unsigned int i=0; i<m_probedefs.size(); i++) {
    def = m_probedefs[i];
    if (def->getProbeType() == PowerISO) {

      m_control.m_enable = true;
      m_control.m_probe_num = def->getProbeNum();
      if (m_debug) fprintf(stderr,"ProbesXactor enable power isolator %d\n",
                           def->getProbeNum());
      m_probectrl.sendMessage(m_control);
    }
  }
}

void ProbesXactor::requestPowerStat()
{
  if (hasPowerProbe() == false)
    return;

  m_control.m_enable = true;
  m_control.m_probe_num = 0x6BFF;
  if (m_debug) fprintf(stderr,"ProbesXactor request power stat\n");
  m_probectrl.sendMessage(m_control);
}

void ProbesXactor::setPowerInterval(const unsigned int x)
{
  if (hasPowerProbe() == false)
    return;
  if (m_debug) fprintf(stderr,"ProbesXactor setPowerInterval %0d\n", x);
  unsigned int xx = x;
  if (x >= 0xFF000000) {
    xx = x & 0xFEFFFFFF;
    fprintf(stderr,"ERROR: value %x exceeds max allowed (0xFEFFFFFF), changed to %x", x, xx);
  }

  m_control.m_enable = true;
  // send least significant piece first, because a zero value here clears the entire word:
  m_control.m_probe_num = 0x6800 | (xx & 0xFF);
  m_probectrl.sendMessage(m_control);
  m_control.m_probe_num = 0x6900 | ((xx >>  8) & 0xFF);
  m_probectrl.sendMessage(m_control);
  m_control.m_probe_num = 0x6A00 | ((xx >> 16) & 0xFF);
  m_probectrl.sendMessage(m_control);
  m_control.m_probe_num = 0x6B00 | ((xx >> 24) & 0xFF);
  m_probectrl.sendMessage(m_control);
}

void ProbesXactor::setWindowSize(const unsigned int x)
{
  PowerCounterIterator ctrItr;
  PowerCounter *ctr;
  for (ctrItr = m_powerCounters.begin();
       ctrItr != m_powerCounters.end();
       ctrItr++) {

    ctr = *ctrItr;
    if (ctr == NULL) continue;

    ctr->m_averager.change_size(x);
  }

}

// Return the state of the probe

unsigned int ProbesXactor::enabled(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? 0 : m_probedefs[probeID]->enabled();
}

unsigned int ProbesXactor::enabledByProbeNum(unsigned int probeNum)
{
  SerialProbeDef *def = m_probedefs_map[probeNum];
  if (!def)
    return 0;
  return def->enabled();
}


unsigned int ProbesXactor::getSamples(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? 0 : m_probedefs[probeID]->getSamples();
}

unsigned int ProbesXactor::getProbeNum(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? 0 : m_probedefs[probeID]->getProbeNum();
}

int ProbesXactor::getProbeID(unsigned int probeNum)
{
  SerialProbeDef *def = m_probedefs_map[probeNum];

  if (def == NULL) return -1;

  return static_cast<int>(def->getID());
}

unsigned int ProbesXactor::getOffset(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? 0 : m_probedefs[probeID]->getOffset();
}

unsigned int ProbesXactor::getWidth(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? 0 : m_probedefs[probeID]->getWidth();
}

const char *ProbesXactor::getBSVType(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? "" : m_probedefs[probeID]->getBSVType();
}

const char *ProbesXactor::getLabel(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? "" : m_probedefs[probeID]->getLabel();
}

const char *ProbesXactor::getPath(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? "" : m_probedefs[probeID]->getPath();
}


unsigned int ProbesXactor::getParamSamples(unsigned int probeID)
{
  return (probeID >= getNumberOfProbes()) ? 0 : theSceMiParams->AttributeIntegerValue("Serial", probeID, "Samples");
}

unsigned int ProbesXactor::getParamNumberOfProbes()
{
  return theSceMiParams->NumberOfObjects("Serial");
}

unsigned int ProbesXactor::getParamProbeNum(unsigned int probeID)
{
  return theSceMiParams->AttributeIntegerValue("Serial", probeID, "PrbNum");
}

unsigned int ProbesXactor::getParamOffset(unsigned int probeID)
{
  return theSceMiParams->AttributeIntegerValue("Serial", probeID, "Offset");
}

unsigned int ProbesXactor::getParamWidth(unsigned int probeID)
{
  return theSceMiParams->AttributeIntegerValue("Serial", probeID, "Width");
}

const char *ProbesXactor::getParamBSVType(unsigned int probeID)
{
  return theSceMiParams->AttributeStringValue("Serial", probeID, "Type");
}

const char *ProbesXactor::getParamLabel(unsigned int probeID)
{
  return theSceMiParams->AttributeStringValue("Serial", probeID, "Label");
}

SerialProbeType ProbesXactor::getParamKind(unsigned int probeID)
{
  return (SerialProbeType)theSceMiParams->AttributeIntegerValue("Serial", probeID, "Kind");
}

const char *ProbesXactor::getParamPath(unsigned int probeID)
{
  return theSceMiParams->AttributeStringValue("Serial", probeID, "Path");
}

const char *ProbesXactor::getParamSubNets(unsigned int probeID)
{
  return theSceMiParams->AttributeStringValue("Serial", probeID, "SubNets");
}

const char *ProbesXactor::getParamSubNetsFromProbeNum(unsigned int probe_num)
{
  SerialProbeDef *def = getProbeDefFromProbeNum(probe_num);

  if (def == NULL) {
    fprintf(stderr,"Error ProbesXactor::getParamSubNetsFromProbeNum: unexpected ProbeNum %d.\n",
            probe_num);
    return NULL;
  }

  SubNetProbeDef *snDef;
  SubNetProbeDefIterator snDefItr;
  subnets_param_retval = "";
  std::stringstream num;
  bool first = true;
  std::string al;

  for (snDefItr = def->subNetsBegin(); snDefItr != def->subNetsEnd(); snDefItr++) {

    snDef = *snDefItr;
    if (first == false)
      subnets_param_retval += ",";
    first = false;
    num << snDef->width();
    al = snDef->alias();
    if (al != "")
      subnets_param_retval += snDef->alias();
    else
      subnets_param_retval += snDef->name();
    subnets_param_retval += ":";
    subnets_param_retval += num.str();
    num.str("");
  }

  return (subnets_param_retval.c_str());
}

SerialProbeType ProbesXactor::getParamType(unsigned int probeID)
{
  unsigned int num = theSceMiParams->AttributeIntegerValue("Serial", probeID, "Kind");
  return (SerialProbeType) num;
}

// Find the largest number of bits needed by all the probes
unsigned int ProbesXactor::getMaxBitSize()
{
  unsigned int maxval = 0;
  unsigned int val;
  SerialProbeDef *def;
  for (unsigned int i=0; i<m_probedefs.size(); i++) {
    def = m_probedefs[i];
    val = getBitSize(def->getProbeNum());
    if (maxval < val)
      maxval = val;
  }
  return maxval;
}

unsigned int ProbesXactor::getMaxRunlengthWidth()
{
  unsigned int maxval = 0;
  unsigned int val;
  SerialProbeDef *def;
  for (unsigned int i=0; i<m_probedefs.size(); i++) {
    def = m_probedefs[i];
    val = def->getOffset();
    if (maxval < val)
      maxval = val;
  }
  return maxval;
}

// Return the probeNum given the 32 data word
//  For scan, the probe
unsigned int ProbesXactor::getCurrentProbe (SceMiU32 data)
{
  unsigned int current_probe = ((data >> 16) & 0xFFFF);  // Shift right 16 bits

  return current_probe;
}

// Return the SerialType (3 bits)
// 0 - Explicit;
// 1 - HdlEdit;
// 2 - Cosim
// 3 - Power;
// 4 - DUMMY4;
// 5 - DUMMY5;
// 5 - DUMMY6;
// 7 - Control;
unsigned int ProbesXactor::getCurrentSerialType (SceMiU32 data)
{
  unsigned int serial_type = (data >> 29);

  return serial_type;
}

unsigned int ProbesXactor::getPowerKind (SceMiU32 data)
{
  unsigned int power_kind = ((data >> 26) & 0x03);

  return power_kind;
}

void ProbesXactor::processProbeMsg(const StampedT<SerialProbeData> &msg)
{
  unsigned int code;
  SerialProbeDef *def = NULL;
  bool is_code;

  SceMiU32 data = msg.getData().get();

  if (m_receiving_data == false) {
    // XXX Temp hack:
    if (data==0) {
      fprintf(stderr, "ZERO: ts %0lld\n", msg.getTimeStamp());
      if (msg.getTimeStamp() > 100 && m_is_first_zero) {
	fprintf(stderr, "%lld: ISOLATION FAILURE: Extnl source 0, Intnl source 2\n",
		msg.getTimeStamp());

	std::fstream file;
	file.open("isolation.fail", std::ios::out | std::ios::app);
	if (file.fail()) {
	  std::cerr << "ERROR HACK" << std::endl;
	  file.close();
	  return;
	}

	file << "[" << std::setw(12) << std::right << msg.getTimeStamp()
	     << "] ISOLATION FAILURE:  Extnl source 0, Intnl source 2" << std::endl;

	file.close();
      }

      m_is_first_zero = !m_is_first_zero;
      return;
    } // XXX End of temp hack

    // The first word has probeID and command code or number of samples when in_receiving_data mode
    m_current_probe = getCurrentProbe(data);
    code = (data & 0xFFFF); // Zero out left 16 bits
    m_samples = code;
    is_code = ((data & 0xFFF8) == 0xFFF8);

    if (m_debug) fprintf(stderr,"\nControl word: probeNum %d code %d is_code %d original(32) %x\n",
                         m_current_probe, code, (is_code ? 1 : 0), data);

    // Timestamp message
    // Flush vcdfile if we got a timestamp
    if (m_current_probe == TIMESTAMP_REQUEST) {
      SceMiU64 cyc = getLatestCycleForUpdate(msg);
      if (m_debug) fprintf(stderr,"Query timestamp %d (%lld)\n", m_current_probe, cyc);
      vcd_writer->updateLatestTime(cyc, true);
      return;
    }

    // Get the probe def
    def = m_probedefs_map[m_current_probe];

    if (!isScanMessage(data)) {

      // Error if control word has a bad probenum
      if (def == 0) {
        fprintf(stderr,"Error ProbesXactor::processProbeMsg: unexpected ProbeNum %d in control word with code %x.\n", m_current_probe, code);
        return;
      }
      if (code == 0) {
        fprintf(stderr,"Error ProbesXactor::processProbeMsg: unexpected code %x\n", code);
        return;
      }
      // Get probe type
      m_probe_type = def->getProbeType();
    }
    else {

      m_probe_type = ScanProbe;
    }

    // Message from a trigger event
    if (code == TRIGGER_COMMAND) {
      if (m_probe_type == TriggerProbe)
        def->noteEvent();
      else if (m_probe_type == CaptureProbe) {
        // Calculate and record the start of the capture cycle stamp
        def->setLatestCycle(msg.getTimeStamp() - def->getOffset());
        def->setSamplesRemaining(def->getSamples()-1);
      }
      return;
    }
    // Disable probe or capture message
    else if (code == DISABLE_COMMAND) {

      if ((m_probe_type==TriggerProbe) || (def->enabled())) {
        def->setEnable(false);
      }

      // If this is a value probe then write out "Z" when it is disabled
      if (m_probe_type==ValueProbe) {
        Packet pkt;
        pkt.data = m_data;
        pkt.num_bits = def->getWidth();
        m_latest_cycle = msg.getTimeStamp(); // Received message time
        BSVType * bsvval = createData(def->getProbeNum(), &pkt, m_latest_cycle);
        if (bsvval != 0) {
          if (m_debug) fprintf(stderr, "%0lld: Write Z\n", m_latest_cycle);
          vcd_writer->addChangeSerial(m_latest_cycle, def->getProbeNum(), bsvval, true);
        }
      }
      return;
    }
    // Enable probe or capture message
    else if (code == ENABLE_COMMAND) {

      if (!def->enabled() || m_probe_type == TriggerProbe)
        def->setEnable(true);
      return;
    }
    else { // Else we will be receiving probe/scan data

      if (m_probe_type != ScanProbe) {

        // Check for unrecognized code
        if (is_code) {
          if (m_debug) fprintf(stderr,"Warning unrecognized code %x is being ignored.\n", code);
          return;
        }

        // Check if the size of the data is larger than the size of the probe value
        if ((m_probe_type != CaptureProbe) && (m_probe_type != NewCaptureProbe) &&
            (m_samples > (((def->getWidth()-1)/32)+1))) {
          fprintf(stderr,"Error ProbesXactor::processProbeMsg: number of bytes of probe message (%d) is larger than size of the probe (%d).\n", m_samples, (((def->getWidth()-1)/32)+1));
          return;
        }
      }

      // Initialize a lot of bookkeeping stuff
      //  these variables are used across multiple calls
      //  because the data comes one packet at a time
      m_receiving_data = true;
      m_nwords = 0;

      if (m_probe_type == ValueProbe) {
        m_remaining = m_samples;
      }
      else if (m_probe_type == CaptureProbe) {
        m_remaining = m_samples;
      }
      else if (m_probe_type == NewCaptureProbe) {
        m_runlength_bits_left = def->getOffset();
        m_runlength_word = 0;
        m_first_word = true;
        m_remaining = m_samples;
        m_runlength = 0;
        m_data_nbits_so_far = 0;
        m_latest_cycle = msg.getTimeStamp(); // Reference time!
        if (m_debug) fprintf(stderr, "Ref time of NewCaptureProbe probeNum %d @ %lld\n",
                             m_current_probe, m_latest_cycle);
      }
      else if (m_probe_type == ScanProbe) {
        //
        // REPLICATED CODE WARNING
        // This is Fragile!!!! (dependent on bit representation of CosimStruct
        // in src/lib/BSVSource/SceMi/SceMiScan.bsv)
        //
        m_current_scan       = (((data << 5) >> 5) >> 23);
        m_remaining = m_samples;
        if (m_current_scan == 0) {
          if (m_debug) fprintf(stderr, "S (%llu)\n", msg.getTimeStamp());
        }
        if ((msg.getTimeStamp() &  0x3FF) == 0) {
          if (m_debug) fprintf(stderr, "I (%llu)\n", msg.getTimeStamp());
        }
        tTime tm = msg.getTimeStamp();
        data_writer->addStart(tm, m_current_probe, m_current_scan, m_previous_probe, m_previous_scan);
        m_latest_cycle = msg.getTimeStamp(); // Reference time!
        m_previous_probe = m_current_probe;
        m_previous_scan  = m_current_scan;
      }
      else if (m_probe_type == PowerMeter) {
        m_remaining = m_samples;
      }
      else if (m_probe_type == PowerChanges) {
        m_remaining = m_samples;
      }
      else if (m_probe_type == PowerISO) {
        m_remaining = m_samples;
      }
      return;
    }
  }
  else { // In receiving_data mode

    if (m_debug) fprintf(stderr," Data word: probenum %d data %x for probetype %d time %llu remaining %d\n",
                      m_current_probe, data,
                      m_probe_type, msg.getTimeStamp(), m_remaining);

    if (m_remaining == 0)
      return;

    if (m_probe_type == ValueProbe)
      processValueProbeMsg(msg);
    else if (m_probe_type == CaptureProbe)
      processCaptureProbeMsg(msg);
    else if (m_probe_type == NewCaptureProbe)
      processNewCaptureProbeMsg(msg);
    else if (m_probe_type == PowerMeter)
      processPowerMeterMsg(msg);
    else if (m_probe_type == PowerChanges)
      processPowerChangesMsg(msg);
    else if (m_probe_type == PowerISO)
      processPowerIsoMsg(msg);
    else if (m_probe_type == ScanProbe)
      processScanMsg(msg);
  }
}

void ProbesXactor::processValueProbeMsg(const StampedT<SerialProbeData> &msg)
{
  // Receive data
  SceMiU32 data = msg.getData().get();
  m_latest_cycle = msg.getTimeStamp();

  if (m_debug) fprintf(stderr," Data word for value probe for probenum: %d data: %x word %d remaining %d\n",
                       m_current_probe, data, m_nwords, m_remaining);

  m_remaining--;
  m_data[m_nwords++] = data;

  // Create data from raw bits
  if (m_remaining == 0) {
    Packet pkt;
    pkt.channel = m_current_probe;
    pkt.num_bits = m_nwords*SCEMI_CHANNEL_WIDTH;
    pkt.data = m_data;

    BSVType *bsvdata = createData(m_current_probe, &pkt, m_latest_cycle);

    if (bsvdata == 0) {
      fprintf(stderr,"Error in ProbesXactor::processValueProbeMsg(): unable to create corresponding probe data in transxactor from HW data packet.\n"
              "Make sure you have built the testbench after properly generating the SceMiProbes.cxx with the appropriate probed params file as input.\n");

      // XXXX need to reset the state back to a stable point.....
      m_receiving_data = false; // Done receiving data
      return;
    }

    // Get the probe def
    SerialProbeDef *def = m_probedefs_map[m_current_probe];

    // Write to vcd file
    if (def->enabled() && bsvdata != 0) {
      //std::cout << "Found a value message: " << bsvdata << std::endl;
      if (m_debug) fprintf(stderr, "%0lld: addChangeSerial1 probe %0d\n", m_latest_cycle, m_current_probe);
      vcd_writer->addChangeSerial(m_latest_cycle, m_current_probe, bsvdata);
    }

    m_receiving_data = false; // Done receiving data
    SceMiU64 tim = getLatestCycleForUpdate(msg);
    if (m_debug) fprintf(stderr, "%0lld: updateLatestTime1 %0lld\n", m_latest_cycle, tim);
    vcd_writer->updateLatestTime(tim, false);
  }
}

void ProbesXactor::processCaptureProbeMsg(const StampedT<SerialProbeData> &msg)
{
  // Receive data
  SceMiU32 data = msg.getData().get();

  // Get the probe def
  SerialProbeDef *def = m_probedefs_map[m_current_probe];

  m_latest_cycle = def->getLatestCycle();

  if (m_debug) fprintf(stderr," Data word for capture probe for probenum: %d data: %x\n",
                       m_current_probe, data);

  m_remaining--;
  m_data[m_nwords++] = data;

  // Process probe data
  if (m_remaining == 0) {

    SceMiU32 runlength;
    runlength = m_data[0];
    m_latest_cycle += runlength;
    def->setLatestCycle(m_latest_cycle);
    def->reduceSamplesRemaining(runlength);
  }
  else return;

  // Create data from raw bits
  Packet pkt;
  pkt.channel = m_current_probe;
  pkt.num_bits = m_nwords*SCEMI_CHANNEL_WIDTH;
  pkt.data = m_data+1;

  BSVType *bsvdata = createData(m_current_probe, &pkt, m_latest_cycle);

  if (def->enabled() && bsvdata != 0) {
    if (m_debug) fprintf(stderr, "%0lld: addChangeSerial2 probe %0d\n", m_latest_cycle, m_current_probe);
    vcd_writer->addChangeSerial(m_latest_cycle, m_current_probe, bsvdata);
  }

  // Write out a z state when all capture data has been received
  if (def->getSamplesRemaining() == 0 && bsvdata != 0) {
    if (m_debug) fprintf(stderr, "%0lld: addChangeSerial3 probe %0d\n", m_latest_cycle, def->getProbeNum());
    SceMiU64 tim = getLatestCycleForUpdate(msg);
    if (m_debug) fprintf(stderr, "%0lld: updateLatestTime2 %0lld\n", m_latest_cycle, tim);
    vcd_writer->addChangeSerial(m_latest_cycle+1, def->getProbeNum(), bsvdata, true);
    vcd_writer->updateLatestTime(tim, false );
  }
  m_receiving_data = false; // Done receiving data
}

void ProbesXactor::processNewCaptureProbeMsg(const StampedT<SerialProbeData> &msg)
{
  SceMiU64 next_runlength;

  // Get the probe def
  SerialProbeDef *def = m_probedefs_map[m_current_probe];

  if (m_debug)
    if (m_runlength_bits_left > 64)
      fprintf(stderr,"Error processNewCaptureProbeMsg(): something wrong, we cannot handle runlength width greater than 64 for probe label: %s %d\n", def->getLabel(), m_runlength_bits_left);

  // Receive data
  next_runlength = assembleNewCaptureRunlengthAndData(msg);

  // Create data from raw bits
  if (m_data_nbits_so_far >= def->getWidth()) {
    Packet pkt;
    pkt.channel = m_current_probe;
    pkt.num_bits = ((m_data_nbits_so_far+(SCEMI_CHANNEL_WIDTH-1))/SCEMI_CHANNEL_WIDTH *
                    SCEMI_CHANNEL_WIDTH);
    pkt.data = m_data;

    BSVType *bsvdata = createData(m_current_probe, &pkt, m_latest_cycle);

    if (def->enabled() && bsvdata != 0) {

      m_latest_cycle -= 1;
      if (m_debug) fprintf(stderr, "%0lld: addChangeSerial4 probe %0d\n", m_latest_cycle, m_current_probe);
      vcd_writer->addChangeSerial(m_latest_cycle, m_current_probe, bsvdata);
      m_latest_cycle = m_latest_cycle - m_runlength;
      m_runlength = next_runlength;
      m_runlength_bits_left = def->getOffset();
      m_data_nbits_so_far = 0;
      m_first_word = true;
      m_nwords = 0;

      // Write out a z state for the next cycle
      if (m_remaining == 1) {
	SceMiU64 tim = msg.getTimeStamp()+1;
	if (m_debug) fprintf(stderr, "%0lld: (%0lld) addChangeSerial5 probe %0d\n", m_latest_cycle, tim, m_current_probe);
        vcd_writer->addChangeSerial(tim, m_current_probe, bsvdata, true);
      }

    }
  }

  m_remaining--;
  if (m_remaining == 0) {
    SceMiU64 tim = getLatestCycleForUpdate(msg);
    if (m_debug) fprintf(stderr, "%0lld: updateLatestTime3 %0lld\n", m_latest_cycle, tim);
    vcd_writer->updateLatestTime(tim, false);
    m_receiving_data = false; // Done receiving data
  }
}

SceMiU64 ProbesXactor::getLatestCycleForUpdate(const StampedT<SerialProbeData> &msg)
 {
   return (m_prev_meter_cycle ? m_prev_meter_cycle : msg.getTimeStamp());
 }

void ProbesXactor::processPowerMeterMsg(const StampedT<SerialProbeData> &msg)
{
  // Receive data
  SceMiU32 data = msg.getData().get();
  m_latest_cycle = msg.getTimeStamp();

  if (m_latest_cycle > m_prev_meter_cycle) {
    if (m_debug) fprintf(stderr, "%0lld: outputPowerMeters1 for %0lld\n",
			 m_latest_cycle, m_prev_meter_cycle);
    outputPowerMeters();
  }

  m_remaining--;

  if (m_debug) fprintf(stderr," Power Counter Data word for probenum: %d data: %x word %d remaining %d time: %lld\n",
                       m_current_probe, data, m_nwords, m_remaining, msg.getTimeStamp());

  // Get the probe def
  SerialProbeDef *def = m_probedefs_map[m_current_probe];

  m_data[m_nwords++] = data;

  // Create data from raw bits
  if (m_remaining == 0) {
    Packet pkt;
    pkt.channel = m_current_probe;
    pkt.num_bits = m_nwords*SCEMI_CHANNEL_WIDTH;
    pkt.data = m_data;

    BSVType *bsvdata = createData(m_current_probe, &pkt, m_latest_cycle);

    if (bsvdata == 0) {
      fprintf(stderr,"Error in ProbesXactor::processValueProbeMsg(): unable to create corresponding probe data in transxactor from HW data packet.\n"
              "Make sure you have built the testbench after properly generating the SceMiProbes.cxx with the appropriate probed params file as input.\n");

      // XXXX need to reset the state back to a stable point.....
      m_receiving_data = false; // Done receiving data
      return;
    }

    // Write to vcd file
    CycleCountStruct *cc = (CycleCountStruct*)bsvdata;
    if (def->enabled() && bsvdata != 0) {
      //std::cout << " Found a power counter message: ";
      //bsvdata->getBitString(std::cout);
      //std::cout << std::endl;
      //std::cout << " Found a power counter message: " << *cc << std::endl;

      BitT<1> *valbit = (BitT<1>*)cc->getMember(0);
      int valid = valbit->get();

      // If the data is not valid, just return right away
      if (valid == 0) {
        m_receiving_data = false; // Done receiving data
        //vcd_writer->updateLatestTime(msg.getTimeStamp(), false);
        return;
      }

      BitT<27> *val = (BitT<27>*)cc->getMember(3);
      unsigned int cycles = val->get();

      float power;
      PowerCounter *pc = getOrCreatePowerCounter(def->getPowerProbeNum());
      if (m_debug) fprintf(stderr, "%0lld: setNextSample1 %x on counter %s\n",
			   m_latest_cycle, cycles, pc->getLabel());
      pc->setNextSample(m_latest_cycle, cycles, (float)getSceMiClockFrequency());

      if (m_latest_cycle < m_prev_meter_cycle) {
	fprintf(stderr, "%0lld: ERROR: m_prev_meter_cycle is %0lld\n",
		m_latest_cycle, m_prev_meter_cycle);
      }
      else {
	m_prev_meter_cycle = m_latest_cycle;
      }

      // Counter Power
      power = pc->dynamicPower(m_latest_cycle);
      if (m_debug) fprintf(stderr, "%0lld: addRealSerial3 probe %0d\n", m_latest_cycle,
	      def->getPowerProbeNum());
      vcd_writer->addRealSerial(m_latest_cycle, def->getPowerProbeNum(), power);

      // Write to vcd file
      if (m_debug) fprintf(stderr, "%0lld: addChangeSerial6 probe %0d\n", m_latest_cycle, m_current_probe);
      // if m_debug ... ? xxx
      vcd_writer->addChangeSerial(m_latest_cycle, m_current_probe, bsvdata);
    }
    //else if (bsvdata != 0)
    //  std::cout << " Found a (not enabled) power counter message: " << *cc << std::endl;

    m_receiving_data = false; // Done receiving data
    SceMiU64 tim = getLatestCycleForUpdate(msg);
    if (m_debug) fprintf(stderr, "%0lld: updateLatestTime5 %0lld\n", m_latest_cycle, tim);
    vcd_writer->updateLatestTime(tim, true);
  }
}

void ProbesXactor::outputPowerMeters() {
  if (m_last_calculated_meter_cycle < m_prev_meter_cycle) {
    float power;

    // Group Power
    PowerCounterGroup *group;
    std::map<std::string, PowerCounterGroup *>::iterator pgItr;

    for (pgItr = m_powerCounterGroups.begin();
	 pgItr != m_powerCounterGroups.end();
	 pgItr++) {

      group = pgItr->second;
      if (group->isDirty()) {
	power = group->calculatePower(m_prev_meter_cycle);
	//printf("GROUP Power %f to probe number %d\n", power, group->getProbeNum());
	if (m_debug) fprintf(stderr, "%0lld: (%0lld) addRealSerial1 probe %0d\n", m_latest_cycle,
			     m_prev_meter_cycle, group->getProbeNum());
	vcd_writer->addRealSerial(m_prev_meter_cycle, group->getProbeNum(), power);
	group->setDirty(false);
      }
    }

    // Total Power
    power = calculateTotalPower(m_prev_meter_cycle);
    if (m_debug) fprintf(stderr, "%0lld: (%0lld) addRealSerial2 probe %0d\n", m_latest_cycle,
			 m_prev_meter_cycle, getTotalPowerProbeNum());
    vcd_writer->addRealSerial(m_prev_meter_cycle, getTotalPowerProbeNum(), power);

    if (m_debug) fprintf(stderr, "%0lld: updateLatestTime4 %0lld\n", m_latest_cycle, m_prev_meter_cycle);
    vcd_writer->updateLatestTime(m_prev_meter_cycle, false);
    m_last_calculated_meter_cycle = m_prev_meter_cycle;
    //m_prev_meter_cycle = m_latest_cycle;
  }
}

void ProbesXactor::processPowerChangesMsg(const StampedT<SerialProbeData> &msg)
{
  // Receive data
  SceMiU32 data = msg.getData().get();
  m_latest_cycle = msg.getTimeStamp();

  if (m_latest_cycle > m_prev_meter_cycle) {
    if (m_debug) fprintf(stderr, "%0lld: outputPowerMeters2 for %0lld\n",
			 m_latest_cycle, m_prev_meter_cycle);
    outputPowerMeters();
  }

  m_remaining--;

  if (m_debug) fprintf(stderr," Power Changes Data word for probenum: %d data: %x word %d remaining %d\n",
                       m_current_probe, data, m_nwords, m_remaining);

  m_data[m_nwords++] = data;

  // Create data from raw bits
  if (m_remaining == 0) {
    Packet pkt;
    pkt.channel = m_current_probe;
    pkt.num_bits = m_nwords*SCEMI_CHANNEL_WIDTH;
    pkt.data = m_data;

    BSVType *bsvdata = createData(m_current_probe, &pkt, m_latest_cycle);

    if (bsvdata == 0) {
      fprintf(stderr,"Error in ProbesXactor::processValueProbeMsg(): unable to create corresponding probe data in transxactor from HW data packet.\n"
              "Make sure you have built the testbench after properly generating the SceMiProbes.cxx with the appropriate probed params file as input.\n");

      // XXXX need to reset the state back to a stable point.....
      m_receiving_data = false; // Done receiving data
      return;
    }

    // Get the probe def
    SerialProbeDef *def = m_probedefs_map[m_current_probe];

    // Write to vcd file
    PowerSwitchStruct *ps = (PowerSwitchStruct*)bsvdata;
    //if (def->enabled() && bsvdata != 0) {
    if (bsvdata != 0) {
      //std::cout << " Found a power change message: " << *ps << std::endl;

      BitT<1> *valbit = (BitT<1>*)ps->getMember(2);
      int valid = valbit->get();

      // If the data is not valid, just return right away
      if (valid == 0) {
        m_receiving_data = false; // Done receiving data
        //vcd_writer->updateLatestTime(msg.getTimeStamp(), false);
        return;
      }

      PowerIx *pi = (PowerIx*)ps->getMember(0);
      //std::cout << std::endl << "PowerIx " << *pi << std::endl;
      int ps_id = ((BitT<2>*)pi->getMember(0))->get();

      BitT<1> *val = (BitT<1>*)ps->getMember(1);
      unsigned int state = val->get();

      //printf("Power ID of message %d\n", ps_id);
      if (state == 1)
        switchPowerOn(ps_id, m_latest_cycle);
      else
        switchPowerOff(ps_id, m_latest_cycle);

      //PowerSource *pwrs = getOrCreatePowerSource(ps_id);
      //float prevpower = pwrs->prevStaticPower();
      //float power = pwrs->calculateStaticPower(m_latest_cycle, (float)getSceMiClockFrequency());
      //if (prevpower != power)
      //vcd_writer->addRealSerial(m_latest_cycle, def->getPowerProbeNum(), power);

      // Total Power
      //power = calculateTotalPower(m_latest_cycle);

      // Write to vcd file
      if (m_debug && def->enabled() && bsvdata != 0)
	fprintf(stderr, "%0lld: addChangeSerial7 probe %0d\n", m_latest_cycle, m_current_probe);
      // if m_debug ... ?   xxx
      vcd_writer->addChangeSerial(m_latest_cycle, m_current_probe, bsvdata);
    }
    //else if (bsvdata != 0)
    //std::cout << " Found a (not enabled) changes power message: " << *ps << std::endl;

    m_receiving_data = false; // Done receiving data
    SceMiU64 tim = getLatestCycleForUpdate(msg);
    if (m_debug) fprintf(stderr, "%0lld: updateLatestTime7 %0lld\n", m_latest_cycle, tim);
    vcd_writer->updateLatestTime(tim, true);
  }
}

void ProbesXactor::processPowerIsoMsg(const StampedT<SerialProbeData> &msg)
{
  // Receive data
  SceMiU32 data = msg.getData().get();
  m_latest_cycle = msg.getTimeStamp();

  m_remaining--;

  if (m_debug) fprintf(stderr," Power Iso Data word for probenum: %d data: %x word %d %d\n",
                       m_current_probe, data, m_nwords, m_remaining);

  m_data[m_nwords++] = data;

  // Create data from raw bits
  if (m_remaining == 0) {
    Packet pkt;
    pkt.channel = m_current_probe;
    pkt.num_bits = m_nwords*SCEMI_CHANNEL_WIDTH;
    pkt.data = m_data;

    BSVType *bsvdata = createData(m_current_probe, &pkt, m_latest_cycle);

    if (bsvdata == 0) {
      fprintf(stderr,"Error in ProbesXactor::processValueProbeMsg(): unable to create corresponding probe data in transxactor from HW data packet.\n"
              "Make sure you have built the testbench after properly generating the SceMiProbes.cxx with the appropriate probed params file as input.\n");

      // XXXX need to reset the state back to a stable point.....
      m_receiving_data = false; // Done receiving data
      return;
    }

    SerialProbeDef *def = m_probedefs_map[m_current_probe];

    IsoFailureStruct *ifs = (IsoFailureStruct*)bsvdata;
    //std::cout << " Found a iso" << std::endl;
    if (ifs != 0) {

      BitT<1> *valbit = (BitT<1>*)ifs->getMember(3);
      int valid = valbit->get();

      //std::cout << " Found a iso valid: " << valid << std::endl;

      if (valid)
	reportIsolationFailure(ifs, def->getPath(), m_latest_cycle);

      // If the data is not valid, just return right away
      if (valid == 0) {
        m_receiving_data = false; // Done receiving data
        return;
      }

      //std::cout << " Found a iso power message: " << *ifs << std::endl;
      //vcd_writer->addChangeSerial(m_latest_cycle, m_current_probe, bsvdata);
    }
    else if (bsvdata != 0)
      std::cout << " Found a (not enabled) iso power message: " << *ifs << std::endl;

    m_receiving_data = false; // Done receiving data
    SceMiU64 tim = getLatestCycleForUpdate(msg);
    if (m_debug) fprintf(stderr, "%0lld: updateLatestTime8 %0lld\n", m_latest_cycle, tim);
    vcd_writer->updateLatestTime(tim, true);
  }
}

void ProbesXactor::reportIsolationFailure(IsoFailureStruct* ifs, const char* path,
					  SceMiU64 attime)
{
  std::fstream file;

  //std::cout << "Writing isolation failure " << *ifs << std::endl;

  PowerIx *intPI, *extPI;
  BitT<2> *intBit, *extBit;
  int internal_id, external_id;

  intPI = (PowerIx*)ifs->getMember(1);
  intBit = (BitT<2>*)intPI->getMember(0);
  internal_id = intBit->get();
  extPI = (PowerIx*)ifs->getMember(0);
  extBit = (BitT<2>*)extPI->getMember(0);
  external_id = extBit->get();

  fprintf(stderr, "%lld: At %s --\n", attime, path);
  fprintf(stderr, "            ISOLATION FAILURE: Extnl source %0d, Intnl source %0d\n",
	  external_id, internal_id);

  file.open("isolation.fail", std::ios::out | std::ios::app);
  if (file.fail()) {
    std::cerr << "ERROR ProbesXactor::reportIsolationFailure(): unable to open file 'isolation.fail' for write"
	      << "." << std::endl;
    file.close();
    return;
  }

  file << "[" << std::setw(12) << std::right << attime
       << "] At " << path << std::endl
       << "             ISOLATION FAILURE:  Extnl source " << external_id
       << ", Intnl source " << internal_id << std::endl;

  file.close();

}

void ProbesXactor::processScanMsg(const StampedT<SerialProbeData> &msg)
{
  // Receive data
  SceMiU32 data = msg.getData().get();

  m_remaining--;
  data_writer->addData(data);

  if (m_remaining == 0)
    m_receiving_data = false; // Done receiving data
}

SceMiU64 ProbesXactor::assembleNewCaptureRunlengthAndData(const StampedT<SerialProbeData> &msg)
{
  SceMiU32 word_data;
  SceMiU64 data, mask;

  // All 1s from bits 0 to n-1 where n is m_runlength_bits_left
  mask = (SceMiU64) pow(2.0, m_runlength_bits_left)-1;

  SerialProbeDef *def = m_probedefs_map[m_current_probe];
  int offset = def->getOffset();
  int first_word_data_width = SCEMI_CHANNEL_WIDTH - (offset % SCEMI_CHANNEL_WIDTH);

  // Receive data
  word_data = msg.getData().get();

  //fprintf(stderr, "\nData word in assemble: %x word_index %d\n",
  //  word_data, m_nwords);

  // Assemble the runlength
  if (m_runlength_bits_left > 0) {

    data = (word_data & mask);
    if (m_runlength_word == 0) {
      m_runlength = 0;
      m_runlength |= data;
    }
    else {
      data = (word_data & mask) << SCEMI_CHANNEL_WIDTH;
      m_runlength |= data;
    }

    //#m_data_start_bit = m_runlength_bits_left % SCEMI_CHANNEL_WIDTH;
    if (m_runlength_bits_left < SCEMI_CHANNEL_WIDTH) {
      m_runlength_bits_left = 0;
    }
    else {
      m_runlength_bits_left -= SCEMI_CHANNEL_WIDTH;
      return 0;
    }
  }

  // Assemble the data
  if (first_word_data_width == 0 || first_word_data_width == SCEMI_CHANNEL_WIDTH) {
    m_data[m_nwords++] = word_data;
    m_data_nbits_so_far += SCEMI_CHANNEL_WIDTH;
  }
  else if (offset > 0) {

    if (m_first_word) {
      m_data[m_nwords] = 0;
      data = (word_data >> offset);
      m_data[m_nwords] |= static_cast<SceMiU32>(data);
      m_first_word = !m_first_word;
      m_data_nbits_so_far += first_word_data_width;
    }
    else {
      data = (word_data << first_word_data_width);
      m_data[m_nwords++] |= static_cast<SceMiU32>(data);
      m_data_nbits_so_far += offset;

      if (m_data_nbits_so_far < def->getWidth()) {
        data = (word_data >> offset);
        m_data[m_nwords] = 0;
        m_data[m_nwords] |= static_cast<SceMiU32>(data);
        m_data_nbits_so_far += first_word_data_width;
      }
    }
  } else {

    if (m_debug)
      fprintf(stderr,"Something wrong we should not have gotten here SCEMI_CHANNEL_WIDTH: %d offset: %d first_word_data_width: %d\n", SCEMI_CHANNEL_WIDTH, offset, first_word_data_width);
  }


  if (m_debug) fprintf(stderr,"Remaining %d\n", m_remaining);

  // Get the probe def
  if (m_data_nbits_so_far >= def->getWidth()) {
    return m_runlength;
  }
  else {
    return 0;
  }
}

// Constructor
HierProbeDefNode::HierProbeDefNode(const char *aName, HierProbeDefNode *aParent,
                                   SerialProbeDef *aDef)
{
  Name = aName;
  Parent = aParent;
  Def = aDef;

  if (Parent)
    Parent->addChild(this);
}

// Destructor of the node tree
HierProbeDefNode::~HierProbeDefNode()
{
  HierProbeDefNode *node;
  HierProbeDefNodeIterator itr;

  for (itr = childBegin(); itr != childEnd(); itr++) {

    node = itr->second;
    delete node;
  }
}

// Add child SerialProbeDef
HierProbeDefNode *HierProbeDefNode::addChild(SerialProbeDef *d)
{
  HierProbeDefNode *child = new HierProbeDefNode(d->getLabel(), this, d);
  return child;
}

HierProbeDefNode *HierProbeDefNode::createTopHierProbeDefNode(const char *name)
{
  HierProbeDefNode *n = new HierProbeDefNode(name, NULL, NULL);
  HierProbeDefNode::TopHierProbeDefNodes[name] = n;

  return n;
}

void PowerCounterGroup::addPowerCounter(PowerCounter *p)
{
  m_powerCounters.push_back(p);
  p->m_group = this;
}

float PowerCounterGroup::calculatePower(SceMiU64 attime)
{
  std::list<PowerCounter*>::iterator ctrItr;
  PowerCounter *ctr;

  m_power = 0.0;
  for (ctrItr = m_powerCounters.begin();
       ctrItr != m_powerCounters.end();
       ctrItr++) {

    ctr = *ctrItr;
    m_power += ctr->dynamicPower(attime);
    //printf("Group %s Counter %s power %f\n", name(), ctr->getLabel(), m_power);
  }

  return m_power;
}

void PowerSourceGroup::addPowerSource(PowerSource *s)
{
  m_powerSources.push_back(s);
  s->m_group = this;
}

void PowerCounter::setNextSample(SceMiU64 current_time, SceMiU64 on_cycles, float frequency)
{
  //printf("\nNext sample for %s current time: %lld last time: %lld cycles: %lld last cycles: %lld consumption rate: %f\n", getLabel(), current_time, m_timeOfLastSample, on_cycles, m_cycles_sofar, m_power_consumption_rate);
  if (current_time == m_timeOfLastSample)
    return;

  // Consumption rate is in nJoules, the final power is in mW.
  m_dynamic_power = m_power_consumption_rate * static_cast<float>(on_cycles) /
    (static_cast<float>(current_time - m_timeOfLastSample) / frequency);
  //setPrevDynamicPower(m_dynamic_power); // Remember prev raw power
  m_dynamic_power = m_averager.average(m_dynamic_power);
  setPrevFilteredDynamicPower(m_dynamic_power);

  m_cycles_sofar += on_cycles;
  m_timeOfLastSample = current_time;

  PowerCounterGroup *group = getPowerCounterGroup();
  if (group)
    group->setDirty(true);
  else if (m_dynamic_power!=0.0)
    printf("Dynamic power not in a group (%zu) time %lld = %f\n", getID(), current_time, m_dynamic_power);
}

float PowerCounter::dynamicPower(SceMiU64 current_time)
{
  return prevFilteredDynamicPower();
}
/*
 if (current_time == m_timeOfLastSample) {
    //printf("Dynamic power2 (%d) time %lld %lld = %f\n", getID(), current_time, m_timeOfLastSample, m_dynamic_power);
    return m_dynamic_power;
  }
  else {
    //printf("Dynamic power3 (%d) time %lld %lld = %f %f\n", getID(), current_time, m_timeOfLastSample, m_prev_filtered_dynamic_power, m_dynamic_power);
    return m_prev_filtered_dynamic_power;
  }
}
*/

float PowerSource::calculateStaticPower(SceMiU64 current_time, float frequency)
{
  SceMiU64 duration = current_time - m_timeOfLastCalculation;
  SceMiU64 timeOn = m_switchonTime + ((m_isOn && (current_time>m_lastSwitchonTime)) ?
                                      current_time-m_lastSwitchonTime : 0);
  if (duration == 0) {
    return m_static_power;
  }

  float realtimeOn = static_cast<float>(timeOn)/frequency;
  float realDuration = static_cast<float>(duration)/frequency;

  //printf("calculate static Power source %s power time %lld last time %lld switchon time %lld timeOn %lld realtimeOn %f numSwitchOns %d frequency %f duration %lld\n", getLabel(), current_time, m_timeOfLastCalculation, m_switchonTime, timeOn, realtimeOn, m_numberOfSwitchons, frequency, duration);

  float leakage_power = (realtimeOn * m_leakageRate) / realDuration;
  float switching_power = 0.0;

  if (m_lastSwitchonTime < current_time) {
    switching_power = (static_cast<float>(m_numberOfSwitchons) * m_switchingEnergy) / realDuration;
    if (m_isOn) {
      m_lastSwitchonTime = current_time;
    }
    m_numberOfSwitchons = 0;
    m_switchonTime = 0;
  }
  else if (m_lastSwitchonTime != current_time) {
    fprintf(stderr, "WARNING: current time %0lld, lastSwitchonTime %0lld\n", current_time, m_lastSwitchonTime);
  }

  m_static_power = leakage_power + switching_power;
  //printf("before filter leakage power: %f switching power %f static power %f\n", leakage_power, switching_power, m_static_power);
  setPrevStaticPower(m_static_power); // Remember prev raw power
  //printf("after filter leakage power: %f switching power %f static power %f\n", leakage_power, switching_power, m_static_power);

  //printf("calculate static power Power source %s current time: %lld last time: %lld timeon: %lld number of switchons: %d Power: %f Leakage rate: %f Switching energy: %f\n", getLabel(), current_time, m_timeOfLastCalculation, timeOn, m_numberOfSwitchons, m_static_power, m_leakageRate, m_switchingEnergy);

  m_timeOfLastCalculation = current_time;

  return m_static_power;
}

PowerSourceGroup *ProbesXactor::getOrCreatePowerSourceGroup(const char *name)
{
  PowerSourceGroup *group;

  group = m_powerSourceGroups[name];
  if (group)
    return group;

  group = new PowerSourceGroup(name, max_power_probe_num);
  m_powerSourceGroups[name] = group;
  max_power_probe_num++;

  return group;
}

PowerCounterGroup *ProbesXactor::getOrCreatePowerCounterGroup(const char *name)
{
  PowerCounterGroup *group;

  //printf("Create PowerCounterGroup %s\n", name);

  group = m_powerCounterGroups[name];
  if (group)
    return group;

  group = new PowerCounterGroup(name, max_power_probe_num);
  m_powerCounterGroups[name] = group;

  // Create definition record for the probe
  //printf("Create probe definition for group %s\n", name);
  SerialProbeDef *def;
  def = new SerialProbeDef(0, 0, 32, "", name, "real");
  def->setProbeType(PowerGroup);
  def->setProbeNum(max_power_probe_num);
  def->setID(m_probedefs.size());

  m_probedefs.push_back (def); // Array of valid probe/capture (throw away trigger)
  m_probedefs_name_map[name] = def; // Also mapping table based on probeNum
  m_probedefs_map[max_power_probe_num] = def; // Also mapping table based on probeNum
  max_power_probe_num++;

  return group;
}

PowerSource *ProbesXactor::getOrCreatePowerSource(unsigned int id, float leakageRate, float switchingEnergy)
{
  //printf("Create1 PowerSource %d\n", id);
  //SerialProbeDef *def;
  PowerSource *ps = getPowerSource(id);
  if (ps == NULL) {
    ps = new PowerSource(id);
    ps->setLeakageRate(leakageRate);
    ps->setSwitchingEnergy(switchingEnergy);
    m_powerSources[id] = ps;
    //def = getProbeDefFromProbeNum(id);
    //if (def)
    //  ps->setLabel(def->getLabel());
  }

  return ps;
}

PowerSource *ProbesXactor::getOrCreatePowerSource(SerialProbeDef *def,
                                                  float leakageRate, float switchingEnergy)
{
  //printf("Create2 PowerSource %d name %s\n", def->getPowerProbeNum(), def->getLabel());
  PowerSource *ps = getPowerSource(def->getPowerProbeNum());
  if (ps == NULL) {
    ps = new PowerSource(def->getPowerProbeNum());
    ps->setLeakageRate(leakageRate);
    ps->setSwitchingEnergy(switchingEnergy);
    m_powerSources[def->getPowerProbeNum()] = ps;
    ps->setLabel(def->getLabel());
  }

  return ps;
}

PowerCounter *ProbesXactor::getOrCreatePowerCounter(unsigned int probe_num,
                                                    float powerConsumptionRate)
{
  SerialProbeDef *def;
  size_t id;
  //printf("HERE getOrCreate Counter %d\n", probe_num);
  PowerCounter *pc = ProbeNumToPowerCounterMap[probe_num];
  if (pc == NULL) {
    pc = new PowerCounter();
    m_powerCounters.push_back(pc);
    id = m_powerCounters.size()-1;
    pc->setPowerConsumptionRate(powerConsumptionRate);
    pc->setID(id);
    ProbeNumToPowerCounterMap[probe_num] = pc;
    def = getProbeDefFromProbeNum(probe_num);
    if (def)
      pc->setLabel(def->getLabel());
  }

  return pc;
}

void ProbesXactor::clearPowerAssociations()
{
  PowerSourceIterator psItr;
  PowerSource *ps;

  for (psItr = m_powerSources.begin();
       psItr != m_powerSources.end();
       psItr++) {

    ps = psItr->second;
    ps->removeAllPowerCounters();
  }
}

/*
void ProbesXactor::setNextCounterSample(unsigned int id, SceMiU64 current_time, SceMiU64 on_cycles)
{
  PowerCounter *pc = m_powerCounters[id];
  if (pc == NULL) {
    fprintf(stderr,"Error in ProbesXactor::setNextCounterSample(): cannot find a PowerCounter with ID %d\n", id);
    return;
  }
  if (m_debug) printf("setNextSample2 with time %lld %lld on counter %s\n",
		      current_time, on_cycles, pc->getLabel());

  pc->setNextSample(current_time, on_cycles, (float)getSceMiClockFrequency());
}
*/

void ProbesXactor::switchPowerOn(unsigned int id, SceMiU64 current_time)
{
  PowerSource *ps;

  //printf("Switch power of id %d ON\n", id);

  ps = getOrCreatePowerSource(id);

  if (ps == NULL) {
    fprintf(stderr,"Error in ProbesXactor::switchPowerOn(): cannot find a PowerSource with ID %d\n", id);
    return;
  }

  ps->switchOn(current_time);
}

void ProbesXactor::switchPowerOff(unsigned int id, SceMiU64 current_time)
{
  PowerSource *ps;
  ps = getOrCreatePowerSource(id);

  //printf("Switch power of id %d OFF\n", id);

  if (ps == NULL) {
    fprintf(stderr,"Error in ProbesXactor::switchPowerOff(): cannot find a PowerSource with ID %d\n", id);
    return;
  }

  ps->switchOff(current_time);
}

float ProbesXactor::calculateTotalPower(SceMiU64 current_time)
{
  PowerSourceIterator psItr;
  PowerSource *ps;
  float total_power = 0.0;
  float prevpower, currpower;
  int i = 0;

  //printf("\nCALCULATE TOTAL POWER\n");

  for (psItr = m_powerSources.begin();
       psItr != m_powerSources.end();
       psItr++) {

    i++;
    ps = psItr->second;
    if (ps) {
      prevpower = ps->prevStaticPower();
      currpower = ps->calculateStaticPower(current_time, (float)getSceMiClockFrequency());
      //printf("Power source %d static power %f %f\n", ps->getSourceID(), prevpower, currpower);
      total_power += currpower;
      if (prevpower != currpower) {
        SerialProbeDef *def = m_probedefs_name_map[ps->getLabel()];
        //printf("DEFFFFFF %p %s %d\n", def, ps->getLabel(), def->getProbeNum());
        if (def) {
	  if (m_debug) fprintf(stderr, "%0lld: (%0lld) addRealSerial6 probe %0d\n", m_latest_cycle,
		  current_time, def->getProbeNum());
          vcd_writer->addRealSerial(current_time, def->getProbeNum(), currpower);
	}
	else printf("DEFFFFFF %p %s omitted (power %g)\n", def, ps->getLabel(), currpower);
      }
    }
  }


  PowerCounterIterator pcItr;
  PowerCounter *pc;

  for (pcItr = m_powerCounters.begin();
       pcItr != m_powerCounters.end();
       pcItr++) {

    pc = *pcItr;
    if (pc) {
      //printf("Power counter %d dynamic power %f\n", pc->getID(), pc->dynamicPower(current_time));
      total_power += pc->dynamicPower(current_time);
    }
  }

  //printf("Total power %f\n", total_power);
  //vcd_writer->updateLatestTime(current_time, false);

  return total_power;
}

void ProbesXactor::parsePowerRateFile(const char *filename)
{
  const char *delimiter = " \n\t,:=";
  std::ifstream file;
  char lineBuf[MAX_LINE_SIZE+1];
  char *token;
  std::string name, key, svalue;
  float value;
  SerialProbeDef *def;
  PowerCounter *ctr;
  PowerSource *src;
  unsigned int ivalue;

  printf("Reading power rate from file %s...\n", filename);

  m_lineno = 0;

  file.open(filename);
  if (file.fail()) {
    std::cerr << "Warning power specification file not found, a default one will be generated"
              << "." << std::endl;
    file.close();
    writePowerRateFile(filename);
    file.open(filename);
  }

  // Read in each line.  Right now assuming each line consists of either:
  while (!file.eof()) {

    file.getline(lineBuf, MAX_LINE_SIZE);
    m_lineno++;
    token = strtok(lineBuf, delimiter);
    //printf("parsing line %d token %s\n", m_lineno, token);
    if (token && (strncmp(token, "//", 2) != 0)) { // Check comment
      //printf("Token %s\n", token);
      if (!strcmp(token, "Source")) {

        token = strtok(0, delimiter);
        name = token;
        if(EOF == sscanf(name.c_str(), "%d", &ivalue)) {
          std::cerr << "Error ProbesXactor::parsePowerRateFile(): illegal source number "
                    << name << " on line number "
                    << m_lineno << " of file " << filename
                    << "." << std::endl;
        }
        src = getOrCreatePowerSource(ivalue);
        //printf("Found Source %d %s\n", ivalue, src->getLabel());
        key = strtok(0, delimiter);
        //printf("key %s\n", key.c_str());

        while (key != "") {
          if (key == "leakage_rate") {
            svalue = strtok(0, delimiter);
            if(EOF == sscanf(svalue.c_str(), "%f", &value))
              {
                std::cerr << "Error ProbesXactor::parsePowerRateFile(): illegal floating point value "
                          << svalue << " on line number "
                          << m_lineno << " of file " << filename
                          << "." << std::endl;
              }
            //printf(" Set leakage rate %f\n", value);
            src->setLeakageRate(value);
            token = strtok(0, delimiter);
            if (token)
              key = token;
            else
              key = "";

          } else if (key == "switching_energy") {
            svalue = strtok(0, delimiter);
            //printf(" switching energy value %s\n", svalue.c_str());
            if(EOF == sscanf(svalue.c_str(), "%f", &value))
              {
                std::cerr << "Error ProbesXactor::parsePowerRateFile(): illegal floating point value "
                          << svalue << " on line number "
                          << m_lineno << " of file " << filename
                          << "." << std::endl;
              }
            //printf(" Set switching energy %f\n", value);
            src->setSwitchingEnergy(value);
            token = strtok(0, delimiter);
            if (token)
              key = token;
            else
              key = "";
          }
        }
      }
      else if (!strcmp(token, "Counter")) {
        token = strtok(0, delimiter);
        name = token;
        def = getProbeDefFromName(name);
        //printf("Counter def %p\n", def);
        if (def == NULL || (def->getProbeType() != PowerMeter)) {
          std::cerr << "Error ProbesXactor::parsePowerRateFile(): PowerCounter name "
                    << name << " on line number "
                    << m_lineno << " of file " << filename
                    << " does not exist." << std::endl;
        } else {
          ctr = getOrCreatePowerCounter(def->getPowerProbeNum());
          key = strtok(0, delimiter);

	  while (key != "") {
	    if (key == "power") {
	      svalue = strtok(0, delimiter);
	      if(EOF == sscanf(svalue.c_str(), "%f", &value)) {
		std::cerr << "Error ProbesXactor::parsePowerRateFile(): illegal floating point value "
			  << svalue << " on line number "
			  << m_lineno << " of file " << filename
			  << "." << std::endl;
	      }
	      //printf("set power rate %f of %s\n", value, name.c_str());
	      ctr->setPowerConsumptionRate(value);
	    } else if (key == "group") {
	      svalue = strtok(0, delimiter);
	      //printf("Found a group %s\n", svalue.c_str());
	      if (svalue != "NONE")
		{
		  PowerCounterGroup *group = getOrCreatePowerCounterGroup(svalue.c_str());
		  group->addPowerCounter(ctr);
		}
	    }
            token = strtok(0, delimiter);
            if (token)
              key = token;
            else
              key = "";
	  }
	}
      }
      else if (!strcmp(token, "Clock")) {
        token = strtok(0, delimiter);
        svalue = token;
        if(EOF == sscanf(svalue.c_str(), "%d", &ivalue))
          {
            std::cerr << "Error ProbesXactor::parsePowerRateFile(): illegal value for Clock "
                      << svalue << " on line number "
                      << m_lineno << " of file " << filename
                      << "." << std::endl;
          }
        setSceMiClockFrequency(ivalue);
      }
      else if (!strcmp(token, "MaxScale")) {
        token = strtok(0, delimiter);
        svalue = token;
	//printf("Found max scale\n");
	if(EOF == sscanf(svalue.c_str(), "%f", &value))
          {
            std::cerr << "Error ProbesXactor::parsePowerRateFile(): illegal value for MaxScale "
                      << svalue << " on line number "
                      << m_lineno << " of file " << filename
                      << "." << std::endl;
          }
	setMaxScale(value);
	if (use_max_scale) {
	  //printf("Set max scale %f\n", value);
	  vcd_writer->setMaxScale(value);
	}
      }
      else {
        std::cerr << "Error ProbesXactor::parsePowerRateFile(): unrecoginized token "
                  << token << " on line number "
                  << m_lineno << " of file " << filename
                  << ".  Valid values are {Source, Counter}." << std::endl;
      }
    }
  }
  file.close();
  //writePowerRateFile(filename);
}

void ProbesXactor::writePowerRateFile(const char *filename)
{
  std::fstream file;
  std::string name, key, svalue;
  float value;

  printf("Writing power rate to file %s...\n", filename);

  file.open(filename, std::ios::out);
  if (file.fail()) {
    std::cerr << "ERROR ProbesXactor::writePowerRateFile(): unable to open rate file "
              << filename
              << "." << std::endl;
    file.close();
    return;
  }

  file.setf(std::ios::fixed, std::ios::floatfield);
  file.precision(2);

  file << "// Clock (mhz). This is the target clock rate of the DUT." << std::endl;
  file << "Clock " << getSceMiClockFrequency() << std::endl;
  file << std::endl;

  PowerSourceIterator srcItr;
  PowerSource *src;

  file << "// Source Name leakage_rate(milli watts) switch_energy(nano joules)" << std::endl;
  for (int i=0; i<4; i++) {

    src = getOrCreatePowerSource(i);
    file << "Source " << src->getSourceID();
    value = src->getLeakageRate();
    file << " leakage_rate=" << value;

    value = src->getSwitchingEnergy();
    file << " switching_energy=" << value;
    file << std::endl;
  }

  file << std::endl;
  file << "// Counter Name power(nano joules per cycle). This is power consumption rate." << std::endl;

  PowerCounterIterator ctrItr;
  PowerCounter *ctr;
  const char *group_name;
  for (ctrItr = m_powerCounters.begin();
       ctrItr != m_powerCounters.end();
       ctrItr++) {

    ctr = *ctrItr;
    if (ctr == NULL) continue;
    file << "Counter " << ctr->getLabel();
    group_name = ctr->getGroupName();
    if (group_name == NULL)
      file << " group=NONE";
    else
      file << " group=" << group_name;
    value = ctr->getPowerConsumptionRate();
    file << " power=" << value;
    file << std::endl;
    //printf("Found a counter %s rate %f\n", ctr->getLabel(), value);
  }

  file << std::endl;
  file << "// Max scale (milli watts) used to size the scale of the output power (0.0 for no scaling)" << std::endl;
  file << "MaxScale 0.0" << std::endl;

  file.close();
}
