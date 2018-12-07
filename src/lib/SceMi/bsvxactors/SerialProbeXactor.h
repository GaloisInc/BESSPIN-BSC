// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <iostream>
#include <fstream>
#include <deque>

#include "scemi.h"
#include "InportProxyT.h"
#include "OutportProxyT.h"
#include "StampedT.h"
#include "BitT.h"
#include "VCDWriter.h"
#include "DataWriter.h"

class IsoFailureStruct;

void eliminate_space(const char *name, std::string &return_name);

typedef BitT<1>  Bool;

class RollingAverager {
 private:
  size_t complete_size;
  std::deque<float> lst;
  float sum;

 public:
  RollingAverager(const size_t orig_size)
    : complete_size(orig_size)
    , lst(std::deque<float>())
    , sum(0.0)
    {}
  void change_size(const size_t new_size);
  float average(const float x);
};

class SerialProbeControl {
 public:
  BitT<16> m_probe_num ;
  BitT<1> m_enable ;

  SerialProbeControl ()
    : m_probe_num()
    , m_enable()
  {}

  SerialProbeControl ( const SceMiMessageData *msg, unsigned int &off )
    : m_probe_num(msg, off)
    , m_enable(msg, off)
  {}

  unsigned int setMessageData (SceMiMessageData &msg, const unsigned int off=0) const {
    unsigned int running = off;
    running = m_probe_num.setMessageData( msg, running );
    running = m_enable.setMessageData( msg, running );
    if (running != off + 17 ) {
      std::cerr << "Mismatch in sizes: " << std::dec <<  running << " vs " << (off + 17) << std::endl;
    }
    return running;
  }

  friend std::ostream & operator<< (std::ostream &os, const SerialProbeControl &obj) {
    os << "{" ;
    os << "m_probe_num " << obj.m_probe_num ;os << " " ;
    os << "enable " << obj.m_enable ;os << "}" ;
    return os;
  }

  std::ostream & getBitString (std::ostream & os) const {
    m_probe_num.getBitString (os);
    m_enable.getBitString (os);
    return os;
  }

  static std::ostream & getBSVType (std::ostream & os) {
    os << "SerialProbeControl" ;
    return os;
  }

  static unsigned int getBitSize () {
    return 17;
  }

  unsigned int getProbeNum() const { return m_probe_num.getTaggedUnionTag(); }
  bool getEnable() const { return m_enable.get(); }
};

typedef BitT<32> SerialProbeData;

// Should be kept compatible with ProbeKind in
// lib/BSVSource/SceMi/SceMiSerialProbe.bsv, and with the use of ProbeType in
// lib/tcllib/scemi/ProbeGui.tcl:
enum SerialProbeType { ValueProbe, CaptureProbe, TriggerProbe, NewCaptureProbe, ScanProbe,
		       PowerMeter, PowerChanges, PowerISO, PowerGroup, PowerSrc };


// Definition for subnet (signals) that can be part of a probe

class SubNetProbeDef {

 protected:

  std::string m_name;
  std::string m_alias;
  int         m_width;
  int         m_frombit;
  int         m_tobit;
  int         m_vcdid;

 public:

 SubNetProbeDef(const char *n, int w, int from, int to,
		int vcdid=0)
   : m_name(n)
    , m_width(w)
    , m_frombit(from)
    , m_tobit(to)
    , m_vcdid(vcdid)
  {}

  const char *name() { return m_name.c_str(); }
  const char *alias() { return m_alias.c_str(); }
  void setAlias(const char *n) { m_alias = n; }
  int width() { return m_width; }
  int fromBit() { return m_frombit; }
  int toBit() { return m_tobit; }
  int vcdID() { return m_vcdid; }
  void setVcdID(int id) { m_vcdid = id; }
};

typedef std::list<SubNetProbeDef*> SubNetProbeDefList;
typedef std::list<SubNetProbeDef*>::iterator SubNetProbeDefIterator;

class PowerSource;
class PowerCounter;

typedef std::list<PowerCounter*> PowerCounterList;
typedef std::list<PowerCounter*>::iterator PowerCounterListIterator;

class PowerCounterGroup {

 protected:

  std::string m_name;
  PowerCounterList m_powerCounters;
  unsigned int m_probe_num;
  float m_power;
  bool  m_dirty;

 public:

  PowerCounterGroup(const char *n, unsigned int probe_num)
    { m_name = n; m_probe_num = probe_num; m_power = 0.0; m_dirty = false; }

  ~PowerCounterGroup() {}

  void setDirty(bool d) { m_dirty = d; }
  int  isDirty() { return m_dirty; }

  const char *name() { return m_name.c_str(); }
  unsigned int getProbeNum() { return m_probe_num; }

  void addPowerCounter(PowerCounter *p);

  PowerCounterListIterator beginPowerCounter()
  { return m_powerCounters.begin(); }
  PowerCounterListIterator endPowerCounter()
  { return m_powerCounters.end(); }

  float calculatePower(SceMiU64 attime);
  float getPower() { return m_power; }

};

class PowerCounter {

  friend class PowerCounterGroup;

 protected:
  SceMiU64 m_timeOfLastSample;
  SceMiU64 m_cycles_sofar;
  float    m_power_consumption_rate;
  float    m_dynamic_power;
  size_t m_id;
  //float    m_prev_dynamic_power;
  float    m_prev_filtered_dynamic_power;
  PowerCounterGroup *m_group;
  std::string   m_label;

  PowerSource *m_power_source;

 public:
  RollingAverager m_averager;
  PowerCounter()
    : m_timeOfLastSample(0)
    , m_cycles_sofar(0)
    , m_power_consumption_rate(1.0)
    //, m_prev_dynamic_power(0.0)
    , m_prev_filtered_dynamic_power(0.0)
    , m_group(NULL)
    , m_averager(RollingAverager(1))
    { m_power_source = NULL; }

  ~PowerCounter() {}

  PowerCounterGroup *getPowerCounterGroup() { return m_group; }
  const char *getGroupName()
  {
    if (m_group)
      return m_group->name();
    return NULL;
  }

  void setPowerSource(PowerSource *source) { m_power_source = source; }
  PowerSource *getPowerSource() { return m_power_source; }
  void setPowerConsumptionRate(float rate) { m_power_consumption_rate = rate; }
  float getPowerConsumptionRate() { return m_power_consumption_rate; }
  void setNextSample(SceMiU64 current_time, SceMiU64 on_cycles, float clock_frequency);
  float dynamicPower(SceMiU64 attime);
  //void setPrevDynamicPower(float val) { m_prev_dynamic_power = val; }
  //float prevDynamicPower() { return m_prev_dynamic_power; }
  void setPrevFilteredDynamicPower(float val) { m_prev_filtered_dynamic_power = val; }
  float prevFilteredDynamicPower() { return m_prev_filtered_dynamic_power; }

  void setLabel(const char *l) { m_label = l; }
  const char *getLabel() { return m_label.c_str(); }

  void setID(size_t i) { m_id = i; }
  size_t getID() { return m_id; }
};


typedef std::vector<PowerCounter* > PowerCounterSet;
typedef std::vector<PowerCounter* >::iterator PowerCounterIterator;
typedef std::list<PowerSource*> PowerSourceList;
typedef std::list<PowerSource*>::iterator PowerSourceListIterator;

class PowerSourceGroup {

 protected:

  std::string m_name;
  PowerSourceList m_powerSources;
  unsigned int m_probe_num;
  float m_power;

 public:

  PowerSourceGroup(const char *n, unsigned int probe_num)
    { m_name = n; m_probe_num = probe_num; m_power = 0.0; }

  ~PowerSourceGroup() {}

  const char *name() { return m_name.c_str(); }

  void addPowerSource(PowerSource *p);

  PowerSourceListIterator beginPowerSource()
  { return m_powerSources.begin(); }
  PowerSourceListIterator endPowerSource()
  { return m_powerSources.end(); }

};

// PowerSource class contains extra bookkeeping info for calculating power
class PowerSource {

  friend class PowerSourceGroup;

 protected:
  bool m_isOn;  // Remember whether the current power source is on/off
  unsigned int m_source_id;
  PowerSourceGroup *m_group;
  SceMiU64 m_timeOfLastCalculation;
  SceMiU64 m_whenSwitchedOn;
  int m_numberOfSwitchons;
  SceMiU64 m_lastSwitchonTime;
  SceMiU64 m_lastSwitchoffTime;
  SceMiU64 m_lastCurrentTime;
  SceMiU64 m_switchonTime;
  float    m_leakageRate;
  float    m_switchingEnergy;
  float    m_static_power;
  float    m_prev_static_power;
  float    m_prev_filtered_static_power;
  std::string   m_label;

  PowerCounterSet m_powerCounters;

 public:

  PowerSource(unsigned int id)
    : m_isOn(false)
    , m_source_id(id)
    , m_group(NULL)
    , m_timeOfLastCalculation(0)
    , m_whenSwitchedOn(0)
    , m_numberOfSwitchons(0)
    , m_lastSwitchonTime(0)
    , m_lastSwitchoffTime(0)
    , m_lastCurrentTime(0)
    , m_switchonTime(0)
    , m_leakageRate(0.0)
    , m_switchingEnergy(0.0)
    , m_static_power(0.0)
    , m_prev_static_power(0.0)
    {}

  ~PowerSource() {}

  unsigned int getSourceID() { return m_source_id; }

  PowerSourceGroup *getPowerSourceGroup() { return m_group; }
  const char *getGroupName()
  {
    if (m_group)
      return m_group->name();
    return NULL;
  }

  void setLeakageRate(float val) { m_leakageRate = val; }
  float getLeakageRate() { return m_leakageRate; }
  void setSwitchingEnergy(float val) { m_switchingEnergy = val; }
  float getSwitchingEnergy() { return m_switchingEnergy; }

  void addPowerCounter(PowerCounter *powerCounter)
  { m_powerCounters.push_back(powerCounter); }

  PowerCounter *getPowerCounter(unsigned int id)
  { return m_powerCounters[id]; }

  void switchOn(SceMiU64 current_time)
  {
    if (m_isOn == false)
      { m_isOn = true; m_numberOfSwitchons++; m_lastSwitchonTime = current_time; }
  }

  void switchOff(SceMiU64 current_time)
  { if (m_isOn == true)
      { m_isOn = false; m_switchonTime += (current_time - m_lastSwitchonTime); }
  }

  void setLabel(const char *l) { m_label = l; }
  const char *getLabel() { return m_label.c_str(); }

  float calculateStaticPower(SceMiU64 current_time, float frequency);
  float getLatestStaticPower() { return m_static_power; }

  void removeAllPowerCounters() { m_powerCounters.clear(); }

  void setPrevStaticPower(float val) { m_prev_static_power = val; }
  float prevStaticPower() { return m_prev_static_power; }
};

typedef std::map<unsigned int, PowerSource* > PowerSourceSet;
typedef std::map<unsigned int, PowerSource* >::iterator PowerSourceIterator;

// SerialProbeDef class contains the definition of each probe.
// The definition comes from scemi .params file usually generated
// by BlueSpec probe insertion tool.
// Here is an example format of a probe definition in .params file:
//  Serial 0 Path           "scemi_server_ci_cCAPTURE1"
//  Serial 0 PrbNum         11
//  Serial 0 Label          "s0"
//  Serial 0 Kind           3
//  Serial 0 Samples        32
//  Serial 0 Offset         16
//  Serial 0 Width          16
//  Serial 0 Type           "Int#(16)"
//  Serial 0 SubNets        "NetName1:8,NetName2:4"

class SerialProbeDef {

 protected:
  std::string path;
  std::string label;
  unsigned int probe_num; // PrbNum field in the Serial struct of the param file
  size_t probe_id;  // ID as it appears in the param file (2nd column)
  unsigned int samples;
  unsigned int samples_remaining;
  unsigned int offset;
  unsigned int width;
  std::string bsvtype;
  SerialProbeType probetype;
  unsigned int m_enabled;
  SceMiU64 latest_cycle;
  SubNetProbeDefList subnets;
  unsigned int power_probe_num;

 public:

 SerialProbeDef(unsigned int samplesx, unsigned int offsetx, unsigned int w,
		const char *pathx, const char *labelx, const char * bsvtypex)
    : path(pathx)
    ,label(labelx)
    ,probe_num(0)
    ,probe_id(0)
    ,samples(samplesx)
    ,samples_remaining(0)
    ,offset(offsetx)
    ,width(w)
    ,bsvtype(bsvtypex)
    ,probetype(ValueProbe)
    ,m_enabled(0)
    ,latest_cycle(0)
    ,power_probe_num(0)
    {}
  ~SerialProbeDef();

  void setPath(const char *p) { path = p; }
  const char *getPath() { return path.c_str(); }

  void setProbeNum(unsigned int n) { probe_num = n; }
  void setPowerProbeNum(unsigned int n) { power_probe_num = n; }
  unsigned int getProbeNum() { return probe_num; }
  unsigned int getPowerProbeNum() { return power_probe_num; }

  void setID(size_t n) { probe_id = n; }
  size_t getID() { return probe_id; }

  void setLabel(const char *l) { label = l; }
  const char *getLabel() { return label.c_str(); }

  void setSamples(unsigned int n) { samples = n; }
  unsigned int getSamples() { return samples; }

  void setSamplesRemaining(unsigned int n) { samples_remaining = n; }
  unsigned int getSamplesRemaining() { return samples_remaining; }
  void decrementSamplesRemaining() { samples_remaining--; }
  void reduceSamplesRemaining(unsigned int n) { samples_remaining -= n; }

  void setOffset(unsigned int n) { offset = n; }
  unsigned int getOffset() { return offset; }

  void setWidth(unsigned int w) { width = w; }
  unsigned int getWidth() { return width; }

  void setBSVType(const char *t) { bsvtype = t; }
  const char *getBSVType() { return bsvtype.c_str(); }

  void setProbeType(SerialProbeType t)
  {
    probetype = t;
  }
  SerialProbeType getProbeType() { return probetype; }
  bool isPowerProbe()
  {
    return ((probetype == PowerMeter) || (probetype == PowerChanges) ||
	    (probetype == PowerGroup) || (probetype == PowerSrc));
  }

  bool isPowerGroupProbe()
  { return (probetype == PowerGroup); }

  bool isPowerSourceProbe()
  { return (probetype == PowerSrc); }

  void setEnable(bool val) { if (val) m_enabled++; else m_enabled = 0; }
  unsigned int enabled() { return m_enabled; }

  void noteEvent() { if (m_enabled!=0) m_enabled--; }

  void setLatestCycle(SceMiU64 n) { latest_cycle = n; }
  SceMiU64 getLatestCycle() { return latest_cycle; }

  size_t numSubNets() { return subnets.size(); }
  void addSubNet(SubNetProbeDef* def) { subnets.push_back(def); }
  SubNetProbeDefIterator subNetsBegin() { return subnets.begin(); }
  SubNetProbeDefIterator subNetsEnd() { return subnets.end(); }
};

class HierProbeDefNode;
//typedef std::list<HierProbeDefNode*> HierProbeDefNodeList;
typedef std::map<std::string,HierProbeDefNode*>::iterator HierProbeDefNodeIterator;
typedef std::map<std::string,HierProbeDefNode*> HierProbeDefNodeMap;

// This is a class that is used to build a hierarchical tree of SerialProbeDef
class HierProbeDefNode {

 protected:

  std::string          Name;
  HierProbeDefNode    *Parent;
  SerialProbeDef      *Def;
  HierProbeDefNodeMap  Children;

  static HierProbeDefNodeMap TopHierProbeDefNodes;

  // Map of written signals
  std::map<std::string, bool> WrittenSignals;

 public:

  // Constructors
  HierProbeDefNode() { Parent = NULL; Def = NULL; }
  HierProbeDefNode(const char *name, HierProbeDefNode *aParent, SerialProbeDef *aDef);

  // Destructor
  ~HierProbeDefNode();

  // Accessor methods
  HierProbeDefNode *parent() { return Parent; }
  SerialProbeDef   *def()    { return Def; }
  const char *name()         { return Name.c_str(); }
  bool writtenSignal(const char *s) { return WrittenSignals[s]; }

  // Modifying methods
  void addChild(HierProbeDefNode *node) { Children[node->name()] = node; }
  HierProbeDefNode *addChild(SerialProbeDef *d);
  HierProbeDefNode *findChild(const char *nm) { return Children[nm]; }
  bool addWrittenSignal(const char *s)
  {
    if (WrittenSignals[s]) return false;
    WrittenSignals[s] = true;
    return true;
  }

  // Iterator of children
  HierProbeDefNodeIterator childBegin() { return Children.begin(); }
  HierProbeDefNodeIterator childEnd()   { return Children.end(); }

  // Iterator of top level nodes
  static HierProbeDefNodeIterator topBegin() { return TopHierProbeDefNodes.begin(); }
  static HierProbeDefNodeIterator topEnd()   { return TopHierProbeDefNodes.end(); }
  static HierProbeDefNode *findTopHierProbeDefNode(const char *name)
  { return TopHierProbeDefNodes[name]; }
  static HierProbeDefNode *createTopHierProbeDefNode(const char *name);
};

// ProbesXactor class which handle instantiated hw probes with
//  mkSerialProbe, mkSerialCapture, and mkSerialTrigger

class ProbesXactor {

 protected:
  SceMiParameters *theSceMiParams;

  OutportProxyT<StampedT<SerialProbeData> >  m_probedata;
  InportProxyT<SerialProbeControl>           m_probectrl;

  SerialProbeControl                         m_control;
  std::vector<SerialProbeDef*>               m_probedefs;
  std::map<unsigned int, SerialProbeDef*>    m_probedefs_map;
  std::map<std::string, SerialProbeDef*>     m_probedefs_name_map;
  std::map<unsigned int, PowerCounter*>      ProbeNumToPowerCounterMap;
  std::string  vcd_filename;
  VCDWriter    *vcd_writer;
  DataWriter   *data_writer;

  std::string  subnets_param_retval;

  PowerSourceSet m_powerSources;
  PowerCounterSet m_powerCounters;
  std::map<std::string, PowerSourceGroup *> m_powerSourceGroups;
  std::map<std::string, PowerCounterGroup *> m_powerCounterGroups;

  bool m_debug;
  bool has_power_probe;
  bool use_max_scale;
  unsigned int power_meter_probe_num_delta;
  unsigned int power_changes_probe_num_delta;
  unsigned int max_power_probe_num;
  SceMiU64 m_prev_meter_cycle;
  SceMiU64 m_last_calculated_meter_cycle;
  unsigned int scemi_clock_frequency;
  float max_scale;
  // XXX Temp hack:
  bool m_is_first_zero;

  int m_lineno;

 private:

  // Disallow default and copy constructors
  ProbesXactor & operator= (const ProbesXactor &);
  ProbesXactor(const ProbesXactor &);

  ProbesXactor(bool debug, const std::string & hier, const std::string & instname, const char *file_base,
	       SceMi *scemi, bool useMaxScale=false);
  ~ProbesXactor();


 public:

  // Constructor given a param file which contains all the
  static ProbesXactor *init(bool debug, const std::string & hier, const std::string & instname,
			    const char *file_base, SceMi *scemi, bool useMaxScale=false);

  static ProbesXactor *init(const std::string & hier, const std::string & instname,
			    const char *file_base, SceMi *scemi, bool useMaxScale=false);


  // Shutdown the transactor (delete the static ProbesXactor)
  static void shutdown();

  // Methods to enable or disable the Probe/Capture
  void enable(unsigned int probeID);
  void enableByProbeNum(unsigned int probeNum);
  void enableAll();
  void disable(unsigned int probeID);
  void disableByProbeNum(unsigned int probeNum);
  void disableAll();

  // Method to initiate a query HW cclock timestamp
  void queryTimestamp();

  // Method to associate block and power
  void associateBlockToPowerSource(unsigned int block_id, unsigned int source_id);

  // Method to enable power isolators
  void enablePowerIsolators();

  void requestPowerStat();
  void setPowerInterval(unsigned int interval);
  void setWindowSize(unsigned int sz);

  // Return the state of the probe
  unsigned int enabled(unsigned int probeID);
  unsigned int enabledByProbeNum(unsigned int probeNum);

  // VCD file name
  const char *vcdFileName() { return vcd_filename.c_str(); }

  // Params file info
  unsigned getCurrentProbe (SceMiU32 data);
  unsigned getCurrentSerialType (SceMiU32 data);
  unsigned getPowerKind (SceMiU32 data);

  // Get the width of the data being probe in bits
  unsigned int getMaxBitSize();

  // Get the max runlength width
  unsigned int getMaxRunlengthWidth();

  // Get the width of the data being probe in bits
  // This function is automatically created.
  unsigned int getBitSize(unsigned int probeID);

  // Get the total power probe num
  unsigned int getTotalPowerProbeNum() { return max_power_probe_num; }

  // The code for this method will be generated.
  BSVType *createData(unsigned int probeNum, Packet *data, SceMiU64 cycle);

  // Return true if the message is a scan message
  // 0x4000: CosimStruct tagged union
  bool isScanMessage(SceMiU32 data) { return ((data & 0x60000000) == 0x40000000); }

  void processProbeMsg(const StampedT<SerialProbeData> &msg);
  void processValueProbeMsg(const StampedT<SerialProbeData> &msg);
  void processCaptureProbeMsg(const StampedT<SerialProbeData> &msg);
  void processNewCaptureProbeMsg(const StampedT<SerialProbeData> &msg);
  void processPowerMeterMsg(const StampedT<SerialProbeData> &msg);
  void processPowerChangesMsg(const StampedT<SerialProbeData> &msg);
  void processPowerIsoMsg(const StampedT<SerialProbeData> &msg);
  void processScanMsg(const StampedT<SerialProbeData> &msg);
  SceMiU64 assembleNewCaptureRunlengthAndData(const StampedT<SerialProbeData> &msg);
  void reportIsolationFailure(IsoFailureStruct* ifs, const char* path, SceMiU64 attime);
  void outputPowerMeters();

  // Get the number of samples for each trigger for Capture
  // For Probe, this always return 1;
  unsigned int getSamples(unsigned int probeID);

  // Get the number of probes in the param file
  size_t getNumberOfProbes() { return m_probedefs.size(); }

  // Get the number of samples for each trigger for Capture
  unsigned int getOffset(unsigned int probeID);

  // Get the width in bits of the sample for each Capture sample
  unsigned int getWidth(unsigned int probeID);

  // Get the probe number from the object id
  unsigned int getProbeNum(unsigned int probeID);

  // Get the probe ID from the the given probeNum
  // Return -1 if none found
  int getProbeID(unsigned int probeNum);

  // Return the label string of the Probe/Capture
  const char *getLabel(unsigned int probeID);

  // Return the path of the Probe/Capture
  const char *getPath(unsigned int probeID);

  // Return the bsvtype of the Probe/Capture
  const char *getBSVType(unsigned int probeID);

  bool isPowerProbe(unsigned int probeNum);
  bool isPowerGroupProbe(unsigned int probeNum);
  bool isPowerSourceProbe(unsigned int probeNum);
  bool hasPowerProbe() { return has_power_probe; }
  SceMiU64 getLatestCycleForUpdate(const StampedT<SerialProbeData> &msg);

  SerialProbeDef *getProbeDefFromIndex(unsigned int index)
  { return m_probedefs[index]; }
  SerialProbeDef *getProbeDefFromName(std::string &name)
  { return m_probedefs_name_map[name]; }
  SerialProbeDef *getProbeDefFromProbeNum(unsigned int index)
  { return m_probedefs_map[index]; }

  void setDebug(bool val) { m_debug = val; }

  void createSubNetProbeDefs(SerialProbeDef *def, const char *subnetsString);

  // Return the bsvtype of the Probe/Capture
  const char *getParamSubNetsFromProbeNum(unsigned int probe_num);

  void sendMessage (SerialProbeControl &msg)
  { m_probectrl.sendMessage(msg); }

  bool sendMessageNonBlocking (SerialProbeControl &msg)
  { return m_probectrl.sendMessageNonBlocking(msg); }

  // Power related methods
  PowerSourceGroup *getOrCreatePowerSourceGroup(const char *name);
  PowerSource *getOrCreatePowerSource(unsigned int id, float leakageRate=0.0,
				      float switchingEnergy=0.0);
  PowerSource *getOrCreatePowerSource(SerialProbeDef *def, float leakageRate=0.0,
				      float switchingEnergy=0.0);
  PowerCounterGroup *getOrCreatePowerCounterGroup(const char *name);
  PowerCounter *getOrCreatePowerCounter(unsigned int id, float powerConsumptionRate=1.0);
  PowerSource *getPowerSource(unsigned int id)
  {
    return m_powerSources[id];
  }
  PowerCounter *getPowerCounter(unsigned int id)
  {
    if (id >= m_powerCounters.size())
      m_powerCounters.resize(id+1, NULL);
    return m_powerCounters[id];
  }
  void clearPowerAssociations();
  void associatePowerAndCounter(PowerSource *ps, PowerCounter *pc)
  { ps->addPowerCounter(pc); }
  //void setNextCounterSample(unsigned int id, SceMiU64 current_time, SceMiU64 on_cycles);
  void switchPowerOn(unsigned int id, SceMiU64 current_time);
  void switchPowerOff(unsigned int id, SceMiU64 current_time);
  float calculateTotalPower(SceMiU64 current_time);
  void parsePowerRateFile(const char *filename);
  void writePowerRateFile(const char *filename);

  void setSceMiClockFrequency(unsigned int c) { scemi_clock_frequency = c; }
  unsigned getSceMiClockFrequency() { return scemi_clock_frequency; }

  void setMaxScale(float v) { max_scale = v; }
  float getMaxScale() { return max_scale; }

  void setUseMaxScale(bool u) { use_max_scale = u; };
  bool getUseMaxScale() { return use_max_scale; };

 protected:



  unsigned int    m_current_probe; // The probenum of current active probe/capture
  unsigned int    m_samples;       // The number of samples in current stream of data
  unsigned int    m_remaining;     // Number of words remaining in current stream of data
  unsigned int    m_nwords;        // The index into the received data words
  unsigned int    m_max_bitsize;   // Maximum number of bits for whole packetized data
  SceMiU32       *m_data;          // Pointer to array of bits of data
  SceMiU64        m_runlength;     // Runlength of the data for value or capture
  unsigned int    m_runlength_bits_left;  // Number of bits left of runlength to be received
  unsigned int    m_runlength_word;       // The index into the runlength data array
  unsigned int    m_data_start_bit;       // The start bit for data portion of a receiving word
  unsigned int    m_data_nbits_so_far;    // The number of bits of the data received so far
  SceMiU64        m_latest_cycle;         // Latest timestamp
  int             m_current_scan;         // Index of current active scan
  int             m_previous_scan;        // Index of previous scan
  int             m_previous_probe;        // Index of previous probe
  SerialProbeType m_probe_type;
  bool            m_receiving_data;  // Flag indicating whether the transactor is in
                                     // data (true) or command (false) receiving mode
  bool            m_first_word;      // State of the filling the left or right half of a word
                                     // during the packing of data into m_data field.

  static void logProbe (void *vptr, const StampedT<SerialProbeData> &msg) {
    ProbesXactor *xactor = (ProbesXactor*)vptr;
    xactor->processProbeMsg(msg);
  }

  // Get the number of samples for each trigger for Capture
  // For Probe, this always return 1;
  unsigned int getParamSamples(unsigned int probeID);

  // Get the number of probes in the param file
  unsigned int getParamNumberOfProbes();

  // Get the number of samples for each trigger for Capture
  unsigned int getParamOffset(unsigned int probeID);

  // Get the number of samples for each trigger for Capture
  unsigned int getParamWidth(unsigned int probeID);

  // Get the probe number from the object id
  unsigned int getParamProbeNum(unsigned int probeID);

  // Return the label string of the Probe/Capture
  const char *getParamLabel(unsigned int probeID);

  // Return the probetype of the Probe/Capture
  SerialProbeType getParamKind(unsigned int probeID);

  // Return the path of the Probe/Capture
  const char *getParamPath(unsigned int probeID);

  // Return the bsvtype of the Probe/Capture
  const char *getParamBSVType(unsigned int probeID);

  SerialProbeType getParamType(unsigned int probeID);

  // Return the bsvtype of the Probe/Capture
  const char *getParamSubNets(unsigned int probeID);

};
