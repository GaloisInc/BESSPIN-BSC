// Copyright 2010 Bluespec Inc.  All rights reserved
#pragma once

// Common type defintion -- wrapper around std types.

#include "VeriModule.h"
#include "VeriMisc.h"
#include "VeriId.h"
#include "Types.h"

#include "EditCompile.h"

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif

enum SpecFileMode { UNKNOWN, FABRIC_MODE, IO_CONNECTION_MODE, DIRECT_CONNECTION_MODE,
		    FIXED_TERMINAL_MODE, INSTANCES_MODE };

enum ConnectionType { SINGLE_ENDED, DIFF_PAIR, FIXED };

class ConnBundle;

// IO Connection
class Connection {
  BString m_fromPort;
  BString m_toPort;
  BString m_fromNet;
  BString m_toNet;
  unsigned int m_width;
  ConnBundle *m_owner;
  ConnectionType m_conn_type;

 public:

  Connection (ConnBundle *b, const BString &from, const BString &to,
	      const BString &fromNet, const BString &toNet,
	      int width, ConnectionType t)
    : m_fromPort(from), m_toPort(to), m_fromNet(fromNet), m_toNet(toNet),
    m_width(width), m_owner(b), m_conn_type(t) {};

  ~Connection() {}

  ConnBundle *getConnBundle() { return m_owner; }
  void setConnBundle(ConnBundle* b) { m_owner = b; }

  const char *getFromPort() { return m_fromPort.c_str(); }
  const char *getToPort() { return m_toPort.c_str(); }

  const char *getFromNet() { return m_fromNet.c_str(); }
  const char *getToNet() { return m_toNet.c_str(); }

  void setFromNet(const char *n) { m_fromNet = n; }
  void setToNet(const char *n) { m_toNet = n; }

  unsigned int getWidth() { return m_width; }
  void incrementWidth(unsigned int v) { m_width += v; }

  void setConnType(ConnectionType t) { m_conn_type = t; }
  ConnectionType getConnType() { return m_conn_type; }

};

typedef std::list< Connection > ConnectionList;
typedef std::list< Connection >::iterator ConnectionIterator;

class Fpga;

// Bundle
class ConnBundle {

  Fpga *m_fromFpga;
  Fpga *m_toFpga;
  unsigned int m_width;
  unsigned int m_unused;
  ConnectionList m_connections;
  
  ConnectionIterator m_ConnItr;
  unsigned int m_BitsSoFar;
  unsigned int m_BitsLeft;

  std::map<std::string, int> m_dp_connections;
  std::map<std::string, int> m_se_connections;

 public:
  
  ConnBundle (Fpga *from, Fpga *to)
    : m_fromFpga(from), m_toFpga(to), m_width(0), m_unused(0) {}
  
  ~ConnBundle() {}

  Fpga *getFromFpga() { return m_fromFpga; }
  Fpga *getToFpga() { return m_toFpga; }

  const char *getFromModule();
  const char *getToModule();
  
  void setWidth(unsigned int v) { m_width = v; }
  unsigned int getWidth() { return m_width; }
  void incrementWidth(unsigned int v) { m_width += v; }
  
  void setUnused(unsigned int v) { m_unused = v; }
  unsigned int getUnused() { return m_unused; }
  void decrementUnused(unsigned int v) { m_unused -= v; }

  void addConnection(const BString &fromPort, const BString &toPort,
		     const BString &fromNet, const BString &toNet, unsigned int width,
		     ConnectionType t)
  { //printf(" add connection from port %s toport %s fromnet %s tonet %s\n",
    //	   fromPort.c_str(), toPort.c_str(), fromNet.c_str(), toNet.c_str());
    m_connections.push_back(Connection(this, fromPort, toPort, fromNet, toNet, width, t)); }

  // Iterator for connections
  ConnectionIterator beginConnection()
  { return m_connections.begin(); }
  ConnectionIterator endConnection()
  { return m_connections.end(); }

  int getNextConn(ConnectionIterator &itr, unsigned int &fabricLSB,
		  unsigned int &fabricMSB, unsigned int width);

  int getNextConn(ConnectionIterator &itr, unsigned int &fabricLSB,
		   unsigned int &fabricMSB);

  void initializeConnections();

  void addDpConnMap(const char* signal) { m_dp_connections[signal] = 1; }
  int findDpConn(const char* signal) { return m_dp_connections[signal]; }

  void addSEConnMap(const char* signal) { m_se_connections[signal] = 1; }
  int findSEConn(const char* signal) { return m_se_connections[signal]; }
};

typedef std::map<std::string, int> StringMap;
typedef std::map<std::string, int>::iterator StringMapIterator;

class Fabric {

  Fpga *m_fpga;
  StringMap m_assign_tos;
  BStringList m_single_ended_clocks;
  BStringList m_single_ended_sources;

 public:

  // Constructor
  Fabric(Fpga *fpga) { m_fpga = fpga; }

  // Destructor
  ~Fabric() {}

  void addAssignToSignal(BString &signal) { m_assign_tos[signal] = 0; }
  void setAssignToSignal(BString &signal)  { m_assign_tos[signal] = 1; }
  int getAssignToSignal(BString &signal)  { return m_assign_tos[signal]; }
  void resetAssignToSignal(BString &signal)  { m_assign_tos[signal] = 0; }
  StringMapIterator beginAssignToSignal() { return m_assign_tos.begin(); }
  StringMapIterator endAssignToSignal() { return m_assign_tos.end(); }


  StringMapIterator beginAssignTo()
  { return m_assign_tos.begin(); }
  StringMapIterator endAssignTo()
  { return m_assign_tos.end(); }

  void addSingleEndedClock(BString &signal) { m_single_ended_clocks.push_back(signal); }
  const char *getSingleEndedClock()
  {
    if (m_single_ended_clocks.size())
      return m_single_ended_clocks.front().c_str();
    return NULL;
  }

  BStringListIterator beginSingleEndedClock()
  { return m_single_ended_clocks.begin(); }
  BStringListIterator endSingleEndedClocks()
  { return m_single_ended_clocks.end(); }

  void addSingleEndedSource(BString &signal) { m_single_ended_sources.push_back(signal); }

  BStringListIterator beginSingleEndedSource()
  { return m_single_ended_sources.begin(); }
  BStringListIterator endSingleEndedSource()
  { return m_single_ended_sources.end(); }
};

class PartitionData;
class FpgaBoardSpec;

typedef std::map<std::string, PartitionData* > PartitionDataList;
typedef std::map<std::string, PartitionData* >::iterator PartitionDataIterator;
typedef std::map<std::string, std::string > InstancePartitionMap;
typedef std::map<std::string, std::string >::iterator InstancePartitionMapIterator;
typedef std::map<std::string, ConnBundle* > ConnBundleList;
typedef std::map<std::string, ConnBundle* >::iterator ConnBundleIterator;
typedef std::list<ConnBundle* > ConnBundleLList;
typedef std::list<ConnBundle* >::iterator ConnBundleLIterator;
typedef std::list< Fabric* > FabricList;
typedef std::list< Fabric* >::iterator FabricIterator;

// Fpga - class for representing a fpga on a board
class Fpga {
  
  char m_fpga_letter;    // Letter designattion {A,B,D or E}
  VeriModule *m_module;  // Pointer to verific module of this fpga
  BString m_module_path; // Path of verific module
  VeriInstId *m_io_inst; // Pointer to instance of substrate IO module containing connections
  FpgaBoardSpec *m_board_spec; // Board spec

  ConnBundleList m_in_se_connection_bundles; // List of input single-ended connection bundles
  ConnBundleList m_out_se_connection_bundles;// List of output single-ended connection bundles
  ConnBundleList m_in_dp_connection_bundles; // List of input diffpair connection bundles
  ConnBundleList m_out_dp_connection_bundles;// List of output diffpair connection bundles

  PartitionData *m_pdata; // PartitionData for this fpga

  Fabric *m_fabric;

  ConnBundleIterator m_InCBListItr;
  ConnectionIterator m_InConnItr;
  unsigned int m_InBitsSoFar;
  ConnBundleIterator m_OutCBListItr;
  ConnectionIterator m_OutConnItr;
  unsigned int m_OutBitsSoFar;

  ConnBundleIterator m_InDPCBListItr;
  ConnectionIterator m_InDPConnItr;
  unsigned int m_InDPBitsLeft;
  ConnBundleIterator m_OutDPCBListItr;
  ConnectionIterator m_OutDPConnItr;
  unsigned int m_OutDPBitsLeft;

  unsigned int m_InDPBitsSoFar;
  unsigned int m_OutDPBitsSoFar;


  Fabric *createFabric() { m_fabric = new Fabric(this); return m_fabric; }

  std::map<std::string, int> m_wire_statements;
  std::map<std::string, int> m_dp_statements;
  std::map<std::string, int> m_syncflop_statements;

 public:
  
  // Constructor
  Fpga(FpgaBoardSpec *board_spec, VeriModule *m)
    { m_io_inst = NULL; m_pdata = NULL; m_fabric = NULL; m_board_spec = board_spec;
      setModule(m); createFabric(); }
  
  // Destructor
  ~Fpga() {} ;
  
  // Module accessor
  VeriModule *getModule() { return m_module; }
  void setModule(VeriModule *m);
  void setLetterSuffix();
  

  // Module name accessor
  const char *getModuleName();
  const char *getModulePath() { return m_module_path.c_str(); }

  // Fpga name accessor
  const char getLetterSuffix() { return m_fpga_letter; }

  // Board spec
  FpgaBoardSpec *getFpgaBoardSpec() { return m_board_spec; }

  // IO accessor
  VeriInstId *getIOInstance() { return m_io_inst; }
  void setIOInstance(VeriInstId *i) { m_io_inst = i; }
  VeriModule *getIOModule();

  // Assign PartitionData to this fpga
  void assignPartitionData(PartitionData *pdata)
  { m_pdata = pdata; }
  PartitionData *getPartitionData() { return m_pdata; }

  // ************
  // Single-ended
  // ************

  // Add connection
  void addInSEConn(Fpga *fromFpga, const char *fromPort, const char *toPort,
		   const char *fromNet, const char *toNet, unsigned int width);
  void addOutSEConn(Fpga *toFpga, const char *fromPort, const char *toPort,
		    const char *fromNet, const char *toNet, unsigned int width);

  // Get connection
  //ConnectionIterator &getInSEConn() { return m_InConnItr; }
  //ConnectionIterator &getOutSEConn() { return m_OutConnItr; }
  //ConnBundleIterator &getInCBListItr() { return m_InCBListItr; }
  //ConnBundleIterator &getOutCBListItr() { return m_OutCBListItr; }
  //bool endInSEConn() { return (m_InConnItr == (m_InCBListItr->second)->endConnection()); }
  //bool endOutSEConn() { return (m_OutConnItr == (m_OutCBListItr->second)->endConnection()); }
  //unsigned getNumberOfUnusedSEConn(Fpga *toFpga);
  //unsigned getNumberOfUnusedSEConn(Fpga *toFpga);
  //void decrementUnusedSEConn(unsigned int n) { (m_InCBListItr->second)->decrementUnused(n); }
  //void decrementUnusedSEConn(unsigned int n) { (m_OutCBListItr->second)->decrementUnused(n); }
  //unsigned int &getInSEBitsSoFar() { return m_InBitsSoFar; }
  //unsigned int &getOutSEBitsSoFar() { return m_OutBitsSoFar; }

  // Find next single-ended connection
  void resetSEConn();

  // Iterator for single-ended connectin bundle
  ConnBundleIterator beginInSEConnBundle()
  { return m_in_se_connection_bundles.begin(); }
  ConnBundleIterator endInSEConnBundle()
  { return m_in_se_connection_bundles.end(); }
  ConnBundleIterator beginOutSEConnBundle()
  { return m_out_se_connection_bundles.begin(); }
  ConnBundleIterator endOutSEConnBundle()
  { return m_out_se_connection_bundles.end(); }


  // *********
  // Diff Pair
  // *********

  // Add connection
  void addInDPConn(Fpga *fromFpga, const char *fromPort, const char *toPort,
		   const char *fromNet, const char *toNet, unsigned int width);
  void addOutDPConn(Fpga *toFpga, const char *fromPort, const char *toPort,
		    const char *fromNet, const char *toNet, unsigned int width);

  // Get connection
  //ConnectionIterator &getInDPConn() { return m_InDPConnItr; }
  //ConnectionIterator &getOutDPConn() { return m_OutDPConnItr; }
  //ConnBundleIterator &getInDPCBListItr() { return m_InDPCBListItr; }
  //ConnBundleIterator &getOutDPCBListItr() { return m_OutDPCBListItr; }
  //bool endInDPConn() { return (m_InDPConnItr == (m_InDPCBListItr->second)->endConnection()); }
  //bool endOutDPConn() { return (m_OutDPConnItr == (m_OutDPCBListItr->second)->endConnection()); }
  //unsigned getNumberOfUnusedDPConn(Fpga *toFpga);
  //void decrementUnusedDPConn(unsigned int n) { (m_InDPCBListItr->second)->decrementUnused(n); }
  //unsigned int &getInDPBitsSoFar() { return m_InDPBitsSoFar; }
  //unsigned int &getOutDPBitsSoFar() { return m_OutDPBitsSoFar; }
  //unsigned int getInDPBitsLeft() { return m_InDPBitsLeft; }
  //unsigned int getOutDPBitsLeft() { return m_OutDPBitsLeft; }

  // Find next single-ended connection
  void resetDPConn();

  // Iterator for connectin bundle (diffpair)
  ConnBundleIterator beginInDPConnBundle()
  { return m_in_dp_connection_bundles.begin(); }
  ConnBundleIterator endInDPConnBundle()
  { return m_in_dp_connection_bundles.end(); }
  ConnBundleIterator beginOutDPConnBundle()
  { return m_out_dp_connection_bundles.begin(); }
  ConnBundleIterator endOutDPConnBundle()
  { return m_out_dp_connection_bundles.end(); }

  // Create a connection in a bundle
  void createConnection(ConnBundle *c, BString &fromPort, BString &toPort);


  // Fabrics
  Fabric *getFabric() { return m_fabric; }

  // Init the IO ports
  void initializeCrossFpgaConnections();

  ConnBundle *findDPConnBundleToFpga(Fpga *toFpga)
  { return m_out_dp_connection_bundles[toFpga->getModuleName()]; }

  ConnBundle *findSEConnBundleToFpga(Fpga *toFpga)
  { return m_out_se_connection_bundles[toFpga->getModuleName()]; }

  // Get next DP connection
  int getNextInDPConn(Fpga *fromFpga, ConnectionIterator &itr,
		      unsigned int &fabricLSB, unsigned int &fabricMSB,
		       unsigned int width);
  int getNextOutDPConn(Fpga *toFpga,
		       ConnectionIterator &itr, unsigned int &fabricLSB, unsigned int &fabricMSB,
		       unsigned int width);

  // Get next SE connection
  int getNextInSEConn(Fpga *fromFpga, ConnectionIterator &itr,
		      unsigned int &fabricLSB, unsigned int &fabricMSB,
		       unsigned int width);
  int getNextOutSEConn(Fpga *toFpga,
		       ConnectionIterator &itr, unsigned int &fabricLSB, unsigned int &fabricMSB,
		       unsigned int width);

  void addWireStatement(const char* signal) { m_wire_statements[signal] = 1; }
  int findWireStatement(const char* signal) { return m_wire_statements[signal]; }

  void addDPStatement(const char* signal) { m_dp_statements[signal] = 1; }
  int findDPStatement(const char* signal) { return m_dp_statements[signal]; }

  void addSyncFlopStatement(const char* signal) { m_syncflop_statements[signal] = 1; }
  int findSyncFlopStatement(const char* signal) { return m_syncflop_statements[signal]; }

};

typedef std::map<std::string, Fpga* > FpgaList;
typedef std::map<std::string, Fpga* >::iterator FpgaIterator;


// Board - class representing the fpga board

class FpgaBoardSpec {

  BString m_filename; // Name of board spec file
  FpgaList m_fpgas;   // List of Fpga class
  
  int m_stat;
  int m_lineno;

 private:

  FpgaBoardSpec() {}

  void parseConnection(std::ifstream &file, char *lineBuf);
  void parseFabric(std::ifstream &file, char *lineBuf);

 public:

  // Constructor (parsing of spec file occurs here)
  FpgaBoardSpec(const char *specfile);

  // Destructor
  ~FpgaBoardSpec() {}

  // Name of spec file
  const char *getSpecFile() { return m_filename.c_str(); }

  // 0 - if the something wrong during parsing
  int valid() { return m_stat; }

  // Get Fpga by name
  Fpga *findFpga(const char *name) { return m_fpgas[name]; }
  Fpga *findFpgaByLetterSuffix(const char l);
  
  // Create Fpga
  Fpga *createFpga(BString &fpga_module_name);

  int  setIOInstance(Fpga *fpga, BString &io_instname);
  
  FpgaIterator beginFpga() { return m_fpgas.begin(); }
  FpgaIterator endFpga() { return m_fpgas.end(); }
  int numFpgas() { return m_fpgas.size(); }


  // Initialize fpgas
  void initializeFpgas();

};

typedef std::map<VeriInstId*, int> VeriInstIdMap;

// This class contains the specification from partition spec file and part.data file.
// It has a list of PartitionData which has the list of design instances in the partition.

class PartitionSpec {

  friend class PartitionData;

  BString m_module_specfile;
  FpgaBoardSpec *m_board_spec;         // Board spec
  PartitionDataList m_partitiondatas;  // List of PartitionData
  PartitionDataList m_modulepart_map;  // Map of module name to PartitionData
  PartitionData *m_default_partdata;
  InstancePartitionMap m_instpart_map; // Map of instance name to PartitionData
  BString m_containing_module_path;         
  VeriInstIdMap m_instid_map;
  
  int m_stat;
  int m_lineno;
  
 public:
  int parsePartitionSpecFile(const char *specfile);  // Parse and create PartitionData data structure
  int parseModuleSpecFile(const char *specfile);  // Parse and fill PartitionData data structure

  int parseFixedTerminals(std::ifstream &file, char *lineBuf);

  int parsePartition(std::ifstream &file, char *lineBuf);

 public:

  PartitionSpec();
  ~PartitionSpec();

  // Get board spec
  void setBoardSpec(FpgaBoardSpec *bs) { m_board_spec = bs; }
  FpgaBoardSpec *getBoardSpec() { return m_board_spec; }

  PartitionData *getDefaultPartData() { return m_default_partdata; }

  // For each instance in the partition spec file, add it to the PartitionData
  int insertSpecInstance(const BString &partname, const BString &instname);

  // Given instance name (full path), find the PartitionData it belongs to
  const char *getContainingPartition(const BString &inst)
  { return m_instpart_map[inst].c_str(); }

  // Given the partition name (ie A,B,D or E), fine the representing PartitionData class
  PartitionData *findPartitionDataByFpgaName(const BString &partname)
  { return m_partitiondatas[partname]; }

  // Given the module name (short), find the PartitionData
  PartitionData *findPartitionDataByModuleName(const BString &name)
  { return m_modulepart_map[name]; }

  // Iterator for all PartitionDatas
  PartitionData *findOrCreatePartitionData(const BString &name);
  PartitionDataIterator beginPartitionData() { return m_partitiondatas.begin(); }
  PartitionDataIterator endPartitionData() { return m_partitiondatas.end(); }
  unsigned int numPartitionDatas() { return m_partitiondatas.size(); }

  // Find the cut set based on the instance and add it to the inputs and outputs of the PartitionDat
  int collectAndPopulatePortsData(PartitionData *cm);
  
  void addPartitionedInst(VeriInstId *inst) { m_instid_map[inst] = 1; }
  int isPartitionedInst(VeriInstId *inst) { return m_instid_map[inst]; }
};


typedef std::map<std::string, std::string> StringToStringMap;
typedef std::map<std::string, std::string>::iterator AssignmentIterator;

// A class for storing the list of module instances
// of a partition

class PartitionData {

  VeriModule *m_containing_module; // Module containing the partitioned instance
  BString m_containing_module_path; // Module containing the partitioned instance
  BString m_containing_module_prefix; // Module containing the partitioned instance
  VeriInstId *m_current_instance;  // Current instance being partitioned
  BString m_hier_inst_name;        // Full path name of the current instance
  BString m_hier_path_name;        // Full path name of the containing module
  PartitionSpec *m_spec;           // Pointer to PartitionSpec container
  Fpga *m_fpga;                    // Pointer to the fpga that this data is assigned to
  int m_default;  // A flag designate that this is the main default partition or not.
  BStringList m_instances; // List of instances in this partition
  ModuleTerminalList m_inputs;  // List of generated inputs port for the cut set from this part
  ModuleTerminalList m_outputs; // List of generated outputs port for the cut set into this part
  ModuleTerminalList m_inouts;  // List of generated inouts port for the cut set into this part
  std::map<std::string, FixedTerminalTable> m_module_table; // List of port with preassigned nets
  BString m_sync_clock; // The name of sync clock, the scemi controlled clock distributed this part
  SingleEndedTerminals  m_single_ended_terminals; // List of ports in the cut set that are 
                                                  // intended for singled-ended connection
  SingleEndedTerminals  m_single_ended_data_terminals; // List of ports in the cut set that are 
                                                       // intended for singled-ended-data connection
  StringToStringMap m_assign_tos;
  ModuleTerminalList m_assigned_se_terminals;
  StringToStringMap m_assigned_se_terminals_netname;

  StringMap m_net_map;

 public:

  // Constructor
  PartitionData(const BString &name, PartitionSpec *spec);

  // Destructor
  ~PartitionData() {}

  // Set module name, info in module spec file is mapped to this PartitionData through this name
  const char *getModuleName();

  // Fpga name {A,B,D or E}
  const char getFpgaLetterSuffix() 
  { if (m_fpga) return m_fpga->getLetterSuffix();
    else return ' '; } 

  Fpga *getFpga() { return m_fpga; }
  void setFpga(Fpga *fpga) { m_fpga=fpga; }

  // The containing PartitionSpec
  PartitionSpec *getPartitionSpec() { return m_spec; }

  const char *getPartitionedInstName();

  // The name of containing Partition {A,B,D or E} given the instance name
  const char *getContainingPartition(const BString &inst)
  { return m_spec->getContainingPartition(inst); }

  // Insert the instance into the PartitionData
  void insertInstance(const BString &inst)
  { m_instances.push_back(inst); }

  // Iterator for the instances in this partition
  BStringListIterator beginInstance() { return m_instances.begin(); }
  BStringListIterator endInstance() { return m_instances.end(); }

  // Iterator for all the input and output cut set (ModuleTerminals)
  ModuleTerminalList & getInputList() { return m_inputs;}
  ModuleTerminalList & getOutputList() { return m_outputs;}
  ModuleTerminalList & getInoutList() { return m_inouts;}

  // Clear the collected cut set
  void resetCollectedData () {
    m_inputs.clear();
    m_outputs.clear();
    m_inouts.clear();
  }

  // Add a cut into the cut set
  void addInput( const BString &portName, const BString &netName, int width)
  {
    m_inputs.push_back ( ModuleTerminal (portName, netName, d_input, width) );
  }
  void addOutput( const BString &portName, const BString &netName, int width)
  {
    m_outputs.push_back ( ModuleTerminal (portName, netName, d_output, width) );
  }
  void addInout( const BString &portName, const BString &netName, int width)
  {
    m_inouts.push_back ( ModuleTerminal (portName, netName, d_inout, width) );
  }

  // Fixed a cut to a net name
  void addFixedTerminal(const BString &modname, const BString &portName, const BString &val)
  {
    (m_module_table[modname])[portName] = val;
  }
  void getFixedTerminalValue(BString &modname, BString &portName, BString &retName);
  void getFixedTerminalValue(const char *modname, const char *portName, BString &retName);

  // Sync clock
  void setSyncClock(const BString & sigName) { m_sync_clock = sigName; }
  const char *getSyncClock() { return m_sync_clock.c_str(); }

  // Add single-ended designation of a signal
  void addSingleEndedTerminal(const BString &portName, const BString &val)
  {
    m_single_ended_terminals[portName] = val;
  }
  const char *getSingleEndedTerminalValue(BString &portName)
  { return (m_single_ended_terminals[portName].c_str()); }

  // Add single-ended-data designation of a signal 
  void addSingleEndedDataTerminal(const BString &portName, const BString &val)
  {
    m_single_ended_data_terminals[portName] = val;
  }
  const char *getSingleEndedDataTerminalValue(BString &portName)
  { return (m_single_ended_data_terminals[portName].c_str()); }
  const char *getSingleEndedDataTerminalValue(const char *portName)
  { return (m_single_ended_data_terminals[portName].c_str()); }

  // Default partition (usually fpga_a)
  void setDefault(int d) { m_default = d; }
  int isDefault() { return m_default; }

  void setHierInstanceName(const BString &name) { m_hier_inst_name = name; }
  const char *getHierInstanceName() { return m_hier_inst_name.c_str(); }

  void setHierPathName(const BString &name) { m_hier_path_name = name; }
  const char *getHierPathName() { return m_hier_path_name.c_str(); }

  void setContainingVerificModule(PartitionData *pdata);
  //void setCurrentVerificInstance(const char *inst_fullname);

  VeriModule *getContainingModule() { return m_containing_module; }
  void setContainingModule(VeriModule *mod) { m_containing_module = mod; }
  const char *getContainingModulePath() { return m_containing_module_path.c_str(); }
  void setContainingModulePath(const char *p) { m_containing_module_path = p; }
  VeriInstId *getCurrentVerificInstance() { return m_current_instance; }
  const char *getContainingModulePrefix();

  int setVerific();
  int setDefaultVerific(BString &path);

  void addAssign(const char *key, const char *signal) { m_assign_tos[key] = signal; }
  const char *getAssign(const char *key)  { return m_assign_tos[key].c_str(); }

  AssignmentIterator beginAssign() { return m_assign_tos.begin(); }
  AssignmentIterator endAssign() { return m_assign_tos.end(); }

  ModuleTerminal *findTerminal(const char *name);

  void addAssignedSETerminal(const BString &portName, const BString &netName, int width);

  ModuleTerminalList & getAssignedSETerminals() { return m_assigned_se_terminals; }
  const char *getAssignedSETerminalNetname(const char *portname)
  { return m_assigned_se_terminals_netname[portname].c_str(); }

  void addMergingNet(const char *netname);
  void addMergingNet(BString &netname) { addMergingNet(netname.c_str()); }
  int getMergingNetCount(const char *netname) { return m_net_map[netname]; }
  int getMergingNetCount(BString &netname) { return getMergingNetCount(netname.c_str()); }
  void clearMergingNetCount() { m_net_map.clear(); }
};

