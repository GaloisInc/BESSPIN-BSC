// Copyright 2010 Bluespec Inc.  All rights reserved
#pragma once

// Common type defintion -- wrapper around std types.

#include <stdio.h>
#include "EditCompilePartitionData.h"

class VeriNodeInfo;
class VeriNodeInfoTable;

// EditCompilePP
class EditCompilePP : public EditCompileBase {

  static int m_net_gen_number;

  StringMap m_net_map;

public:
  // Constructor
  EditCompilePP (const BString &iname)
    : EditCompileBase(iname)
  {}
  virtual int process(ModInstSetPairIter_t &range)
  {
    int status = 0;
    for (ModInstSetIter_t iter = range.first; iter != range.second && status == 0 ; ++ iter ) {
      status = (*iter)->accept (this);
    }
    return status;
  };
  int novisitor(class CktMod *cm) { return 0;}

  int visit(class Partition *cm);

  // Take the collected cut set and sort them into the bin (single-ended, diffpairs, non)
  void collectCrossPartitionSignals(PartitionSpec *spec, ConnBundleLList &list);
  
  // Find connected terminal
  ModuleTerminal *findConnectedTerminal(ModuleTerminal *source, PartitionData *from,
					PartitionData *to, VeriNodeInfoTable *table);

  // Add a module to the fpga and assign the cut set to the substrate
  //bool insertModule(Fpga *fpga, PartitionData *p1, PartitionData *p2, VeriModule *module,
  //BString &mod_instname);
  
  // Add a design to the default fpga
  int placeDefaultFpga(Fpga *defaultFpga, BString &instname,
		       ConnBundleLList &connBundles);

  int placeFpga(Fpga *defaultFpga, BString &instname,
		ConnBundleLList &connBundles);

  int routeConnections(PartitionSpec *spec, ConnBundleLList &connBundles);

  void collectPartitionTerminals(PartitionData *partdata,
				 ConnBundleLList &connBundles,
				 ModuleTerminalList &terminals);
  int collectCrossPartitionSignals(PartitionData *pdata1,
				   PartitionData *pdata2, ConnBundleLList &connBundles);
  void collectCrossPartitionSignalsFromDefault(PartitionData *pdata1,
					       PartitionData *pdata2, ConnBundleLList &connBundles);
  void collectCrossPartitionSignalsToDefault(PartitionData *pdata1,
					     PartitionData *pdata2, ConnBundleLList &connBundles);

  // Add an instance to the fpga and assign the cut set to the substrate
  //bool insertInstance(Fpga *fpga, PartitionData *pdef);
  

  // Connect the Partition signals to the IO (diffpars)
  //bool connectToIO(Fpga *fpga, BString &prefix, PartitionData *pdef, VeriModule *module,
  //ModuleTerminalList &inputs, ModuleTerminalList &outputs,
  //		   bool trySingleEnded=true);
  
  // Connect
  bool connectToDiffPair(Connection &conn,
			 BString &prefix,
			 bool trySingleEnded);
    
  // Assign the partition signals to singled ended connections
  //bool connectToSingleEndedPorts(Fpga *fpga, BString &prefix,
  //				 PartitionData *pdef, VeriModule *module,
  //				 ModuleTerminalList &fromList,
  //				 ModuleTerminalList &outputList,
  //				 bool tryIO=true);

  bool connectToSingleEnded(Connection &conn,
			    BString &prefix,
			    bool tryDiffPair);

  // Assign the partition signals to singled ended connections
  //bool connectInPlaceToSingleEndedPorts(Fpga *fpga, BString &prefix,
  //					PartitionData *pdef, VeriModule *module,
  //					ModuleTerminalList &fromList,
  //					bool tryIO=true);

  // Assign unsed IO (single-ended) to '0
  bool connectUnusedDPWires(Fpga *fpga);
  bool connectUnusedSEWires(Fpga *fpga);

  // Assign unsed IO (single-ended) to '0
  bool connectUnusedWires(Fpga *fpga, Partition *cm, 
			  ModuleTerminalIterator &dutItr,
			  int &dutBitSoFar, ModuleTerminalList &outputs);
  
  // Create the ModuleTerminalList from the module inputs and outputs
  void createModuleTerminalList(VeriModule* module, PartitionData *pdef,
				ModuleTerminalList &mtlist);

  // Write report
  bool writeReport(PartitionSpec *pspec, FpgaBoardSpec *board, ConnBundleLList &connBundles,
		   FILE *file = NULL);
		   
    
  bool checkAssignToSignals(Fpga *fpga);

  // Misc helper methods

  void getTopPortPrefix(PartitionData *pdef, BString &prefix);


  //static void getNextInSingleEndedPort(Fpga *fpga, unsigned int width,
  //				       BString &netname);

  //static unsigned int getNextInSingleEndedPort(Fpga *fpga, BString &netname);
  //
  //static void getNextOutSingleEndedPort(Fpga *fpga, unsigned int width,
  //					BString &netname);
  //static unsigned int getNextOutSingleEndedPort(Fpga *fpga, BString &netname);

  static void generateInstanceName(BString &path, BString &generatedName, int secondlevel=1);

  //-------------------------------------
  // Verific verilog modification methods
  //-------------------------------------

  // AddInstance of sync flop
  void verificAddSync2Flop(Fpga *fpga, PartitionData *pdef, BString &instname,
			   BString &clkName, BString &sig, BString &io);

  // AddInstance of sync flop
  void verificAddSync2NFlop(Fpga *fpga, PartitionData *pdef, BString &instname,
			    BString &clkName, BString &sig, BString &io);

  // AddPort
  void verificAddPorts(BString &modpath, ModuleTerminalList &mtlist);

  // AddNet
  void verificAddWires(BString &modpath, ModuleTerminalList &mtlist);
  
  // AddNet
  void verificAddFromWire(Fpga *fpga, Connection &conn, BString &prefix);
  void verificAddToWire(Fpga *fpga, Connection &conn, BString &prefix);
  
  // Draw{In,Out}Signal
  void verificDrawUpPortsOfPartitionedInstance(PartitionData *pdata, VeriModule *instmaster);
  void verificDrawUpPortsOfPartitionedInst(PartitionData *pdata, VeriModule *instmaster);
  void verificDrawUpOutputConns(PartitionData *pdata, ConnBundle *connBundle,
				VeriModule *instmaster);
  void verificDrawUpInputConns(PartitionData *pdata, ConnBundle *connBundle,
			       VeriModule *instmaster);
  
  // AddSimpleAssign
  bool verificAddAssignment(Fpga *fpga,
			    ModuleTerminalIterator &dutItr,
			    unsigned int &dutBitSoFar,
			    ConnectionIterator &ioItr,
			    unsigned int &ioBitSoFar);
  bool verificAddDPAssignment(Connection &dut_conn);
  bool verificAddSEAssignment(Connection &dut_conn);

  // Change fragment file 
  void changeFragmentFile(BString &instname, BString &modulename);

  // Helper for verificAddAssignment
  void makeAssignText(ModuleTerminalIterator &dutItr,
		      ConnectionIterator &ioItr, unsigned int delta,
		      BString &dut_msb, BString &dut_lsb,
		      BString &io_msb, BString &io_lsb,
		      BString &assign_lhs, BString &assign_rhs);

  void makeFromAssignText(Connection &dut_conn, Connection &fabric_conn,
			  unsigned int delta,
			  BString &dutLSB, BString &dutMSB,
			  BString &fabricLSB, BString &fabricMSB,
			  BString &assign_lhs, BString &assign_rhs, BString &netprefix,
			  BString &portprefix);
  void makeToAssignText(Connection &dut_conn, Connection &fabric_conn,
			unsigned int delta,
			BString &dutLSB, BString &dutMSB,
			BString &fabricLSB, BString &fabricMSB,
			BString &assign_lhs, BString &assign_rhs, BString &prefix);

  void generateNetName(const char *oldname, BString &newname);



  int isConnectedToDefaultPartition(VeriNodeInfo *node, PartitionSpec *spec);
  int isConnectedToDefaultPartitionFromNode(VeriNodeInfo *node, PartitionSpec *spec);
  int isConnectedFromDefaultPartition(VeriNodeInfo *node, PartitionSpec *spec);

  void addMergingNet(const char *netname);
  void addMergingNet(BString &netname) { addMergingNet(netname.c_str()); }
  int getMergingNetCount(const char *netname) { return m_net_map[netname]; }
  int getMergingNetCount(BString &netname) { return getMergingNetCount(netname.c_str()); }
  void clearMergingNetCount() { m_net_map.clear(); }
};
