#include <iostream>
#include <cstring>

#include "EditCompile.h"
#include "EditCompilePartition.h"
#include <iostream>
#include "HdlUtils.h"
#include "VeriNodeInfo.h"
#include "tcl.h"

#define MAX_LINE_SIZE 1023

int EditCompilePP::m_net_gen_number = 1;

// partitionPass
int EditCompile::partitionPass()
{
  ModInstSetIter_t iter;
  int status = 0;

  for (iter = m_instSet.begin(); iter != m_instSet.end(); iter = m_instSet.upper_bound(*iter) ) {
    BString iname = (*iter)->getInstName();
    ModInstSetPairIter_t one_edit = std::pair<ModInstSetIter_t, ModInstSetIter_t> (iter, m_instSet.upper_bound(*iter));
    EditCompilePP ppvisitor(iname);
    status = ppvisitor.process (one_edit);
    if (status != 0) break;
    m_instSet.insert (ppvisitor.newFirst(), ppvisitor.newEnd() );
  }

  return status;
}

void getTopPath(BString& path, BString &returnpath)
{
  size_t slash;
  
  if (path[0] == '/')
    slash = path.find_first_of('/', 1);
  else
    slash = path.find_first_of('/');

  if (slash != string::npos)
    returnpath = path.substr(0, slash);
  else
    returnpath = path;
}

// Change fragment file
void EditCompilePP::changeFragmentFile(BString &instname, BString &modulename)
{
  // Create new fragment file for simulation if needed
  BString topname = "top.";
  BString newtopname = topname + "fpgaA." + instname + ".";
  BString input_path;
  HdlUtils::findFragmentFilePath(input_path);
  //printf("changeFragmentFile %s %s %s %s\n", modulename.c_str(), input_path.c_str(),
  //topname.c_str(), newtopname.c_str());
  if (CktEdits::s_trace) {
    printf("changeFragmentFile %s %s %s %s\n", modulename.c_str(), input_path.c_str(),
           topname.c_str(), newtopname.c_str());
  }
  if (input_path != "")
    addNewItem(new ChangeFragmentFile(modulename,
				      input_path, topname, newtopname));
  else
    cerr << "Warning EditCompilePP::changeFragmentFile: cannot find fragment file 'scemilink.vlog_fragment' from the given --ydir search paths." << endl;
}



// Top level partitioning method.
// This is called when 'Apply' is active during hdledit process.
int EditCompilePP::visit(class Partition *cm)
{
  BString foundpath;
  
  //printf("Visit\n");
  // Read in the board spec file
  FpgaBoardSpec *board = new FpgaBoardSpec(cm->getBoardSpecFile());
  if (!board->valid())
    return 1;

  //printf("Visit2\n");
  cm->getPartitionSpec()->setBoardSpec(board);
  if (!cm->getPartitionSpec()->parseModuleSpecFile(cm->getModuleSpecFile()))
    return 1;

  //printf("Visit3\n");
  // Initialize the fpgas and the port iterators
  board->initializeFpgas();

  // Find the module and the instance to be partitioned
  PartitionData *pdata;
  PartitionDataIterator pdataItr;
  Fpga *defaultFpga, *partFpga;

  // Collect port connections -- list of inputs and outputs and widths
  ConnBundleLList connBundles;
  //printf("Collect\n");
  collectCrossPartitionSignals(cm->getPartitionSpec(), connBundles);

  writeReport(cm->getPartitionSpec(), board, connBundles);

  // Now add specified assign statements from specfile  //printf("Route\n");
  //printf("Route\n");
  if (routeConnections(cm->getPartitionSpec(), connBundles) == 1)
    return 1;

  //printf("Done Route\n");
  // Generate top module instance name
  BString topmod_instname = "top_";

  // Iterate through each partition specified by PartitionData
  for (pdataItr = cm->getPartitionSpec()->beginPartitionData();
       pdataItr != cm->getPartitionSpec()->endPartitionData();
       pdataItr++) {

    pdata = pdataItr->second;
    Fpga *fpga = board->findFpgaByLetterSuffix(pdata->getFpgaLetterSuffix());

    // Now add specified assign statements from specfile
    AssignmentIterator aItr;
    string key, value;
    
    for(aItr = pdata->beginAssign();
	aItr != pdata->endAssign();
	aItr++) {
      
      key = aItr->first;
      value = aItr->second;
      addNewItem(new AddSimpleAssign(fpga->getModulePath(), key, value));
      if (fpga->getFabric())
	fpga->getFabric()->setAssignToSignal(key);
    }

    // Check and make sure all the required assign to are done
    if (checkAssignToSignals(fpga) == false)  return 1;

    // Default fpga (the main fpga that has original whole design)
    if (pdata->isDefault()) {
      BString temp = pdata->getHierPathName();
      size_t slash;
      if (temp[0] == '/')
	slash = temp.find_first_of('/', 1);
      else
	slash = temp.find_first_of('/');
      BString instname;
      if (slash != string::npos)
	instname = temp.substr(1, slash-1);
      else
	instname = temp.substr(2);
      defaultFpga = board->findFpgaByLetterSuffix(pdata->getFpgaLetterSuffix());
      topmod_instname += instname;
      topmod_instname += "_EDITED";
      //printf("placeDefaultFpga\n");
      placeDefaultFpga(defaultFpga, topmod_instname, connBundles);
      //printf("Done place default\n");
      connectUnusedSEWires(defaultFpga);
      temp = pdata->getContainingModulePath();
      changeFragmentFile(topmod_instname, temp);  
    }
    else {
      
      partFpga = board->findFpgaByLetterSuffix(pdata->getFpgaLetterSuffix());
      if (partFpga == NULL) {
	cerr << "Error EditCompilePP::visit(): fpga "
	     << pdata->getFpgaLetterSuffix() 
	     << "is not found." << endl;
	return 1;
      }
      topmod_instname = partFpga->getPartitionData()->getPartitionedInstName();
      //printf("placeFpga\n");
      placeFpga(partFpga, topmod_instname, connBundles);
      //printf("done placeFpga\n");
      connectUnusedSEWires(partFpga);
      //printf("Done connect unused se\n");
    }
  }
  //printf("done visit\n");
  
  return 0;
}

void EditCompilePP::generateNetName(const char *oldname, BString &newname)
{
  stringstream strm;

  strm << EditCompilePP::m_net_gen_number;
  EditCompilePP::m_net_gen_number++;
  newname = BString(oldname) + "_generated_" + strm.str();
}

int EditCompilePP::routeConnections(PartitionSpec *spec, ConnBundleLList &connBundles)
{
  ConnBundleLIterator connBundleItr;
  ConnBundle *connBundle;
  ConnectionIterator connItr;
  Fpga *fromFpga, *toFpga;
  BString prefix;

  for (connBundleItr = connBundles.begin();
       connBundleItr != connBundles.end();
       connBundleItr++) {

    connBundle = *connBundleItr;
    fromFpga = connBundle->getFromFpga();
    toFpga = connBundle->getToFpga();

    printf("\nRouteConnections from %s to %s\n", fromFpga->getModuleName(), toFpga->getModuleName());
    if (fromFpga->getPartitionData()->isDefault())
      prefix = toFpga->getPartitionData()->getContainingModulePrefix();
    else
      prefix = fromFpga->getPartitionData()->getContainingModulePrefix();

    printf("PREFIX %s\n", prefix.c_str());
    
    for (connItr = connBundle->beginConnection();
	 connItr != connBundle->endConnection();
	 connItr++) {
      
      // Diff-pair
      // - addwire
      // - assign to the fabric connection
      if (connItr->getConnType() == DIFF_PAIR) {
	
	// Add wire on both fpgas
	verificAddFromWire(fromFpga, *connItr, prefix);
	verificAddToWire(toFpga, *connItr, prefix);
	
	printf("ConnectToDiffPair\n");
	if (connectToDiffPair(*connItr, prefix, true) == false)
	  return 1;
      }
      else if (connItr->getConnType() == SINGLE_ENDED) {
	
	printf("ConnectToSE\n");
	if (connectToSingleEnded(*connItr, prefix, true) == false)
	  return 1;
      }
    }
  }
  
  FpgaIterator fpgaItr;
  for (fpgaItr = spec->getBoardSpec()->beginFpga();
       fpgaItr != spec->getBoardSpec()->endFpga();
       fpgaItr++) {
    
    //printf("Connectunused DP\n");
    connectUnusedDPWires(fpgaItr->second);
  }

  //printf("Done RouteConnections \n");
  return 0;
}


int EditCompilePP::placeDefaultFpga(Fpga *fpga, BString &topmod_instname,
				    ConnBundleLList &connBundles)
{
  PartitionData *partdata = fpga->getPartitionData();
  BString fullname = partdata->getHierInstanceName();
  size_t stloc = fullname.find_last_of('/');
  BString path = fullname.substr(0, stloc);

  ////////////////////////////////////////////////////////////////
  // Within the parent module:
  //  1. Remove the partitioned instance
  //  2. Draw in and out ports that are connected to the instance

  PartitionDataIterator pdataItr;
  PartitionData *pdata;
  PartitionSpec *spec = partdata->getPartitionSpec();

  ModuleTerminalList in_diffpair_terminals, out_diffpair_terminals,
    new_module_terminals, single_ended_terminals;

  // Terminal list of the original module
  createModuleTerminalList(partdata->getContainingModule(), partdata, new_module_terminals);
  //createModuleTerminalList(connBundles, partdata, new_module_terminals);

  for (pdataItr = spec->beginPartitionData();
       pdataItr != spec->endPartitionData();
       pdataItr++) {

    pdata = pdataItr->second;
    if (pdata->isDefault()) continue;

    //printf("RmInstance %s %s\n", pdata->getHierPathName(), pdata->getPartitionedInstName());
    addNewItem(new RmInstance (pdata->getHierPathName(), pdata->getPartitionedInstName()));
    verificDrawUpPortsOfPartitionedInstance(pdata, pdata->getCurrentVerificInstance()->
					    GetInstantiatedModule());
  }

  BString fpga_path = "/";
  VeriModule *module = fpga->getModule();
  fpga_path += module->Name();

  BString master_name = partdata->getContainingModule()->Name();
  master_name += "_EDITED";
  ParameterList plist;

  HdlUtils::createModuleParameterList(partdata->getContainingModule(), plist);

  //collectPartitionTerminals(partdata, connBundles, new_module_terminals);

  ModuleTerminalList &se_list = fpga->getPartitionData()->getAssignedSETerminals();
  new_module_terminals.insert(new_module_terminals.end(), se_list.begin(), se_list.end());

  cout << "add instance " << fpga_path << " " << topmod_instname << endl;
  AddInstance *ai = new AddInstance (fpga_path, topmod_instname,
				     master_name, plist, new_module_terminals);
  ai->setOnePortPerLine(true);
  addNewItem(ai);

  return 0;
}

int EditCompilePP::placeFpga(Fpga *fpga, BString &topmod_instname,
			     ConnBundleLList &connBundles)
{
  PartitionData *partdata = fpga->getPartitionData();
  BString fullname = partdata->getHierInstanceName();
  size_t stloc = fullname.find_last_of('/');
  BString path = fullname.substr(0, stloc);

  ////////////////////////////////////////////////////////////////
  // Within the parent module:
  //  1. Remove the partitioned instance
  //  2. Draw in and out ports that are connected to the instance

  ModuleTerminalIterator tItr;
  ModuleTerminalList &inputs = partdata->getInputList();
  ModuleTerminalList &outputs = partdata->getOutputList();
  ModuleTerminalList &inouts = partdata->getInoutList();
  BString netname, portname;

  ModuleTerminalList in_diffpair_terminals, out_diffpair_terminals,
    new_module_terminals, single_ended_terminals;

  BString fpga_path = "/";
  VeriModule *module = fpga->getModule();
  fpga_path += module->Name();

  BString foundpath;
  BString instance_path = partdata->getHierInstanceName();
  VeriModule *master = HdlUtils::findModuleFromPath(NULL, instance_path, foundpath);

  BString master_name;
  if (master->GetOriginalModuleName() != NULL)
    master_name = master->GetOriginalModuleName();
  else
    master_name = master->Name();

  ParameterList plist;
  BString prefix;

  //printf("place Fpga %s\n", fpga->getModuleName());

  prefix = fpga->getPartitionData()->getContainingModulePrefix();
  //getTopPortPrefix(partdata, prefix);

  for (tItr = inputs.begin(); tItr != inputs.end(); tItr++) {
    partdata->getFixedTerminalValue(master_name, tItr->m_portName, netname);
    if (netname == "")
      netname = partdata->getSingleEndedTerminalValue(tItr->m_portName);
    if (netname == "PLACE_HOLDER") {
      netname = partdata->getAssignedSETerminalNetname(tItr->m_portName.c_str());
      //printf("SE PLACEHOLDER input port %s %s\n", tItr->m_portName.c_str(), netname.c_str());
    }
    if ((netname == "") && (tItr->m_netName != "")) {
      netname = prefix;
      netname += "_" + tItr->m_netName;
      //in_diffpair_terminals.push_back(ModuleTerminal(tItr->m_portName, netname,
      //					     tItr->m_dir, tItr->m_width));
    }
    
    //printf("SE input port %s %s\n", tItr->m_portName.c_str(), netname.c_str());
    single_ended_terminals.push_back(ModuleTerminal(tItr->m_portName, netname,
						    tItr->m_dir, tItr->m_width));
  }
  for (tItr = outputs.begin(); tItr != outputs.end(); tItr++) {
    partdata->getFixedTerminalValue(master_name, tItr->m_portName, netname);
    if ((netname == "") && (tItr->m_netName != "")) {
      netname = prefix;
      netname += "_" + tItr->m_netName;
      //out_diffpair_terminals.push_back(ModuleTerminal(tItr->m_portName, netname,
      //					      tItr->m_dir, tItr->m_width));
    }
    
    //printf("output port %s %s\n", tItr->m_portName.c_str(), netname.c_str());
    single_ended_terminals.push_back(ModuleTerminal(tItr->m_portName, netname,
						    tItr->m_dir, tItr->m_width));
  }
  for (tItr = inouts.begin(); tItr != inouts.end(); tItr++) {
    partdata->getFixedTerminalValue(master_name, tItr->m_portName, netname);
    if ((netname == "") && (tItr->m_netName != "")) {
      netname = prefix;
      netname += "_" + tItr->m_netName;
      //out_diffpair_terminals.push_back(ModuleTerminal(tItr->m_portName, netname,
      //					      tItr->m_dir, tItr->m_width));
    }
    
    //printf("output port %s %s\n", tItr->m_portName.c_str(), netname.c_str());
    single_ended_terminals.push_back(ModuleTerminal(tItr->m_portName, netname,
						    tItr->m_dir, tItr->m_width));
  }

  // Instantiate new instance
  HdlUtils::createModuleParameterList(master, plist);

  AddInstance *ai = new AddInstance (fpga_path, topmod_instname,
				     master_name, plist, single_ended_terminals);
  ai->setOnePortPerLine(true);
  addNewItem(ai);

  return 0;
}

void EditCompilePP::collectPartitionTerminals(PartitionData *partdata,
					      ConnBundleLList &connBundles,
					      ModuleTerminalList &terminals)
{
  ConnBundleLIterator connBundleItr;
  ConnBundle *connBundle;
  ConnectionIterator connItr;
  BString netname;
  BString portname;
  PartitionData *other_pdata;
  BString mod_instname;
  BString prefix;

  for (connBundleItr = connBundles.begin();
       connBundleItr != connBundles.end();
       connBundleItr++) {

    connBundle = *connBundleItr;
    if (partdata == connBundle->getToFpga()->getPartitionData())
      other_pdata = connBundle->getFromFpga()->getPartitionData();
    else
      other_pdata = connBundle->getToFpga()->getPartitionData();
    if (partdata->isDefault()) {
      mod_instname = other_pdata->getPartitionedInstName();
      prefix = other_pdata->getContainingModulePrefix();
    }
    else {
      mod_instname = partdata->getPartitionedInstName();
      prefix = partdata->getContainingModulePrefix();
    }

    if ((connBundle->getFromFpga() == partdata->getFpga()) ||
	connBundle->getToFpga() == partdata->getFpga()) {
      for (connItr = connBundle->beginConnection();
	   connItr != connBundle->endConnection();
	   connItr++) {
	netname = connItr->getFromNet();
	if (netname != "") {
	  netname = prefix + "_" + connItr->getFromNet();
	  portname = mod_instname + "_" + connItr->getFromPort();
	  //printf("Partition Terminal %s %s\n", portname.c_str(), netname.c_str());
	  if (connItr->getConnType() == DIFF_PAIR) {
	    //printf("Diffpair Partition Terminal %s %s\n", portname.c_str(), netname.c_str());
	    terminals.push_back(ModuleTerminal(portname.c_str(), netname.c_str(),
					       d_output, connItr->getWidth()));
	  }
	}
      }
    }
  }
}

// Take the collected cut set and sort them into the bin (single-ended, diffpairs, non)
void EditCompilePP::collectCrossPartitionSignals(PartitionSpec *partSpec,
						 ConnBundleLList &connBundles)
{
  PartitionData *pdata1, *pdata2;
  PartitionDataIterator pdataItr1, pdataItr2;
  
  // Iterate through each partition specified by PartitionData
  for (pdataItr1 = partSpec->beginPartitionData();
       pdataItr1 != partSpec->endPartitionData();
       pdataItr1++) {

    pdata1 = pdataItr1->second;

    for (pdataItr2 = pdataItr1;
	 pdataItr2 != partSpec->endPartitionData();
	 pdataItr2++) {
    
      pdata2 = pdataItr2->second;
      if (pdata1 == pdata2)
	continue;

      // If pdata1 is default then swap
      if (pdata1->isDefault()) {
	collectCrossPartitionSignalsFromDefault(pdata1, pdata2, connBundles);
	collectCrossPartitionSignalsToDefault(pdata2, pdata1, connBundles);
      }
      else if (pdata2->isDefault()) {
	collectCrossPartitionSignalsFromDefault(pdata2, pdata1, connBundles);
	collectCrossPartitionSignalsToDefault(pdata1, pdata2, connBundles);
      }
      else {
	collectCrossPartitionSignals(pdata1, pdata2, connBundles);
	collectCrossPartitionSignals(pdata2, pdata1, connBundles);
      }
    }
  }
}	

int EditCompilePP::collectCrossPartitionSignals(PartitionData *pdata1,
						PartitionData *pdata2,
						ConnBundleLList &connBundles)
{
  Fpga *fpga1, *fpga2;
  ConnBundle *connBundle;
  ModuleTerminalIterator tItr;
  ModuleTerminal *terminal1, *terminal2;
  ConnectionType type;
  BString single_ended_name, se_data_name;
  BString modname;
  BString fixed_name;
  VeriNodeInfoTable *table;

  if (pdata1->isDefault() || pdata2->isDefault()) {
    cerr << "Error EditCompilePP::collectCrossPartitionSignals(): cannot process default partition."
	 << endl;
    return 0;
  }

  // Load info table
  table = VeriNodeInfoTable::load(pdata1->getContainingModule());
  //printf("AFTER LOAD\n");
  fpga1 = pdata1->getFpga();
  fpga2 = pdata2->getFpga();
  connBundle = new ConnBundle(fpga1, fpga2);
  connBundles.push_back(connBundle);
  
  //printf("CollectCrossPartitionSignals from %s to %s\n", fpga1->getModuleName(), fpga2->getModuleName());

  ModuleTerminalList &outputs = pdata1->getOutputList();
  for (tItr = outputs.begin(); tItr != outputs.end(); tItr++) {
    
    modname = pdata1->getModuleName();
    pdata1->getFixedTerminalValue(modname, tItr->m_portName, fixed_name);
    single_ended_name = pdata1->getSingleEndedTerminalValue(tItr->m_portName);
    se_data_name = pdata1->getSingleEndedDataTerminalValue(tItr->m_portName);
    //printf("Port %s Net %s\n", tItr->m_portName.c_str(), tItr->m_netName.c_str());
    if (fixed_name != "")
      type = FIXED;
    if ((single_ended_name != "") || (se_data_name != ""))
      type = SINGLE_ENDED;
    else
      type = DIFF_PAIR;
    
    terminal1 = &(*tItr);
    terminal2 = findConnectedTerminal(terminal1, pdata1, pdata2, table);
    if (terminal2) {
      
      //printf("found terminal2\n");
      connBundle->addConnection(terminal1->m_portName, terminal2->m_portName,
				terminal1->m_netName, terminal2->m_netName,
				terminal1->m_width, type);
    }
    //else
    //  printf("Not found terminal2\n");
  }
  return 1;
}    

void EditCompilePP::collectCrossPartitionSignalsFromDefault(PartitionData *pdata1,
							    PartitionData *pdata2,
							    ConnBundleLList &connBundles)
{
  Fpga *fpga1, *fpga2;
  ConnBundle *connBundle;
  ModuleTerminalIterator tItr;
  ModuleTerminal *terminal1, *terminal2;
  ConnectionType type;
  BString single_ended_name, se_data_name;
  BString modname;
  BString fixed_name;
  VeriNodeInfoTable *table;
  VeriNodeInfo *toNode;

  // Load info table
  table = VeriNodeInfoTable::load(pdata2->getContainingModule());
  //printf("after load\n");


  fpga1 = pdata1->getFpga();
  fpga2 = pdata2->getFpga();
  connBundle = new ConnBundle(fpga1, fpga2);
  connBundles.push_back(connBundle);
  
  printf("CollectCrossPartitionSignalsFromDefault from %s to %s\n", fpga1->getModuleName(), fpga2->getModuleName());

  ModuleTerminalList &inputs = pdata2->getInputList();
  for (tItr = inputs.begin(); tItr != inputs.end(); tItr++) {
    
    printf("\ninput terminal %s(%s)\n", tItr->m_portName.c_str(), tItr->m_netName.c_str());
    modname = pdata2->getModuleName();
    pdata2->getFixedTerminalValue(modname, tItr->m_portName, fixed_name);
    single_ended_name = pdata2->getSingleEndedTerminalValue(tItr->m_portName);
    se_data_name = pdata2->getSingleEndedDataTerminalValue(tItr->m_portName);
    printf("fixed name %s single-ended name %s modname %s\n", fixed_name.c_str(), single_ended_name.c_str(), modname.c_str());

    if (fixed_name != "") {
      type = FIXED;
      //printf("Type is fixed\n");
    }
    else if ((single_ended_name != "") || (se_data_name != "")) {
      type = SINGLE_ENDED;
      //printf("Type is singled-ended\n");
    }
    else {
      type = DIFF_PAIR;
      //printf("Type is diff-pair\n");
    }

    terminal2 = &(*tItr);
    toNode = table->findSink(terminal2->m_netName.c_str());
    if (toNode == NULL)
      continue;
    printf("toNode: %p from %s\n", toNode, terminal2->m_netName.c_str()); //toNode->dump();
    if (isConnectedFromDefaultPartition(toNode, pdata2->getPartitionSpec())) {
      printf("Yes connected to default\n");
      terminal1 = terminal2;
      connBundle->addConnection(terminal1->m_portName, terminal2->m_portName,
				terminal1->m_netName, terminal2->m_netName,
				terminal1->m_width, type);
    }
    //else
    //printf("No input not connected to default\n");
  }

  /*
  ModuleTerminalList &outputs = pdata2->getOutputList();
  for (tItr = outputs.begin(); tItr != outputs.end(); tItr++) {
    
    printf("output terminal %s(%s)\n", tItr->m_portName.c_str(), tItr->m_netName.c_str());
    modname = pdata2->getModuleName();
    pdata2->getFixedTerminalValue(modname, tItr->m_portName, fixed_name);
    single_ended_name = pdata2->getSingleEndedTerminalValue(tItr->m_portName);
    
    if (fixed_name != "")
      type = FIXED;
    if (single_ended_name != "")
      type = SINGLE_ENDED;
    else
      type = DIFF_PAIR;
    
    terminal1 = &(*tItr);
    toNode = table->getVeriNodeInfo(terminal1->m_netName);
    printf("toNode: "); toNode->dump();
    if (isConnectedToDefaultPartition(toNode, pdata2->getPartitionSpec())) {
      printf("Yes connected to default\n");
      terminal2 = terminal1;
      connBundle->addConnection(terminal1->m_portName, terminal2->m_portName,
				terminal1->m_netName, terminal2->m_netName,
				terminal1->m_width, type);
    }
    else
      printf("No output not connected to default\n");
  }
  */
}

void EditCompilePP::collectCrossPartitionSignalsToDefault(PartitionData *pdata1,
							  PartitionData *pdata2,
							  ConnBundleLList &connBundles)
{
  Fpga *fpga1, *fpga2;
  ConnBundle *connBundle;
  ModuleTerminalIterator tItr;
  ModuleTerminal *terminal1, *terminal2;
  ConnectionType type;
  BString single_ended_name, se_data_name;
  BString modname;
  BString fixed_name;
  VeriNodeInfoTable *table;
  VeriNodeInfo *fromNode;

  // Load info table
  table = VeriNodeInfoTable::load(pdata1->getContainingModule());

  fpga1 = pdata1->getFpga();
  fpga2 = pdata2->getFpga();
  connBundle = new ConnBundle(fpga1, fpga2);
  connBundles.push_back(connBundle);
  
  printf("CollectCrossPartitionSignalsToDefault from %s to %s\n", fpga1->getModuleName(), fpga2->getModuleName());
  /*
  ModuleTerminalList &inputs = pdata1->getInputList();
  for (tItr = inputs.begin(); tItr != inputs.end(); tItr++) {
    
    printf("output terminal2 %s(%s)\n", tItr->m_portName.c_str(), tItr->m_netName.c_str());
    modname = pdata1->getModuleName();
    pdata1->getFixedTerminalValue(modname, tItr->m_portName, fixed_name);
    single_ended_name = pdata1->getSingleEndedTerminalValue(tItr->m_portName);
    
    if (fixed_name != "")
      type = FIXED;
    if (single_ended_name != "")
      type = SINGLE_ENDED;
    else
      type = DIFF_PAIR;
    
    terminal1 = &(*tItr);
    fromNode = table->getVeriNodeInfo(terminal1->m_netName);
    if (isConnectedToDefaultPartition(fromNode, pdata1->getPartitionSpec())) {
      printf("Yes connected to default\n");
      terminal2 = terminal1;
      connBundle->addConnection(terminal1->m_portName, terminal2->m_portName,
				terminal1->m_netName, terminal2->m_netName,
				terminal1->m_width, type);
    }
    else
      printf("No output not connected to default\n");
  }
  */
  ModuleTerminalList &outputs = pdata1->getOutputList();
  for (tItr = outputs.begin(); tItr != outputs.end(); tItr++) {
    
    printf("output terminal2 %s(%s)\n", tItr->m_portName.c_str(), tItr->m_netName.c_str());
    modname = pdata1->getModuleName();
    pdata1->getFixedTerminalValue(modname, tItr->m_portName, fixed_name);
    single_ended_name = pdata1->getSingleEndedTerminalValue(tItr->m_portName);
    se_data_name = pdata2->getSingleEndedDataTerminalValue(tItr->m_portName);
    
    if (fixed_name != "")
      type = FIXED;
    else if ((single_ended_name != "") || (se_data_name != ""))
      type = SINGLE_ENDED;
    else
      type = DIFF_PAIR;
    
    terminal1 = &(*tItr);

    if (terminal1->m_netName == "") continue;

    printf("findSink %s table:%p\n", terminal1->m_netName.c_str(), table);
    fromNode = table->findSink(terminal1->m_netName.c_str());
    if (fromNode)
      fromNode->dump();
    else
      printf("fromNode is NULL\n");
    if (fromNode && isConnectedToDefaultPartition(fromNode, pdata1->getPartitionSpec())) {
      printf("Yes connected to default\n");
      terminal2 = terminal1;
      connBundle->addConnection(terminal1->m_portName, terminal2->m_portName,
				terminal1->m_netName, terminal2->m_netName,
				terminal1->m_width, type);
    }
    //else
    //  printf("No output not connected to default\n");
  }
  //printf("done collecting to\n");
}

int EditCompilePP::isConnectedToDefaultPartition(VeriNodeInfo *node, PartitionSpec *spec)
{
  VeriNodeInfo *sink;
  VeriNodeInfo *source = node->getVirtualSource();

  VeriNodeInfoListIterator sinkItr;
  VeriNodeType type;

  if (source == NULL)
    return 0;

  // If this is connected to an inst that is not being partitioned
  type = source->getType();
  printf("here0 %d %d %p\n", spec->isPartitionedInst((VeriInstId*)source->getVeriNode()),
  	 spec->numPartitionDatas(),
  	 source->getVeriNode());
  if ((type == NodeInfoInstOutput) &&
      ((!spec->isPartitionedInst((VeriInstId*)source->getVeriNode())) ||
       (spec->numPartitionDatas() <= 2))) {
    printf("here0x %d\n", spec->numPartitionDatas());
    return 1;
  }

  for (sinkItr = source->sinkBegin();
       sinkItr != source->sinkEnd();
       sinkItr++) {

    printf("here1\n");
    sink = *sinkItr;

    if (sink == node) continue;

    sink->dump();
    type = sink->getType();

    // If this is a source of an expression of always block
    if ((type == NodeInfoExpression) || 
	(type == NodeInfoExpressionSrc) ||
	(type == NodeInfoAlwaysBlockAssign) ||
	(type == NodeInfoModuleOutput) ||
	//(type == NodeInfoAssign) ||
	(type == NodeInfoConstant)) {

      printf("here2\n");
      return 1;
    }
    
    // If this is connected to an inst that is not being partitioned
    if ((type == NodeInfoInstInput) &&
	(!spec->isPartitionedInst((VeriInstId*)sink->getVeriNode()))) {
      printf("here3 %d %p\n", spec->isPartitionedInst((VeriInstId*)sink->getVeriNode()),
	     sink->getVeriNode());
      return 1;
    }

    if (type == NodeInfoAssign) {
      printf("here4\n");
      if (isConnectedToDefaultPartitionFromNode(sink, spec))
        return 1;
    }
  }
  
  return 0;
}


int EditCompilePP::isConnectedToDefaultPartitionFromNode(VeriNodeInfo *node, PartitionSpec *spec)
{
  VeriNodeInfo *sink;
  VeriNodeInfoListIterator sinkItr;
  VeriNodeType type;

  //printf(" here0\n ");
  //node->dump();
  for (sinkItr = node->sinkBegin();
       sinkItr != node->sinkEnd();
       sinkItr++) {
    
    sink = *sinkItr;
    //printf(" here1 %p %p\n", sink, node);

    if (sink == node) continue;

    //sink->dump();
    type = sink->getType();

    // If this is a source of an expression of always block
    if ((type == NodeInfoExpression) ||
	(type == NodeInfoExpressionSrc) ||
        (type == NodeInfoAlwaysBlockAssign) ||
        (type == NodeInfoModuleOutput) ||
        //(type == NodeInfoAssign) ||
        (type == NodeInfoConstant)) {

      //printf(" here2\n");
      return 1;
    }

    // If this is connected to an inst that is not being partitioned
    if ((type == NodeInfoInstInput) &&
        (!spec->isPartitionedInst((VeriInstId*)sink->getVeriNode()))) {
      //printf(" here3 %d %p\n", spec->isPartitionedInst((VeriInstId*)sink->getVeriNode()),
      //     sink->getVeriNode());
      return 1;
    }

    if (type == NodeInfoAssign) {
      //printf(" here4\n");
      if (isConnectedToDefaultPartitionFromNode(sink, spec))
        return 1;
    }
  }

  return 0;
}

int EditCompilePP::isConnectedFromDefaultPartition(VeriNodeInfo *node, PartitionSpec *spec)
{
  VeriNodeType type;
  VeriNodeInfo *source = node->getVirtualSource();

  printf("here0a\n");
  node->dump();
  if (source == NULL)
    return 0;
  printf("here1a\n");

  type = source->getType();
  source->dump();

  // If this is a source of an expression of always block
  if ((type == NodeInfoExpression) || 
      (type == NodeInfoExpressionSrc) ||
      (type == NodeInfoAlwaysBlockAssign) ||
      (type == NodeInfoModuleInput) ||
      (type == NodeInfoConstant)) {
    
    //printf("here2a\n");
    return 1;
  }

  //printf("here3 %d %d %p %d\n", (type == NodeInfoInstOutput),
  //	 spec->isPartitionedInst((VeriInstId*)source->getVeriNode()),
  //	 node->getVeriNode(), spec->numPartitionDatas());
  // If this is connected to an inst that is not being partitioned
  if ((type == NodeInfoInstOutput) &&
      ((!spec->isPartitionedInst((VeriInstId*)source->getVeriNode())) ||
       (spec->numPartitionDatas() <= 2))) {
    //printf("here3a %d %p\n", spec->isPartitionedInst((VeriInstId*)source->getVeriNode()),
    //	   node->getVeriNode());
    return 1;
  }
  
  return 0;
}


ModuleTerminal *EditCompilePP::findConnectedTerminal(ModuleTerminal *source, PartitionData *from,
						     PartitionData *to, VeriNodeInfoTable *table)
{
  //VeriModule *module;
  VeriInstId *toInst;
  //VeriIdDef *from_signal, *to_signal;
  ModuleTerminal *terminal;
  VeriNodeInfo *fromNode, *sourceNode;

  if (source->m_netName == "")
    return NULL;

  sourceNode = table->findSink(source->m_netName.c_str());

  if (sourceNode == NULL) {
    cout << "Something wrong, no node for " << source->m_netName;
    return NULL;
  }
    
  //printf("From %s to %s\n", from->getFpga()->getModuleName(), to->getFpga()->getModuleName());
  //printf("Find connected Terminal source: %s %p\n", source->m_netName.c_str(), source);
  //module = to->getContainingModule();
  //fromInst = from->getCurrentVerificInstance();
  toInst = to->getCurrentVerificInstance();
  
  //printf("fromInst %s toInst %s\n", fromInst->Name(), toInst->Name());
  fromNode = sourceNode->getVirtualSource();
  if (fromNode == NULL) {
    cout << "No virtual source node for " << source->m_netName << endl;
    //toNode = fromNode;
  }
  //printf("VeriNode %p fromInst %p toInst %p type %d\n", fromNode->getVeriNode(), fromInst, toInst,
  //	 fromNode->getType());

  VeriNodeInfo *sink;
  VeriNodeInfoListIterator sinkItr;

  //printf("from: ");
  //fromNode->dump();

  for (sinkItr = fromNode->sinkBegin();
       sinkItr != fromNode->sinkEnd();
       sinkItr++) {

    sink = *sinkItr;
    //printf("sink: ");
    //sink->dump();
    if (sink->getVeriNode() == toInst) {
      //printf("find terminal with name %s\n", sink->getSinkName());
      terminal = to->findTerminal(sink->getSourceName());
      //printf("here8 %p\n", terminal);
      return terminal;
    }
  }
  
  //printf("here10\n");
  return NULL;
}

// returns false on error
bool EditCompilePP::checkAssignToSignals(Fpga *fpga)
{
  Fabric *fabric = fpga->getFabric();

  if (fabric == NULL)
    return true;

  StringMapIterator asItr;

  for (asItr = fabric->beginAssignToSignal();
       asItr != fabric->endAssignToSignal();
       asItr++) {

    if (asItr->second == 0) {

      cerr << "Error EditCompilePP::checkAssignToSignals(): signal "
	   << asItr->first << " must be assigned in fpga " << fpga->getModuleName()
	   << ". Make sure the design spec file has the proper assign statement." << endl;
      return false;
    }
  }
  
  return true;
}

// Connect the Partition signals to the IO (diffpars)
bool EditCompilePP::connectToDiffPair(Connection &conn,
				      BString &prefix,
				      bool trySingleEnded)
{
  Fpga *fromFpga, *toFpga;
  ConnBundle *connBundle;
  unsigned int unused = 0;

  fromFpga = conn.getConnBundle()->getFromFpga();
  toFpga = conn.getConnBundle()->getToFpga();
  connBundle = fromFpga->findDPConnBundleToFpga(toFpga);

  printf("connectToDiffPair from %s to %s, fromport %s(%s) toport %s(%s)\n",
	 fromFpga->getModuleName(), toFpga->getModuleName(),
  	 conn.getFromPort(), conn.getFromNet(), conn.getToPort(), conn.getToNet());

  unused = 0;

  if (connBundle != NULL) {
    unused = connBundle->getUnused();
  }
  else {
    cerr << "Error EditCompilePP::connectToDiffPair(): there are no DP connections from fpga "
	 << fromFpga->getModuleName()
	 << " to fpga " << toFpga->getModuleName()
	 << " when attempting to connect " <<  conn.getFromPort()
	 << " of " << fromFpga->getPartitionData()->getHierInstanceName()
	 << "/" << fromFpga->getPartitionData()->getPartitionedInstName()
	 << "." << endl;
    return false; // Failed
  }

  trySingleEnded = false;
  //printf("Unused**************** %d %d\n", unused, conn.getWidth());
  if (conn.getWidth() <= unused) {
    connBundle->decrementUnused(conn.getWidth());
  }
  else if (trySingleEnded == true) {
    cerr << "Warning EditCompilePP::connectToDiffPair(): there are not enough transmitting DP connections from fpga "
	 << fromFpga->getModuleName()
	 << " to fpga " << toFpga->getModuleName()
	 << " when attempting to connect " <<  conn.getFromPort()
	 << " of " << fromFpga->getPartitionData()->getHierInstanceName()
	 << "/" //<< fromFpga->getPartitionData()->getPartitionedInstName()
	 << ".  The port requires " << conn.getWidth() << " channels but there are only "
	 << unused
	 << ". The assignment will be attempted on the single-ended pair connections."
	 << endl;
    
    if (!connectToSingleEnded(conn, prefix, false))
      return false;
    else {
      return true;  // Sucessfully assigned to single-ended
    }
  }
  else {
    cerr << "Error EditCompilePP::connectToDiffPair(): there are not enough transmitting DP connections from fpga "
	 << fromFpga->getModuleName()
	 << " to fpga " << toFpga->getModuleName()
	 << " when attempting to connect " <<  conn.getFromPort()
	 << " of " << fromFpga->getPartitionData()->getHierInstanceName()
	 << "/" //<< fromFpga->getPartitionData()->getPartitionedInstName()
	 << ".  The port requires " << conn.getWidth() << " channels but there are only "
	 << unused
	 << "." << endl;
    return false;
  }

  //printf("HERE1\n");
  // If this is a fixed terminal then just return, no need for an assignment
  PartitionData *pdata = fromFpga->getPartitionData();
  PartitionData *other_pdata = toFpga->getPartitionData();
  BString cmpstr, mod_instname, netname, portname;
  if (pdata->isDefault()) {
    other_pdata->getFixedTerminalValue(other_pdata->getModuleName(), conn.getToPort(), cmpstr);
  }
  else {
    pdata->getFixedTerminalValue(pdata->getModuleName(), conn.getFromPort(), cmpstr);
  }
  //printf("HERE2\n");
  if (cmpstr != "")
    return true;
  

  //printf("HERE3\n");
  verificAddDPAssignment(conn);
  //printf("HERE4\n");

  if (pdata->isDefault()) {
    mod_instname = other_pdata->getPartitionedInstName();
    prefix = other_pdata->getContainingModulePrefix();
    netname = prefix + "_" + conn.getFromNet();
    portname = mod_instname + "_" + conn.getFromPort();
    printf("AddAssignedSE 2 %s %s\n", portname.c_str(), netname.c_str());
    pdata->addAssignedSETerminal(portname, netname, conn.getWidth());
  }
  else if (other_pdata->isDefault()) {
    mod_instname = pdata->getPartitionedInstName();
    prefix = pdata->getContainingModulePrefix();
    netname = prefix + "_" + conn.getFromNet();
    portname = mod_instname + "_" + conn.getFromPort();
    printf("AddAssignedSE 3 %s %s\n", portname.c_str(), netname.c_str());
    other_pdata->addAssignedSETerminal(portname, netname, conn.getWidth());
  }
  
  return true;
}
    
// Connect the Partition signals to the IO (diffpars)
bool EditCompilePP::connectToSingleEnded(Connection &conn,
					 BString &prefix,
					 bool tryDiffPair)
{
  Fpga *fromFpga, *toFpga;
  ConnBundle *connBundle;
  unsigned int unused = 0;

  fromFpga = conn.getConnBundle()->getFromFpga();
  toFpga = conn.getConnBundle()->getToFpga();  connBundle = fromFpga->findSEConnBundleToFpga(toFpga);
  printf("connectToSingleEnded from %s to %s, fromport %s(%s) toport %s(%s) %d %p\n",
	 fromFpga->getModuleName(), toFpga->getModuleName(),
  	 conn.getFromPort(), conn.getFromNet(), conn.getToPort(), conn.getToNet(),
  	 conn.getWidth(), connBundle);

  tryDiffPair = false;
  if (connBundle != NULL)
    unused = connBundle->getUnused();
  else {
    cerr << "Error EditCompilePP::connectToSingleEnded(): there are no SE connections from fpga "
	 << fromFpga->getModuleName()
	 << " to fpga " << toFpga->getModuleName()
	 << " when attempting to connect " <<  conn.getFromPort()
	 << " of " << fromFpga->getPartitionData()->getHierInstanceName()
	 << "/" //<< fromFpga->getPartitionData()->getPartitionedInstName()
	 << "." << endl;
    return false;
  }

  tryDiffPair = false;
  //printf("Unused**************** %d %d\n", unused, conn.getWidth());
  if (conn.getWidth() <= unused) {
    connBundle->decrementUnused(conn.getWidth());
  }
  else if (tryDiffPair == true) {
    cerr << "Warning EditCompilePP::connectToSingleEnded(): there are not enough SE connections from fpga "
	 << fromFpga->getModuleName()
	 << " to fpga " << toFpga->getModuleName()
	 << " when attempting to connect " <<  conn.getFromPort()
	 << " of " << fromFpga->getPartitionData()->getHierInstanceName()
	 << "/" << fromFpga->getPartitionData()->getPartitionedInstName()
	 << ".  The port requires " << conn.getWidth() << " channels but there are only "
	 << connBundle->getUnused()
	 << ". The assignment will be attempted on the diff pair connections."
	 << endl;

    if (!connectToDiffPair(conn, prefix, false))
      return false;
    else {
      return true;  // Sucessfully assigned to single-ended
    }
  }
  else {
    cerr << "Error EditCompilePP::connectToSingleEnded(): there are not enough SE connections from fpga "
	 << fromFpga->getModuleName()
	 << " to fpga " << toFpga->getModuleName()
	 << " when attempting to connect " <<  conn.getFromPort()
	 << " of " << fromFpga->getPartitionData()->getHierInstanceName()
	 << "/" //<< fromFpga->getPartitionData()->getPartitionedInstName()
	 << ".  The port requires " << conn.getWidth() << " channels but there are only "
	 << unused
	 << "." << endl;
    return false; // Failed
  }
  // If this is a fixed terminal then just return, no need for an assignment
  //printf("Unused2**************** %d\n", unused);
  PartitionData *pdata = fromFpga->getPartitionData();
  BString cmpstr;
  if (pdata->isDefault()) {
    pdata = toFpga->getPartitionData();
    pdata->getFixedTerminalValue(pdata->getModuleName(), conn.getToPort(), cmpstr);
  }
  else {
    pdata->getFixedTerminalValue(pdata->getModuleName(), conn.getFromPort(), cmpstr);
  }
  //printf("before AddSEAssignment %s\n", cmpstr.c_str());
  if (cmpstr != "")
    return true;
  
  //printf("AddSEAssignment\n");
  verificAddSEAssignment(conn);
  return true;
}

bool EditCompilePP::connectUnusedDPWires(Fpga *fpga)
{
  BString assign_lhs, assign_rhs, conn_msb, conn_lsb;
  unsigned int lsb, msb, nbits;
  int bitsLeft;
  char str[99];

  if ((fpga->getPartitionData() == NULL) ||
      ((fpga->getPartitionData()->getPartitionedInstName() == NULL)
       && (!fpga->getPartitionData()->isDefault())))
    return true;

  //printf("Connect unused DP for fpga %s %d %d %p\n", fpga->getModuleName(),
  //	 (fpga->getPartitionData() == NULL),
  //	 (fpga->getPartitionData()->getPartitionedInstName() == NULL),
  //	 (fpga->getPartitionData()));

  ConnBundleIterator cbItr;
  ConnBundle *cb;
  ConnectionIterator cItr;

  for (cbItr = fpga->beginOutDPConnBundle();
       cbItr != fpga->endOutDPConnBundle();
       cbItr++) {

    cb = cbItr->second;

    if (cb == NULL) continue;

    bitsLeft = cb->getNextConn(cItr, lsb, msb);

    while (bitsLeft >= 0) {

      conn_lsb = itoa(lsb);
      conn_msb = itoa(msb);

      nbits = msb-lsb+1;
      snprintf(str, 99, "%d", nbits);
      
      assign_rhs = str;
      assign_rhs += "'b0";
      
      assign_lhs = cItr->getFromNet();
      if (nbits != cItr->getWidth()) {
	assign_lhs += "[" + conn_msb;
	if (conn_msb != conn_lsb)
	  assign_lhs += ":" + conn_lsb;
	assign_lhs += "]";
      }
      
      //printf("UnusedDP: %s = %s\n", assign_lhs.c_str(), assign_rhs.c_str());
      addNewItem(new AddSimpleAssign(fpga->getModulePath(), assign_lhs, assign_rhs));

      //printf("getNextConn next\n");
      bitsLeft = cb->getNextConn(cItr, lsb, msb);
      //printf("after getNextConn next %d\n", bitsLeft);
    }
  }
  //printf("Connect Unused DP done\n");
  return true;
}     

bool EditCompilePP::connectUnusedSEWires(Fpga *fpga)
{
  BString assign_lhs, assign_rhs, conn_msb, conn_lsb;
  unsigned int lsb, msb, nbits;
  int bitsLeft;
  char str[99];

  if ((fpga->getPartitionData() == NULL) ||
      ((fpga->getPartitionData()->getPartitionedInstName() == NULL)
       && (!fpga->getPartitionData()->isDefault())))
    return true;

  //printf("Connect unused SE for fpga %s\n", fpga->getModuleName());

  ConnBundleIterator cbItr;
  ConnBundle *cb;
  ConnectionIterator cItr;

  for (cbItr = fpga->beginOutSEConnBundle();
       cbItr != fpga->endOutSEConnBundle();
       cbItr++) {

    cb = cbItr->second;

    if (cb == NULL) continue;

    bitsLeft = cb->getNextConn(cItr, lsb, msb);
    //printf("after get next conn1\n");

    while (bitsLeft >= 0) {

      conn_lsb = itoa(lsb);
      conn_msb = itoa(msb);
      
      nbits = msb-lsb+1;
      snprintf(str, 99, "%d", nbits);
      
      assign_rhs = str;
      assign_rhs += "'b0";
      
      assign_lhs = cItr->getFromNet();
      if (nbits != cItr->getWidth()) {
	assign_lhs += "[" + conn_msb;
	if (conn_msb != conn_lsb)
	  assign_lhs += ":" + conn_lsb;
	assign_lhs += "]";
      }
      //printf("UnusedSE: %s = %s\n", assign_lhs.c_str(), assign_rhs.c_str());
      addNewItem(new AddSimpleAssign(fpga->getModulePath(), assign_lhs, assign_rhs));

      //printf("before get next conn\n");
      bitsLeft = cb->getNextConn(cItr, lsb, msb);
      //printf("after get next conn\n");
    }
  }
  //printf("Connect Unused SE done\n");
  return true;
}     

// Assign unsed IO (single-ended) to '0
bool EditCompilePP::connectUnusedWires(Fpga *fpga, Partition *cm,
				       ModuleTerminalIterator &dutItr,
				       int &dutBitSoFar, ModuleTerminalList &outputs)
{
  BString assign_lhs, assign_rhs, dut_msb, dut_lsb;
  int nbits;
  char str[99];

  if ((dutBitSoFar != 0) && (dutItr != outputs.end())) {

    dut_lsb = itoa(dutBitSoFar);
    dut_msb = itoa(dutItr->m_width-1);

    nbits = dutItr->m_width - dutBitSoFar;
    snprintf(str, 99, "%d", nbits);

    assign_rhs = str;
    assign_rhs += "'b0";

    assign_lhs = dutItr->m_netName;
    assign_lhs += "[" + dut_msb;
    if (dut_msb != dut_lsb)
      assign_lhs += ":" + dut_lsb;
    assign_lhs += "]";

    addNewItem(new AddSimpleAssign(fpga->getModulePath(), assign_lhs, assign_rhs));

    dutBitSoFar = 0;
    dutItr++;
  }

  while (dutItr != outputs.end()) {

    nbits = dutItr->m_width;
    snprintf(str, 99, "%d", nbits);

    assign_rhs = str;
    assign_rhs += "'b0";

    assign_lhs = dutItr->m_netName;

    addNewItem(new AddSimpleAssign(fpga->getModulePath(), assign_lhs, assign_rhs));

    dutItr++;
  }

  return true;
}      

// Create the ModuleTerminalList from the module inputs and outputs
void EditCompilePP::createModuleTerminalList(VeriModule* module, PartitionData *pdata,
					     ModuleTerminalList &mtlist)
{
  unsigned it;
  VeriIdDef *port;
  int size;
  BString portname, netname;

  BString modname = module->Name();

  FOREACH_ARRAY_ITEM(module->GetPorts(), it, port) {
    if (port == NULL)
      continue;
    portname = port->GetName();

    pdata->getFixedTerminalValue(modname, portname, netname);
    VeriRange* range = port->GetDimensionAt(0);

    size = 1;

    if (range) {
      range->StaticReplaceConstantExpr(1);

      int min = 0, max = 0;
      
      min = range->GetLsbOfRange();
      max = range->GetMsbOfRange();

      if (min > max) {
	min = range->GetMsbOfRange();
	max = range->GetLsbOfRange();
      }

      size = max - min + 1;
    }
    
    printf("createModuleTerminal %s %s\n", portname.c_str(), netname.c_str());
    if (port->IsInput()) {
      mtlist.push_back (ModuleTerminal (portname, netname, d_input, size));
    } else if (port->IsOutput()) {
      mtlist.push_back (ModuleTerminal (portname, netname, d_output, size));
    } else {
      mtlist.push_back (ModuleTerminal (portname, netname, d_inout, size));
    }
  }
}


// Write Report
bool EditCompilePP::writeReport(PartitionSpec *pspec, FpgaBoardSpec *board,
				ConnBundleLList &connBundles, FILE *file)
				
{
  ModuleTerminalIterator tItr;
  ModuleTerminalList in_diffpair_terminals, out_diffpair_terminals,
    new_module_terminals, single_ended_terminals;

  FpgaIterator nextFpgaItr;
  FpgaIterator fpgaItr = board->beginFpga();
  ConnectionIterator inConnItr, outConnItr;
  ConnectionIterator inDPConnItr, outDPConnItr;
  ConnBundleIterator inCBListItr, outCBListItr;
  ConnBundleIterator inDPCBListItr, outDPCBListItr;
  
  PartitionDataIterator pItr1, pItr2;

  Fpga *fpgaA, *fpgaB;
  int board_total;
  int board_out_se_count, board_in_se_count, board_in_dp_count, board_out_dp_count;
  ConnBundle *bundle;
  //ConnBundleLList connBundles;

  if (file == NULL)
    file = fopen ("partition.rpt","w");

  fprintf(file, "\n\nPartitioning Report for Signals Routing Requirement Across FPGA Board\n");
  fprintf(file, "---------------------------------------------------------------------\n");
  fprintf(file, "\nBoard: %s\n", board->getSpecFile());
  fprintf(file, "-----\n");

  while (fpgaItr != board->endFpga()) {
    fpgaA = fpgaItr->second;
    fpgaItr++;
    nextFpgaItr = fpgaItr;
    while (nextFpgaItr != board->endFpga()) {
      fpgaB = nextFpgaItr->second;
      nextFpgaItr++;
      board_out_se_count = board_in_se_count = board_in_dp_count = board_out_dp_count = 0;
      board_total = 0;

      fprintf(file, "\n -- Fabric between %s <-> %s --\n", fpgaA->getModuleName(),
	      fpgaB->getModuleName());

      fprintf(file, "\n Single-ended Connections between %s <-> %s\n", fpgaA->getModuleName(),
	      fpgaB->getModuleName());
      
      // Write out the cross partitions connections

      bundle = fpgaA->findSEConnBundleToFpga(fpgaB);

      if (bundle)
	for (outConnItr = bundle->beginConnection();
	     outConnItr != bundle->endConnection();
	     outConnItr++) {
	  
	  fprintf(file, "  Single-ended Connection: %s -> %s Width: %d\n", outConnItr->getFromPort(),
		  outConnItr->getToPort(), outConnItr->getWidth());
	  board_out_se_count += outConnItr->getWidth();
	}
      else
	fprintf(file, "  No Single-ended Connection found.\n");

      board_total += board_out_se_count;
      
      bundle = fpgaB->findSEConnBundleToFpga(fpgaA);
	
      if (bundle)
	for (inConnItr = bundle->beginConnection();
	     inConnItr != bundle->endConnection();
	     inConnItr++) {
	  
	  fprintf(file, "  Single-ended Connection: %s <- %s Width: %d\n", inConnItr->getFromPort(),
		  inConnItr->getToPort(), inConnItr->getWidth());
	  board_in_se_count += inConnItr->getWidth();
	}
      else
	fprintf(file, "  No Single-ended Connection found.\n");

      board_total += board_in_se_count;
    
      fprintf(file, " Total single-ended capacity: %d\n", board_total);
    
      fprintf(file, "\n Diffpair Connections between %s -> %s\n", fpgaA->getModuleName(),
	      fpgaB->getModuleName());
      
      bundle = fpgaA->findDPConnBundleToFpga(fpgaB);
      
      if (bundle)
	for (outDPConnItr = bundle->beginConnection();
	     outDPConnItr != bundle->endConnection();
	     outDPConnItr++) {
	  
	  fprintf(file, "  Output Diffpair Connection: %s -> %s Width: %d\n", outDPConnItr->getFromPort(),
		  outDPConnItr->getToPort(), outDPConnItr->getWidth());
	  board_out_dp_count += outDPConnItr->getWidth();
	}
      else
	fprintf(file, "  No Diffpair Connection found.\n");
      
      board_total += board_out_dp_count;
      
      fprintf(file, " Total output diffpair capacity: %d\n", board_out_dp_count);
      
      fprintf(file, "\n Diffpair Connections between %s <- %s\n", fpgaA->getModuleName(),
	      fpgaB->getModuleName());
      
      bundle = fpgaB->findDPConnBundleToFpga(fpgaA);
      
      if (bundle)
	for (inDPConnItr = bundle->beginConnection();
	     inDPConnItr != bundle->endConnection();
	     inDPConnItr++) {
	  
	  fprintf(file, "  Input Diffpair Connection: %s <- %s Width: %d\n", inDPConnItr->getFromPort(),
		  inDPConnItr->getToPort(), inDPConnItr->getWidth());
	  board_in_dp_count += inDPConnItr->getWidth();
	}
      else
	fprintf(file, "  No Diffpair Connection found.\n");
      
      board_total += board_in_dp_count;
      
      fprintf(file, " Total input diffpair capacity: %d\n", board_in_dp_count);
      fprintf(file, "\n Total overall connections between %s and %s: %d\n", fpgaA->getModuleName(),
	      fpgaB->getModuleName(), board_total);
    }
  }

  fprintf(file, "-------------------------------------------------------------------\n\n");

  //collectCrossPartitionSignals(pspec, connBundles);

  ConnBundleLIterator connBundleItr;
  ConnBundle *connBundle;
  ConnectionIterator connItr;
  Fpga *fromFpga, *toFpga;
  BString prefix;
  BString dname1, dname2;
  unsigned int dp_count, se_count, dp_unused, se_unused;
  ConnBundle *sebundle, *dpbundle;

  fprintf(file, "Design Summary Report\n");
  fprintf(file, "---------------------\n");

  for (connBundleItr = connBundles.begin();
       connBundleItr != connBundles.end();
       connBundleItr++) {

    connBundle = *connBundleItr;
    fromFpga = connBundle->getFromFpga();
    toFpga = connBundle->getToFpga();

    sebundle = fromFpga->findSEConnBundleToFpga(toFpga);
    dpbundle = fromFpga->findDPConnBundleToFpga(toFpga);
    dp_count = se_count = dp_unused = se_unused = 0;
    if (dpbundle)
      dp_unused = dpbundle->getUnused();
    if (sebundle)
      se_unused = sebundle->getUnused();

    //fprintf(file, "here se %p unused: %d dp %p unused: %d\n", sebundle, se_unused, dpbundle, dp_unused);

    if (fromFpga->getPartitionData()->getModuleName() == NULL)
      dname1 = "DEFAULT";
    else
      dname1 = fromFpga->getPartitionData()->getModuleName();
    if (toFpga->getPartitionData()->getModuleName() == NULL)
      dname2 = "DEFAULT";
    else
      dname2 = toFpga->getPartitionData()->getModuleName();
    fprintf(file, "\n Connections from %c(%s) -> %c(%s)\n\n", fromFpga->getLetterSuffix(), dname1.c_str(),
	    toFpga->getLetterSuffix(), dname2.c_str());
    if (fromFpga->getPartitionData()->isDefault())
      prefix = toFpga->getPartitionData()->getContainingModulePrefix();
    else
      prefix = fromFpga->getPartitionData()->getContainingModulePrefix();

    //fprintf(file, "PREFIX %s\n", prefix.c_str());
    
    for (connItr = connBundle->beginConnection();
	 connItr != connBundle->endConnection();
	 connItr++) {
      
      if (connItr->getConnType() == DIFF_PAIR) {

	dp_count += connItr->getWidth();
	if ((dpbundle == NULL) || (dp_count > dpbundle->getUnused()))
	  fprintf(file, "  Diffpair From %s(%s) -> %s(%s) width: %d ===> ** capacity exceeded **\n",
		  connItr->getFromPort(), connItr->getFromNet(),
		  connItr->getToPort(), connItr->getToNet(), connItr->getWidth());
	else
	  fprintf(file, "  Diffpair From %s(%s) -> %s(%s) width: %d\n",
		  connItr->getFromPort(), connItr->getFromNet(),
		  connItr->getToPort(), connItr->getToNet(), connItr->getWidth());
	
      }
      else if (connItr->getConnType() == SINGLE_ENDED) {
	
	se_count += connItr->getWidth();
	if ((sebundle == NULL) || (se_count > sebundle->getUnused()))
	  fprintf(file, "  Single-ended From %s(%s) -> To %s(%s) width: %d ===> ** capacity exceeded **\n",
		  connItr->getFromPort(), connItr->getFromNet(),
		  connItr->getToPort(), connItr->getToNet(), connItr->getWidth());
	else
	  fprintf(file, "  Single-ended From %s(%s) -> To %s(%s) width: %d\n",
		  connItr->getFromPort(), connItr->getFromNet(),
		  connItr->getToPort(), connItr->getToNet(), connItr->getWidth());
      }
    }
    if (dp_count == 0 && se_count == 0)
      fprintf(file, "  None\n");
  }
  
  return true;
}


void EditCompilePP::getTopPortPrefix(PartitionData *pdata, BString &prefix)
{
  size_t slash;
  BString path = pdata->getHierInstanceName();

  if (path[0] == '/') {
    path = path.substr(1);
  }

  slash = path.find_first_of('/');
  
  if (slash != string::npos)
    prefix = path.substr(slash+1);
  else
    prefix = "";

  slash = prefix.find_first_of('/');
  while (slash != string::npos) {
    prefix[slash] = '_';
    slash = prefix.find_first_of('/');
  }

  //if (prefix != "")
  //  prefix += "_";
  //prefix += pdata->getPartitionedInstName();
}

void EditCompilePP::generateInstanceName(BString &path, BString &generatedName, int secondlevel)
{
  size_t loc = 0;

  if (path[0] == '/')
    generatedName = path.substr(1);
  else
    generatedName = path;

  loc = generatedName.find('/');

  if (secondlevel != 0 && loc != string::npos) {
    loc = generatedName.find('/', loc);
    generatedName = path.substr(loc+2);
  }

  loc = generatedName.find('/');
  while (loc != string::npos) {

    generatedName[loc] = '_';
    loc = generatedName.find('/', loc);
  }
}

// AddInstance of sync flop
void EditCompilePP::verificAddSync2Flop(Fpga *fpga, PartitionData *pdata, BString &instname,
					BString &clkName, BString &sig, BString &io)
{
  ModuleTerminalList mtlist;
  ParameterList plist;
  BString mod_instname = instname + "_sync2flop";

  mtlist.push_back(ModuleTerminal("clk", clkName, d_input, 1, 1));
  mtlist.push_back(ModuleTerminal("q", sig, d_output, 1, 1));
  mtlist.push_back(ModuleTerminal("i", io, d_input, 1, 1));
  //printf("VERIFIC: add sync2flop(%s,%s,%s)  path %s instname: %s\n", clkName.c_str(), sig.c_str(), io.c_str(),
  //	 fpga->getModulePath(), mod_instname.c_str());

  if (fpga->findSyncFlopStatement(mod_instname.c_str()))
    return;
  fpga->addSyncFlopStatement(mod_instname.c_str());

  addNewItem(new AddInstance (fpga->getModulePath(), mod_instname,
			      "sync2flop", plist, mtlist));
}

// AddInstance of sync flop
void EditCompilePP::verificAddSync2NFlop(Fpga *fpga, PartitionData *pdata, BString &instname,
					 BString &clkName, BString &sig, BString &io)
{
  ModuleTerminalList mtlist;
  ParameterList plist;
  BString mod_instname = instname + "_sync2flop";

  mtlist.push_back(ModuleTerminal("clk", clkName, d_input, 1, 1));
  mtlist.push_back(ModuleTerminal("sync_clk", "sync_clock", d_input, 1, 1));
  mtlist.push_back(ModuleTerminal("q", sig, d_output, 1, 1));
  mtlist.push_back(ModuleTerminal("i", io, d_input, 1, 1));
  //printf("VERIFIC: add sync2nflop(%s,%s,%s,%s)  path %s instname: %s\n", clkName.c_str(), syncClkName.c_str(), sig.c_str(), io.c_str(),
  //	 fpga->getModulePath(), mod_instname.c_str());

  if (fpga->findSyncFlopStatement(mod_instname.c_str()))
    return;
  fpga->addSyncFlopStatement(mod_instname.c_str());

  addNewItem(new AddInstance (fpga->getModulePath(), mod_instname,
			      "sync2nflop", plist, mtlist));
}

// addTerminals
void EditCompilePP::verificAddPorts(BString &modpath, ModuleTerminalList &mtlist)
{
  ModuleTerminalIterator tItr;
  BString portname;
  BString signalname;

  // wire for each partitioned port
  for (tItr = mtlist.begin(); tItr != mtlist.end(); tItr++) {
    portname = tItr->m_portName;
    signalname = tItr->m_netName;
    printf("VERIFIC: Add port %s in %s\n", signalname.c_str(), modpath.c_str());
    addNewItem(new AddPort(modpath, signalname, tItr->m_dir, tItr->m_width));
  }
}

// addWires
void EditCompilePP::verificAddWires(BString &modpath, ModuleTerminalList &mtlist)
{
  ModuleTerminalIterator tItr;
  BString signalname;

  // wire for each partitioned port
  for (tItr = mtlist.begin(); tItr != mtlist.end(); tItr++) {
    signalname = tItr->m_netName;
    printf("VERIFIC:: Add wire %s in %s\n", signalname.c_str(), modpath.c_str());
    addNewItem(new AddNet(modpath, signalname, tItr->m_width));
  }
}


// addWire
void EditCompilePP::verificAddFromWire(Fpga *fpga, Connection &conn, BString &prefix)
{
  BString modpath = fpga->getModulePath();
  BString signalname = prefix + "_" + conn.getFromNet();

  if (fpga->findWireStatement(signalname.c_str()) == 0) {
    fpga->addWireStatement(signalname.c_str());

    printf("VERIFIC: Add from wire %s in %s\n", signalname.c_str(), modpath.c_str());
    addNewItem(new AddNet(modpath, signalname, conn.getWidth()));
  }
}

// addWire
void EditCompilePP::verificAddToWire(Fpga *fpga, Connection &conn, BString &prefix)
{
  BString modpath = fpga->getModulePath();
  BString signalname = prefix + "_" + conn.getToNet();

  if (fpga->findWireStatement(signalname.c_str()) == 0) {
    fpga->addWireStatement(signalname.c_str());

    printf("VERIFIC: Add to wire %s in %s\n", signalname.c_str(), modpath.c_str());
    addNewItem(new AddNet(modpath, signalname, conn.getWidth()));
  }
}


// draw the wires up the hierarchy
void EditCompilePP::verificDrawUpPortsOfPartitionedInstance(PartitionData *pdata,
							    VeriModule *instmaster)
{
  ModuleTerminalIterator tItr;
  ModuleTerminalList &inputs = pdata->getInputList();
  ModuleTerminalList &outputs = pdata->getOutputList();
  BString netname;

  // DrawIn for each partitioned output port
  for (tItr = outputs.begin(); tItr != outputs.end(); tItr++) {
    BString signalname = tItr->m_netName;

    // Merge the output net
    if (getMergingNetCount(signalname) > 0) continue;

    BString portname = tItr->m_portName;
    BString modname = instmaster->Name();
    pdata->getFixedTerminalValue(modname, portname, netname);
    if (netname != "")
      continue;
    if (HdlUtils::findPortOfModuleByName(instmaster, portname) == NULL) {
      if (signalname != "") {
	//printf("VERIFIC: DrawUpPart1 in signal %s %s on %s\n", portname.c_str(), signalname.c_str(), pdata->getHierPathName()); 
	addMergingNet(signalname); // Hash the signal that it has already been driven out (merging)
	addNewItem(new DrawInSignal (pdata->getHierPathName(), "/", signalname,
				     portname, tItr->m_width));
      }
    }
    else {
      if (signalname != "") {
	portname = BString(pdata->getPartitionedInstName()) + "_" + tItr->m_portName;
	//printf("VERIFIC: DrawUpPart2 in signal %s %s on %s\n", portname.c_str(), signalname.c_str(), pdata->getHierPathName());
	addMergingNet(signalname); // Hash the signal that it has already been driven out (merging)
	addNewItem(new DrawInSignal (pdata->getHierPathName(), "/", signalname,
				     portname, tItr->m_width));
      }
    }
    //printf("In draw up4\n");      
  }
  for (tItr = inputs.begin(); tItr != inputs.end(); tItr++) {
    BString signalname = tItr->m_netName;

    // Merge the output net
    if (getMergingNetCount(signalname) > 0) continue;

    BString portname = tItr->m_portName;
    BString modname = instmaster->Name();
    pdata->getFixedTerminalValue(modname, portname, netname);
    if (netname != "")
      continue;
        if (HdlUtils::findPortOfModuleByName(instmaster, portname) == NULL) {
      if (signalname != "") {
	//printf("VERIFIC: Draw3 out signal %s %s on %s\n", portname.c_str(), signalname.c_str(),
	//     pdata->getHierPathName()); 
	addMergingNet(signalname); // Hash the signal that it has already been driven out (merging)
	addNewItem(new DrawOutSignal (pdata->getHierPathName(), "/", signalname, portname,
				      tItr->m_width));
      }
    }
    else {
      if (signalname != "") {
	portname = BString(pdata->getPartitionedInstName()) + "_" + tItr->m_portName;
	//printf("VERIFIC: Draw4 out signal %s %s on %s\n", portname.c_str(), signalname.c_str(),
	//     pdata->getHierPathName()); 
	addMergingNet(signalname); // Hash the signal that it has already been driven out (merging)
	addNewItem(new DrawOutSignal (pdata->getHierPathName(), "/", signalname, portname,
				    tItr->m_width));
      }
    }
  }
}


// draw the wires up the hierarchy
void EditCompilePP::verificDrawUpPortsOfPartitionedInst(PartitionData *pdata,
							VeriModule *instmaster)
{
  ModuleTerminalIterator tItr;
  BString netname;
  ConnBundle *connBundle;
  ConnBundleIterator connBundleItr;

  // For output SE connections
  for (connBundleItr = pdata->getFpga()->beginOutSEConnBundle();
       connBundleItr != pdata->getFpga()->endOutSEConnBundle();
       connBundleItr++) {
 
    connBundle = connBundleItr->second;

    if (connBundle)
      verificDrawUpOutputConns(pdata, connBundle, instmaster);
  }

  // For output DP connections
  for (connBundleItr = pdata->getFpga()->beginOutDPConnBundle();
       connBundleItr != pdata->getFpga()->endOutDPConnBundle();
       connBundleItr++) {

    connBundle = connBundleItr->second;

    if (connBundle)
      verificDrawUpOutputConns(pdata, connBundle, instmaster);
  }

  // For input SE connections
  for (connBundleItr = pdata->getFpga()->beginInSEConnBundle();
       connBundleItr != pdata->getFpga()->endInSEConnBundle();
       connBundleItr++) {

    connBundle = connBundleItr->second;

    if (connBundle)
      verificDrawUpOutputConns(pdata, connBundle, instmaster);
  }

  // For output DP connections
  for (connBundleItr = pdata->getFpga()->beginInDPConnBundle();
       connBundleItr != pdata->getFpga()->endOutDPConnBundle();
       connBundleItr++) {

    connBundle = connBundleItr->second;

    if (connBundle)
      verificDrawUpOutputConns(pdata, connBundle, instmaster);
  }
}

void EditCompilePP::verificDrawUpOutputConns(PartitionData *pdata, ConnBundle *connBundle,
					     VeriModule *instmaster)
{
  ConnectionIterator connItr;
  BString modname, netname, portname, signalname;

  for (connItr = connBundle->beginConnection();
       connItr != connBundle->endConnection();
       connItr++) {

    if (connItr->getFromNet()) {
      signalname = connItr->getFromNet();
    }
    else
      signalname = "";

    // Merge the output net
    if (getMergingNetCount(signalname) > 0) continue;

    portname = connItr->getFromPort();
    modname = instmaster->Name();
    pdata->getFixedTerminalValue(modname, portname, netname);
    if (netname != "")
      continue;
    if (HdlUtils::findPortOfModuleByName(instmaster, portname) == NULL) {
      if (signalname != "") {
	printf("VERIFIC: DrawUpOutputConn1 in signal %s %s on %s\n", portname.c_str(), signalname.c_str(), pdata->getHierPathName()); 
	addMergingNet(signalname); // Hash the signal that it has already been driven out (merging)
	addNewItem(new DrawInSignal (pdata->getHierPathName(), "/", signalname,
				     portname, connItr->getWidth()));
      }
    }
    else {
      if (signalname != "") {
	portname = BString(pdata->getPartitionedInstName()) + "_" + connItr->getFromPort();
	printf("VERIFIC: DrawUpOutputConn2 in signal %s %s on %s\n", portname.c_str(), signalname.c_str(), pdata->getHierPathName());
	addMergingNet(signalname); // Hash the signal that it has already been driven out (merging)
	addNewItem(new DrawInSignal (pdata->getHierPathName(), "/", signalname,
				     portname, connItr->getWidth()));
      }
    }
  }
}

void EditCompilePP::verificDrawUpInputConns(PartitionData *pdata, ConnBundle *connBundle,
					    VeriModule *instmaster)
{
  ConnectionIterator connItr;
  BString modname, netname, portname, signalname;

  for (connItr = connBundle->beginConnection();
       connItr != connBundle->endConnection();
       connItr++) {

    if (connItr->getFromNet())
      signalname = connItr->getToNet();
    else
      signalname = "";

    // Merge the output net
    if (getMergingNetCount(signalname) > 0) continue;

    portname = connItr->getToPort();
    modname = instmaster->Name();
    pdata->getFixedTerminalValue(modname, portname, netname);
    if (netname != "")
      continue;
        if (HdlUtils::findPortOfModuleByName(instmaster, portname) == NULL) {
      if (signalname != "") {
	printf("VERIFIC: Draw3 out signal %s %s on %s\n", portname.c_str(), signalname.c_str(),
	     pdata->getHierPathName()); 
	addMergingNet(signalname); // Hash the signal that it has already been driven out (merging)
	addNewItem(new DrawOutSignal (pdata->getHierPathName(), "/", signalname, portname,
				      connItr->getWidth()));
      }
    }
    else {
      if (signalname != "") {
	portname = BString(pdata->getPartitionedInstName()) + "_" + connItr->getToPort();
	printf("VERIFIC: Draw4 out signal %s %s on %s\n", portname.c_str(), signalname.c_str(),
	     pdata->getHierPathName()); 
	addMergingNet(signalname); // Hash the signal that it has already been driven out (merging)
	addNewItem(new DrawOutSignal (pdata->getHierPathName(), "/", signalname, portname,
				      connItr->getWidth()));
      }
    }
  }
}


// Helper for verificAddAssignment
void EditCompilePP::makeAssignText(ModuleTerminalIterator &dutItr,
				   ConnectionIterator &ioItr, unsigned int delta,
				   BString &dut_msb, BString &dut_lsb,
				   BString &io_msb, BString &io_lsb,
				   BString &assign_lhs, BString &assign_rhs)
{
  //printf("ASSIGN io->fromPort %s width %d dut->netName %s width %d\n", ioItr->getFromPort(), ioItr->getWidth(), dutItr->m_netName.c_str(), dutItr->m_width);
  if (dutItr->m_dir == d_input) {
    //printf("Input\n");
    assign_lhs = dutItr->m_netName;
    if (delta != dutItr->m_width) {
      assign_lhs += "[" + dut_msb;
      if (dut_msb != dut_lsb)
	assign_lhs += ":" + dut_lsb;
      assign_lhs += "]";
    }
    assign_rhs = ioItr->getToNet();
    if (delta != ioItr->getWidth()) {
      assign_rhs += "[" + io_msb;
      if (io_msb != io_lsb)
	assign_rhs += ":" + io_lsb;
      assign_rhs += "]";
    }
    
  } else {
    
    //printf("Output\n");
    assign_lhs = ioItr->getFromNet();
    if (delta != ioItr->getWidth()) {
      assign_lhs += "[" + io_msb;
      if (io_msb != io_lsb)
	assign_lhs += ":" + io_lsb;
      assign_lhs += "]";
    }
    assign_rhs = dutItr->m_netName;
    if (delta != dutItr->m_width) {
      assign_rhs += "[" + dut_msb;
      if (dut_msb != dut_lsb)
	assign_rhs += ":" + dut_lsb;
      assign_rhs += "]";
    }
  }    
  //printf("VERIFIC: assign %s -> %s\n", assign_lhs.c_str(), assign_rhs.c_str());
}

// Helper for verificAddAssignment
void EditCompilePP::makeFromAssignText(Connection &dut_conn, Connection &fabric_conn,
				       unsigned int delta,
				       BString &dutLSB, BString &dutMSB,
				       BString &fabricLSB, BString &fabricMSB,
				       BString &assign_lhs, BString &assign_rhs, BString &netprefix,
				       BString &portprefix)
{
  //printf("ASSIGN fabric->fromPort %s width %d dut->netName %s width %d %s\n", dut_conn.getFromPort(), dut_conn.getWidth(), dut_conn.getFromNet(), dut_conn.getWidth(), fabric_conn.getFromNet());

  assign_lhs = fabric_conn.getFromNet();
  if (delta != fabric_conn.getWidth()) {
    assign_lhs += "[" + fabricMSB;
    if (fabricMSB != fabricLSB)
      assign_lhs += ":" + fabricLSB;
    assign_lhs += "]";
  }

  assign_rhs = portprefix + "_" + dut_conn.getFromNet();
  if (delta != dut_conn.getWidth()) {
    assign_rhs += "[" + dutMSB;
    if (dutMSB != dutLSB)
      assign_rhs += ":" + dutLSB;
    assign_rhs += "]";
  }
  
  //printf("VERIFICX: assign %s -> %s\n", assign_lhs.c_str(), assign_rhs.c_str());
}

// Helper for verificAddAssignment
void EditCompilePP::makeToAssignText(Connection &dut_conn, Connection &fabric_conn,
				     unsigned int delta,
				     BString &dutLSB, BString &dutMSB,
				     BString &fabricLSB, BString &fabricMSB,
				     BString &assign_lhs, BString &assign_rhs,
				     BString &prefix)
{
  //printf("ASSIGN fabric->fromPort %s width %d dut->netName %s width %d %s\n", dut_conn.getFromPort(), dut_conn.getWidth(), dut_conn.getFromNet(), dut_conn.getWidth(), fabric_conn.getFromNet());

  assign_lhs = prefix + "_" + dut_conn.getFromNet();
  if (delta != dut_conn.getWidth()) {
    assign_lhs += "[" + dutMSB;
    if (dutMSB != dutLSB)
      assign_lhs += ":" + dutLSB;
    assign_lhs += "]";
  }
  assign_rhs = fabric_conn.getToNet();
  if (delta != fabric_conn.getWidth()) {
    assign_rhs += "[" + fabricMSB;
    if (fabricMSB != fabricLSB)
      assign_rhs += ":" + fabricLSB;
    assign_rhs += "]";
  }
  
  //printf("VERIFICX: assign %s -> %s\n", assign_lhs.c_str(), assign_rhs.c_str());
}

// AddSimpleAssign
bool EditCompilePP::verificAddDPAssignment(Connection &dut_conn)
{
  // Get the Fpgas
  Fpga *fromFpga, *toFpga;
  fromFpga = dut_conn.getConnBundle()->getFromFpga();
  toFpga = dut_conn.getConnBundle()->getToFpga();

  if (toFpga->findDPStatement(dut_conn.getToNet()))
    return true;

  toFpga->addDPStatement(dut_conn.getToNet());

  BString prefix;
  if (fromFpga->getPartitionData()->isDefault()) {
    prefix = toFpga->getPartitionData()->getContainingModulePrefix();
  }
  else {
    prefix = fromFpga->getPartitionData()->getContainingModulePrefix();
  }

  BString assign_lhs, assign_rhs;
  ConnectionIterator fabric_connItr;
  unsigned int fabric_lsb, fabric_msb, dut_lsb, dut_msb;
  BString dutLSB, dutMSB, fabricLSB, fabricMSB;
  int dutBitsLeft, dutBitsSoFar;
  unsigned int width;
  unsigned int delta;

  dutBitsSoFar = 0;
  width = dut_conn.getWidth();
  dutBitsLeft = fromFpga->getNextOutDPConn(toFpga, fabric_connItr, fabric_lsb, fabric_msb, width);
  fabricLSB = itoa(fabric_lsb);
  fabricMSB = itoa(fabric_msb);

  //printf("\n\ndutBitsLeft %d fmsb: %d\n", dutBitsLeft, fabric_msb);

  // There is not enough
  if (dutBitsLeft < 0)
    return false;

  //printf("dutwidth: %d fabricwidth: %d fabricLSB: %s fabricnet: %s\n", width, fabric_connItr->getWidth(), fabricLSB.c_str(), fabric_connItr->getFromNet());
  // The channel is the same size as the signal width
  if ((width == fabric_connItr->getWidth()) && (fabricLSB == "0")) {

    assign_lhs = dut_conn.getFromNet();
    assign_rhs = fabric_connItr->getFromNet();
    //printf("VERIFIC1: assign %s -> %s path: %s\n", assign_lhs.c_str(), assign_rhs.c_str(), fromFpga->getModulePath());
    addNewItem(new AddSimpleAssign(fromFpga->getModulePath(), assign_lhs, assign_rhs));

    assign_lhs = fabric_connItr->getToNet();
    assign_rhs = dut_conn.getToNet();
    //printf("VERIFIC2: assign %s -> %s path: %s\n", assign_lhs.c_str(), assign_rhs.c_str(), toFpga->getModulePath());
    addNewItem(new AddSimpleAssign(toFpga->getModulePath(), assign_lhs, assign_rhs));

    BString fromNet = dut_conn.getFromNet();
    BString toNet = dut_conn.getToNet();
    if (fromNet != toNet) {
      assign_lhs = prefix + "_" + toNet;
      assign_rhs = prefix + "_" + fromNet;
      verificAddFromWire(toFpga, dut_conn, prefix);
      addNewItem(new AddSimpleAssign(toFpga->getModulePath(), assign_lhs, assign_rhs));
    }
  }
  else {
    
    delta = width - dutBitsLeft - dutBitsSoFar;
    dut_lsb = dutBitsSoFar;
    dut_msb = dutBitsSoFar + fabric_msb - fabric_lsb;
    dutLSB = itoa(dut_lsb);
    dutMSB = itoa(dut_msb);

    //printf("DUT LSB: %d MSB: %d\n", dut_lsb, dut_msb);
    makeFromAssignText(dut_conn, *fabric_connItr, delta, dutLSB, dutMSB, fabricLSB, fabricMSB,
		       assign_lhs, assign_rhs, prefix, prefix);

    //printf("VERIFIC23: on fpga: %s assign %s -> %s\n", fromFpga->getModuleName(), assign_lhs.c_str(), assign_rhs.c_str());
    addNewItem(new AddSimpleAssign(fromFpga->getModulePath(), assign_lhs, assign_rhs));

    makeToAssignText(dut_conn, *fabric_connItr, delta, dutLSB, dutMSB, fabricLSB, fabricMSB,
		     assign_lhs, assign_rhs, prefix);
    //printf("VERIFIC24: on fpga: %s assign %s -> %s\n", toFpga->getModuleName(), assign_lhs.c_str(), assign_rhs.c_str());
    addNewItem(new AddSimpleAssign(toFpga->getModulePath(), assign_lhs, assign_rhs));
    dutBitsSoFar = fabric_msb - fabric_lsb + 1;

    while (dutBitsLeft > 0) {
   
      delta = dutBitsLeft;

      dutBitsLeft = fromFpga->getNextOutDPConn(toFpga, fabric_connItr, fabric_lsb,
					       fabric_msb, dutBitsLeft);

      if (dutBitsLeft < 0)
	return true;

      //printf("DutBitsLeft: %d\n", dutBitsLeft);

      fabricLSB = itoa(fabric_lsb);
      fabricMSB = itoa(fabric_msb);

      dut_lsb = dutBitsSoFar;
      dut_msb = dutBitsSoFar + fabric_msb - fabric_lsb;
      dutLSB = itoa(dut_lsb);
      dutMSB = itoa(dut_msb);

      delta -= dutBitsLeft;

      dutBitsSoFar = width - dutBitsLeft;
      //printf("DUT2 LSB: %d MSB: %d bitsleft: %d\n", dut_lsb, dut_msb, dutBitsLeft);

      makeFromAssignText(dut_conn, *fabric_connItr, delta, dutLSB, dutMSB, fabricLSB, fabricMSB,
			 assign_lhs, assign_rhs, prefix, prefix);

      //printf("VERIFIC3: assign %s -> %s\n", assign_lhs.c_str(), assign_rhs.c_str());
      addNewItem(new AddSimpleAssign(fromFpga->getModulePath(), assign_lhs, assign_rhs));

      makeToAssignText(dut_conn, *fabric_connItr, delta, dutLSB, dutMSB, fabricLSB, fabricMSB,
		       assign_lhs, assign_rhs, prefix);
      //printf("VERIFIC4: assign %s -> %s\n", assign_lhs.c_str(), assign_rhs.c_str());
      addNewItem(new AddSimpleAssign(toFpga->getModulePath(), assign_lhs, assign_rhs));
    }

    BString fromNet = dut_conn.getFromNet();
    BString toNet = dut_conn.getToNet();
    if (fromNet != toNet) {
      assign_lhs = prefix + "_" + toNet;
      assign_rhs = prefix + "_" + fromNet;
      verificAddFromWire(toFpga, dut_conn, prefix);
      addNewItem(new AddSimpleAssign(toFpga->getModulePath(), assign_lhs, assign_rhs));
    }

    return true;
  }
  return false;
}
  
// AddSimpleAssign
bool EditCompilePP::verificAddSEAssignment(Connection &dut_conn)
{
  // Get the Fpgas
  Fpga *fromFpga, *toFpga;
  fromFpga = dut_conn.getConnBundle()->getFromFpga();
  toFpga = dut_conn.getConnBundle()->getToFpga();

  BString netprefix, portprefix, instpostfix, se_clk, instname;
  if (fromFpga->getPartitionData()->isDefault()) {
    portprefix = toFpga->getPartitionData()->getPartitionedInstName();
    netprefix = toFpga->getPartitionData()->getContainingModulePrefix();
    instpostfix = toFpga->getPartitionData()->getPartitionedInstName();
  }
  else {
    portprefix = fromFpga->getPartitionData()->getPartitionedInstName();
    netprefix = fromFpga->getPartitionData()->getContainingModulePrefix();
    instpostfix = fromFpga->getPartitionData()->getPartitionedInstName();
  }

  if (toFpga->getFabric())
    se_clk = toFpga->getFabric()->getSingleEndedClock();
  else
    se_clk = "sync_clock";

  BString assign_lhs, assign_rhs, netname, portname;
  ConnectionIterator fabric_connItr;
  unsigned int fabric_lsb, fabric_msb, dut_lsb, dut_msb;
  BString dutLSB, dutMSB, fabricLSB, fabricMSB;
  int dutBitsLeft, dutBitsSoFar;
  unsigned int width;
  unsigned int delta;

  dutBitsSoFar = 0;
  width = dut_conn.getWidth();
  dutBitsLeft = fromFpga->getNextOutSEConn(toFpga, fabric_connItr, fabric_lsb, fabric_msb, width);
  fabricLSB = itoa(fabric_lsb);
  fabricMSB = itoa(fabric_msb);

  //printf("\n\ndutBitsLeft %d fmsb: %d\n", dutBitsLeft, fabric_msb);
  // There is not enough
  if (dutBitsLeft < 0)
    return false;

  //printf("dutwidth: %d fabricwidth: %d fabricLSB: %s\n", width, fabric_connItr->getWidth(), fabricLSB.c_str());
  // The channel is the same size as the signal width
  if ((width == fabric_connItr->getWidth()) && (fabricLSB == "0")) {

    assign_lhs = dut_conn.getFromNet();
    assign_rhs = fabric_connItr->getFromNet();
    //printf("VERIFIC1: assign SE %s -> %s path: %s\n", dut_conn.getFromPort(), fabric_connItr->getFromNet(), fromFpga->getModulePath());

    verificAddFromWire(fromFpga, dut_conn, netprefix);
    netname = netprefix + "_" + dut_conn.getFromNet();
    instname = assign_rhs + "_" + instpostfix;
    verificAddSync2Flop(fromFpga, fromFpga->getPartitionData(), instname, se_clk, assign_lhs,
			netname);
    assign_rhs = portprefix + "_" + dut_conn.getFromPort();
    fromFpga->getPartitionData()->addAssignedSETerminal(assign_rhs, netname,
							dut_conn.getWidth());
    assign_lhs = fabric_connItr->getToNet();
    assign_rhs = dut_conn.getToNet();
    //printf("VERIFIC2: assign %s -> %s path: %s\n", assign_lhs.c_str(), assign_rhs.c_str(), toFpga->getModulePath());
    verificAddToWire(toFpga, dut_conn, netprefix);
    verificAddSync2NFlop(toFpga, toFpga->getPartitionData(), assign_rhs, se_clk, assign_rhs,
			 assign_lhs);
    assign_rhs = portprefix + "_" + dut_conn.getToPort();
    toFpga->getPartitionData()->addAssignedSETerminal(assign_rhs, assign_lhs,
							dut_conn.getWidth());
  }
  else {
    
    delta = width - dutBitsLeft - dutBitsSoFar;
    dut_lsb = dutBitsSoFar;
    dut_msb = dutBitsSoFar + fabric_msb - fabric_lsb;
    dutLSB = itoa(dut_lsb);
    dutMSB = itoa(dut_msb);

    //printf("DUT LSB: %d MSB: %d prefix: %s\n", dut_lsb, dut_msb, prefix.c_str());

    //printf("VERIFIC23a: on fpga: %s assign %s -> %s fromport: %s fromnet: %s\n", fromFpga->getModuleName(), assign_lhs.c_str(), assign_rhs.c_str(), dut_conn.getFromPort(), dut_conn.getFromNet());
    makeFromAssignText(dut_conn, *fabric_connItr, delta, dutLSB, dutMSB, fabricLSB, fabricMSB,
		       assign_lhs, assign_rhs, netprefix, portprefix);
    verificAddFromWire(fromFpga, dut_conn, netprefix);
    //printf("Addsync2flop %s %s\n", assign_lhs.c_str(), assign_rhs.c_str());
    netname = netprefix + "_" + dut_conn.getFromNet();
    instname = netname + "_" + instpostfix;
    verificAddSync2Flop(fromFpga, fromFpga->getPartitionData(), instname, se_clk, assign_lhs,
			netname);
    
    portname = portprefix + "_" + dut_conn.getFromPort();
    fromFpga->getPartitionData()->addAssignedSETerminal(portname, netname,
							dut_conn.getWidth());
    makeToAssignText(dut_conn, *fabric_connItr, delta, dutLSB, dutMSB, fabricLSB, fabricMSB,
		     assign_lhs, assign_rhs, netprefix);
    //printf("VERIFIC24: on fpga: %s assign %s -> %s\n", toFpga->getModuleName(), assign_lhs.c_str(), assign_rhs.c_str());
    verificAddToWire(toFpga, dut_conn, netprefix);
    BString cmpst = toFpga->getPartitionData()->
      getSingleEndedDataTerminalValue(dut_conn.getToPort());
    
    //printf("VERIFIC24x: to Net %s %s\n", dut_conn.getToNet(), cmpst.c_str());

    if (cmpst == "")
      verificAddSync2Flop(toFpga, toFpga->getPartitionData(), assign_lhs, se_clk, assign_lhs,
    		  assign_rhs);
    else
    
      verificAddSync2NFlop(toFpga, toFpga->getPartitionData(), assign_lhs, se_clk, assign_lhs,
			   assign_rhs);
    assign_rhs = portprefix + "_" + dut_conn.getToPort();
    toFpga->getPartitionData()->addAssignedSETerminal(assign_rhs, assign_lhs,
						      dut_conn.getWidth());
    dutBitsSoFar = fabric_msb - fabric_lsb + 1;
    
    while (dutBitsLeft > 0) {
      
      delta = dutBitsLeft;

      dutBitsLeft = fromFpga->getNextOutSEConn(toFpga, fabric_connItr, fabric_lsb,
					       fabric_msb, dutBitsLeft);
      if (dutBitsLeft < 0)
	return true;

      fabricLSB = itoa(fabric_lsb);
      fabricMSB = itoa(fabric_msb);

      dut_lsb = dutBitsSoFar;
      dut_msb = dutBitsSoFar + fabric_msb - fabric_lsb;
      dutLSB = itoa(dut_lsb);
      dutMSB = itoa(dut_msb);

      delta -= dutBitsLeft;

      dutBitsSoFar = width - dutBitsLeft;
      //printf("DUT2 LSB: %d MSB: %d\n", dut_lsb, dut_msb);

      //printf("VERIFIC3: assign %s -> %s\n", assign_lhs.c_str(), assign_rhs.c_str());
      makeFromAssignText(dut_conn, *fabric_connItr, delta, dutLSB, dutMSB, fabricLSB, fabricMSB,
			 assign_lhs, assign_rhs, netprefix, portprefix);
      verificAddFromWire(fromFpga, dut_conn, netprefix);
      netname = netprefix + "_" + dut_conn.getFromNet();
      instname = assign_rhs + "_" + instpostfix;
      verificAddSync2Flop(fromFpga, fromFpga->getPartitionData(), instname, se_clk, assign_lhs,
			  netname);
      portname = portprefix + "_" + dut_conn.getFromPort();
      fromFpga->getPartitionData()->addAssignedSETerminal(portname, netname,
							  dut_conn.getWidth());
      makeToAssignText(dut_conn, *fabric_connItr, delta, dutLSB, dutMSB, fabricLSB, fabricMSB,
		       assign_lhs, assign_rhs, netprefix);
      //printf("VERIFIC4: assign %s -> %s\n", assign_lhs.c_str(), assign_rhs.c_str());
      //printf("VERIFIC4x: to Net %s\n", dut_conn.getToNet());
      BString cmpst = toFpga->getPartitionData()->
	getSingleEndedDataTerminalValue(dut_conn.getToPort());
      
      if (cmpst == "")
	verificAddSync2Flop(toFpga, toFpga->getPartitionData(), assign_lhs, se_clk, assign_lhs,
			    assign_rhs);
      else
      
	verificAddSync2NFlop(toFpga, toFpga->getPartitionData(), assign_lhs, se_clk, assign_lhs,
			     assign_rhs);

      assign_rhs = portprefix + "_" + dut_conn.getToPort();
      toFpga->getPartitionData()->addAssignedSETerminal(assign_rhs, assign_lhs,
							dut_conn.getWidth());
    }
    
    return true;
  }
  return false;
}

// AddSimpleAssign
bool EditCompilePP::verificAddAssignment(Fpga *fpga,
					 ModuleTerminalIterator &dutItr,
					 unsigned int &dutBitSoFar,
					 ConnectionIterator &ioItr,
					 unsigned int &ioBitSoFar)
{
  BString assign_lhs, assign_rhs, io_msb, io_lsb, dut_msb, dut_lsb;
  int delta;
  bool bothBitSoFarZero = (dutBitSoFar == 0) && (ioBitSoFar == 0);
  unsigned int dutBitsLeft = dutItr->m_width - dutBitSoFar;
  unsigned int ioBitsLeft = ioItr->getWidth() - ioBitSoFar;

  //printf("Create1 assignment statement for dutbitsleft %d iobitsleft %d dutbitssofar %d iobitssofar %d\n", dutBitsLeft, ioBitsLeft, dutBitSoFar, ioBitSoFar);
  if (dutBitsLeft >= ioBitsLeft)
    delta = ioBitsLeft;
  else
    delta = dutBitsLeft;

  //printf("Delta %d\n", delta);
  io_lsb = itoa(ioBitSoFar);
  dut_lsb = itoa(dutBitSoFar);
  ioBitSoFar += delta;
  dutBitSoFar += delta;
  io_msb = itoa(ioBitSoFar-1);
  dut_msb = itoa(dutBitSoFar-1);

  //printf("Create2 assignment statement for %s dutbitsleft %d iobitsleft %d dutbitssofar %d iobitssofar %d\n", fpga->getModuleName(), dutBitsLeft, ioBitsLeft, dutBitSoFar, ioBitSoFar);
  if ((dutBitsLeft == ioBitsLeft) && (bothBitSoFarZero)) {
    
    if (dutItr->m_dir == d_input) {
      assign_lhs = dutItr->m_netName;
      assign_rhs = ioItr->getToNet();
    }
    else {
      assign_lhs = ioItr->getFromNet();
      assign_rhs = dutItr->m_netName;
    }
    //cerr << "Assign " << assign_lhs.c_str() << " -> " << assign_rhs.c_str() << endl;;
    addNewItem(new AddSimpleAssign(fpga->getModulePath(), assign_lhs, assign_rhs));
    dutItr++;
    ioItr++;
    dutBitSoFar = 0;
    ioBitSoFar = 0;
    
    //printf("Create3 assignment statement for  %s dutbitsleft %d iobitsleft %d dutbutssofar %d iobitssofar %d\n", fpga->getModuleName(), dutBitsLeft, ioBitsLeft, dutBitSoFar, ioBitSoFar);
    return true;
      
  } else if (dutBitsLeft > ioBitsLeft) {

    makeAssignText(dutItr, ioItr, delta, dut_msb, dut_lsb, io_msb, io_lsb, assign_lhs, assign_rhs);
    //cerr << "Assign " << assign_lhs.c_str() << " -> " << assign_rhs.c_str() << endl;;
    addNewItem(new AddSimpleAssign(fpga->getModulePath(), assign_lhs, assign_rhs));
    ioItr++;
    ioBitSoFar = 0;

    //printf("Create4 assignment statement for %s dutbitsleft %d iobitsleft %d dutbutssofar %d iobitssofar %d\n", fpga->getModuleName(), dutBitsLeft, ioBitsLeft, dutBitSoFar, ioBitSoFar);
    return false;

  } else {

    makeAssignText(dutItr, ioItr, delta, dut_msb, dut_lsb, io_msb, io_lsb, assign_lhs, assign_rhs);
    //cerr << "Assign " << assign_lhs.c_str() << " -> " << assign_rhs.c_str() << endl;;
    addNewItem(new AddSimpleAssign(fpga->getModulePath(), assign_lhs, assign_rhs));
    dutItr++;
    dutBitSoFar = 0;
    if (ioBitSoFar == ioItr->getWidth()) {
      ioItr++;
      ioBitSoFar = 0;
    }
    
    //printf("Create5 assignment statement for %s dutbitsleft %d iobitsleft %d dutbutssofar %d iobitssofar %d\n", fpga->getModuleName(), dutBitsLeft, ioBitsLeft, dutBitSoFar, ioBitSoFar);
    return true;
  }
  
  //printf("Something wrong: should never get here\n");
  return false;
}

void EditCompilePP::addMergingNet(const char *netname)
{
  int num = m_net_map[netname];
  m_net_map[netname] = ++num;
}
