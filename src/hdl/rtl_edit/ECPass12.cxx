// Copyright 2010 Bluespec Inc. All rights reserved
#include <string.h>
#include <math.h>

#include "EditCompile.h"
#include "HdlUtils.h"
#include "VeriConstVal.h"

using namespace std ;
using namespace Verific ;

// This visitor class compile the changes from abstract changes to additional abstract changes,
// E.g adding probe muxes
// This code is independent of the Netlist model
class EditCompileP1 : public EditCompileBase {
private:
  std::list<unsigned> m_probelist; // Items which have probe bus to connect

public:
  EditCompileP1 (const BString &);
  virtual int process(ModInstSetPairIter_t &);
  virtual int novisitor(class CktMod *cm) { return 0;}
private:
  void addProbeMuxesAndBus();
  void addUpdatedParamValues();

protected:
  virtual int visit(class AddProbe *)  ;
  virtual int visit(class AddCosim *)  ;
  virtual int visit(class AddCapture *)  ;
  virtual int visit(class AddTrigger *)  ;
  virtual int visit(class ProbedInstance *)  ;
  virtual int visit(class CosimFill *)  ;

};

// This class process the abstract changes into small atomic changes.
// E.g. adding instance, taking care of wires, etc.
class EditCompileP2 : public EditCompileBase {
protected:
  AddedPortList 	        m_addedPorts;
  ScanMap_t                     m_scanmap; // map ScanData id to scan data;

public:
  EditCompileP2 (const BString & inst);
  virtual int process(ModInstSetPairIter_t &);

protected:
  virtual int novisitor(class CktMod *cm) { return 0;}

  void addBroadcastSignalsToProbeModule (ModuleTerminalList & mtlist, bool include_commands, bool include_timer, bool include_data_down);
  void addBUpLinkSignalsToProbeModule (ModuleTerminalList & mtlist, unsigned int key,
                                       bool createNets, const BString & portPostFix);

protected:
  const AddedPortList & getAddedPorts() const;
  const BString & genPortName (const BString & root, bool broadcast, unsigned int width);

  int probeConnectionHelper ( const char * formal, const Verific::VeriExpression *, unsigned int);
  int addScanPorts (ScanData & data, ModuleTerminalList & mtlist);
  int addScanNets  (ScanData & data, ModuleTerminalList & mtlist, bool for_cosim);
  int addScanNets  (ScanData & data, ModuleTerminalList & mtlist);
  ScanData & getShared(ScanData & data);

protected:
  virtual int visit(class ProbedInstance *)  ;
  virtual int visit(class ScannedInstance *)  ;
  virtual int visit(class AddProbe *)  ;
  virtual int visit(class AddCosim *)  ;
  virtual int visit(class AddScan *)  ;
  virtual int visit(class AddCapture *)  ;
  virtual int visit(class AddTrigger *)  ;
  virtual int visit(class ProbeMux *)  ;
  virtual int visit(class ProbeBus *)  ;
  virtual int visit(class ProbeConnection *cm);
  virtual int visit(class DrawInSignal *cm);
  virtual int visit(class DrawOutSignal *cm);

};

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////
// Combined passes over each instance in the hierarchy
int EditCompile::pass12()
{
  int status = 0;
  // If there are no modification then we need to write out the top into
  // the top_EDITED.v file, so our build flow is happy.
  if (m_instSet.size() == 0) {
    string modname = HdlUtils::init()->getTopMod();
    m_instSet.insert(new ReplaceModule (modname, modname));
    //    m_instSet.insert (new ChangeModuleName (modname, modname, 0)); // A no-op...
  }

  // m_instSet is sorted from bottom to top of the inst tree.
  ModInstSetIter_t iter;
  for (iter = m_instSet.begin(); iter != m_instSet.end(); iter = m_instSet.upper_bound(*iter) ) {

    BString iname = (*iter)->getInstName();

    //cerr << "pass1 on instance: " << iname << endl;
    ModInstSetPairIter_t  thisinst = m_instSet.equal_range(*iter);

    EditCompileP1 visitor1(iname);
    status = visitor1.process (thisinst);
    if (status != 0) {
      m_errMsg = visitor1.getError();
      break;
    }
    m_instSet.insert (visitor1.newFirst(), visitor1.newEnd() );

    // Pass 2
    //cerr << "pass2 on instance: " << iname << endl;
    thisinst = m_instSet.equal_range(*iter);

    // PASS 2 over this instance...
    // find the new bounds for this pass
    thisinst = m_instSet.equal_range(*iter);

    EditCompileP2 visitor2(iname);
    status = visitor2.process (thisinst);
    if (status != 0) {
      m_errMsg = visitor2.getError();
      break;
    }
    m_instSet.insert (visitor2.newFirst(), visitor2.newEnd() );
  }
  return status;
}



/////////////////////////////////////////////////////////////////////////////////////////
// Edit Compile Pass 1
// constructor
EditCompileP1::EditCompileP1 (const BString & pathName)
  : EditCompileBase(pathName)
  , m_probelist()
{
}

// Core processing function
int EditCompileP1::process(ModInstSetPairIter_t & range)
{
  int status = 0;
  for (ModInstSetIter_t iter = range.first; iter != range.second && status == 0 ; ++ iter ) {
      status = (*iter)->accept (this);
  }
  if ( status == 0 ) {
    addProbeMuxesAndBus();
    addUpdatedParamValues();
  }
  return status;
}

// Add muxing modules
void EditCompileP1::addProbeMuxesAndBus()
{
  CktMod *newpm ;
  // Build tree of probe muxes
  while (m_probelist.size() > 1) {
    unsigned fr_key = m_probelist.front();
    m_probelist.pop_front();
    unsigned sn_key = m_probelist.front();
    m_probelist.pop_front();

    // Add probe mux
    newpm = new ProbeMux (pathName(), fr_key, sn_key);
    addNewItem(newpm);

    m_probelist.push_back(newpm->getKey());
  }
  // Add probebus
  if (m_probelist.size() == 1) {
    unsigned sn_key = m_probelist.front();
    m_probelist.pop_front();
    if (isProbeSink()) {
      newpm = new ProbeConnection (pathName(), sn_key);
      addNewItem(newpm);

    } else {
      newpm = new ProbeBus (pathName(), sn_key);
      addNewItem(newpm);
    }
  }
}

// Add the parameters since these should be set as well.
void EditCompileP1::addUpdatedParamValues()
{
  VeriModule *mod = getMasterModule();

  if (mod == NULL)
    return;

  Array *params = mod->GetParameters();
  unsigned itema;
  VeriIdDef *para;
  FOREACH_ARRAY_ITEM (params, itema, para) {
    VeriExpression *expr = para->GetInitialValue();
    //VeriConst *exprC = expr->ConstCast();

    char * val = expr->Image();
    addNewItem(new UpdateParam(pathName(), para->Name(), val) );
    Strings::free(val);
  }
}


int EditCompileP1::visit(class AddProbe *pr)
{
  m_probelist.push_back (pr->getKey());
  return 0;
}
int EditCompileP1::visit(class AddCosim *pr)
{
  m_probelist.push_back (pr->getKey());
  return 0;
}
int EditCompileP1::visit(class AddCapture *pr)
{
  m_probelist.push_back (pr->getKey());
  return 0;
}
int EditCompileP1::visit(class AddTrigger *pr)
{
  m_probelist.push_back (pr->getKey());
  return 0;
}
int EditCompileP1::visit(class ProbedInstance *pr)
{
  m_probelist.push_back (pr->getKey());
  return 0;
}

int EditCompileP1::visit(class CosimFill *pr)
{
  VeriModule *mod = getMasterModule();
  const char * modname = mod->GetOriginalModuleName();
  if (0 == modname) modname = mod->Name();

  BString new_name;

  if (isTopInst()) {
    new_name  = modname;
    //    new_name += "_COSIM";
  } else {
    new_name  = modname;
    // new_name += "_COSIM_";
    // new_name += itoa(pr->getKey());
  }

  if (!isTopInst()) {
    ParameterList plist;
    ModuleTerminalList mtlist;
    mtlist.push_back (ModuleTerminal ("CLK", "1'b0"));   // matches Empty.v
    mtlist.push_back (ModuleTerminal ("RST_N", "1'b0")); // matches Empty.v

    BString upInst, thisInst;
    splitInstanceName (pathName(), upInst, thisInst );

    addNewItem(new AddInstance(upInst, thisInst, new_name, plist, mtlist, 0),
	       CosimMods);
  }
  return 0;
}

///////////////////////////////////////////////////////////////
///  Pass 2
///////////////////////////////////////////////////////////////

EditCompileP2::EditCompileP2(const BString & inst)
  : EditCompileBase(inst)
  , m_addedPorts()
  , m_scanmap()
{
}


const AddedPortList & EditCompileP2::getAddedPorts() const
{
  return m_addedPorts;
}

// Port names are in a separate scope use a new look up
const BString & EditCompileP2::genPortName (const BString & portName, bool broadcast, unsigned int width)
{
  BString actualName(portName);
  // TODO XXX check the portName is unique in the port list
  m_addedPorts.push_back (PushedPort(portName, actualName, broadcast, width));
  return m_addedPorts.back().m_uniqPortName;
}

// Core processing function
int EditCompileP2::process(ModInstSetPairIter_t & range)
{

  bool probe_mods = false;
  bool cosim_mods = false;

  // Visit all modification at this instance
  // TODO  check return status and error out...
  int status = 0;
  for (ModInstSetIter_t iter = range.first; iter != range.second && status == 0; ++ iter ) {
    status = (*iter)->accept (this);
    if ((*iter)->getModSet() == ProbeMods) { probe_mods = true; }
    if ((*iter)->getModSet() == CosimMods) { cosim_mods = true; }
  }
  for (ScanMap_t::iterator iter = m_scanmap.begin(); iter != m_scanmap.end(); ++iter) {
    ScanData* data = (*iter).second;
    if (!data->isFirstScan()) {
      addNewItem (data->getLastAssign());
    }
  }

  // "/" pathname signifies a nonactive CktMod for pass2
  //  as in AddBsvProbes CktMod
  if ((status == 0) && (pathName() != "/")){
    // Once processed, we need to change the module name
    char buf[256];
    VeriModule *mod = getMasterModule();
    const char * modname = mod->GetOriginalModuleName();
    if (0 == modname) modname = mod->Name();

    if (probe_mods) {
      if (isTopInst()) {
	snprintf (buf, 256, "%s_EDITED", modname );
	addNewItem (new ChangeModuleName (pathName(), buf, 0));
      } else {
	snprintf (buf, 256, "%s_EDITED", modname );
	addNewItem (new ChangeModuleName (pathName(), buf, (*range.first)->getKey()));
      }

      // Mark the upinst as changed as well
      if (!isTopInst()) {
	BString upInst, thisInst;
	splitInstanceName (pathName(), upInst, thisInst );
	addNewItem (new ChangeInstModuleName(upInst, thisInst, buf, (*range.first)->getKey()));
      }
    }

    if (cosim_mods) {
      if (isTopInst()) {
	snprintf (buf, 256, "%s_COSIM", modname );
	addNewItem (new ChangeModuleName (pathName(), buf, 0), CosimMods);
      } else {
	snprintf (buf, 256, "%s_COSIM", modname );
	addNewItem (new ChangeModuleName (pathName(), buf, (*range.first)->getKey()), CosimMods);
      }

      // Mark the upinst as changed as well
      if (!isTopInst()) {
	BString upInst, thisInst;
	splitInstanceName (pathName(), upInst, thisInst );
	addNewItem (new ChangeInstModuleName(upInst, thisInst, buf, (*range.first)->getKey(), true), CosimMods);
      }
    }
  }

  return status ;
}


////////////////////////
// Probe modules such as ProbeMux and ProbeValue
// share several braodcast signals.  This function add to that list
void EditCompileP2::addBroadcastSignalsToProbeModule (ModuleTerminalList & mtlist, bool include_commands, bool include_timer, bool include_data_down)
{

  std::list<const char*> broadcast_list;
  broadcast_list.push_front("UCLK");
  broadcast_list.push_front("URST");
  if (include_commands) {
    broadcast_list.push_front("CMDEN");
    broadcast_list.push_front("CMD");
  }
  if (include_timer) {
    broadcast_list.push_front("CTIMER");
  }
  if (include_data_down) {
    //    broadcast_list.push_front("DATADOWN");
    //    broadcast_list.push_front("DD_EN");
  }

  while (broadcast_list.size() > 0) {
    const char*  label = broadcast_list.back();
    BString tmp = "BPROBE_";
    tmp += label;
    const BString & id = getSharedIdentifier (tmp);
    mtlist.push_back (ModuleTerminal (label, id));
    broadcast_list.pop_back();
  }
}

// Instance specific data from Probe and probemux.  The data lane, ack and delay signals
void EditCompileP2::addBUpLinkSignalsToProbeModule (ModuleTerminalList & mtlist, unsigned int key,
                                                    bool createNets, const BString & portPostfix)
{
  char buf[32];
  snprintf(buf, 32, "BPROBE_DELAY_%d", key);
  const BString & delay = getSharedIdentifier (buf);
  mtlist.push_back(ModuleTerminal ("DELAY" + portPostfix, delay ));

  snprintf(buf, 32, "BPROBE_DATAUP_%d", key);
  const BString & dataup = getSharedIdentifier (buf);
  mtlist.push_back(ModuleTerminal ("DATAUP"  + portPostfix, dataup ));

  snprintf(buf, 32, "BPROBE_DATAVALID_%d", key);
  const BString & datavalid = getSharedIdentifier (buf);
  mtlist.push_back(ModuleTerminal ("DATAVALID"  + portPostfix, datavalid));

  snprintf(buf, 32, "BPROBE_ACKIN_%d", key);
  const BString & ackin = getSharedIdentifier (buf);
  mtlist.push_back(ModuleTerminal ("ACK"  + portPostfix, ackin ));

  if (createNets) {
    // Add Wire defs
    addNewItem (new AddNet (pathName(), delay, 1));
    addNewItem (new AddNet (pathName(), dataup, dataWidth));
    addNewItem (new AddNet (pathName(), datavalid, 1));
    addNewItem (new AddNet (pathName(), ackin, 1));
  }
}

int EditCompileP2::visit(class AddProbe *cm) {
  // Adds the probemodule instance and wires.

  ParameterList plist;
  unsigned int width = cm->getWidth();
  plist.push_back (Parameter ("ProbeId", cm->getKey()));
  plist.push_back (Parameter ("ProbeWidth", width));


  // These are the port connections for the probe module
  ModuleTerminalList mtlist;
  addBroadcastSignalsToProbeModule(mtlist, true, false, false);
  addBUpLinkSignalsToProbeModule(mtlist, cm->getKey(), true, "");

  BStringList prsigs;
  cm->getSignalNames(prsigs);
  BString prconcat;
  buildConcatString(prsigs, prconcat);
  mtlist.push_back (ModuleTerminal ("CLK", cm->getClock()) );
  mtlist.push_back (ModuleTerminal ("PROBEIN", prconcat));
  mtlist.push_back (ModuleTerminal ("PROBEEN", cm->getEnable()) );


  BString iName = getUniqueIdentifier (cm->getName());
  addIdentifier (iName);

  CktMod *pnew = new AddInstance (pathName(), iName, "ProbeValue", plist, mtlist);
  addNewItem (pnew);

  return 0;
}

int EditCompileP2::visit(class AddCosim *cm) {

  ParameterList plist;
  plist.push_back (Parameter ("id", cm->getKey()));

  CosimFlavor flavor = cm->getFlavor();

  // These are the port connections for the probe module
  ModuleTerminalList mtlist;
  addBroadcastSignalsToProbeModule(mtlist, true, false, true);
  addBUpLinkSignalsToProbeModule(mtlist, cm->getKey(), true, "");

  mtlist.push_back (ModuleTerminal ("CLK", cm->getClock()));
  mtlist.push_back (ModuleTerminal ("TRIGGER", cm->getTrigger()));

  BString iName = getUniqueIdentifier (cm->getName(), true);

  if (flavor == Observe) {
    int status = 0;
    ScanDataList* datalist = cm->getData();
    bool first = true;
    for (ScanDataList::iterator iter = (*datalist).begin(); iter != (*datalist).end(); ++iter) {
      ScanData & data = getShared(**iter);
      if (first) {
	status = status || addScanNets(data, mtlist, true);
      }

      status = status || addScanNets(data, mtlist);

      plist.push_back (Parameter (data.mkChainId("length"), data.getLength()));

      first = false;
    }
    if (status == 0) {
      CktMod *pnew = new AddInstance (pathName(), iName, "cosimObserve", plist, mtlist);
      addNewItem (pnew);
    }
    return status;
  }
  if (flavor == Replace) {
    int status = 0;
    ScanDataList* datalist = cm->getData();
    bool first = true;
    for (ScanDataList::iterator iter = (*datalist).begin(); iter != (*datalist).end(); ++iter) {
      ScanData & data = getShared(**iter);
      if (first) {
	status = status || addScanNets(data, mtlist, true);
      }

      status = status || addScanNets(data, mtlist);

      plist.push_back (Parameter (data.mkChainId("length"), data.getLength()));

      first = false;
    }
    if (status == 0) {
      CktMod *pnew = new AddInstance (pathName(), iName, "cosimReplace", plist, mtlist);
      addNewItem (pnew);
    }
    return status;
  }

  setError("Unhandled Cosim Flavor");

  return 1;
}

int EditCompileP2::visit(class AddScan *pr)
{

  bool top = pr->scanTop();
  tIdList input_ids  = pr->getInputIds();
  tIdList output_ids = pr->getOutputIds();

  ModuleTerminalList mtlist;
  ParameterList plist;

  ScanData & data = getShared(pr->getData());

  if (!top) {
    addScanPorts(data, mtlist);
  } else {
    addScanNets (data, mtlist, false);
  }

  if (input_ids.empty() && !top) {
    BString upInst, thisInst;
    splitInstanceName (pathName(), upInst, thisInst );
    CktMod *p = new ScannedInstance (upInst, data, thisInst);
    addNewItem(p);
    return 0;
  }

  if (!top) {
    BString upInst, thisInst;
    splitInstanceName (pathName(), upInst, thisInst );
    CktMod *p = new ScannedInstance (upInst, data, thisInst);
    addNewItem(p);

  }

  VeriModule* module = getMasterModule();
  VeriExpression* concat_expr = NULL;

  ScanPath* path_local = new ScanPath();
  if (!input_ids.empty()) {
    path_local = new ScanPath();

    concat_expr = HdlUtils::createConcatExpression(module, input_ids, &path_local);
    if (concat_expr) {
      BString concat = HdlUtils::getImage(concat_expr);
      mtlist.push_back (ModuleTerminal ("D_IN", concat));
    }
  }

  if (!output_ids.empty()) {
    path_local = new ScanPath();
    concat_expr = HdlUtils::createConcatExpression(module, output_ids, &path_local);
    if (concat_expr) {
      BString concat = HdlUtils::getImage(concat_expr);
      mtlist.push_back (ModuleTerminal ("D_OUT", concat));
    }
  }

  // The width of the concat expression is the same as the fixed part of the path length
  unsigned int width = path_local->Length(true); 
  plist.push_back (Parameter ("SCAN_WIDTH", data.getWidth()));
  plist.push_back (Parameter ("width", width));

  if (data.isFirstScan()) {

    const BString & in_net = getSharedIdentifier(data.mkId("BSCAN_IN"));
    mtlist.push_back (ModuleTerminal ("SCAN_IN", in_net));

  } else {

    const BString & in_net = getUniqueIdentifier(data.mkId("_SCAN"), true);
    addNet(in_net, data.getWidth());
    mtlist.push_back (ModuleTerminal ("SCAN_IN", in_net));

  }

  const BString & out_port = getSharedIdentifier(data.mkId("BSCAN_OUT"));

  if (concat_expr) {

    const BString & out_net = getUniqueIdentifier(data.mkId("_SCAN"), false);
    addNet(out_net, data.getWidth());
    mtlist.push_back (ModuleTerminal ("SCAN_OUT", out_net));

    if (data.getFlavor() == Outputs) {

      const BString & iName = getUniqueIdentifier(data.mkId("scan_in"), true);
      CktMod *pnew = new AddInstance (pathName(), iName, "ScanIn", plist, mtlist);
      addNewItem (pnew);

    } else {

      const BString & iName = getUniqueIdentifier(data.mkId("sample_reg"), true);
      CktMod *pnew = new AddInstance (pathName(), iName, "SampleReg", plist, mtlist);
      addNewItem (pnew);

    }

    AddSimpleAssign* assign = new AddSimpleAssign (pathName(), out_port, out_net);
    data.setLastAssign(assign);

  } else {

    AddSimpleAssign* assign = new AddSimpleAssign (pathName(), out_port, mtlist.back().m_netName);
    data.setLastAssign(assign);
  }

  return 0;
}

////////////////////////
int EditCompileP2::visit(class AddCapture *cm) {
  // Adds the probemodule instance and wires.

  ParameterList plist;
  plist.push_back (Parameter ("ProbeId",      cm->getKey()));
  plist.push_back (Parameter ("ProbeWidth",   cm->getWidth() ));
  plist.push_back (Parameter ("MemSize",      cm->getDepth() ));
  plist.push_back (Parameter ("RunLenWidth",  cm->getRunWidth() ));
  plist.push_back (Parameter ("TriggerToDump",  cm->getDumpDelay() ));

  unsigned int addrWidth = (unsigned int) ceil(log2((float) cm->getDepth() ));
  plist.push_back (Parameter ("MemAddrWidth",  addrWidth ));

  // These are the port connections for the probe module
  ModuleTerminalList mtlist;
  addBroadcastSignalsToProbeModule(mtlist, true, true, false);
  addBUpLinkSignalsToProbeModule(mtlist, cm->getKey(), true, "");

  BStringList prsigs;
  cm->getSignalNames(prsigs);
  BString prconcat;
  buildConcatString(prsigs, prconcat);
  mtlist.push_back (ModuleTerminal ("CLK", cm->getClock()) );
  mtlist.push_back (ModuleTerminal ("PROBEIN", prconcat));
  mtlist.push_back (ModuleTerminal ("PROBEEN", cm->getEnable()) );
  mtlist.push_back (ModuleTerminal ("TRIGGER", cm->getTrigger()) );


  BString iName = getUniqueIdentifier (cm->getName());
  addIdentifier (iName);

  CktMod *pnew = new AddInstance (pathName(), iName, "ProbeCapture", plist, mtlist);
  addNewItem (pnew);

  return 0;
}

////////////////////////
int EditCompileP2::visit(class AddTrigger *cm) {
  // Adds the probemodule instance and wires.

  ParameterList plist;
  plist.push_back (Parameter ("TriggerId", cm->getKey()));

  const UIntSet keys = cm->getCaptureKeys();
  plist.push_back (Parameter ("NumWords", keys.size() ));

  BString captureids;
  char buf[32];
  snprintf(buf, 32, "%d'h", 16 * (int)keys.size());
  captureids = buf;
  bool first = true;
  for (UIntSet::const_iterator iter = keys.begin(); iter != keys.end(); ++iter) {
    const char *fmt = first ? "%04x" : "_%04x";
    snprintf(buf, 32, fmt, *iter);
    captureids += buf;
    first = false;
  }
  plist.push_back (Parameter ("CaptureIds", captureids));

  int cwidth = 1 +  ((int) log2(keys.size()));
  plist.push_back (Parameter ("CWidth", cwidth));


  // These are the port connections for the probe module
  ModuleTerminalList mtlist;
  addBroadcastSignalsToProbeModule(mtlist, true, false, false);
  addBUpLinkSignalsToProbeModule(mtlist, cm->getKey(), true, "");


  mtlist.push_back (ModuleTerminal ("CLK", cm->getClock()) );
  mtlist.push_back (ModuleTerminal ("TRIGGER", cm->getExpr()));


  BString iName = getUniqueIdentifier (cm->getName());
  addIdentifier (iName);

  CktMod *pnew = new AddInstance (pathName(), iName, "ProbeTrigger", plist, mtlist);
  addNewItem (pnew);

  return 0;
}

////////////////////////
int EditCompileP2::visit(class ProbeMux *cm) {
  // Adds a  probeMux instance and wires.

  char buf[32];
  ParameterList plist;          // empty

  ModuleTerminalList mtlist;
  // These are the port connections for the probe module
  addBroadcastSignalsToProbeModule (mtlist, false, false, false);
  addBUpLinkSignalsToProbeModule (mtlist, cm->getKey(), true, "");

  addBUpLinkSignalsToProbeModule (mtlist, cm->getSrc1(), false, "A");
  addBUpLinkSignalsToProbeModule (mtlist, cm->getSrc2(), false, "B");

  // instance name
  snprintf(buf, 32, "ProbeMux_%d", cm->getKey());
  const BString & newInstName = getUniqueIdentifier (buf);
  addNewItem (new AddInstance (pathName(), newInstName, "ProbeMux", plist, mtlist));

  return 0;
}

////////////////////////
int EditCompileP2::visit(class ProbeBus *cm) {

  typedef std::pair<const BString, const unsigned int> tPInfo;
  char buf[32];
  unsigned int key = cm->getSrc();

  std::list<tPInfo> in_shared;
  in_shared.push_front(tPInfo("BPROBE_UCLK", 1));
  in_shared.push_front(tPInfo("BPROBE_URST", 1));
  in_shared.push_front(tPInfo("BPROBE_CMDEN", 1));
  in_shared.push_front(tPInfo("BPROBE_CMD", cmdWidth));
  in_shared.push_front(tPInfo("BPROBE_CTIMER", 1));
  //  in_shared.push_front(tPInfo("BPROBE_DD_EN", 1));
  //  in_shared.push_front(tPInfo("BPROBE_DATADOWN", dataWidth));

  while (in_shared.size() > 0) {
    tPInfo pr = in_shared.back();
    const BString & id  = getSharedIdentifier (pr.first);
    const BString & idP = genPortName (pr.first, true, pr.second);
    addNewItem (new AddPort (pathName(), idP, d_input, pr.second));
    addNewItem (new AddNet  (pathName(), id, pr.second, true));
    if (idP != id) addNewItem (new AddSimpleAssign (pathName(), id, idP));
    in_shared.pop_back();
  }

  std::list<tPInfo> in_list;
  in_list.push_front(tPInfo("BPROBE_ACKIN", 1));
//  in_list.push_front(tPInfo("BPROBE_DATADOWN", dataWidth));

  while (in_list.size() > 0) {
    tPInfo pr = in_list.back();
    BString pat = pr.first + "_%d";
    snprintf(buf, 32, pat.c_str(), key);
    const BString & id = getSharedIdentifier (buf);

    const BString & idP  = genPortName (pr.first, false, pr.second);
    addNewItem (new AddPort (pathName(), idP, d_input, pr.second));
    addNewItem (new AddSimpleAssign (pathName(), id, idP));
    in_list.pop_back();
  }

  std::list<tPInfo> out_list;
  out_list.push_front(tPInfo("BPROBE_DELAY", 1));
  out_list.push_front(tPInfo("BPROBE_DATAUP", dataWidth));
  out_list.push_front(tPInfo("BPROBE_DATAVALID", 1));

  while (out_list.size() > 0) {
    tPInfo pr = out_list.back();
    BString pat = pr.first + "_%d";
    snprintf(buf, 32, pat.c_str(), key);
    const BString & id = getSharedIdentifier (buf);

    const BString & idP  = genPortName (pr.first, false, pr.second);
    addNewItem (new AddPort (pathName(), idP, d_output, pr.second));
    addNewItem (new AddSimpleAssign (pathName(), idP, id));
    out_list.pop_back();
  }



  // Let the calling module know what is coming....
  if (!isProbeSink()) {
    BString upInst, thisInst;
    splitInstanceName (pathName(), upInst, thisInst );
    if (upInst != "")
      addNewItem (new ProbedInstance (upInst, thisInst, getAddedPorts() ));
  }

  return 0;
}
int EditCompileP2::visit(class ProbedInstance *cm)
{
  // Need to add the port connection here. -- wire names,etc??
  // Probed instance was added from lower module
  char buf[128];

  const AddedPortList &ports = cm->getAddedPorts();
  for (AddedPortList::const_iterator iter = ports.begin(); iter != ports.end(); ++ iter) {
    const PushedPort &prt = *iter;
    if (prt.m_broadcast) {
      const BString & net = getSharedIdentifier (prt.m_portName);
      addNewItem (new AddInstanceConnection (pathName(), cm->getContainingInst(), prt.m_uniqPortName, net));
    }
    else {
      // Create the connection and the net
      snprintf(buf, 128, "%s_%d", prt.m_portName.c_str(), cm->getKey());
      const BString & net = getSharedIdentifier (buf);
      addNewItem (new AddInstanceConnection (pathName(), cm->getContainingInst(), prt.m_uniqPortName, net));
      addNewItem (new AddNet (pathName(), net, prt.m_width ));
    }

  }

  return 0;
}

int EditCompileP2::visit(class ScannedInstance *cm)
{

  ScanData & data_child = cm->getData();
  ScanData & data = getShared(cm->getData());

  const AddedPortList &ports = data_child.getAddedPorts();
  for (AddedPortList::const_iterator iter = ports.begin(); iter != ports.end(); ++ iter) {
    const PushedPort &prt = *iter;
    if (prt.m_broadcast) {
      const BString & net = getSharedIdentifier (prt.m_portName);
      addNewItem (new AddInstanceConnection (pathName(), cm->getContainingInst(), prt.m_uniqPortName, net));
    }
  }

  const BString & label_in = data_child.mkId("BSCAN_IN");

  const AddedPortList& in_portl = getPushedPort(ports, label_in);
  if (in_portl.empty()) {
    setError("BSCAN_IN port not found");
    return 1;
  }

  if (data.isFirstScan()) {
    const PushedPort& port = in_portl.front();
    const BString & net = getSharedIdentifier(label_in);
    addNewItem (new AddInstanceConnection (pathName(), cm->getContainingInst(), port.m_uniqPortName, net));
  } else {
    const PushedPort& port = in_portl.front();
    const BString & net = getUniqueIdentifier(data.mkId("_SCAN"), true);
    addNet(net, data.getWidth());
    addNewItem (new AddInstanceConnection (pathName(), cm->getContainingInst(), port.m_uniqPortName, net));
  }

  const BString & label_out = data_child.mkId("BSCAN_OUT");

  const AddedPortList& out_portl = getPushedPort(ports, label_out);
  if (out_portl.empty()) {
    setError("BSCAN_OUT port not found");
    return 1;
  }

  const PushedPort& port = out_portl.front();
  const BString & net = getUniqueIdentifier(data.mkId("_SCAN"), false);
  addNet(net, data.getWidth());
  addNewItem (new AddInstanceConnection (pathName(), cm->getContainingInst(), port.m_uniqPortName, net));

  const BString & out_port = getSharedIdentifier(label_out);
  AddSimpleAssign* assign = new AddSimpleAssign (pathName(), out_port, net);
  data.setLastAssign(assign);

  return 0;
}

int EditCompileP2::visit(class ProbeConnection *cm)
{
  // Order based on declaration in Verilog library
  static const char *hookPortOrder[] = 
    {"UCLK", "URST", "ACK", "DATAUP", "DATAVALID", "DELAY", "CMDEN", "CMD", "CTIMER", 0 };
  //    {"UCLK", "URST", "ACK", "DATAUP", "DATAVALID", "DELAY", "CMDEN", "CMD", "CTIMER", "DATADOWN", "DD_EN", 0 };

  VeriModule *mod = getMasterModule();
  unsigned int src = cm->getSrc() ;
  int status = 0;

  std::list<VeriInstId *> insts;
  unsigned int found = HdlUtils::findInstsByMasterName (mod, "ProbeHook", insts);
  if (found != 1) {
    setError ("Could not find an instance of module ProbeHook in top instance");
    return 1;
  }
  VeriInstId * thehook = insts.front();

  // We'll need to remove this instance here.
  BString hookinstname(thehook->InstName());
  addNewItem (new RmInstance (pathName(), hookinstname));

  // TODO Check that range is null
  Array * portConnects = thehook->GetPortConnects();

  if (thehook->HasNamedPortAssoc()) {
    unsigned int it_a;
    VeriExpression *expr;
    FOREACH_ARRAY_ITEM (portConnects, it_a, expr ) {
      VeriExpression *conn = expr->GetConnection();
      const char * formal = expr->NamedFormal();
      status = probeConnectionHelper (formal, conn, src);
      if (status != 0) {
        BString errMsg = "Could not reconnect port " ;
        errMsg += formal ;
        errMsg += " on ProbeHook module" ;
        setError (errMsg);
        break;
      }
    }
  }
  else if (thehook->HasOrderedPortAssoc()) {
    unsigned int it_a;
    VeriExpression *expr;
    FOREACH_ARRAY_ITEM (portConnects, it_a, expr ) {
      VeriExpression *conn = expr->GetConnection();
      const char * formal = hookPortOrder[it_a];
      status = probeConnectionHelper (formal, conn, src);

      if (status != 0) {
        BString errMsg = "Could not reconnect port " ;
        errMsg += formal ;
        errMsg += " on ProbeHook module" ;
        setError (errMsg);
        break;
      }
    }
  }
  else {
    cerr << "Assert EditCompileP2::visit(class ProbeConnection *cm)" << endl;
    status = 1;
  }

  return status;
}


int EditCompileP2::probeConnectionHelper ( const char * formal, const VeriExpression *conn, unsigned int key)
{
  char buf[32];

  VeriName *vname = conn->NameCast();
  if (vname == 0) {
    return 1;
  }
  const char *connName = vname->GetName();

  if (0 == strcmp("UCLK", formal)) {
    const BString & net = getSharedIdentifier ("BPROBE_UCLK");
    addNewItem (new AddNet(pathName(), net, 1));
    addNewItem (new AddSimpleAssign(pathName(), net, connName));
  }

  else if (0 == strcmp("URST", formal)) {
    const BString & net = getSharedIdentifier ("BPROBE_URST");
    addNewItem (new AddNet(pathName(), net, 1));
    addNewItem (new AddSimpleAssign(pathName(), net, connName));

  }
  else if (0 == strcmp("CMDEN", formal)) {
    const BString & net = getSharedIdentifier ("BPROBE_CMDEN");
    addNewItem (new AddNet(pathName(), net, 1));
    addNewItem (new AddSimpleAssign(pathName(), net, connName));

  }
  else if (0 == strcmp("CMD", formal)) {
    const BString & net = getSharedIdentifier ("BPROBE_CMD");
    addNewItem (new AddNet(pathName(), net, cmdWidth));
    addNewItem (new AddSimpleAssign(pathName(), net, connName));
  }
  else if (0 == strcmp("CTIMER", formal)) {
    const BString & net = getSharedIdentifier ("BPROBE_CTIMER");
    addNewItem (new AddNet(pathName(), net, 1));
    addNewItem (new AddSimpleAssign(pathName(), net, connName));
  }

  // Now the data up signals -- uniquely names, and already defined.
  else if (0 == strcmp("ACK", formal)) {
    snprintf(buf, 32, "BPROBE_ACKIN_%d", key);
    const BString & net = getSharedIdentifier (buf);
    addNewItem (new AddSimpleAssign(pathName(), net, connName));
  }

  else if (0 == strcmp("DATAUP", formal)) {
    snprintf(buf, 32, "BPROBE_DATAUP_%d", key);
    const BString & net = getSharedIdentifier (buf);
    addNewItem (new AddSimpleAssign(pathName(), connName, net));
 }

  else if (0 == strcmp("DATAVALID", formal)) {
    snprintf(buf, 32, "BPROBE_DATAVALID_%d", key);
    const BString & net = getSharedIdentifier (buf);
    addNewItem (new AddSimpleAssign(pathName(), connName, net));
 }

  else if (0 == strcmp("DELAY", formal)) {
    snprintf(buf, 32, "BPROBE_DELAY_%d", key);
    const BString & net = getSharedIdentifier (buf);
    addNewItem (new AddSimpleAssign(pathName(), connName, net));
  }
  else if (0 == strcmp("DD_EN", formal)) {
    const BString & net = getSharedIdentifier ("BPROBE_DD_EN");
    addNewItem (new AddNet(pathName(), net, 1));
    addNewItem (new AddSimpleAssign(pathName(), net, connName));
  }
  else if (0 == strcmp("DATADOWN", formal)) {
    const BString & net = getSharedIdentifier ("BPROBE_DATADOWN");
    addNewItem (new AddNet(pathName(), net, dataWidth));
    addNewItem (new AddSimpleAssign(pathName(), net, connName));
  }
  else {
    return 1;
  }
  return 0;
}

// XXX TODO more design effort is needed.
// Where do signals connect in the down path?
// Application needed...
int EditCompileP2::visit(class DrawInSignal *cm)
{
  if (cm->getDownPath() != cm->getTopPath()) {
    unsigned width = cm->getWidth();
    const BString &portName = cm->getPortName();
    addNewItem (new AddPort (pathName(), portName, d_input, width) );
    addNewItem (new AddNet (pathName(), portName, width) );
    if (portName != cm->getSignal()) {
      addNewItem (new AddSimpleAssign(pathName(), cm->getSignal(), portName ));
    }

    if (!isTopInst()){
      // Change the port connects going up
      BString upPath, thisInst;
      splitInstanceName (pathName(), upPath, thisInst );
      //BString newportName = thisInst + "_" + portName;
      BString newportName = portName;
      addNewItem (new AddInstanceConnection (upPath, thisInst, newportName, newportName ) );
      addNewItem (new DrawInSignal (upPath, cm->getTopPath(), newportName, newportName, width ) );
    }
  }
  return 0;
}

int EditCompileP2::visit(class DrawOutSignal *cm)
{

  if (cm->getDownPath() != cm->getTopPath()) {
    unsigned width = cm->getWidth();
    const BString &portName = cm->getPortName();
    addNewItem (new AddPort (pathName(), portName, d_output, width) );
    addNewItem (new AddNet (pathName(), portName, width) );
    if (portName != cm->getSignal()) {
      addNewItem (new AddSimpleAssign(pathName(), portName, cm->getSignal() ));
    }

    if (!isTopInst()){
      // Change the port connects going up
      BString upPath, thisInst;
      splitInstanceName (pathName(), upPath, thisInst );
      //BString newportName = thisInst + "_" + portName;
      BString newportName = portName;
      addNewItem (new AddInstanceConnection (upPath, thisInst, newportName, newportName ) );
      addNewItem (new DrawOutSignal (upPath, cm->getTopPath(), newportName, newportName, width ) );
    }
  }
  return 0;
}

////////////////////////////////////////////////////////////////////////////////
/// addScanPorts (used for all AddScan modifications but the top one)
////////////////////////////////////////////////////////////////////////////////

int EditCompileP2::addScanPorts (ScanData & data, ModuleTerminalList & mtlist)
{

  unsigned width = data.getWidth();
  bool added;

  // mtlist is for SampleReg
  const BString & uclk = getSharedIdentifier("BSCAN_UCLK", true);
  added = addNet(uclk, 1);

  if (added) {
    const BString &uclkP = data.genPortName ("BSCAN_UCLK", true, 1);
    added = addNet(uclkP, 1);
    addNewItem (new AddPort (pathName(), uclkP, d_input, 1));
    if (uclkP != uclk) addNewItem (new AddSimpleAssign (pathName(), uclk, uclkP));
  }
  mtlist.push_back (ModuleTerminal ("CLK", uclk));

  const BString & label_any = data.mkProbeId("BSCAN_ANY");
  const BString & scan_any = getSharedIdentifier(label_any, true);
  added = addNet(scan_any, 1);

  if (added) {
    const BString &scan_anyP = data.genPortName (label_any, true, 1);
    addNet(scan_anyP, 1);
    addNewItem (new AddPort (pathName(), scan_anyP, d_input, 1));
    if (scan_anyP != scan_any) addNewItem (new AddSimpleAssign (pathName(), scan_any, scan_anyP));
  }
  mtlist.push_back (ModuleTerminal ("SCAN_ANY", scan_any));

  const BString & label_mode = data.mkId("BSCAN_MODE");

  const BString & scan_mode = getSharedIdentifier(label_mode, true);
  addNet(scan_mode, 1);
  const BString &scan_modeP = data.genPortName (label_mode, true, 1);
  addNet(scan_modeP, 1);
  addNewItem (new AddPort (pathName(), scan_modeP, d_input, 1));
  if (scan_modeP != scan_mode) addNewItem (new AddSimpleAssign (pathName(), scan_mode, scan_modeP));
  mtlist.push_back (ModuleTerminal ("SCAN_MODE", scan_mode));

  const BString & label_in = data.mkId("BSCAN_IN");

  const BString & scan_in  = getSharedIdentifier(label_in, true);
  addNet(scan_in, width);
  const BString & scan_inP = data.genPortName (label_in, false, width);
  addNet(scan_inP, width);
  addNewItem (new AddPort (pathName(), scan_inP, d_input, width));
  if (scan_inP != scan_in) addNewItem (new AddSimpleAssign (pathName(), scan_in, scan_inP));

  const BString & label_out = data.mkId("BSCAN_OUT");

  const BString & scan_out  = getSharedIdentifier(label_out, true);
  addNet(scan_out, width);
  const BString & scan_outP = data.genPortName (label_out, false, width);
  addNet(scan_outP, width);
  addNewItem (new AddPort (pathName(), scan_outP, d_output, width));
  if (scan_outP != scan_out) addNewItem (new AddSimpleAssign (pathName(), scan_out, scan_outP));

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
/// addScanNets (used only for the top AddScan modification, as well
/// as for AddCosim)
////////////////////////////////////////////////////////////////////////////////


int EditCompileP2::addScanNets (ScanData & data, ModuleTerminalList & mtlist, bool for_cosim)
{

  bool added;

  if (!for_cosim) {

    // mtlist is for top SampleReg;
    const BString & probe_uclk = getSharedIdentifier("BPROBE_UCLK");
    const BString & uclk       = getSharedIdentifier("BSCAN_UCLK");
    added = addNet(uclk, 1);
    if (added) {
      addNewItem (new AddSimpleAssign (pathName(), uclk, probe_uclk));
    }
    mtlist.push_back (ModuleTerminal ("CLK", uclk));

    const BString & label_mode = data.mkId("BSCAN_MODE");
    const BString & scan_mode = getSharedIdentifier(label_mode);
    addNet(scan_mode, 1);
    mtlist.push_back (ModuleTerminal ("SCAN_MODE", scan_mode));

    const BString & label_any = data.mkProbeId("BSCAN_ANY");

    const BString & scan_any = getSharedIdentifier(label_any);
    added = addNet(scan_any, 1);
    mtlist.push_back (ModuleTerminal ("SCAN_ANY", scan_any));

    return 0;

  } else {

    const BString & label_any = data.mkProbeId("BSCAN_ANY");

    // mtlist is for cosimObserve
    const BString & scan_any = getSharedIdentifier(label_any);
    added = addNet(scan_any, 1);
    if (added) {
      mtlist.push_back (ModuleTerminal ("SCAN_ANY", scan_any));
    }
    return 0;
  }
}

int EditCompileP2::addScanNets (ScanData & data, ModuleTerminalList & mtlist)
{

  const char* prefix = "";
  ScanFlavor flavor = data.getFlavor();

  if (flavor == State) {
    prefix = "ST_";
  }

  if (flavor == Outputs) {
    prefix = "OUT_";
  }

  if (flavor == Inputs) {
    prefix = "IN_";
  }

  if (flavor == InputsBB) {
    prefix = "IN_";
  }

  unsigned width = data.getWidth();
  BString formal;

  // mtlist is for cosimObserve/cosimReplace
  const BString & label_mode = data.mkId("BSCAN_MODE");
  const BString & scan_mode = getSharedIdentifier(label_mode);
  addNet(scan_mode, 1);
  formal = prefix;
  formal += "SCAN_MODE";
  mtlist.push_back (ModuleTerminal (formal, scan_mode));

  const BString & label_in = data.mkId("BSCAN_IN");
  const BString & scan_in = getSharedIdentifier(label_in);
  addNet(scan_in, width);
  formal = prefix;
  formal += "SCAN_IN";
  mtlist.push_back (ModuleTerminal (formal, scan_in));

  const BString & label_out = data.mkId("BSCAN_OUT");
  const BString & scan_out = getSharedIdentifier(label_out);
  addNet(scan_out, width);
  formal = prefix;
  formal += "SCAN_OUT";
  mtlist.push_back (ModuleTerminal (formal, scan_out));

  return 0;
}


ScanData & EditCompileP2::getShared(ScanData & data)
{

  ScanMap_t::iterator f = m_scanmap.find (data.getId());
  if (f != m_scanmap.end() ) {
//    printf("HIT %s %d\n", pathName().c_str(), data.getId());
    return (*(*f).second) ;
  }
//  printf("MISS %s %d\n", pathName().c_str(), data.getId());
  ScanData* data_new = new ScanData(data);
  pair<ScanMap_t::iterator,bool> iloc = m_scanmap.insert ( pair<unsigned int,ScanData*> (data.getId(), data_new));
  ScanData & ret = *iloc.first->second;
  return ret;

}
