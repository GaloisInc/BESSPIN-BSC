// Copyright 2010 Bluespec Inc. All rights reserved
#include <string.h>
#include <math.h>
#include <iostream>
#include <sstream>
#include <set>

#include "EditCompile.h"
#include "HdlUtils.h"
#include "veri_tokens.h"


using namespace std ;
using namespace Verific ;

void toLowerCase(std::string &str);

class EditCompileP0 : public EditCompileBase {
private:

  static unsigned int GenProbeNameIndex;

public:
  EditCompileP0 (const BString &);
  virtual int process(ModInstSetPairIter_t &);
  virtual int novisitor(class CktMod *cm) { return 0;}
  int addScanMods(Verific::VeriModule* module, ScanData & data, const BString & path, ScanPath** scan_path, const bool & top,  BStringSet & uclocks = *(new BStringSet()));
  int addScanMod (Verific::VeriModule* module, ScanData & data, const BString & path, ScanPath** scan_path, const bool & top,  BStringSet & uclocks = *(new BStringSet()));
  int addCosimFillMods(BString pathName, bool first);
  tIdSet getStateIds  (Verific::VeriModule* module, const bool & top, const bool & blackboxed = false, BStringSet & uclocks = *(new BStringSet()));
  tIdSet getInputsIds (Verific::VeriModule* module, const bool & top, const BString & path, const bool & include_blackboxed = true, BStringSet & uclocks = *(new BStringSet()));
  tIdSet getOutputsIds(Verific::VeriModule* module, const bool & top);

  int addDutIOSignals(GenSceMi *gen, std::map<string, string> &clk_rst_map);

// private:
//  void addProbeMuxesAndBus();
//  void addUpdatedParamValues();

protected:
  virtual int visit(class AddBsvProbes *)  ;
  virtual int visit(class AddCosim *)  ;
  virtual int visit(class GenSceMi *)  ;

};

unsigned int EditCompileP0::GenProbeNameIndex = 0;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

int EditCompile::pass0()
{

  ModInstSetIter_t iter;
  int status = 0;

  for (iter = m_instSet.begin(); iter != m_instSet.end(); iter = m_instSet.upper_bound(*iter) ) {
    BString iname = (*iter)->getInstName();
    ModInstSetPairIter_t thisinst = m_instSet.equal_range(*iter);
    EditCompileP0 visitor0(iname);
    status = visitor0.process (thisinst);
    if (status != 0) break;
    m_instSet.insert (visitor0.newFirst(), visitor0.newEnd() );
  }

  return status;
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

EditCompileP0::EditCompileP0 (const BString & pathName)
  : EditCompileBase(pathName)
{
}

// Core processing function
int EditCompileP0::process(ModInstSetPairIter_t & range)
{

  int status = 0;
  for (ModInstSetIter_t iter = range.first; iter != range.second && status == 0 ; ++ iter ) {
    status = (*iter)->accept (this);
  }
  return status;
}

int EditCompileP0::visit(class AddBsvProbes *pr)
{
  VeriModule *module, *submodule;
  string foundPath, clock, enable, bsvtype;
  unsigned int width;
  netCollection sigs;
  BStringList pathList;
  char probename[256];
  char buf[32];
  BString dummy;
  BStringList paths;
  std::list<BString>::iterator sitr;
  std::list<BString>::iterator msitr;

  //printf("Find paths from pattern %s\n", pr->getHierPattern().c_str());
  HdlUtils::findPathsFromPattern(pr->getHierPattern(), pathList);
  //printf("Found %d paths\n", pathList.size());

  for (sitr = pathList.begin(); sitr != pathList.end(); sitr++) {

    //printf("Find module from %s\n", (*sitr).c_str());
    // Find the top module for adding probes
    module = HdlUtils::findModuleFromPath(NULL, *sitr, foundPath);
    dummy = *sitr;
    //printf("BEFORE RecursiveFindAllPaths\n");
    HdlUtils::recursiveFindAllPaths(module, paths, dummy);
    //printf("AFTER RecursiveFindAllPaths\n");

    for (msitr = paths.begin(); msitr != paths.end(); msitr++) {

      //printf("Path %s\n", (*msitr).c_str());
      // Find the child module for adding probes
      submodule = HdlUtils::findModuleFromPath(NULL, *msitr, foundPath);

      if (submodule == NULL)
	continue;

      //printf("Submodule %s\n", submodule->Name());
      sigs.clear();

      HdlUtils::findSignalsInModuleByExpression(submodule, pr->getPattern(), sigs,
						width, pr->getSignalType());

      if (width > 0) {
	snprintf (probename, 256, "%s_%s_%d", pr->getName().c_str(), module->Name(),
		  GenProbeNameIndex++);
	snprintf( buf, 31, "Bit#(%d)", width);
	bsvtype = buf ;
	
	BString clockname;
	HdlUtils::findClockSignalInModule(module, clockname);
	if (clockname == "")
	  clockname = "CLK";

	//printf(" AddProbe %s %s\n", probename, bsvtype.c_str());
	AddProbe * p = new AddProbe((*msitr).c_str(), probename, sigs, sigs,
				    width, clockname.c_str(), "1'b1", bsvtype, pr->getSignalType());
	addNewItem(p);
      }
    }
  }

  return 0;
}

int EditCompileP0::visit(class AddCosim *pr)
{
  VeriModule* module = getMasterModule();
  pr->setInst(pathName());
  pr->setDef(module->Name());

  CosimFlavor flavor = pr->getFlavor();

  HdlUtils::visitRefs(module);

  if (flavor == Observe) {
    
    printf("\n");

    ScanPath* state_path = new ScanPath();
    ScanData* state_data = new ScanData(State, pr->getKey(), 0, 32);

    BStringSet & uclocks = *(new BStringSet());
    BString uclock = pr->getUClock();
    if (uclock.length() > 0) {
      uclocks.insert(uclock);
    }
    printf("Adding State scan chain for module: %s ...\n", pathName().c_str());
    int status = addScanMods(module, *state_data,  pathName(), &state_path, true, uclocks);
    status = status || addCosimFillMods(pathName(), true);
    if (status == 0) {
      printf("Adding State scan chain for module: %s complete.\n\n", pathName().c_str());
      state_data->setPath(state_path);
      pr->getData()->push_back(state_data);

      printf("Adding Inputs scan chain for module: %s ...\n", pathName().c_str());
      ScanPath* in_path = new ScanPath();
      ScanData* in_data = new ScanData(InputsBB, pr->getKey(), 1, 32);
      status = status || addScanMods(module, *in_data,  pathName(), &in_path, true, uclocks);
      if (status == 0) {
	printf("Adding Inputs scan chain for module: %s complete.\n\n", pathName().c_str());
	in_data->setPath(in_path);
	pr->getData()->push_back(in_data);

      }
    }
    return status;
  }
  if (flavor == Replace) {

    printf("\n");
    
    ScanPath* out_path = new ScanPath();
    ScanData* out_data = new ScanData(Outputs, pr->getKey(), 0, 32);

    printf("Adding Outputs scan chain for module: %s ...\n", pathName().c_str());
    int status = addScanMods(module, *out_data,  pathName(), &out_path, true);
    status = status || addCosimFillMods(pathName(), true);
    if (status == 0) {
      printf("Adding Outputs scan chain for module: %s complete.\n\n", pathName().c_str());
      out_data->setPath(out_path);
      pr->getData()->push_back(out_data);

      printf("Adding Inputs scan chain for module: %s ...\n", pathName().c_str());
      ScanPath* in_path = new ScanPath();
      ScanData* in_data = new ScanData(Inputs, pr->getKey(), 1, 32);
      status = status || addScanMods(module, *in_data,  pathName(), &in_path, true);

      if (status == 0) {
	printf("Adding Inputs scan chain for module: %s complete.\n\n", pathName().c_str());
	in_data->setPath(in_path);
	pr->getData()->push_back(in_data);

      }
    }
    return status;
  }
  setError("Unhandled Cosim Flavor");
  return 1;
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

int EditCompileP0::addScanMods(VeriModule* module, ScanData & data, const BString & path, ScanPath** scan_path, const bool & top, BStringSet & uclocks)
{
  int status = 0;
  VeriModuleInstantiation *module_inst;
  foreachModuleInstance(module, module_inst) {
    VeriModule* module_def = module_inst->GetInstantiatedModule();
    if (module_def) {
      VeriInstId* inst;
      foreachInstId(module_inst, inst) {

	if (HdlUtils::isBlackboxed(inst, uclocks)) continue;

	BStringSet & uclocks_new = *(new BStringSet());

	BString path_new = path;
	path_new += "/";
	path_new += inst->Name();

	Array *port_connects = inst->GetPortConnects();
	unsigned it;
	VeriExpression *expr, *conn;
	std::ostringstream conn_name;
	FOREACH_ARRAY_ITEM(port_connects, it, expr) {
	  if (expr->GetClassId() == ID_VERIPORTCONNECT) {
	    BString formal = expr->NamedFormal();
	    conn_name.str("");
	    if ((conn = expr->GetConnection()) != NULL) {
	      conn->PrettyPrint(conn_name, 0);
	    }
	    if (uclocks.end() != uclocks.find(conn_name.str())) {
	      uclocks_new.insert(formal);
	    }
//	    printf("PATH: %s, FORMAL: %s CONN: %s\n", path_new.c_str(), formal.c_str(), conn_name.str().c_str());
	  }
	}

	ScanPath* path_local = new ScanPath();
	status = addScanMods(module_def, data, path_new, &path_local, false, uclocks_new);
	ScanPath* path_inst = new ScanPath(inst->Name(), path_local);
	(*scan_path)->Postpend(path_inst);
      }
    }
  }

  if (status != 0) {
    return status;
  }

//  cout << "UCLOCKS " << uclocks << " " << path.c_str() << endl;

  status = addScanMod(module, data, path, scan_path, top, uclocks);
  return status;

}

int EditCompileP0::addScanMod(VeriModule* module, ScanData & data, const BString & path, ScanPath** scan_path, const bool & top, BStringSet & uclocks)
{

  tIdSet all_ids = tIdSet();

   bool found = false;
  if (data.getFlavor() == State) {
    all_ids = getStateIds(module, top, false, uclocks);
    found = true;
  }

  if (data.getFlavor() == InputsBB) {
    all_ids = getInputsIds(module, top, path, true, uclocks);
    found = true;
  }

  if (data.getFlavor() == Inputs) {
    all_ids = getInputsIds(module, top, path, false, uclocks);
    found = true;
  }

  if (data.getFlavor() == Outputs) {
    all_ids = getOutputsIds(module, top);
    found = true;
  }

  if (!found) {
    return 1;
  }


  tIdList* input_ids  = new tIdList();
  
  if  (!all_ids.empty()) {

    foreachSetId(all_ids, it) {
      VeriIdDef* id = (VeriIdDef*) *it;
      (*input_ids).push_back(id);
    }

    //    printf("TOP? %d\n", top);
    //    cout << "IDS " << all_ids << path.c_str() << endl;

    ScanPath* path_local = new ScanPath();
    HdlUtils::createConcatExpression(module,  (*input_ids), &path_local); // called for ScanPath creation
    (*scan_path)->Postpend(path_local);

    if (data.getFlavor() == Outputs) { 

      tIdList* input_ids  = new tIdList();
      tIdList* output_ids = new tIdList();

      foreachSetId(all_ids, it) {
	VeriIdDef* id = (VeriIdDef*) *it;
	const BString & name = id->Name();
	BString name_new = getUniqueIdentifier (name + "_Orig", false);
	VeriIdDef* id_new = module->AddSignal(name_new.c_str(), new VeriDataType(VERI_WIRE, 0, NULL), 0); // size doesn't matter
	(*input_ids).push_back(id_new);
	(*output_ids).push_back(id);
	CktMod *p = new RenameSignal (path, name, name_new, false);
	addNewItem(p);
      }
      CktMod *p = new AddScan (path, data, (*input_ids), (*output_ids), top);
      addNewItem(p);
    } else {
      CktMod *p = new AddScan (path, data, (*input_ids), top);
      addNewItem(p);
    }
  } else {

    if((*scan_path)->Length(1) > 0 || top) {
      CktMod *p = new AddScan (path, data, (*input_ids), top);
      addNewItem(p);
    }
  }
  return 0;
}

tIdSet EditCompileP0::getStateIds(VeriModule* module, const bool & top, const bool & blackboxed, BStringSet & uclocks)
{

  tIdSet all_ids;

  VeriEventControlStatement *stmt;
  foreachAlways(module, stmt) {
    if (HdlUtils::isClocked(stmt) && !HdlUtils::isClocked(stmt, &uclocks)) {
      VeriStatement* body = stmt->GetStmt();
      tIdSet id_set = HdlUtils::getAssignmentIds(body);
      foreachSetId(id_set, it) {
	VeriIdDef* id = (VeriIdDef*) *it;
	if (HdlUtils::isBlackboxed(id) == blackboxed) {
//	  cout << "MOD: " << module->GetName() << " UCLOCKS: " << uclocks << endl;
	  all_ids.insert(id);
	}
      }
    }
  }

  return all_ids;
}

tIdSet EditCompileP0::getInputsIds(VeriModule* module, const bool & top, const BString & path, const bool & include_blackboxed, BStringSet & uclocks)
{

  tIdSet all_ids;
  tIdSet bb_ids;
  tIdSet cont_ids;
  if (include_blackboxed) {
    bb_ids = getStateIds(module, top, true, uclocks);

    VeriModuleInstantiation *module_inst;
    foreachModuleInstance(module, module_inst) {
      VeriModule* module_def = module_inst->GetInstantiatedModule();
      if (module_def) {
	VeriInstId* inst;
	foreachInstId(module_inst, inst) {
	  if (HdlUtils::isBlackboxed(inst, uclocks)) {

	    VeriModuleItem* item = (VeriModuleItem*) module_inst;
	    CktMod *p = new RmCode (path, item);
	    addNewItem(p, CosimMods);

	    Array *port_connects = inst->GetPortConnects();
	    unsigned it;
	    VeriExpression *expr, *conn;
	    FOREACH_ARRAY_ITEM(port_connects, it, expr) {
	      if (expr->GetClassId() == ID_VERIPORTCONNECT) {
// This doesn't seem to work (not sure why)
//		printf("B0 %d\n", expr->PortExpressionDir()); 
		bool output = false;
		BString formal = expr->NamedFormal();
		VeriDataDecl* decl;
		foreachDataDecl(module_def, decl) {
		  Array* ids = decl->GetIds();
		  unsigned it;
		  VeriIdDef* id;
		  FOREACH_ARRAY_ITEM(ids, it, id) {
		    if (strcmp(id->GetName(), formal.c_str()) == 0) {
		      if (decl->GetDir() == VERI_OUTPUT) {
			output = true;
		      }
		      break;
		    }
		  }
		}
		if (output) {
		  if ((conn = expr->GetConnection()) != NULL) {
		    VeriIdDef* out_id = conn->GetId();
		    if (out_id != NULL) {
		      all_ids.insert(out_id);
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }


  }

  foreachSetId(bb_ids, it) {
    VeriIdDef* bb_id = (VeriIdDef*) *it;
    printf("NOTICE: Array variable %s/%s is being blackboxed.\n", path.c_str(), bb_id->Name());
    VeriEventControlStatement *stmt;
    foreachAlways(module, stmt) {
      if (HdlUtils::isClocked(stmt) && !HdlUtils::isClocked(stmt, &uclocks)) {
	VeriStatement* body = stmt->GetStmt();
	tIdSet id_set = HdlUtils::getAssignmentIds(body, bb_id);
	foreachSetId(id_set, it) {
	  VeriIdDef* id = (VeriIdDef*) *it;
	  if (!HdlUtils::isBlackboxed(id)) {
	    all_ids.insert(id);
	  }
	}
      }
    }

    ContAssignmentVisitor assign_visitor(bb_id);
    module->Accept( assign_visitor ) ;

    foreachSetId(assign_visitor._assigned_ids, it) {
      VeriIdDef* id = (VeriIdDef*) *it;
      if (!HdlUtils::isBlackboxed(id)) {
 	all_ids.insert(id);
      }
    }
    foreachSetItem(assign_visitor._assigns, it0) {
      VeriModuleItem* item = (VeriModuleItem*) *it0;
      CktMod *p = new RmCode (path, item);
      addNewItem(p, CosimMods);
    }
  }

  // cout << "BB IDS " << all_ids << path.c_str() << endl;

  if (top) {
    VeriDataDecl* decl;
    foreachDataDecl(module, decl) {
      if (decl->GetDir() == VERI_INPUT) {
	VeriIdDef* id;
	Array* ids = decl->GetIds();
	unsigned it;
	FOREACH_ARRAY_ITEM(ids, it, id) {
	  all_ids.insert(id);
	}
      }
    }
  }

  return all_ids;
}

tIdSet EditCompileP0::getOutputsIds(VeriModule* module, const bool & top)
{

  tIdSet all_ids;

  if (top) {
    VeriDataDecl* decl;
    foreachDataDecl(module, decl) {
      if (decl->GetDir() == VERI_OUTPUT) {
	VeriIdDef* id;
	Array* ids = decl->GetIds();
	unsigned it;
	FOREACH_ARRAY_ITEM(ids, it, id) {
	  all_ids.insert(id);
	}
      }
    }
  }

  return all_ids;
}

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

int EditCompileP0::addCosimFillMods(BString path, bool first)
{

  BString::size_type delim  = path.rfind("/");
  bool is_top = delim == 0 || delim == BString::npos;

  if (!first) {
    addNewItem (new CosimFill (path), CosimMods);
    addNewItem (new ReplaceModule    (path, "Empty"), CosimMods);
  }

  BString upInst, thisInst;
  splitInstanceName (path, upInst, thisInst );
  if (!is_top) {
    addCosimFillMods(upInst, false);
  }

  if (first) {
    VeriModule *mod = getMasterModule();
    const char * modname = mod->GetOriginalModuleName();
    BString xx;
    if (0 == modname) {
      xx = mod->Name();
    } else {
      xx = modname;
    }
    // xx += "_COSIM_";
    // xx += itoa(key);

    ParameterList plist = createCosimParameterList(mod);
    ModuleTerminalList mtlist = createCosimTerminalList(mod);
    addNewItem(new AddInstance(upInst, thisInst, xx, plist, mtlist, 0),
	       CosimMods);
  }
  return 0;
}

int EditCompileP0::visit(class GenSceMi *gen)
{
  VeriModule *module;
  netCollection sigs;
  std::list<BString>::iterator sitr;
  HdlUtils *hdlUtils = HdlUtils::init();
  string s, m, pn;

  //printf("Start of GenSceMi compile\n");

  // Create hash of clk and reset signal names
  gen->getClkRstSignals(sigs);
  
  // Find the module
  module = HdlUtils::findModuleByName(gen->getModuleName());
  if (module == NULL)
    return 1;

  // Generate SceMiLayer and wrapped Dut
  if (gen->getPhase() == 0) {
    hdlUtils->generateSceMiLayer(module, gen->getNewModuleName(), "SceMiLayer.bsv",
				 gen->getOutputDir());
  }
  else {
    
    // Create a new simple verilog shell for the DUT

    ModuleTerminalIterator mtItr;
    string lower_new_mod_name = gen->getNewModuleName();
    if (isupper(lower_new_mod_name[0]))
      lower_new_mod_name[0] = tolower(lower_new_mod_name[0]);


    // Parameter list
    ParameterList plist;
    HdlUtils::createModuleParameterList(module, plist);

    // Regular terminal list
    ModuleTerminalList new_terminals;
    HdlUtils::createModuleTerminalList2(module, new_terminals);

    // Create the verilog shell
    string indir = gen->getOutputDir();
    size_t i = indir.find_last_of("_scemi");
    indir = indir.substr(0, i-5);

    //HdlUtils::generateDutVerilog(module, gen->getModuleName(), lower_new_mod_name.c_str(), plist,
    //new_terminals, indir.c_str(), gen->getOutputDir());

    //printf("HERE outputdir: %s lower_new_mod_name: %s, modulename: %s\n", gen->getOutputDir(), lower_new_mod_name.c_str(), gen->getModuleName());
 
    // Remove the code in the body of the verilog
    RmBody *rmb = new RmBody (gen->getInstName());
    addNewItem(rmb);
    
    // Set Rdy output signals to high for each input port
    // Mimic toGet() by having Rdy always set to high (always ready).
    if (!HdlUtils::isPortLockstep()) {
      std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
      ModuleTerminal *terminal;
      for (mtItr = new_terminals.begin(); mtItr != new_terminals.end(); mtItr++) {
	terminal = &(*mtItr);
	if (terminal->m_dir == d_input) {
	  if (HdlUtils::init()->isRegularModuleTerminal(terminal)) {
	    if ((*map)[terminal->m_portName] == "") {
	      pn = terminal->m_portName;
	      toLowerCase(pn);
	      s = "RDY_";
	      s += pn;
	      s += "_put";
	      addNewItem(new AddSimpleAssign(gen->getInstName(), s, "1'b1"));
	    }
	  }
	}
	else if (terminal->m_dir == d_output) {
	  if (HdlUtils::init()->isRegularModuleTerminal(terminal)) {
	    if ((*map)[terminal->m_portName] == "") {
	      pn = terminal->m_portName;
	      toLowerCase(pn);
	      s = "RDY_";
	      s += pn;
	      s += "_get";
	      addNewItem(new AddSimpleAssign(gen->getInstName(), s, "1'b1"));
	    }
	  }
	}
      }
    }

    // Add the Dut
    AddInstance *ai = new AddInstance (gen->getInstName(), lower_new_mod_name,
				       gen->getModuleName(), plist, new_terminals);
    addNewItem(ai);
    
    // Do reset signal
    if (!HdlUtils::isPortLockstep()) {
      string foundPath;
      string parentname = gen->getInstName();
      size_t loc = parentname.find_last_of('/');
      parentname = parentname.substr(0, loc);
      if (parentname != "") {
	
	AddNet *reset_net = new AddNet(parentname.c_str(), "dut_reset_final_join_reset", 1);
	addNewItem(reset_net);
	
	RenameInstanceConnection *rn_conn = 
	  new RenameInstanceConnection(parentname.c_str(), "scemi_reset_join_reset",
				       "RST_OUT", "dut_reset_final_join_reset");
	addNewItem(rn_conn);
	
	rn_conn = 
	  new RenameInstanceConnection(parentname.c_str(), "scemi_dut_dutIfc",
				       "RST_N", "dut_reset_final_join_reset");
	addNewItem(rn_conn);
      }
    }
  }
  return 0;
}

int EditCompileP0::addDutIOSignals(GenSceMi *gen, std::map<string, string> &clk_rst_map)
{
  // Generate all the data type
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;

  string lower_new_mod_name = gen->getNewModuleName();

  if (isupper(lower_new_mod_name[0]))
    lower_new_mod_name[0] = tolower(lower_new_mod_name[0]);
  

  VeriModule *module = HdlUtils::findModuleByName(gen->getModuleName());

  HdlUtils::createModuleTerminalList(module, mtlist);

  // Inputs
  string signal, rhs;
  int count = 0;
  char str[99];
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == string(""))) {
      signal = lower_new_mod_name;
      signal += "$" + terminal->m_portName;
      addNewItem(new AddNet(gen->getInstName(), signal, terminal->m_width));
      count += terminal->m_width;
    }
  }

  // Now the assign statement
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == string(""))) {
      signal = lower_new_mod_name;
      signal += "$" + terminal->m_portName;
      rhs = "request_put";
      if (terminal->m_width > 1) {
	rhs += "[";
	snprintf(str, 99, "%d", count-1);
	rhs += str;
	rhs += ":";
	count -= terminal->m_width;
	snprintf(str, 99, "%d", count);
	rhs += str;
	rhs += "]";
      } else if (count != 1) {
	rhs += "[";
	snprintf(str, 99, "%d", count-1);
	rhs += str;
	count -= terminal->m_width;
	rhs += "]";
      }
      addNewItem(new AddSimpleAssign(gen->getInstName(), signal, rhs));
    }
  }
  // Output
  int num_outputs = 0;
  string concat_signal;
  count = 0;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == string(""))) {
      num_outputs++;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == string(""))) {
      if (count == 0)
	if (num_outputs > 1)
	  concat_signal = "{";
	else
	  concat_signal = "";
      else
	concat_signal += ", ";

      signal = lower_new_mod_name;
      signal += "$" + terminal->m_portName;
      concat_signal += signal;
      addNewItem(new AddNet(gen->getInstName(), signal, terminal->m_width));
      count += terminal->m_width;
    }
  }
  if (num_outputs > 1)
    concat_signal += "}";

  // Change the TLM signals for the Dut inputs/outputs
  snprintf(str, 99, "%d", count);
  rhs = str;
  rhs += "'b0";
  addNewItem(new ChangeAssignRHS(gen->getInstName(), "response_get", rhs, concat_signal));
  rhs = "1'b0";
  signal = "state";
  addNewItem(new ChangeAssignRHS(gen->getInstName(), "RDY_response_get", rhs, signal));
  addNewItem(new ChangeAssignRHS(gen->getInstName(), "CAN_FIRE_response_get", rhs, signal));

  return 1;
}



