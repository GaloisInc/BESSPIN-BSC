

#include "process.h"
#include "Dbg.h"
#include "VUtils.h"
#include "Globals.h"
#include "Scan.h"

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif


VeriIdDef* createScanAny(VeriModule* module, VeriIdDef* scan_mode)
{

  const char* name = getAttribute(module, "scanAny");

  if (name) {
    VeriIdDef* id = module->GetScope()->Find(name);
    if (id) {
      VeriContinuousAssign* assign = getAssign(module, id);
      if (assign) {
	unsigned i ;
	VeriNetRegAssign *nr_assign;
	VeriMapForCopy id_map_table ;
	FOREACH_ARRAY_ITEM(assign->GetNetAssigns(), i, nr_assign) {
	  VeriExpression* rhs     = nr_assign->GetRValExpr();
	  VeriExpression* rhs_new = new VeriBinaryOperator(VERI_LOGOR, rhs->CopyExpression(id_map_table), ref(scan_mode));
	  nr_assign->ReplaceChildExpr(rhs, rhs_new);
	}
	Dbg::printf(2,"ASSIGN \n");
	assign->PrettyPrint(std::cout,0);
	Dbg::printf(2,"\n");
	return id;
      }
    }

    fprintf(stderr, "createScanAny: unable to resolve net named '%s'.\n", name);
    exit(1);

  } else {
    char* scanAnyName = (char*) getUnusedName(module, "SCAN_ANY", 0);
    VeriIdDef* scanAny = addSignal(module, scanAnyName, 1);
    addAssign(module, scanAny, ref(scan_mode));

    addAttribute(module, "scanAny", scanAnyName);
    return scanAny;

  }
}

bool hasTermination(VeriModule* module)
{
  VeriInstId* inst_id;
  foreachInstId(module, inst_id) {
    VeriModuleInstantiation* module_inst = inst_id->GetModuleInstance();
    if (module_inst) {
      VeriModule* module_def = module_inst->GetInstantiatedModule();
      if (module_def) {
	const char* value = getAttribute(module_def, "TERMINATION");
	if (value) {
	  return true;
	}
      }
    }
  }
  return false;
}

bool isDut(VeriModule* module)
{

  const char* value = getAttribute(module, "DUT");
  if (value) { 
    return true;
  }
  if (!strcmp(getModuleNameBase(module), "mkDut")) { 
    return true; 
  }
  if (!strcmp(getModuleNameBase(module), "mkDUT")) { 
    return true; 
  }
  if (!strcmp(getModuleNameBase(module), "mkDUTV")) { 
    return true; 
  }
  return false;
}

VeriInstId* getDutInstId(VeriModule* module, const char* parent)
{

  std::string prefix (parent);
  prefix += "/";

  boost::match_results<std::string::const_iterator> what;
  boost::regex filter(Globals::GetRegExpr());

  VeriInstId* inst_id = NULL;
  VeriInstId* dut_inst_id = NULL;
  VeriModule* dut = NULL;
  foreachInstId(module, inst_id) {
    VeriModuleInstantiation* module_inst = inst_id->GetModuleInstance();
    if (module_inst) {
      const char* inst_name = getInstanceName(module_inst);

      if (boost::regex_match((prefix + inst_name), what, filter, boost::match_default | boost::match_partial)) {
	VeriModule* module_def = module_inst->GetInstantiatedModule(); 
	if (module_def) {
	  if (dut) {
	    fprintf(stderr, "getDut: More than one dut found (%s and %s). Exiting.\n", dut->Name(), module_def->Name());
	    exit(1);
	  } else {
	    Dbg::printf(2, "getDut: FOUND %s.\n", module_def->Name());
	    dut = module_def;
	    dut_inst_id = inst_id;
	  }
	}
      }
    }
  }
  if (!dut_inst_id) {
    fprintf(stderr, "getDut: No dut found. Exiting.\n");
    exit(1);
  }
  return dut_inst_id;
}

VeriConstVal* getParameterValue(VeriModule* module, const char* param_name)
{

  Array* ports = module->GetParameters();
  unsigned it;
  VeriExpression* expr;
  VeriIdDef* id;
  FOREACH_ARRAY_ITEM(ports, it, id) {
    if (!strcmp(id->Name(), param_name)) {
      expr = id->GetInitialValue();
      if (expr) {
	if (expr->IsConst()) {
	  VeriConstVal* constant_value = static_cast<VeriConstVal*>(expr);
	  return constant_value;
	}
      }
    }
  }
  return NULL;
};

ModuleDesc* FindAndProcessModule(VeriModule* module, const char *parent, const char *name, unsigned level)
{

  Dbg::SetIndent(level);

  std::string prefix (parent);
  prefix += "/";
  prefix += name;

  bool first = true;
  ModuleDesc* result;
  if (hasTermination(module)) {
    VeriInstId* dut_inst_id = getDutInstId(module, prefix.c_str());
    VeriModule* dut;
    const char* actual;
    if (dut_inst_id) {
      VeriModuleInstantiation* dut_inst = dut_inst_id->GetModuleInstance();
      dut = dut_inst->GetInstantiatedModule();
      Dbg::printf(2,"Y2Y %s X2x\n", dut_inst_id->Name());
      VeriInstId* inst_id;
      char* value;
      char* actual;
      Array *ids     = new Array() ;
      Array *actuals = new Array() ;
      foreachInstId(module, inst_id) {
	VeriModuleInstantiation* module_inst = inst_id->GetModuleInstance();
	if (module_inst) {
	  VeriModule* module_def = module_inst->GetInstantiatedModule();
	  if (module_def) {
	    value = (char*) getAttribute(module_def, "SCANCOMMON");
	    if (value) {
	      ids->InsertFirst(inst_id);
	    }
	    value = (char*) getAttribute(module_def, "TERMINATION");
	    if (value) {
	      ids->InsertFirst(inst_id);

	      VeriConstVal* expr_id    = getParameterValue(module_def, "id");
	      VeriConstVal* expr_mode  = getParameterValue(module_def, "mode");
	      VeriConstVal* expr_width = getParameterValue(module_def, "width");
	      if (expr_id && expr_mode && expr_width) {

		int id        =            expr_id->Integer();
		ScanMode mode = (ScanMode) expr_mode->Integer();
		int width     =            expr_width->Integer();
		const char *parent, *name;

		Dbg::printf(2,"FOO %d %d %d %s\n", expr_id->Integer(), expr_mode->Integer(), expr_width->Integer(), dut->Name());
		boost::regex filter(Globals::GetRegExpr());

		result = ProcessModule(dut, filter, prefix.c_str(), dut_inst_id->Name(), level + 1, 1, mode, width);
		Dbg::SetIndent(level);

		if (result) {
		  ScanPath* path = result->PathGet();

		  Dbg::printf(2,"PATH: %s\n", path->ToString("", "\n", "r: /top").c_str());

		  std::string file = Globals::GetPath();
		  file += "/";
		  file += "info.sh";

		  if (first) {
		    FILE* out_file = fopen(file.c_str(), "w");
		    fprintf(out_file, "#!/usr/bin/csh\n\n");
		    fprintf(out_file, "set COSIM_TOP_MODULE = %s\n", Globals::GetCosimMod());
		    fclose(out_file);
		  }

		  file = Globals::GetPath();
		  file += "/";
		  file += "path.map";

		  const char* fmode = "a";

		  dut  = result->ModuleGet();


		  
		  if (first) {
		    fmode = "w";
		  }

		  FILE* out_file = fopen(file.c_str(), fmode);
		  int length = strlen(prefix.c_str()) + 1 + strlen(dut_inst_id->Name());
		  const char* cosim_inst = Globals::GetCosimInst() + length;
		  if (first) {
		    fprintf(out_file, "d: %s\n", Globals::GetCosimMod());
		    fprintf(out_file, "i: /top%s\n", cosim_inst);
		    first = false;
		  }
		  fprintf(out_file, "p: %d %d\n", id, width);
		  if (mode == GET_INPUTS) {
		    fprintf(out_file, "c: /top%s/%s\n", cosim_inst, "CLK");
		  }
		  fprintf(out_file, "%s", path->ToString("", "\n", "r: /top").c_str());
		  fclose(out_file);



		  actual = (char*) getAttribute(dut, "SCAN_IN");
		  actuals->InsertFirst(strdup(actual));
		  actual = (char*) getAttribute(dut, "SCAN_OUT");
		  actuals->InsertFirst(strdup(actual));
		  actual = (char*) getAttribute(dut, "SCAN_MODE");
		  actuals->InsertFirst(strdup(actual));
		  actual = (char*) getAttribute(dut, "SCAN_LENGTH");
		  actuals->InsertFirst(strdup(actual));
		
		}
	      }
	    }
	  }
	}
      }
      setInstanceModule(dut_inst, dut);
      foreachInstId(module, inst_id) {
	VeriModuleInstantiation* module_inst = inst_id->GetModuleInstance();
	if (module_inst) {
	  VeriModule* module_def = module_inst->GetInstantiatedModule();
	  if (module_def) {
	    value = (char*) getAttribute(module_def, "SCANCOMMON");
	    if (value) {
	      inst_id = (VeriInstId*) ids->GetLast();

	      actual = (char*) getAttribute(dut, "SCAN_UCLOCK");
	      addConnection(module, dut_inst_id,  inst_id, "SCAN_UCLOCK", "DSCAN_CLK", "_UCLOCK");
	      actual = (char*) getAttribute(dut, "SCAN_ANY");
	      addConnection(module, dut_inst_id,  inst_id, strdup(actual), "DSCAN_ANY", "_ANY");

	      ids->RemoveLast();
	    }
	    value = (char*) getAttribute(module_def, "TERMINATION");
	    if (value) {
	      inst_id = (VeriInstId*) ids->GetLast();

	      actual = (char*) actuals->GetLast();
	      addConnection(module, dut_inst_id,  inst_id, actual, "DSCAN_IN", "_IN");
	      actuals->RemoveLast();
	      actual = (char*) actuals->GetLast();
	      addConnection(module, dut_inst_id,  inst_id, actual, "DSCAN_OUT", "_OUT");
	      actuals->RemoveLast();
	      actual = (char*) actuals->GetLast();
	      addConnection(module, dut_inst_id,  inst_id, actual, "DSCAN_MODE", "_MODE");
	      actuals->RemoveLast();
	      actual = (char*) actuals->GetLast();
	      addConnection(module, dut_inst_id,  inst_id, actual, "DSCAN_LENGTH", "_LENGTH");
	      actuals->RemoveLast();

	      ids->RemoveLast();
	    }
	  }
	}
      }
    }
    return result;
  }
  
  VeriInstId* inst_id;
  foreachInstId(module, inst_id) {
    VeriModuleInstantiation* module_inst = inst_id->GetModuleInstance();
    if (module_inst) {
      VeriModule* module_def = module_inst->GetInstantiatedModule();
      const char* inst_name = getInstanceName(module_inst);
      if (module_def) {
	ModuleDesc* desc = FindAndProcessModule(module_def, prefix.c_str(), inst_name, level+1);
	Dbg::SetIndent(level);
	if (desc) {
	  desc->ModuleSet(module);
	  return desc;
	}
      }
    }
  }
  return NULL;
}

unsigned ProcessSkip(const char *parent, const char* name, boost::regex filter)
{

  std::string prefix (parent);
  prefix += "/";
  prefix += name;

  boost::match_results<std::string::const_iterator> what;
  if(0 == boost::regex_match(prefix, what, filter, boost::match_default | boost::match_partial))
    {
      Dbg::printf("Skip No match %s\n", prefix.c_str());
      return(1);
    } else {
      return(0);
    }
}

ScanPath* createScanPath(VeriModule* module, tStringList ids)
{
  ScanPath* path = new ScanPath();
  foreachInStringList(&ids, it) {
    VeriIdDef* id = module->GetScope()->Find((*it).c_str());
    if (id) {
      int size = getSignalSize(id);
      ScanPath* path_reg = new ScanPath(strdup((*it).c_str()), size);
      path->Postpend(path_reg);
    } else {
      fprintf(stderr, "createScanPath: unable to resolve net named '%s'.\n", (*it).c_str());
      exit(1);
    }
  }
  ScanPath* path_extra = new ScanPath((char*) "_SCAN", Globals::GetWidth());
  path->Prepend(path_extra);
  return path;
}


ModuleDesc* ProcessModule(VeriModule* modulex, boost::regex filter, const char *parent, const char *name, unsigned level, unsigned copy, ScanMode mode, int width)
{

  bool full_match = false;

  Dbg::SetIndent(level);

  std::string prefix (parent);
  prefix += "/";
  prefix += name;

  boost::match_results<std::string::const_iterator> what;
  ModuleDesc* md; (void) md;
  if(0 == boost::regex_match(prefix, what, filter, boost::match_default | boost::match_partial))
    {
      Dbg::printf("No match %s\n", prefix.c_str());
      md = ModuleDescMap::GetNoneDesc(modulex);
      if(md) {
	return md;
     } else {
	md = new ModuleDesc(modulex);
	md = ModuleDescMap::GetDesc(md); // just records it.
	return md;
      }
    }
  else if(what[0].matched) 
    {
      full_match = true;
      Dbg::printf(2,"Full match %s\n", prefix.c_str());
      md = ModuleDescMap::GetAllDesc(modulex);
      if (md) {
	Dbg::printf(2,"MATCH!");
	return md;
      } else {}
    } else {
      Dbg::printf("Partial match %s\n", prefix.c_str());
    }

  // If we get to here, we need to create a new module
  
  VeriModule* module;
  if (copy) {
    module = moduleCopyWithNewName(modulex);
  } else {
    module = modulex;
  }

  md = new ModuleDesc(module);

  if (mode == GET_INPUTS && full_match) {

    Globals::SetCosimMod(getModuleNameBase(module));
    Globals::SetCosimInst(prefix.c_str());

    char*      expr_name            = (char*) getUnusedName(module, "_WIDTH");
    VeriIdDef* scan_width      = module->AddParameter(strdup(expr_name), 0 /* data type */, new VeriIntVal(width) /* initial value */) ;
    addAttribute(module, "SCAN_WIDTH", expr_name);
    Globals::SetWidth(new VeriIdRef(scan_width));

    expr_name                = (char*) getUnusedName(module, "_IN");
    VeriIdDef* scan_internal = addPort(module, expr_name, VERI_INPUT, Globals::GetWidth());
    addAttribute(module, "SCAN_IN", expr_name);

    expr_name           = (char*) getUnusedName(module, "_OUT");
    VeriIdDef* scan_out = addPort(module, expr_name, VERI_OUTPUT, Globals::GetWidth());
    addAttribute(module, "SCAN_OUT", expr_name);

    expr_name           = (char*) getUnusedName(module, "_MODE");
    VeriIdDef* scan_mode      = addPort(module, expr_name, VERI_INPUT, 1);
    addAttribute(module, "SCAN_MODE",  expr_name);

    expr_name                 = (char*) getUnusedName(module, "_LENGTH");
    VeriIdDef* scan_length    = addPort(module, expr_name, VERI_OUTPUT, 32);
    addAttribute(module, "SCAN_LENGTH", expr_name);

    VeriModule* orig = getModuleOrig(module);
    tStringList inputs  = createInputList(orig);
    VeriIdDef* concat = createConcatSignal(module, inputs);
    addSampleReg(module, concat, &scan_internal, scan_mode, scan_length, "sample_inputs", width);
    connectScanOuput(module, scan_internal,  scan_out);
    ScanPath* path = createScanPath(module, inputs);
    md->AllSet(0);
    md->NoneSet(0);
    md->PathSet(path);
    return md;
  }

  const char* xx;
  if (full_match) {
    xx = ".*";
  } else {
    xx = Globals::GetRegExpr();
  }
  boost::regex filter_down(xx);

  unsigned includes_all    = 1;
  ScanPath* path = new ScanPath();

  // First, recurse down into the module instantiations;

  VeriModuleInstantiation *module_inst;
  foreachModuleInstance(module, module_inst) {

    const char* inst_name = getInstanceName(module_inst);

    if (ProcessSkip(prefix.c_str(), inst_name, filter_down)) {
      continue;
    }

    Dbg::printf(2,"INST %s %s\n", prefix.c_str(), inst_name);

    VeriModule *module_def = module_inst->GetInstantiatedModule();
    if (module_def) {

      const char* value = getAttribute(module_def, "NO_SCAN");

      if (value) {
	Dbg::printf(2,"SKIP: %s\n", module_def->Name());
	continue;
      }

      Map* mp = createParamsMap(module_inst);

      const char* ignore    = getModuleNameBase(module_def);
      Dbg::printf(2,"BASE %s\n", ignore);
      
      module_def = moduleCopyWithNewName(module_def);
      setParameters(module_def, mp);

      ModuleDesc* md = ProcessModule(module_def, filter_down, prefix.c_str(), inst_name, level + 1, 1, mode, width);

      if (md->IncludesNone()) {
	unsigned local_all = md->IncludesAll(); 
	VeriModule* module_mod =  md->ModuleGet();
	Dbg::printf("NONE: %s %s %s\n", inst_name, module_mod->GetName(), module_def->GetName());
// 	if (module_mod != module_def) {
// 	  Dbg::printf("REMOVE %s\n", module_mod->GetName());
// 	  removeModule(module_mod);
// 	}
	md = new ModuleDesc(module_def);
	md->NoneSet(1);
	md->AllSet(local_all);
      }
      ModuleDesc* md_new = ModuleDescMap::GetDesc(md); // will return the (already created) one of these if it exists.
//      if (md_new->ModuleGet()->GetName() && md != md_new) {
//	Dbg::printf("REMOVEX %s %s\n", md->ModuleGet()->GetName(), md_new->ModuleGet()->GetName());
//	removeModule(md->ModuleGet());
//	md = md_new;
//      }
      md = md_new;
      setInstanceModule(module_inst, md->ModuleGet());
      Dbg::printf(2,"ADDING %s to %s\n", md->ModuleGet()->GetName(), module->GetName());

    }
  }

  addStateChain(module, &prefix, filter_down, path, &includes_all, width);

  md->AllSet(includes_all);
  //  md->NoneSet(path->Length(1) == 0);
  md->NoneSet(0);
  md->PathSet(path);

//   Dbg::printf("EXIT %s %d %d\n",     module->GetName(), md->IncludesAll(), md->IncludesNone());
//   Dbg::printf("PATH %s\n",           path->ToString().c_str());
//   Dbg::printf("PATH FLAT %s\n",      path->ToString((char*)prefix.c_str()).c_str());
//   Dbg::printf("LENGTH %d\n",         path->Length());
//   Dbg::printf("LENGTH FIXED  %d\n",  path->Length(1));
//   Dbg::printf("LENGTH SCALED %d\n",  path->Length(0));
  
  return md;
}

