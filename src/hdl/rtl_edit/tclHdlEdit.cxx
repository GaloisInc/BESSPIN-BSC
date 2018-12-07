// Copyright 2009-2010 Bluespec Inc.  All rights reserved

#include "stdio.h"
#include <unistd.h>
#include <iostream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <streambuf>

#include <tcl.h>
#include "flexlm.h"

#include "tclHdlEdit.h"
#include "tclModifier.h"

#include "Map.h"
#include "veri_file.h"
#include "VeriScope.h"
#include "VeriModule.h"
#include "VeriId.h"
#include "VeriMisc.h"
#include "VeriStatement.h"
#include "VeriExpression.h"
#include "HdlUtils.h"
#include "TclUtils.h"
#include "TclModelUtils.h"
#include "TestBenchGenerator.h"

using namespace std;

// Forward reference for function
#define BUFSIZE 256


// package name and namespace for this extension
#define PKG_NAME    "BhdlEdit"
#define PKG_VERSION "1.0"
#define NS          "BhdlEdit"

/* static extension data */
// static HdlEdit * hdledit = NULL;
//static unsigned int verbose = 1;
static HdlUtils *hdlUtils = NULL;

// forward declarations
// Links to commands
extern "C" {

  int Bhdledit_Init(Tcl_Interp *interp);

  static int HhdlNetlist_Cmd(ClientData clientData,
                             Tcl_Interp *interp,
                             int objc,
                             Tcl_Obj * objv[]);

  static void HhdlNetlist_Cmd_Delete(ClientData clientData);


} // extern "C"


class LsInstsVisitor : public VeriVisitor
{
public :
  Tcl_Interp *  m_interp ;
  Tcl_Obj *     m_list ;
  const BString &m_path;

public :
 LsInstsVisitor(Tcl_Interp *interp, const BString & path)
   : VeriVisitor ()
   , m_interp   (interp)
   , m_list     (Tcl_NewListObj (0,0)) // XXXX
   , m_path(path)
  {} ;
  ~LsInstsVisitor() {} ;

  void appendResult (const char *i, const char *m) {
    if (i == NULL)
      return;
    BString name = m_path + i;
    Tcl_Obj *l = toTclList(m_interp, toTclObj(m_interp, name), toTclObj(m_interp, m));
    appendTclResult(m_interp, l);
  }


    // Collect instantiations :
  virtual void Visit(VeriModuleInstantiation &node) {
    Array *inst_arr = node.GetInstances() ;
    unsigned i ;
    VeriInstId *inst ;
    FOREACH_ARRAY_ITEM(inst_arr, i, inst) {
      const char * mod = 0;
      VeriModule *imod = node.GetInstantiatedModule();
      if (imod) {
        mod = imod->GetOriginalModuleName();
      }
      if (mod == 0) {
        mod = node.GetModuleName();
      }
      if (mod == 0) { mod = "WTF !!!";}
      appendResult ( inst->Name(), mod );
    }
  }

  // Collect instantiations :
  virtual void Visit(VeriGateInstantiation &node) {
    Array *inst_arr = node.GetInstances() ;
    unsigned i ;
    VeriInstId *inst ;
    FOREACH_ARRAY_ITEM(inst_arr, i, inst) {
      const char * mod = VeriNode::PrintToken(node.GetInstType());
      appendResult ( inst->Name(),mod );
    }
  }
  virtual void Visit(VeriAlwaysConstruct &node) {/* STOP analysis */};
  virtual void Visit(VeriAssign &node) {/* STOP analysis */};

} ;


// useful constants
//static const char* boolean_names[] = { "0", "1", "off", "on", "no", "yes" };

// exported package init routine -- called by tcl interp when package is loaded.
int Bhdledit_Init(Tcl_Interp *interp)
{
  Tcl_Namespace* nsptr = NULL;
  try {

    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
      return TCL_ERROR;
    }

    nsptr = (Tcl_Namespace*) Tcl_CreateNamespace(interp, NS, NULL, NULL);
    if (nsptr == NULL) {
      return TCL_ERROR;
    }

    if (Tcl_PkgProvide(interp, PKG_NAME, PKG_VERSION) != TCL_OK) {
      return TCL_ERROR;
    }

    // Commands in this package and namespace
    // netlist command
    Tcl_CreateObjCommand(interp,
			 NS "::netlist",
			 (Tcl_ObjCmdProc *) HhdlNetlist_Cmd,
			 (ClientData)NULL,
			 (Tcl_CmdDeleteProc *) HhdlNetlist_Cmd_Delete);
    Tcl_Export(interp, nsptr, "netlist", 0);

    // Setup verific area.
    hdlUtils = HdlUtils::init();
    if (hdlUtils == NULL) {
      Tcl_AppendResult(interp, "cannot load hdl analyzer" , NULL);
      return TCL_ERROR ;
    }
  

    // editnl command
    Tcl_CreateObjCommand(interp,
			 NS "::editnl",
			 (Tcl_ObjCmdProc *) HhdlEditNL_Cmd,
			 (ClientData) NULL,
			 (Tcl_CmdDeleteProc *) HhdlEditNL_Cmd_Delete);
    Tcl_Export(interp, nsptr, "editnl", 0);

  } catch (const exception &e) {
    cerr << "Exception caught in initializing tcl command: " << e.what() << endl;
    Tcl_AppendResult( interp, "exception caught: ", e.what(),  NULL) ;
    return TCL_ERROR;
  }

  return TCL_OK;
}
////////////////////////////  Cheetah traversal function






static int netlist_analyze (ClientData clientData,	/* Not used. */
	    Tcl_Interp *interp,         /* Current interpreter */
	    int objc,                   /* Number of arguments */
	    Tcl_Obj * objv[]		/* Argument strings */
	    )
{
  // Force V-2001 compiliation
  //veSetCommandLineOptions(VE_2000_OPT, NULL);
  int stat = getlm(false, bfl_netlist);
  if (stat != 0) return TCL_ERROR;

  //char *arg, *token;
  char **argv = (char **) malloc (objc * sizeof (char *)*3+4) ;
  argv[0] = (char *) "Bluespec::netlist" ;
  for (int i = 2 ; i < objc; ++i) {
    argv[i-1] = Tcl_GetString (objv[i]);
    //printf("arg %s\n", argv[i-1]);
  }

  // Decode the options and read in the verilog files 
  string errstring;
  stat = hdlUtils->decode_options(objc-1, argv, errstring);

  if (stat)
    stat &= hdlUtils->analyze(errstring);

  //printf("Stat %d %s\n", stat, errstring.c_str());

  free (argv);

  if (!stat) {
    // XXX get the error message
    if (errstring.size() != 0) {
      //printf("%s\n", errstring.c_str());
      toTclResult(interp, errstring);
    } else
      toTclResult (interp, "netlist_analyze(): something wrong with the given options or verilog files");
    return TCL_ERROR;
  }
  toTclResult (interp, "Analyzed");
  return TCL_OK;
}
static int netlist_shutdown (ClientData clientData,	/* Not used. */
	    Tcl_Interp *interp,         /* Current interpreter */
	    int objc,                   /* Number of arguments */
	    Tcl_Obj * objv[]		/* Argument strings */
	    )
{
  HdlUtils::shutdown();
  return TCL_OK;
}

static int netlist_lsinsts  (ClientData clientData,	/* Not used. */
                             Tcl_Interp *interp,         /* Current interpreter */
                             int objc,                   /* Number of arguments */
                             Tcl_Obj * objv[]		/* Argument strings */
			     )
{
  int stat = getlm(false, bfl_netlist);
  if (stat != 0) return TCL_ERROR;

  if (objc > 3) {
    Tcl_WrongNumArgs (interp, 2, objv, "path");
    return TCL_ERROR;
  }
  toTclResult (interp, Tcl_NewListObj(0,0));

  BString path("");
  if (objc == 3) {
    path = Tcl_GetString( objv[2] );
  }

  BString found ;
  VeriModule *module = tc_findModuleTcl (interp, NULL, path, found);

  // Error if none found
  if (module == NULL && path != "") {
    return TCL_ERROR;
  }

  // Assume we are looking for stuff at the top if
  // the module is null and path is empty
  if (module == NULL && path.empty()) {

    // Get all the top level modules
    Array *all_top_modules = veri_file::GetTopModules() ;
    unsigned i;
    VeriModule *mod;
    // Get a handle of the first top level module, if any.
    FOREACH_ARRAY_ITEM(all_top_modules, i, mod) {
      const char *name = mod->Name();
      Tcl_Obj *n = toTclObj(interp, name);
      appendTclResult(interp, toTclList(interp, n, n));
    }
  }
  else if (path == "") {
  }
  else {
    string temp;
    if (path[0] != '/')
      temp = "/";
    temp += path;
    if (temp[temp.length()-1] != '/') temp += "/";

    LsInstsVisitor lsinst_visitor(interp, temp);
    module->Accept(lsinst_visitor);
  }

  return TCL_OK;
}

static int netlist_lsnets  (ClientData clientData,	/* Not used. */
                            Tcl_Interp *interp,         /* Current interpreter */
                            int objc,                   /* Number of arguments */
                            Tcl_Obj * objv[]		/* Argument strings */
                            )
{
  int stat = getlm(false, bfl_netlist);
  if (stat != 0) return TCL_ERROR;

  if (objc > 3) {
    Tcl_WrongNumArgs (interp, 2, objv, "path");
    return TCL_ERROR;
  }
  toTclResult (interp, Tcl_NewListObj(0,0));

  BString path("");
  if (objc == 3) {
    path = Tcl_GetString( objv[2] );
  }
  if (path == "")
    return TCL_OK;

  BString found;
  VeriModule *module = tc_findModuleTcl (interp, NULL, path, found);

  if (module == NULL) {
    return TCL_ERROR;
  }


  // Get the scope of this module :
  VeriScope *module_scope = module->GetScope() ;
 
  // Now iterate over the declared identifiers ((VeriIdDef*s) in the hash table (scope->DeclArea()) in this scope :
  MapIter mi ;
  VeriIdDef *id ;
  char *id_name ;
  FOREACH_MAP_ITEM(module_scope->DeclArea(), mi, &id_name, &id) {
    // Rule out all identifiers declared here, except for 'nets' :

    //cerr << "netid -- " ;  id->PrettyPrint (cerr, 1);  cerr << endl;

    if (!(id->IsNet() || id->IsReg())) continue ;
    
    // Here, 'id' is a 'net', declared in the module
    // Use the extended VhdlIdDef class API (in file VeriId.h) to do what you want
    string name = path + "/" + id->Name();
    string exname;
    HdlUtils::getNetName(id, exname);
    name += exname;
    appendTclResult (interp, name.c_str());
  }

  return TCL_OK;
}

// get the list of verilog files used
static int netlist_getfiles (ClientData clientData,	/* Not used. */
	    Tcl_Interp *interp,         /* Current interpreter */
	    int objc,                   /* Number of arguments */
	    Tcl_Obj * objv[]		/* Argument strings */
	    )
{
  int stat = getlm(false, bfl_netlist);
  if (stat != 0) return TCL_ERROR;

  if (objc > 3) {
    Tcl_WrongNumArgs (interp, 2, objv, "path");
    return TCL_ERROR;
  }
  toTclResult (interp, Tcl_NewListObj(0,0));

  BString path("");
  if (objc == 3) {
    path = Tcl_GetString( objv[2] );
  }
  if (path == "")
    return TCL_OK;

  BString found;
  VeriModule *module = tc_findModuleTcl (interp, NULL, path, found);

  if (module == NULL) {
    return TCL_ERROR;
  }

  // Get the list of verilog files used by the module
  std::map<string, int> list;
  HdlUtils::getVerilogFileList(module, list);

  std::map<string, int>::iterator fileIter;
  char currentPath[FILENAME_MAX];
  string ret_filename;
  const char *file_name;

  getcwd(currentPath, FILENAME_MAX);

  for (fileIter = list.begin(); fileIter != list.end(); fileIter++) {

    file_name = (*fileIter).first.c_str();

    // If it is not fully qualified directory then add 'pwd'
    if (file_name[0] != '/') {
      ret_filename = currentPath;
      ret_filename += "/";
    }
    else
      ret_filename = "";

    ret_filename += file_name;
    appendTclResult (interp, ret_filename.c_str());
  }

  return TCL_OK;
}


static int netlist_gen_scemi  (ClientData clientData,    /* Not used. */
			       Tcl_Interp *interp,       /* Current interpreter */
			       int objc,                 /* Number of arguments */
			       Tcl_Obj * objv[]	     /* Argument strings */
			       )
{
  int stat = getlm(false, bfl_netlist);
  if (stat != 0) return TCL_ERROR;


  BString modname("");
  BString wrapped_module("");
  if (objc > 2) {
    modname = Tcl_GetString( objv[2] );
    wrapped_module = Tcl_GetString( objv[3] );
  }
  if (modname == "")
    return TCL_OK;

  BString scemi_layer_filename("");
  if (scemi_layer_filename == "")
    scemi_layer_filename = "SceMiLayer.bsv";

  BString found("");
  VeriModule *module = tc_findModuleByNameTcl (interp, modname);

  if (module == NULL) {
    return TCL_ERROR;
  }

  /*
  BString sig;
  int objcc;
  Tcl_Obj *objPtr;
  int ret = Tcl_ListObjLength (interp, objv[4], & objcc );
  string m, s;
  std::map<string, string> stmap;
  size_t pos;

  for (int i=0; i < objcc; ++i) {
    ret = Tcl_ListObjIndex(interp, objv[4], i, &objPtr);
    sig = Tcl_GetStringFromObj (objPtr, NULL);
    s = sig;
    m = "";
    pos = s.find(':');
    if (pos != string::npos) {
      m = s.substr(0, pos);
      s = s.substr(pos+1);
    }
    stmap[s] = m;
    if (TCL_OK != ret) {
      return TCL_ERROR;
    }
  }
  */

  hdlUtils->generateSceMiLayer(module, wrapped_module.c_str(), scemi_layer_filename.c_str(),
			       "");

  return TCL_OK;
}

static int netlist_gen_pin_file  (ClientData clientData,    /* Not used. */
				  Tcl_Interp *interp,       /* Current interpreter */
				  int objc,                 /* Number of arguments */
				  Tcl_Obj * objv[]	    /* Argument strings */
				  )
{
  int stat = getlm(false, bfl_netlist);
  if (stat != 0) return TCL_ERROR;

  BString modname("");
  BString filename("");
  if (objc > 2) {
    filename = Tcl_GetString( objv[2] );
    modname = Tcl_GetString( objv[3] );
  }
  if (modname == "")
    return TCL_OK;

  if (filename == "")
    filename = modname + ".pin";

  BString found("");
  VeriModule *module = tc_findModuleByNameTcl (interp, modname);

  if (module == NULL) {
    return TCL_ERROR;
  }

  hdlUtils->generatePinFile(filename.c_str(), modname.c_str());

  return TCL_OK;
}

static int netlist_gen_tbtemplate (ClientData clientData,    /* Not used. */
				  Tcl_Interp *interp,       /* Current interpreter */
				  int objc,                 /* Number of arguments */
				  Tcl_Obj * objv[]	    /* Argument strings */
				  )
{
  int stat = getlm(false, bfl_netlist);
  if (stat != 0) return TCL_ERROR;

  BString modname("");
  BString cfgfilename("");
  BString pinfilename("");
  BString outdir("");
  BString capidir("");

  if (objc > 5) {
    modname = Tcl_GetString( objv[2] );
    cfgfilename = Tcl_GetString( objv[3] );
    pinfilename = Tcl_GetString( objv[4] );
    outdir = Tcl_GetString( objv[5] );
    capidir = Tcl_GetString( objv[6] );
  }

  //printf("objc: %d netlist generate tbtemplate cfgfile: %s pinfile: %s outdir: %s capidir: %s\n", objc, cfgfilename.c_str(), pinfilename.c_str(),
  //	 outdir.c_str(), capidir.c_str());

  BString errMsg;
  TestBenchGenerator *tbgen = new TestBenchGenerator();
  stat = tbgen->readSpecFile(cfgfilename.c_str());
  if (!stat) {
    tbgen->getErrorMessage(errMsg);
    appendTclResultError (interp, errMsg);
    delete tbgen;
    return TCL_ERROR;
  }

  // Read pin file
  HdlUtils *hdlUtils = HdlUtils::init();
  stat &= hdlUtils->readPinFile(pinfilename.c_str());

  // Generate the capi
  if (capidir != ".none") {
    BString found("");
    VeriModule *module = tc_findModuleByNameTcl (interp, modname);

    if (module == NULL) {
      stat = 0;
    } else {
      stat &= tbgen->generateDutXactor(module, tbgen->bsvModuleName(), capidir.c_str());
    }
  }

  // Generate the testbench files
  stat &= tbgen->generateTestBenchTemplate(outdir.c_str());

  // Clean up
  HdlUtils::shutdown();
  delete tbgen;

  if (stat == 0)
    return TCL_ERROR;

  return TCL_OK;
}

static int netlist_gen_simtbtemplate (ClientData clientData,    /* Not used. */
				      Tcl_Interp *interp,       /* Current interpreter */
				      int objc,                 /* Number of arguments */
				      Tcl_Obj * objv[]	    /* Argument strings */
				      )
{
  int stat = getlm(false, bfl_netlist);
  if (stat != 0) return TCL_ERROR;

  BString modname("");
  BString cfgfilename("");
  BString pinfilename("");
  BString outdir("");

  if (objc > 4) {
    modname = Tcl_GetString( objv[2] );
    cfgfilename = Tcl_GetString( objv[3] );
    pinfilename = Tcl_GetString( objv[4] );
    outdir = Tcl_GetString( objv[5] );
  }

  //printf("objc: %d netlist generate simtbtemplate cfgfile: -%s- pinfile: +%s+ outdir: %s\n", objc,
  //	 cfgfilename.c_str(), pinfilename.c_str(),
  //	 outdir.c_str());

  BString errMsg;
  TestBenchGenerator *tbgen = new TestBenchGenerator();
  if (cfgfilename != ".none") {
    stat = tbgen->readSpecFile(cfgfilename.c_str());
    if (!stat) {
      tbgen->getErrorMessage(errMsg);
      appendTclResultError (interp, errMsg);
      delete tbgen;
      return TCL_ERROR;
    }
  } else stat = 1;

  // Read pin file
  HdlUtils *hdlUtils = HdlUtils::init();
  stat &= hdlUtils->readPinFile(pinfilename.c_str());

  // Get the module
  VeriModule *module = tc_findModuleByNameTcl (interp, modname);

  // Generate the simtb verilog file
  stat &= tbgen->generateSimTbTemplate(module, outdir.c_str(), 1);

  // Clean up
  HdlUtils::shutdown();
  delete tbgen;

  if (stat == 0)
    return TCL_ERROR;

  return TCL_OK;
}

// implementation of the netlist command ensemble
static int
HhdlNetlist_Cmd(ClientData clientData,	/* Not used. */
		Tcl_Interp *interp,         /* Current interpreter */
		int objc,                   /* Number of arguments */
		Tcl_Obj * objv[]		/* Argument strings */
		)
{
  int index;
  static const cmd_struct_funptr decoder[] = {
    {"analyze",		"<options + files>",	netlist_analyze}
    ,{"shutdown",	"",			netlist_shutdown}
    ,{"lsinsts",	"<path>?",		netlist_lsinsts}
    ,{"lsnets",		"<path>?",		netlist_lsnets}
    ,{"getfiles",	"<path>?",		netlist_getfiles}
    ,{"gen_scemi",      "<module + newmodname + {clk,rst}>?", netlist_gen_scemi}
    ,{"gen_pinfile",	"<outfilename + modname>?", netlist_gen_pin_file}
    ,{"gen_tbtemplate",	"<modname + cfgfile + pinfile + outdir + capidir>?",  netlist_gen_tbtemplate}
    ,{"gen_simtbtemplate","<modname + cfgfile + pinfile + outdir>?",  netlist_gen_simtbtemplate}
    ,{0}
  };

  if (objc == 1) {
    dumpArguments (interp, decoder, Tcl_GetString(objv[0]));
    return TCL_ERROR ;
  }
  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], decoder, sizeof(cmd_struct_funptr),
                                          "command", 0, &index ) ) {
    return TCL_ERROR;
  }

  TclFunPtr func = decoder[index].function;
  int stat ;

  try {
    stat = func(clientData, interp, objc, objv);
  } catch (const exception &e) {
    cerr << "Exception caught: " << e.what() << endl;
    Tcl_AppendResult( interp, "exception caught: ", e.what(),  NULL) ;
    return TCL_ERROR;
  }

  return stat;
}

static void
HhdlNetlist_Cmd_Delete(ClientData clientData)
{
  rellm(bfl_netlist);
//   if (hdledit != NULL) {
//     delete hdledit;
//   }
}
