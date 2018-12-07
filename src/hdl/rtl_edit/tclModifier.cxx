
#include <iostream>
#include <errno.h>
#include <string.h>
#include "tclModifier.h"
#include "CktEdits.h"
#include "TclUtils.h"
//#include "VerificUtils.h"

using namespace std;

// Forward function declarations
#define TCLFUNC(name) static int name(ClientData, Tcl_Interp *, int objc, Tcl_Obj * objv[])
TCLFUNC(modify_list);
TCLFUNC(modify_replay);
TCLFUNC(modify_delete);
TCLFUNC(modify_addprobe);
TCLFUNC(modify_add_bsv_probes);
TCLFUNC(modify_addcosim);
TCLFUNC(modify_addcapture);
TCLFUNC(modify_addtrigger);
TCLFUNC(modify_addport);
TCLFUNC(modify_crossreference);
TCLFUNC(modify_compile);
TCLFUNC(modify_apply);
TCLFUNC(modify_params);
TCLFUNC(modify_drawin);
TCLFUNC(modify_drawout);
TCLFUNC(modify_partition);
TCLFUNC(modify_geninitscemi);
TCLFUNC(modify_genfinalscemi);
TCLFUNC(modify_gentestbench);

/////////////////////////////////////////////////
// TCL API on top of the CktEdits container class
int HhdlEditNL_Cmd(ClientData clientData,
                   Tcl_Interp *interp,
                   int objc,
                   Tcl_Obj * objv[])
{
  int index;
  static const cmd_struct_funptr decoder[] = {
    {"list",		"<indexes>?",		modify_list}
    ,{"dump",		"<indexes>?",		modify_list}
    ,{"replay",		"<indexes>?",		modify_replay}
    ,{"delete",		"<indexes>?",		modify_delete}
    ,{"addprobe",	"name path signal clock enable type?",	modify_addprobe}
    ,{"addbsvprobes",   "name pattern mpattern? type?",   modify_add_bsv_probes}
    ,{"addcosim",	"name path clock uclock? trigger? width? flavor?", modify_addcosim}
    ,{"addcapture",	"name path signal clock enable trigger depth runwidth delay type?",	modify_addcapture}
    ,{"addtrigger",	"name path expr clock captures",			modify_addtrigger}
    ,{"addport",	"path name type expression width",	modify_addport}
    ,{"drawout",	"downpath toppath signal port",		modify_drawout}
    ,{"drawin",		"downpath toppath signal port",		modify_drawin}
    ,{"partition",      "boardspec modspec",    modify_partition}
    ,{"geninitscemi",   "cfg pinfile outdir",   modify_geninitscemi}
    ,{"genfinalscemi",  "cfg pinfile outdir",   modify_genfinalscemi}
    ,{"gentestbench",   "cfg pinfile",          modify_gentestbench}
    ,{"updateparams",	"filein <fileout>?",	modify_params}
    ,{"crossreference",	"",			modify_crossreference}
    ,{"compile",	"",			modify_compile}
    ,{"apply",		"outdir",	        modify_apply}
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


void HhdlEditNL_Cmd_Delete(ClientData clientData){
}


//  Sub commands
///////////////////////////////////////////////////////////////////
static int modify_list(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{

  int stat = TCL_OK;
  BString cmdname(Tcl_GetString(objv[1]));
  bool useronly = cmdname == "list";

  IntSet idlist;
  if (objc == 2) {
    // idlist is empty -- this is ok
  } else if (objc == 3) {
    stat = getInts (interp, objv[2], idlist);
  } else {
    Tcl_WrongNumArgs (interp, 2, objv, "<list of ids>");
    stat = TCL_ERROR;
  }

  if (stat == TCL_OK) {
    Tcl_Obj *o = CktEdits::dump (interp, useronly, idlist);
    toTclResult (interp, o);
  }
  return stat;
}
static int modify_replay(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
  IntSet idlist;
  int stat = TCL_OK;
  if (objc == 2) {
    // idlist is empty -- this is ok
  } else if (objc == 3) {
    stat = getInts (interp, objv[2], idlist);
  } else {
    Tcl_WrongNumArgs (interp, 2, objv, "<list of ids>");
    stat = TCL_ERROR;
  }

  if (stat == TCL_OK) {
    Tcl_Obj *o = CktEdits::replay (interp, idlist);
    toTclResult (interp, o);
  }
  return TCL_OK;
}

///////////////////////////////////////////////////////////////////
static int modify_delete(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
  IntList idlist;
  int stat = TCL_OK;
  if (objc == 3) {
    stat = getInts (interp, objv[2], idlist);
  } else {
    Tcl_WrongNumArgs (interp, 2, objv, "<list of ids>");
    stat = TCL_ERROR;
  }

  if (stat == TCL_OK) {
    Tcl_Obj *result ;
    stat = CktEdits::rmEdit (interp, idlist, &result);
    toTclResult (interp, result);
  }
  return stat ;
}

///////////////////////////////////////////////////////////////////
static int modify_addprobe(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
    if (objc < 7 || objc > 9) {
      Tcl_WrongNumArgs (interp, 2, objv, (char*) "<name> <instance> <pattern> <clock> <enable> are required");
      return TCL_ERROR ;
    }
    int retStatus = CktEdits::addProbe (interp, objc, objv);
    return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_add_bsv_probes(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
    if (objc < 4 || objc > 6) {
      Tcl_WrongNumArgs (interp, 2, objv, (char*) "<name> <hierpattern> are required");
      return TCL_ERROR ;
    }
    int retStatus = CktEdits::addBsvProbes (interp, objc, objv);
    return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_addcosim(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
    if (objc < 5 || objc > 9) {
      Tcl_WrongNumArgs (interp, 2, objv, (char*) "<name> <instance> <clock> are required");
      return TCL_ERROR ;
    }
    int retStatus = CktEdits::addCosim (interp, objc, objv);
    return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_addcapture(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
    if (objc < 11 || objc > 13) {
      Tcl_WrongNumArgs (interp, 2, objv, (char*) "<name> <instance> <expr> <clock> <enable> <trigger> <depth> <width> <delay> are required");
      return TCL_ERROR ;
    }
    int retStatus = CktEdits::addCapture (interp, objc, objv);
    return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_addtrigger(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
    if (objc != 7) {
      Tcl_WrongNumArgs (interp, 2, objv, (char*) "<name> <instance> <bool_expr> <clock> <captures> are required");
      return TCL_ERROR ;
    }
    int retStatus = CktEdits::addTrigger (interp, objc, objv);
    return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_addport(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
    if (objc != 7) {
      Tcl_WrongNumArgs (interp, 2, objv, (char*) "<instance> <name> <type> <expression> <width> are required");
      return TCL_ERROR ;
    }
    int retStatus = CktEdits::addPort (interp, objc, objv);
    return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_compile(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
  if (objc != 2) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "does not any arguments");
    return TCL_ERROR ;
  }
  int retStatus = CktEdits::compile (interp, objc, objv);
  return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_crossreference(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
  if (objc != 2) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "does not any arguments");
    return TCL_ERROR ;
  }
  int retStatus = CktEdits::crossReference (interp, objc, objv);
  return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_apply(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
  if (objc != 3) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "<outdir> are required");
    return TCL_ERROR ;
  }

  int retStatus = CktEdits::apply (interp, objc, objv);
  return retStatus ;
}

///////////////////////////////////////////////////////////////////
static int modify_params(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
  BString infile;
  BString outfile;
  int retStatus ;

  if (objc == 3) {
    if  ( TCL_OK != (retStatus = getString(interp, objv[2], infile)) ) {
      return retStatus;
    }
    outfile = infile;
  }
  else if (objc == 4) {
    if (TCL_OK != (retStatus = getString(interp, objv[2], infile)) ||
        TCL_OK != (retStatus = getString(interp, objv[3], outfile)) ) {
      return retStatus;
    }
  }
  else {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "infile <outfile>?");
    return TCL_ERROR ;
  }


  BString tmpfile = outfile + ".tmp";
  while (tmpfile == infile) {
    tmpfile += ".tmp";
  }
  retStatus = CktEdits::params (interp, infile, tmpfile);
  if (retStatus == TCL_OK) {
    retStatus = rename (tmpfile.c_str(), outfile.c_str());
    if (retStatus == 0) {
      toTclResult (interp, outfile);
      retStatus = TCL_OK;
    }
    else {
      BString errMsg;
      errMsg = "Cannot open file '" + outfile + "' for writing; ";
      errMsg += strerror(errno);
      toTclResult (interp, errMsg);
      retStatus = TCL_ERROR;
    }
  }
  return retStatus ;
}

// "downpath toppath signal port"
static int modify_drawout(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
  if (objc != 6) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "down_path top_path signal port");
    return TCL_ERROR ;
  }

  return CktEdits::drawout (interp, objc, objv);
}

static int modify_drawin(ClientData clientData,
                Tcl_Interp *interp,
                int objc,
                Tcl_Obj * objv[])
{
  if (objc != 6) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "down_path top_path signal port");
    return TCL_ERROR ;
  }
  return CktEdits::drawin (interp, objc, objv);
}

static int modify_partition(ClientData clientData,
                            Tcl_Interp *interp,
                            int objc,
                            Tcl_Obj * objv[])
{
  if (objc != 4) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "boardspec modspec");
    return TCL_ERROR ;
  }
  return CktEdits::partition (interp, objc, objv);
}

static int modify_geninitscemi(ClientData clientData,
			       Tcl_Interp *interp,
			       int objc,
			       Tcl_Obj * objv[])
{
  if (objc != 5) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "cfgfile pinfile outdir");
    return TCL_ERROR ;
  }
  return CktEdits::genInitSceMi (interp, objc, objv);
}


static int modify_genfinalscemi(ClientData clientData,
				Tcl_Interp *interp,
				int objc,
				Tcl_Obj * objv[])
{
  if (objc != 5) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "cfgfile pinfile outdir");
    return TCL_ERROR ;
  }
  return CktEdits::genFinalSceMi (interp, objc, objv);
}

static int modify_gentestbench(ClientData clientData,
			       Tcl_Interp *interp,
			       int objc,
			       Tcl_Obj * objv[])
{
  if (objc != 5) {
    Tcl_WrongNumArgs (interp, 2, objv, (char*) "cfgfile pinfile outdir");
    return TCL_ERROR ;
  }
  return CktEdits::genTestbench (interp, objc, objv);
}

