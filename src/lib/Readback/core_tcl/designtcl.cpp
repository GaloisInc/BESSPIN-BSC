
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#include <tcl.h>
#include "designtcl.h"
#include "Design.hpp"
#include "Utils.hpp"

#include "bstcl_base.h"

#define PKG_NAME    "llbits"
#define PKG_VERSION "1.0"
#define NS          "llbits"


static Design * global;
static unsigned int cmdlog;
static std::ofstream file;
static unsigned int fileopen = 0;
extern std::ofstream *rdback_log_file;

void remove_curly_bracket(std::string &s)
{
  if (s[0] == '{' && s[s.length()-1] == '}')
    s = s.substr(1, s.length()-2);
}

static int llbits_netlist_load(
    ClientData clientData,	// ReaderData *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{

    Design * design = *(Design**)clientData;

    std::string rtlpath;
    std::string llpath;
    std::string synthpath;
    std::string logpath;
    std::string skip;
    std::string unit;

    if (objc >= 3)
      {
	rtlpath = Tcl_GetString(objv[2]);
      }

    if (rtlpath.empty())
      {
	return TCL_ERROR;
      }

    if (objc >= 4)
      {
	llpath = Tcl_GetString(objv[3]);
      }

    if (llpath.empty())
      {
	return TCL_ERROR;
      }

    if (objc >= 5)
      {
	synthpath = Tcl_GetString(objv[4]);
      }

    if (synthpath.empty())
      {
	return TCL_ERROR;
      }

    if (objc >= 6)
      {
	logpath = Tcl_GetString(objv[5]);
      }

    if (logpath.empty())
      {
	return TCL_ERROR;
      }

    bool skip_vcd = true;
    if (objc >= 7) {
      skip = Tcl_GetString(objv[6]);
      
      if (skip.empty()) {
	return TCL_ERROR;
      }
      if (skip.compare("false") == 0 || skip.compare("0") == 0) {
	skip_vcd = false;
      }
    } else {
      skip_vcd = false;
    }

    bool unit_time = true;
    if (objc >= 8) {
      unit = Tcl_GetString(objv[7]);
      
      if (unit.empty()) {
	return TCL_ERROR;
      }
      if (unit.compare("false") == 0 || unit.compare("0") == 0) {
	unit_time = false;
      }
    } else {
      unit_time = false;
    }


    int ret = 0;

    // if (ret == 0) {
    //   ret = design->parse_edf(synthpath);
    // }

    if (ret == 0) {
      ret = design->parse_rtl(rtlpath);
    }

    if (ret == 0) {
      ret = design->parse_synth(synthpath);
    }

    if (ret == 0) {
      ret = design->parse_ll(llpath);
    }

    if (ret == 0) {
      ret = design->parse_log(logpath, skip_vcd);
    }

    if (ret == 0) {
      ret = design->syncConfig();
    }

    if (ret == 0 && unit_time) {
      ret = design->setUnitTime();
    }

    // Log this function call
    if (cmdlog)
      {
	//file << "bsdebug::netlist load " << synthpath << " " << rtlpath << " " << llpath << " " << logpath << std::endl;
      }

    return (ret == 0) ? TCL_OK : TCL_ERROR;

}

static int llbits_netlist_lsinst(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    Design * design = *(Design**)clientData;

    std::string path;
    if (objc == 3)
    {
      path.append(Tcl_GetString(objv[2]));
    }

    Tcl_Obj * r = Tcl_NewListObj(0,0);

    Module * module = design->findModule(RTL, path);

    if (module == NULL)
    {
        std::cerr << "cannot find module: " << path << std::endl;
        return TCL_ERROR;
    } else {
      //      std::cerr << "found module: " << path << std::endl;
    }

    const tModuleSet & children = module->getChildren(true); // true means exclude hidden children
    // std::cerr << "mod: " << module << std::endl;
    // std::cerr << "child count: " << children.size() << std::endl;

    for (tModuleSet::const_iterator it = children.begin();
	 it != children.end(); it++)
      {
	Module * child = (*it);
	Tcl_Obj * o = Tcl_NewListObj(0,0);

	std::string netname = child->getPath();
	std::string shortname = child->getName();
	netname.append("/");
	netname.append(shortname);

	Tcl_Obj * id = Tcl_NewStringObj(netname.c_str(), netname.size());
	Tcl_ListObjAppendElement(interp, o, id);

	Tcl_Obj * n = Tcl_NewStringObj(shortname.c_str(), shortname.size());
	Tcl_ListObjAppendElement(interp, o, n);

//	printf("NET: %s SHORT: %s\n", netname.c_str(), shortname.c_str());

	Tcl_ListObjAppendElement(interp, r, o);
      }

    Tcl_SetObjResult(interp, r);

    // Log this function call
    if (cmdlog)
      {
	//file << "bsdebug::netlist lsinst " << path << std::endl;
      }

    return TCL_OK;
}


static int llbits_netlist_lsnet(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    Design * design = *(Design**)clientData;

    std::string path;
    if (objc == 3)
    {
        path.append(Tcl_GetString(objv[2]));
    }

    Tcl_Obj * r = Tcl_NewListObj(0,0);

    remove_curly_bracket(path);

    Module * module = design->findModule(RTL, path);
    if (module == NULL)
    {
        std::cerr << "cannot find module: " << path << std::endl;
        return TCL_ERROR;
    }

    //    std::cerr << "found module: " << path << std::endl;


    const tSignalSet & signals = module->getSignals(true);
    //    std::cerr << "signal count: " << signals.size() << std::endl;

    for (tSignalSet::const_iterator it = signals.begin();
	 it != signals.end(); it++)
      {
	Signal * signal = (*it);

	if (!signal->isHidden()) {
	  Tcl_Obj * o = Tcl_NewListObj(0,0);

	  std::string netname = module->getPath();
	  std::string shortname = signal->getName();
	  netname.append("/");
	  netname.append(module->getName());
	  netname.append("/");
	  netname.append(shortname);

	  Tcl_Obj * id = Tcl_NewStringObj(netname.c_str(), netname.size());
	  Tcl_ListObjAppendElement(interp, o, id);

	  if (signal->getWidth() > 1) {

	    std::ostringstream width;
	    width << signal->getWidth();
	    std::ostringstream avail;
	    avail << signal->getAvail();

	    if (signal->getAvail() == signal->getWidth() || signal->getAvail() == 0) {
	      shortname.append(" [");
	      shortname.append(width.str());
	      shortname.append("]");
	    } else {
	      shortname.append(" [");
	      shortname.append(avail.str());
	      shortname.append("/");
	      shortname.append(width.str());
	      shortname.append("]");
	    }
	  }
	  Tcl_Obj * n = Tcl_NewStringObj(shortname.c_str(), shortname.size());
	  Tcl_ListObjAppendElement(interp, o, n);

	  Tcl_Obj * tags = Tcl_NewListObj(0,0);
	
	  // create tags here
	  if (signal->getAvail() == 0)
	    {
	      std::string tag = "unavailable";
	      Tcl_Obj * n = Tcl_NewStringObj(tag.c_str(), tag.size());
	      Tcl_ListObjAppendElement(interp, tags, n);
	    }
	  else if (signal->isEnabled())
	    {
	      std::string tag = "enabled";
	      Tcl_Obj * n = Tcl_NewStringObj(tag.c_str(), tag.size());
	      Tcl_ListObjAppendElement(interp, tags, n);
	    }
	  else if (signal->getUnread() == signal->getWidth())
	    {
	      std::string tag = "pruned";
	      Tcl_Obj * n = Tcl_NewStringObj(tag.c_str(), tag.size());
	      Tcl_ListObjAppendElement(interp, tags, n);
	    }

	  Tcl_ListObjAppendElement(interp, o, tags);

	  Tcl_ListObjAppendElement(interp, r, o);
	}
      }

    Tcl_SetObjResult(interp, r);

    // Log this function call
    if (cmdlog)
      {
	//file << "bsdebug::netlist lsnet " << path << std::endl;
      }

    return TCL_OK;
}


static int llbits_netlist_enable(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    Design * design = *(Design**)clientData;

    std::string path;
    if (objc == 3)
    {
        path = Tcl_GetString(objv[2]);
    }

    Signal * signal = design->findSignal(RTL, path);
    if (signal == NULL)
    {
        std::cerr << "cannot find signal: " << path << std::endl;
        return TCL_ERROR;
    }
    design->enableSignal(signal);

    // Log this function call
    if (cmdlog)
      {
	file << "bsdebug::netlist enable " << path << std::endl;
      }

    return TCL_OK;
}



static int llbits_netlist_disable(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
  Design * design = *(Design**)clientData;

  std::string path;
  if (objc == 3)
    {
        path = Tcl_GetString(objv[2]);
    }

    Signal * signal = design->findSignal(RTL, path);
    if (signal == NULL)
    {
        std::cerr << "cannot find signal: " << path << std::endl;
        return TCL_ERROR;
    }
    design->disableSignal(signal);

    // Log this function call
    if (cmdlog)
      {
	file << "bsdebug::netlist disable " << path << std::endl;
      }

    return TCL_OK;
}

static int llbits_netlist_isenabled(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    Design * design = *(Design**)clientData;

    std::string path;
    if (objc == 3)
    {
        path = Tcl_GetString(objv[2]);
    }

    Signal * signal = design->findSignal(RTL, path);
    if (signal == NULL)
    {
        std::cerr << "cannot find signal: " << path << std::endl;
        return TCL_ERROR;
    }

    if (signal->isEnabled()) {
      Tcl_Obj * r = Tcl_NewStringObj("1", 1);
      Tcl_SetObjResult(interp, r);
    } else {
      Tcl_Obj * r = Tcl_NewStringObj("0", 1);
      Tcl_SetObjResult(interp, r);
    }

    return TCL_OK;
}


static int llbits_netlist_addterm(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    Design * design = *(Design**)clientData;

    std::string path;
    if (objc >= 3)
    {
        path = Tcl_GetString(objv[2]);
    }

    Signal * signal = design->findSignal(RTL, path);
    if (signal == NULL)
    {
        std::cerr << "cannot find signal: " << path << std::endl;
        return TCL_ERROR;
    }

    std::string value_str;
    if (objc >= 4)
      {
	value_str = Tcl_GetString(objv[3]);
      }

    std::string mask_str;
    if (objc >= 5)
      {
	mask_str = Tcl_GetString(objv[4]);
      }

    std::string track_str;
    if (objc >= 6)
      {
	track_str = Tcl_GetString(objv[5]);
      }

    unsigned int num, track;
    unsigned int matches;
    track = 0;
    matches = sscanf (value_str.c_str(), "%x", &num);
    if (matches == 0) {
      return TCL_ERROR;
    }

    matches = sscanf (track_str.c_str(), "%d", &track);
    if (matches == 0) {
      return TCL_ERROR;
    }

    unsigned int id = design->addTerm(signal, num, mask_str, track);

    std::ostringstream id_stream;
    id_stream << id;
    std::string id_str = id_stream.str();

    Tcl_Obj * r = Tcl_NewStringObj(id_str.c_str(), id_str.size());

    Tcl_SetObjResult(interp, r);

    // Log this function call
    if (cmdlog)
      {
	file << "bsdebug::netlist addterm " << path << " " << value_str << " " << mask_str << " " << track_str << std::endl;
      }

    return TCL_OK;
}

static int llbits_netlist_removeterm(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    Design * design = *(Design**)clientData;

    std::string id_str;
    if (objc >= 3)
      {
	id_str = Tcl_GetString(objv[2]);
      }

    unsigned int id;
    unsigned int matches;
    id = 0;

    matches = sscanf (id_str.c_str(), "%d", &id);
    if (matches == 0) {
      return TCL_ERROR;
    }

    design->removeTerm(id);

    // Log this function call
    if (cmdlog)
      {
	file << "bsdebug::netlist removeterm " << id_str << std::endl;
      }

    return TCL_OK;
}

static int llbits_netlist_addBreakCode(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    Design * design = *(Design**)clientData;

    std::string code_str;
    if (objc >= 3)
      {
	code_str = Tcl_GetString(objv[2]);
      }

    std::string orig_code = code_str;

    code_str = utils::binaryToHex(code_str);
    unsigned int code;
    unsigned int matches;
    code = 0;

    matches = sscanf (code_str.c_str(), "%x", &code);
    if (matches == 0) {
      return TCL_ERROR;
    }

    design->addBreakCode(code);

    // Log this function call
    if (cmdlog)
      {
	file << "bsdebug::netlist addcode " << orig_code << std::endl;
      }

    return TCL_OK;
}

static int llbits_netlist_cmdLog(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    //Design * design = *(Design**)clientData;
    const char *filename = NULL;
    std::string val;
    if (objc >= 3)
      {
	val = Tcl_GetString(objv[2]);
      }

    // Log this function call
    if (cmdlog)
      {
	//file << "bsdebug::netlist setCmdLog " << val << std::endl;
      }

    if (val == "on") {
      cmdlog = 1;
      if (fileopen == 0) {
	filename = getenv("RDBACKLOG");
	if (filename == NULL) {
	  filename = "rdback.log";
	}
	file.open(filename);
	rdback_log_file = &file;
	fileopen = 1;
      }
    } else {
      cmdlog = 0;
    }

    return TCL_OK;
}

static int llbits_netlist_refreshHW(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{

  Design * design = *(Design**)clientData;
  design->refreshHW();

  // Log this function call
  if (cmdlog)
    {
      file << "bsdebug::netlist refreshhw" << std::endl;
    }

  return TCL_OK;
}


static int llbits_netlist_framecount(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
    Design * design = *(Design**)clientData;

    unsigned int count = design->getInUseFrameCount();

    std::ostringstream num;
    num << count;
    std::string size = num.str();

    Tcl_Obj * r = Tcl_NewStringObj(size.c_str(), size.size());

    Tcl_SetObjResult(interp, r);

    // Log this function call
    if (cmdlog)
      {
	//file << "bsdebug::netlist framecount" << std::endl;
      }

    return TCL_OK;
}


static int llbits_netlist_dump(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
//     ReaderData * data = *(ReaderData**)clientData;

//     data->logic->dump();
//     data->hier->dump();

  return TCL_OK;
}

static int llbits_netlist_flush(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{

  Design * design = *(Design**)clientData;
  design->flushVCD();

  // Log this function call
  if (cmdlog)
    {
      file << "bsdebug::netlist flush" << std::endl;
    }

  return TCL_OK;
}


static int llbits_netlist_reset(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{

  Design * design = *(Design**)clientData;
  design->doReset();

  // Log this function call
  if (cmdlog)
    {
      file << "bsdebug::netlist reset" << std::endl;
    }

  return TCL_OK;
}


static int llbits_netlist_invalidate(
    ClientData clientData,	// Design *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{
// #if defined(READBACKPROBES)
//     ReaderData * data = *(ReaderData**)clientData;

//     data->logic->invalidateAll();
//     data->logic->flushVCD();
// #endif

  return TCL_OK;
}

static int llbits_netlist_export(
    ClientData clientData,	// ReaderData *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{

    Design * design = *(Design**)clientData;

    std::string filename;
    std::string include;

    if (objc >= 3)
      {
	filename = Tcl_GetString(objv[2]);
      }

    if (filename.empty())
      {
	return TCL_ERROR;
      }

    bool include_hidden = true;
    if (objc >= 4) {
      include = Tcl_GetString(objv[3]);
      
      if (include.empty()) {
	return TCL_ERROR;
      }
      if (include.compare("false") == 0 || include.compare("0") == 0) {
	include_hidden = false;
      }
    } else {
      include_hidden = false;
    }

    int ret = 0;

    if (ret == 0) {
      ret = design->exportDesign(filename, include_hidden);
    }

    return (ret == 0) ? TCL_OK : TCL_ERROR;

}

static int llbits_netlist_import(
    ClientData clientData,	// ReaderData *
    Tcl_Interp *interp,         // Current interpreter
    int objc,                   // Number of arguments
    Tcl_Obj * const objv[]	// Argument strings
    )
{

    Design * design = *(Design**)clientData;

    std::string filename;
    std::string skip;
    std::string unit;

    if (objc >= 3)
      {
	filename = Tcl_GetString(objv[2]);
      }

    if (filename.empty())
      {
	return TCL_ERROR;
      }

    bool skip_vcd = true;
    if (objc >= 4) {
      skip = Tcl_GetString(objv[3]);
      
      if (skip.empty()) {
	return TCL_ERROR;
      }
      if (skip.compare("false") == 0 || skip.compare("0") == 0) {
	skip_vcd = false;
      }
    } else {
      skip_vcd = false;
    }

    bool unit_time = true;
    if (objc >= 5) {
      unit = Tcl_GetString(objv[4]);
      
      if (unit.empty()) {
	return TCL_ERROR;
      }
      if (unit.compare("false") == 0 || unit.compare("0") == 0) {
	unit_time = false;
      }
    } else {
      unit_time = false;
    }

    int ret = 0;

    if (ret == 0) {
      ret = design->parse_xrf(filename, skip_vcd);
    }

    if (ret == 0) {
      ret = design->syncConfig();
    }

    if (ret == 0 && unit_time) {
      ret = design->setUnitTime();
    }

    return (ret == 0) ? TCL_OK : TCL_ERROR;

}

extern "C" int llbits_netlist_cmd(
    ClientData clientData,    //  &(GlobalXactor),
    Tcl_Interp *interp,      	// Current interpreter
    int objc,               	// Number of arguments
    Tcl_Obj *const objv[]   	// Argument strings
    )
{
    static const cmd_struct_funptr cmds[] = {
        {"load",       "<path> <path> <skip_vcd>?", llbits_netlist_load},
        {"lsinst",     "<path>?",                   llbits_netlist_lsinst},
        {"lsnet",      "<path>?",                   llbits_netlist_lsnet},
        {"enable",     "<path>?",                   llbits_netlist_enable},
        {"disable",    "<path>?",                   llbits_netlist_disable},
        {"isenabled",  "<path>?",                   llbits_netlist_isenabled},
	{"addterm",    "<path> value mask track",   llbits_netlist_addterm},
	{"removeterm", "id",                        llbits_netlist_removeterm},
	{"addcode",    "binary",                    llbits_netlist_addBreakCode},
	{"cmdlog",     "val",                       llbits_netlist_cmdLog},
	{"refreshhw",  "",                          llbits_netlist_refreshHW},
	{"framecount", "",                          llbits_netlist_framecount},
        {"dump",       "",                          llbits_netlist_dump},
        {"flush",      "",                          llbits_netlist_flush},
        {"reset",      "",                          llbits_netlist_reset},
        {"invalidate", "",                          llbits_netlist_invalidate},
        {"export",     "filename",                  llbits_netlist_export},
        {"import",     "filename <skip_vcd>?",      llbits_netlist_import},
        {NULL},
    };

    if (objc == 1)
    {
        //dumpArguments( interp, cmds, Tcl_GetString(objv[0]));
        return TCL_ERROR;
    }

    int index;
    if (TCL_OK != Tcl_GetIndexFromObjStruct(interp, objv[1], cmds, sizeof(cmd_struct_funptr), "command", 0, &index ) )
    {
        return TCL_ERROR;
    }

    tclfunptr func = cmds[index].function;
    int stat;

    try
    {
        stat = func(clientData, interp, objc, objv);
    }
    catch (const std::exception &e)
    {
        std::cerr << "Exception caught: " << e.what() << std::endl;
        Tcl_AppendResult( interp, "exception caught: ", e.what(),  NULL) ;
        return TCL_ERROR;
    }

    return stat;
}

extern "C" void llbits_netlist_cmd_delete(ClientData clientData)
{
}


// Exit handler called during exit.
extern "C" void llbits_ExitHandler (ClientData clientData)
{
}

// Function called if/when the dynamic library is unloaded
// Function name must match package library name
extern "C" int llbits_Unload (Tcl_Interp * interp,  int flags)
{
    if (flags & TCL_UNLOAD_DETACH_FROM_PROCESS)
    {
        Tcl_DeleteExitHandler ( llbits_ExitHandler, NULL );

	delete global;
	global = NULL;
    }

    return TCL_OK;
}

// Package initialization function -- called during package load/require
// function name must match package library name
extern "C" int llbits_Init(Tcl_Interp *interp)
{
    Tcl_Namespace* nsptr = NULL;

    try
    {
        // register the exit handler
        Tcl_CreateExitHandler( llbits_ExitHandler, NULL );

        // Dynmaic binding of this extension to tcl
        if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL)
        {
            return TCL_ERROR;
        }

        // Create a namespace NS
        nsptr = (Tcl_Namespace*) Tcl_CreateNamespace(interp, NS, NULL, NULL);
        if (nsptr == NULL)
        {
            return TCL_ERROR;
        }

        // Provide the tcl package
        if (Tcl_PkgProvide(interp, PKG_NAME, PKG_VERSION) != TCL_OK)
        {
            return TCL_ERROR;
        }

        // Register commands to this tcl extension
        // A top-level tcl llbits::netlist command -- application specific boilerplate
	global = new Design;

        Tcl_CreateObjCommand(interp,
                             NS "::netlist",
                             (Tcl_ObjCmdProc *) llbits_netlist_cmd,
                             (ClientData) &global,
                             (Tcl_CmdDeleteProc *) llbits_netlist_cmd_delete);
        Tcl_Export(interp, nsptr, "netlist", 0);

        // Other command can go here
    }
    catch (const std::exception & error)
    {
        Tcl_AppendResult(interp, error.what()
                         ,"\nCould not initialize " PKG_NAME " tcl package"
                         ,(char *) NULL);
        return TCL_ERROR;
    }

    return TCL_OK;
}


