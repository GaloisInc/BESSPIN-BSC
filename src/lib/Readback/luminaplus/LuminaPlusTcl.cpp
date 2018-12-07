// Copyright Bluespec Inc. 2009-2016

//#include <iostream>
//#include <string>
//#include <cstdlib>
//#include <cstring>

#include "tcl.h"
#include "bstcl_base.h"
#include "designtcl.h"

#include "Design.hpp"

#include "LuminaPlusControl.hpp"

using namespace std;

#define PKG_NAME    "BSDebug"
#define NS          "bsdebug"
#define PKG_VERSION "1.0"

static unsigned int verbose = 0;
static unsigned int cmdlog = 0;
std::ofstream *rdback_log_file = NULL;

/////////////////////////////////////////////////////////////////
//
/////////////////////////////////////////////////////////////////

void dumpArguments (Tcl_Interp *interp, const cmd_struct cmds[], const char * given )
{
  Tcl_AppendResult(interp, "wrong # args: should be one of:\n",  (char *) NULL);
  for (const cmd_struct *p = & cmds[0];  p->cname ; ++p) {
    Tcl_AppendResult(interp, "  ", given, " ",
                     p->cname, " ", p->help, "\n", (char *) NULL);
  }
}

void dumpArguments (Tcl_Interp *interp, const cmd_struct_funptr cmds[], const char * given )
{
  Tcl_AppendResult(interp, "wrong arguments: should be one of:\n",  (char *) NULL);
  for (const cmd_struct_funptr *p = & cmds[0];  p->cname ; ++p) {
    Tcl_AppendResult(interp, "  ", given, " ",
                     p->cname, " ", p->help, "\n", (char *) NULL);
  }
}

void dumpArguments (Tcl_Interp *interp, const cmd_struct_funptr_2 cmds[], const char * given )
{
  Tcl_AppendResult(interp, "wrong arguments: should be one of:\n",  (char *) NULL);
  for (const cmd_struct_funptr_2 *p = & cmds[0];  p->cname ; ++p) {
    Tcl_AppendResult(interp, "  ", given, " ",
                     p->cname, " ", p->help, "\n", (char *) NULL);
  }
}

/////////////////////////////////////////////////////////////////
//
/////////////////////////////////////////////////////////////////

// static extension global data
class LuminaPlusGlobalData {
public:
  bool                        m_initialized;
  LuminaPlusControl         * m_control;
  Design                    * m_design;
  RdBack::VCDWriter         * m_vcdWriter;

  // Simple initializer invoked when the extension is loaded
  LuminaPlusGlobalData()
    : m_initialized(false)
    , m_control(0)
    , m_design(0)
    , m_vcdWriter(0)
  {}

  ~LuminaPlusGlobalData ()
  {
    if (m_initialized) {
      destroy();
    }
  }

  // Initialization
  void init (const unsigned int port)
  {
    try {
      m_control    = new LuminaPlusControl(port);
      m_design     = new Design();
      m_vcdWriter  = new RdBack::VCDWriter("dump1.vcd");

      // Connect
      RdBackControl *pRdBackControl = m_control; // cast to the base class
      m_design->setControl(pRdBackControl);
      m_design->setVCDWriter(m_vcdWriter);

      m_control->startServiceLoop();

      m_initialized = true;
    } catch (...) {
      throw;
    }
  }

  // Destruction
  void destroy ()
  {
    m_initialized = false;

    if (m_design) {
      delete m_design;
      m_design = 0;
    }

    if (m_vcdWriter) {
      delete m_vcdWriter;
      m_vcdWriter = 0;
    }

    if (m_control) {
      delete m_control;
      m_control = 0;
    }

    fflush(stdout);
  }

} LuminaPlusGlobal;

/////////////////////////////////////////////////////////////////
//
/////////////////////////////////////////////////////////////////

// For C-style linkage
extern "C" {
  // Package intialization  and cleanup
  extern int Luminaplus_Init (Tcl_Interp * interp);
  extern int Luminaplus_Unload (Tcl_Interp * interp,  int flags);
  extern void Luminaplus_ExitHandler (ClientData clientData);

  static int LuminaPlus_Cmd(ClientData clientData,
			    Tcl_Interp *interp,
			    int objc,
			    Tcl_Obj *const objv[]);
  static void LuminaPlus_Cmd_Delete(ClientData clientData);
  
  static int RdBk_Cmd(ClientData clientData,
		      Tcl_Interp *interp,
		      int objc,
		      Tcl_Obj *const objv[]);
  static void RdBk_Cmd_Delete(ClientData clientData);
}

/////////////////////////////////////////////////////////////////
//
/////////////////////////////////////////////////////////////////

// Function called if/when the dynamic library is unloaded
// Function name must match package library name
int Luminaplus_Unload (Tcl_Interp * interp,  int flags)
{
  if (flags & TCL_UNLOAD_DETACH_FROM_PROCESS) {
    LuminaPlusGlobalData *pglobal = & LuminaPlusGlobal;
    pglobal->destroy();
    Tcl_DeleteExitHandler ( Luminaplus_ExitHandler, &LuminaPlusGlobal);
  }
  return TCL_OK;
}

// Exit handler called during exit.
void Luminaplus_ExitHandler (ClientData clientData)
{
  LuminaPlusGlobalData *pglobal = (LuminaPlusGlobalData *) clientData;
  pglobal->destroy();
}

// Package initialization function -- called during package load/require
// function name must match package library name
int Luminaplus_Init(Tcl_Interp *interp)
{
  Tcl_Namespace* nsptr = NULL;

  try {
    // Dynmaic binding of this extension to tcl
    // (This must be called before any other 'Tcl_*' function.)
    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
      return TCL_ERROR;
    }

    // register the exit handler
    Tcl_CreateExitHandler( Luminaplus_ExitHandler, &LuminaPlusGlobal);

    // Create a namespace NS
    nsptr = (Tcl_Namespace*) Tcl_CreateNamespace(interp, NS, NULL, NULL);
    if (nsptr == NULL) {
      return TCL_ERROR;
    }

    // Provide the tcl package
    if (Tcl_PkgProvide(interp, PKG_NAME, PKG_VERSION) != TCL_OK) {
      return TCL_ERROR;
    }

    // Register commands to this tcl extension

    // LuminaPlus initialization
    Tcl_CreateObjCommand(interp,
                         NS "::luminaplus",
                         (Tcl_ObjCmdProc *) LuminaPlus_Cmd,
                         (ClientData) &(LuminaPlusGlobal),
                         (Tcl_CmdDeleteProc *) LuminaPlus_Cmd_Delete);
    Tcl_Export(interp, nsptr, "luminaplus", 0);

    // Bluespec emulation control command with readback
    Tcl_CreateObjCommand(interp,
                         NS "::rdbk",
                         (Tcl_ObjCmdProc *) RdBk_Cmd,
                         (ClientData) &(LuminaPlusGlobal.m_control),
                         (Tcl_CmdDeleteProc *) RdBk_Cmd_Delete);
    Tcl_Export(interp, nsptr, "rdbk", 0);

    // Bluespec probe netlist command
    Tcl_CreateObjCommand(interp,
                         NS "::netlist",
                         (Tcl_ObjCmdProc *) llbits_netlist_cmd,
                         (ClientData) &(LuminaPlusGlobal.m_design),
                         (Tcl_CmdDeleteProc *) llbits_netlist_cmd_delete);
    Tcl_Export(interp, nsptr, "netlist", 0);

  } catch (const exception & error) {
    Tcl_AppendResult(interp, error.what()
                     ,"\nCould not initialize bsdebug tcl package"
                     ,(char *) NULL);
    return TCL_ERROR;
  }
  return TCL_OK;
}
/////////////////////////////////////////////////////////////////
//
/////////////////////////////////////////////////////////////////

// Utility function to report OK or timeout result to interp
static void tcl_result_OK_TIMEOUT (Tcl_Interp *interp,  bool ok)
{
  Tcl_Obj *r;
  if (ok) {
    r = Tcl_NewStringObj( "OK", -1);
  }
  else {
    r = Tcl_NewStringObj( "TIMEOUT", -1);
  }
  Tcl_SetObjResult(interp, r);
}

// useful constants
#define FREE_RUNNING_CYCLES_RDBK 0x1FFFFFFF
static const char* boolean_names[] = { "0", "1", "off", "on", "no", "yes" };

/////////////////////////////////////////////////////////////////
// implementation of the LuminaPlus config commands
/////////////////////////////////////////////////////////////////

static int LuminaPlus_Cmd(ClientData  clientData,          // &(LuminaPlusGlobal),
			  Tcl_Interp *interp,              // current interpreter
			  int         objc,                // Number of arguments
			  Tcl_Obj *const objv[]            // Argument strings
			  )
{
  int port;

  if (clientData == 0) {
    Tcl_AppendResult(interp, "Tcl command, ",
		     Tcl_GetString(objv[0]),
		     ", was called without proper initialization of clientData",
		     (char*)NULL);
    return TCL_ERROR;
  }

  // Command table
  enum LuminaPlusCmds {
    LUMINAPLUS_INIT,
    LUMINAPLUS_DELETE
  };

  static const cmd_struct cmds_str[] = {
    {  "init",            LUMINAPLUS_INIT,                 "<port>" }
    ,{ "delete",          LUMINAPLUS_DELETE,               "" }
    ,{ 0 }
  };

  // Cast clientdata into proper type
  LuminaPlusGlobalData *pGlobal = reinterpret_cast<LuminaPlusGlobalData*>(clientData);

  LuminaPlusCmds command;
  int index;
  if (objc == 1) goto wrongArgs;

  if (TCL_OK != Tcl_GetIndexFromObjStruct(interp, objv[1], cmds_str, sizeof(cmd_struct), "command", 0, &index)) {
    return TCL_ERROR;
  }

  command = static_cast<enum LuminaPlusCmds>(cmds_str[index].enumcode);

  switch(command) {
    case LUMINAPLUS_INIT:
      if (objc != 3) goto wrongArgs;

      if (Tcl_GetIntFromObj(interp, objv[2], &port) != TCL_OK) {
	return TCL_ERROR;
      }

      // Initialize readback
      try {
	pGlobal->init((unsigned int)port);
      } catch (const exception &err) {
	Tcl_AppendResult(interp, err.what(), "\nCould not initialize luminaplus", (char*)NULL);
	return TCL_ERROR;
      } catch (const string &err) {
	Tcl_AppendResult(interp, err.c_str(), "\nCould not initialize luminaplus", (char*)NULL);
	return TCL_ERROR;
      }
      break;

    case LUMINAPLUS_DELETE:
      pGlobal->destroy();
      break;
  }

  return TCL_OK;

 wrongArgs:
  dumpArguments(interp, cmds_str, Tcl_GetString(objv[0]));
  return TCL_ERROR;
}

static void LuminaPlus_Cmd_Delete(ClientData clientData)
{
}

/////////////////////////////////////////////////////////////////
// implementation of the emu command ensemble with readback
/////////////////////////////////////////////////////////////////

static int RdBk_Cmd(ClientData clientData, 	// &(GlobalXactor.simControl)
		    Tcl_Interp *interp,     	// Current interpreter
		    int objc,			// Number of arguments
		    Tcl_Obj *const objv[]	// Argument strings
		    )
{
  if ((clientData == 0) || (* (void **) clientData == 0)) {
    Tcl_AppendResult(interp, "Tcl command, "
                     ,Tcl_GetString(objv[0])
                     ,", was called without proper initialization of clientData"
                     , (char *) NULL);
    return TCL_ERROR;
  }
  LuminaPlusControl * ctrlXact = * (LuminaPlusControl **) clientData;
  enum EmuCmds {
    EMU_RUN, EMU_STOP, EMU_CONTINUE, EMU_FREE_RUNNING,
    EMU_QUERY, EMU_GET, EMU_SET, EMU_RESP, EMU_RDBACK_ON, EMU_RDBACK_OFF, EMU_RDBACK_CLR, EMU_RDBACK_ADD
  };
  static const cmd_struct cmds_str[] = {
    {"run",		EMU_RUN,		"<cycles>"}
    ,{"stop",		EMU_STOP,		""}
    ,{"continue",	EMU_CONTINUE,		""}
    ,{"free_running",	EMU_FREE_RUNNING,	""}
    ,{"query",		EMU_QUERY,		""}
    ,{"checkresponse",	EMU_RESP,		"<timeout>"}
    ,{"get",		EMU_GET,		"var"}
    ,{"set",		EMU_SET,		"var value"}
    ,{"rdback_on",	EMU_RDBACK_ON,		""}
    ,{"rdback_off",	EMU_RDBACK_OFF,		""}
    ,{"rdback_clr",	EMU_RDBACK_CLR,		""}
    ,{"rdback_add",	EMU_RDBACK_ADD,		"<addr>"}
   ,{0}                         // MUST BE LAST
  };

  if (objc == 1) {
    dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));
    return TCL_ERROR ;
  }
  // Get the sub command
  int index;
  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], cmds_str, sizeof(cmd_struct),
                                          "command", 0, &index ) ) {
    return TCL_ERROR;
  }
  EmuCmds command = (enum EmuCmds) cmds_str[index].enumcode;

  if (ctrlXact == NULL) {
    Tcl_AppendResult(interp, "Attempt to use simulation control transactor without initialization"
                     ,(char *) NULL);
    return TCL_ERROR;
  }

  switch (command) {
    case EMU_RUN:
      {
	if (objc != 3) goto wrongArgs;

	int run_cycles = 0;
	if (Tcl_GetIntFromObj(interp, objv[2], &run_cycles) != TCL_OK) {
	  return TCL_ERROR;
	}

	if (run_cycles < 1) {
	  Tcl_AppendResult (interp, "Cycle cound must be > 0", (char *)NULL);
	  return TCL_ERROR;
	} else {
	  bool sent = ctrlXact->sendEmuEdges(run_cycles);
	  if (verbose) {
	    printf("Running emulator for %0d cycles\n", run_cycles);
	    fflush(stdout);
	  }
          tcl_result_OK_TIMEOUT(interp, sent);

	  // Log this function call
	  if (cmdlog && rdback_log_file)
	    {
	      (*rdback_log_file) << "runNClocks " << run_cycles << std::endl;
	    }
	}
	break;
      }
    case EMU_STOP:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendEmuStop();
        tcl_result_OK_TIMEOUT(interp, sent);

	// Log this function call
	if (cmdlog && rdback_log_file)
	  {
	    (*rdback_log_file) << "bsdebug::rdbk stop" << std::endl;
	  }
	break;
      }
    case EMU_CONTINUE:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendEmuResume();
        tcl_result_OK_TIMEOUT(interp, sent);

	// Log this function call
	if (cmdlog && rdback_log_file)
	  {
	    (*rdback_log_file) << "bsdebug::rdbk continue" << std::endl;
	  }
	break;
      }
   case EMU_FREE_RUNNING:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendEmuEdges(FREE_RUNNING_CYCLES_RDBK);
	if (verbose) {
	  printf("Free_running emulator\n");
	  fflush(stdout);
	}
        tcl_result_OK_TIMEOUT(interp, sent);

	// Log this function call
	if (cmdlog && rdback_log_file)
	  {
	    (*rdback_log_file) << "bsdebug::rdbk free_running" << std::endl;
	  }
	break;
      }
    case EMU_RESP:
      {
	if (objc != 3) goto wrongArgs;

	int timeout_ms = 0;
	if (Tcl_GetIntFromObj(interp, objv[2], &timeout_ms) != TCL_OK) {
	  return TCL_ERROR;
	}

        RdBackStatus resp;
        bool stat = ctrlXact->getStatusTimed(resp, 0, 1000 * timeout_ms);
        if (! stat) {
          Tcl_SetObjResult(interp, Tcl_NewStringObj("", 0));
        }
        else {
          Tcl_Obj* objs[5];
          objs[0] = Tcl_NewBooleanObj(resp.running);
          objs[1] = Tcl_NewIntObj(resp.edges);
          objs[2] = Tcl_NewBooleanObj(resp.free_running);
          objs[3] = Tcl_NewBooleanObj(! stat);
          objs[4] = Tcl_NewWideIntObj(resp.cycle);
          Tcl_Obj* list_obj = Tcl_NewListObj(5, objs);
          Tcl_SetObjResult(interp, list_obj);
        }

	// Log this function call
	if (cmdlog && rdback_log_file)
	  {
	    (*rdback_log_file) << "bsdebug::rdbk checkresponse " << timeout_ms << std::endl;
	  }
	break;
      }
    case EMU_QUERY:
      {
	if (objc != 2) goto wrongArgs;

        // It is possible that there is status available from previous edges commands
        // Since edges send a response when completed.   Let use those response before sending
        // a new query request
        RdBackStatus resp;
        bool stat;
        if (! ctrlXact->getStatusNonBlocking(resp) ) {
          stat = ctrlXact->sendEmuQuery();
          stat &= ctrlXact->getStatusTimed(resp, 1, 0); // 1 second timeout
        } else {
	  stat = 1; // There was a previous status left over, we should send back a happy stat
	  while (ctrlXact->getStatusNonBlocking(resp)) {
	    stat = 1;
	  }
	}

	Tcl_Obj* objs[6];
	objs[0] = Tcl_NewBooleanObj(resp.running);
	objs[1] = Tcl_NewIntObj(resp.edges);
	objs[2] = Tcl_NewBooleanObj(resp.free_running);
	objs[3] = Tcl_NewBooleanObj( ! stat);
        objs[4] = Tcl_NewWideIntObj(resp.cycle);
	objs[5] = Tcl_NewBooleanObj(resp.rdback_on);
	Tcl_Obj* list_obj = Tcl_NewListObj(6, objs);
	Tcl_SetObjResult(interp, list_obj);

	break;
      }
    case EMU_GET:
      {
	int vindex;
	static const char *get_vars[] = {
	  "verbose",
	  "cmdlog",
          NULL
	};
	enum GetVars {
	  GET_VERBOSE,
	  GET_CMDLOG
	};

	if (objc != 3) goto wrongArgs;

	if (Tcl_GetIndexFromObj(interp, objv[2], get_vars, "variables", 0, &vindex) != TCL_OK) {
	  return TCL_ERROR;
	}

	switch ((enum GetVars) vindex) {
	  case GET_VERBOSE:
	    {
	      Tcl_Obj* v_obj = Tcl_NewBooleanObj(verbose);
	      Tcl_SetObjResult(interp, v_obj);
	      break;
	    }
	  case GET_CMDLOG:
	    {
	      Tcl_Obj* v_obj = Tcl_NewBooleanObj(cmdlog);
	      Tcl_SetObjResult(interp, v_obj);
	      break;
	    }
	}

	break;
      }
    case EMU_SET:
      {
	int vindex;
	static const char *set_vars[] = {
	  "verbose",
	  "cmdlog",
	  NULL
	};
	enum SetVars {
	  SET_VERBOSE,
	  SET_CMDLOG
	};

	if (objc != 4) goto wrongArgs;

	if (Tcl_GetIndexFromObj(interp, objv[2], set_vars, "variables", 0, &vindex) != TCL_OK) {
	  return TCL_ERROR;
	}

	switch ((enum SetVars) vindex) {
  	  case SET_VERBOSE:
	    {
	      int vindex;
	      if (Tcl_GetIndexFromObj(interp, objv[3], boolean_names, "boolean", 0, &vindex) != TCL_OK) {
		return TCL_ERROR;
	      }
	      verbose = vindex & 0x1;
	      break;
	    }
  	  case SET_CMDLOG:
	    {
	      int vindex;
	      if (Tcl_GetIndexFromObj(interp, objv[3], boolean_names, "boolean", 0, &vindex) != TCL_OK) {
		return TCL_ERROR;
	      }
	      cmdlog = vindex & 0x1;
	      break;
	    }
	}

	break;
      }
    case EMU_RDBACK_ON:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendRdBackOn();
        tcl_result_OK_TIMEOUT(interp, sent);

	// Log this function call
	if (cmdlog && rdback_log_file)
	  {
	    (*rdback_log_file) << "bsdebug::rdbk rdback_on" << std::endl;
	  }
	break;
      }
    case EMU_RDBACK_OFF:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendRdBackOff();
        tcl_result_OK_TIMEOUT(interp, sent);

	// Log this function call
	if (cmdlog && rdback_log_file)
	  {
	    (*rdback_log_file) << "bsdebug::rdbk rdback_off" << std::endl;
	  }
	break;
      }
    case EMU_RDBACK_CLR:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendRdBackClear();
        tcl_result_OK_TIMEOUT(interp, sent);

	// Log this function call
	if (cmdlog && rdback_log_file)
	  {
	    (*rdback_log_file) << "bsdebug::rdbk rdback_clr" << std::endl;
	  }
	break;
      }
    case EMU_RDBACK_ADD:
      {
	if (objc != 3) goto wrongArgs;

	int addr = 0;
	if (Tcl_GetIntFromObj(interp, objv[2], &addr) != TCL_OK) {
	  return TCL_ERROR;
	}

	if (addr < 0) {
	  Tcl_AppendResult (interp, "Addr must be > 0", (char *)NULL);
	  return TCL_ERROR;
	} else {
	  bool sent = ctrlXact->sendRdBackStore(addr);
	  if (verbose) {
	    printf("Adding Frame: %0x\n", addr);
	    fflush(stdout);
	  }
          tcl_result_OK_TIMEOUT(interp, sent);
	}

	// Log this function call
	if (cmdlog && rdback_log_file)
	  {
	    (*rdback_log_file) << "bsdebug::rdbk rdback_add " << addr << std::endl;
	  }
	break;
      }
  }

  return TCL_OK;

wrongArgs:
  dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));
  return TCL_ERROR;
}

static void RdBk_Cmd_Delete(ClientData clientData)
{
}

///////////////////////////////////////////////////////////////////////////////
// 
///////////////////////////////////////////////////////////////////////////////
