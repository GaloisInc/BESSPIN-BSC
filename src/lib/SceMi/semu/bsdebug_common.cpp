// Copyright Bluespec Inc. 2009-2010

#include <iostream>
#include <string>
#include <cstdlib>
#include <cstring>

#include <pthread.h>

#include "bsdebug_common.h"
#include "bsv_scemi.h"
#include "ReadBackControl.h"

using namespace std;

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
//  Set of overloaded function to add objects to a Tcl list.
/////////////////////////////////////////////////////////////////
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, Tcl_Obj *o)
{
  return Tcl_ListObjAppendElement (interp, l, o);
}
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *s)
{
  return Tcl_ListObjAppendElement (interp, l, Tcl_NewStringObj( s, -1));
}
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const int & i )
{
  Tcl_Obj *x = Tcl_NewIntObj(i);
  return addTclList(interp, l, x);
}
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const long & i )
{
  Tcl_Obj *x = Tcl_NewLongObj(i);
  return addTclList(interp, l, x);
}
// only overload the addTclList function for Tcl_WideInt if that type is 
// unique.  Otherwise, it will collide with the definition of another 
// overloaded version.
#if !defined(TCL_WIDE_INT_IS_LONG)
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const Tcl_WideInt & i )
{
  Tcl_Obj *x = Tcl_NewWideIntObj(i);
  return addTclList(interp, l, x);
}
#endif
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const int & i )
{
  char buf[127];
  snprintf(buf, 127, fmt, i);
  return addTclList(interp, l, buf);
}
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const long & i )
{
  char buf[127];
  snprintf(buf, 127, fmt, i);
  return addTclList(interp, l, buf);
}
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const long long & i )
{
  char buf[127];
  snprintf(buf, 127, fmt, i);
  return addTclList(interp, l, buf);
}
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const unsigned int & i )
{
  char buf[127];
  snprintf(buf, 127, fmt, i);
  return addTclList(interp, l, buf);
}
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const unsigned long & i )
{
  char buf[127];
  snprintf(buf, 127, fmt, i);
  return addTclList(interp, l, buf);
}
int addTclList (Tcl_Interp *interp, Tcl_Obj *l, const char *fmt, const unsigned long long & i )
{
  char buf[127];
  snprintf(buf, 127, fmt, i);
  return addTclList(interp, l, buf);
}
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////
// Utility function to report OK or timeout result to interp
/////////////////////////////////////////////////////////////////
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


// file local variable
static unsigned int verbose = 0;
static unsigned int cmdlog = 0;
std::ofstream *rdback_log_file = NULL;

// useful constants
#define FREE_RUNNING_CYCLES      0x3FFFFFFF
#define FREE_RUNNING_CYCLES_RDBK 0x1FFFFFFF
static const char* boolean_names[] = { "0", "1", "off", "on", "no", "yes" };


/////////////////////////////////////////////////////////////////
// implementation of the emu command ensemble
/////////////////////////////////////////////////////////////////

int Emu_Cmd(ClientData clientData, 	// &(GlobalXactor.simControl)
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
  SimulationControl * ctrlXact = * (SimulationControl **) clientData;
  enum EmuCmds {
    EMU_RUN, EMU_STOP, EMU_CONTINUE, EMU_FREE_RUNNING,
    EMU_QUERY, EMU_GET, EMU_SET, EMU_RESP
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
	  bool sent = ctrlXact->sendCommand(Edges, run_cycles);
	  if (verbose) {
	    printf("Running emulator for %0d cycles\n", run_cycles);
	    fflush(stdout);
	  }
          tcl_result_OK_TIMEOUT(interp, sent);
	}
	break;
      }
    case EMU_STOP:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendCommand(Stop);
        tcl_result_OK_TIMEOUT(interp, sent);
	break;
      }
    case EMU_CONTINUE:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendCommand(Resume); 
        tcl_result_OK_TIMEOUT(interp, sent);
	break;
      }
   case EMU_FREE_RUNNING:
      {
	if (objc != 2) goto wrongArgs;

	bool sent = ctrlXact->sendCommand(Edges, FREE_RUNNING_CYCLES);
	if (verbose) {
	  printf("Free_running emulator\n");
	  fflush(stdout);
	}
        tcl_result_OK_TIMEOUT(interp, sent);
	break;
      }
    case EMU_RESP:
      {
	if (objc != 3) goto wrongArgs;

	int timeout_ms = 0;
	if (Tcl_GetIntFromObj(interp, objv[2], &timeout_ms) != TCL_OK) {
	  return TCL_ERROR;
	}

        StampedT<SimStatusResp> respStamped;
        bool stat = ctrlXact->getStatusTimed(respStamped, 0, 1000 * timeout_ms);
        if (! stat) {
          Tcl_SetObjResult(interp, Tcl_NewStringObj("", 0));
        }
        else {
          SimStatusResp resp(respStamped.getData());
          Tcl_Obj* objs[5];
          objs[0] = Tcl_NewBooleanObj(resp.isRunning() ? 1 : 0);
          objs[1] = Tcl_NewIntObj(resp.cyclesRemaining());
          objs[2] = Tcl_NewBooleanObj(resp.isFreeRunning());
          objs[3] = Tcl_NewBooleanObj( ! stat);
          objs[4] = Tcl_NewWideIntObj( respStamped.getTimeStamp() );
          Tcl_Obj* list_obj = Tcl_NewListObj(5, objs);
          Tcl_SetObjResult(interp, list_obj);
        }
	break;
      }
    case EMU_QUERY:
      {
	if (objc != 2) goto wrongArgs;

        // It is possible that there is status available from previous edges commands
        // Since edges send a response when completed.   Let use those response before sending
        // a new query request
        StampedT<SimStatusResp> respStamped;
        bool stat;
        if (! ctrlXact->getStatusNonBlocking(respStamped) ) {
          stat = ctrlXact->sendCommand(Query);
          stat &= ctrlXact->getStatusTimed(respStamped, 1,0 ); // 1 second timeout
        } else {
	  stat = 1; // There was a previous status left over, we should send back a happy stat
	  while (ctrlXact->getStatusNonBlocking(respStamped)) {
	    stat = 1;
	  }
	}
        SimStatusResp resp(respStamped.getData());
	Tcl_Obj* objs[5];
	objs[0] = Tcl_NewBooleanObj(resp.isRunning() ? 1 : 0);
	objs[1] = Tcl_NewIntObj(resp.cyclesRemaining());
	objs[2] = Tcl_NewBooleanObj(resp.isFreeRunning());
	objs[3] = Tcl_NewBooleanObj( ! stat);
        objs[4] = Tcl_NewWideIntObj( respStamped.getTimeStamp() );
	Tcl_Obj* list_obj = Tcl_NewListObj(5, objs);
	Tcl_SetObjResult(interp, list_obj);
	break;
      }
    case EMU_GET:
      {
	int vindex;
	static const char *get_vars[] = {
	  "verbose",
          NULL
	};
	enum GetVars {
	  GET_VERBOSE
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
	}

	break;
      }
    case EMU_SET:
      {
	int vindex;
	static const char *set_vars[] = {
	  "verbose",
	  NULL
	};
	enum SetVars {
	  SET_VERBOSE
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
	}

	break;
      }
  }

  return TCL_OK;

wrongArgs:
  dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));
  return TCL_ERROR;
}

void Emu_Cmd_Delete(ClientData clientData)
{
}

/////////////////////////////////////////////////////////////////
// implementation of the emu command ensemble with readback
/////////////////////////////////////////////////////////////////

int RdBk_Cmd(ClientData clientData, 	// &(GlobalXactor.simControl)
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
  RdBack::SimulationControl * ctrlXact = * (RdBack::SimulationControl **) clientData;
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
	  bool sent = ctrlXact->sendCommand(RdBack::Edges, run_cycles);
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

	bool sent = ctrlXact->sendCommand(RdBack::Stop);
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

	bool sent = ctrlXact->sendCommand(RdBack::Resume);
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

	bool sent = ctrlXact->sendCommand(RdBack::Edges, FREE_RUNNING_CYCLES_RDBK);
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

        StampedT<RdBack::SimStatusResp> respStamped;
        bool stat = ctrlXact->getStatusTimed(respStamped, 0, 1000 * timeout_ms);
        if (! stat) {
          Tcl_SetObjResult(interp, Tcl_NewStringObj("", 0));
        }
        else {
          RdBack::SimStatusResp resp(respStamped.getData());
          Tcl_Obj* objs[5];
          objs[0] = Tcl_NewBooleanObj(resp.isRunning() ? 1 : 0);
          objs[1] = Tcl_NewIntObj(resp.cyclesRemaining());
          objs[2] = Tcl_NewBooleanObj(resp.isFreeRunning());
          objs[3] = Tcl_NewBooleanObj( ! stat);
          objs[4] = Tcl_NewWideIntObj( respStamped.getTimeStamp() );
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
        StampedT<RdBack::SimStatusResp> respStamped;
        bool stat;
        if (! ctrlXact->getStatusNonBlocking(respStamped) ) {
          stat = ctrlXact->sendCommand(RdBack::Query);
          stat &= ctrlXact->getStatusTimed(respStamped, 1,0 ); // 1 second timeout
        } else {
	  stat = 1; // There was a previous status left over, we should send back a happy stat
	  while (ctrlXact->getStatusNonBlocking(respStamped)) {
	    stat = 1;
	  }
	}

        RdBack::SimStatusResp resp(respStamped.getData());
	Tcl_Obj* objs[6];
	objs[0] = Tcl_NewBooleanObj(resp.isRunning() ? 1 : 0);
	objs[1] = Tcl_NewIntObj(resp.cyclesRemaining());
	objs[2] = Tcl_NewBooleanObj(resp.isFreeRunning());
	objs[3] = Tcl_NewBooleanObj( ! stat);
        objs[4] = Tcl_NewWideIntObj( respStamped.getTimeStamp() );
	objs[5] = Tcl_NewBooleanObj(resp.isRdBackOn());
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

	bool sent = ctrlXact->sendCommand(RdBack::RdBackOn);
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

	bool sent = ctrlXact->sendCommand(RdBack::RdBackOff);
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

	bool sent = ctrlXact->sendCommand(RdBack::RdBackCmd, 0);
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
	  bool sent = ctrlXact->sendCommand(RdBack::RdBackStore, addr);
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

void RdBk_Cmd_Delete(ClientData clientData)
{
}


// implementation of the Capture command ensemble
int Capture_Cmd(ClientData clientData,  	// &(GlobalXactor.probeControl),
                Tcl_Interp *interp,     	// Current interpreter
                int objc,                       // Number of arguments
                Tcl_Obj *const objv[]           // Argument strings
         )
{
  if ((clientData == 0) || (* (void **) clientData == 0)) {
    Tcl_AppendResult(interp, "Tcl command, "
                     ,Tcl_GetString(objv[0])
                     ,", was called without proper initialization of clientData"
                     , (char *) NULL);
    return TCL_ERROR;
  }
  ProbesXactor  *probe_xactor = * (ProbesXactor **) clientData;
  enum CaptureCmds {
    CAPTURE_ENABLE, CAPTURE_DISABLE, CAPTURE_QUERY, CAPTURE_QUERY_SUBNETS,
    CAPTURE_QUERY_VCDFILE, CAPTURE_QUERY_TIMESTAMP, CAPTURE_QUERY_POWER,
    CAPTURE_QUERY_TOTAL_POWER_ID, CAPTURE_QUERY_POWER_GROUP, CAPTURE_QUERY_POWER_SOURCE
  };

  static const cmd_struct cmds_str[] = {
    {"enable",		CAPTURE_ENABLE,		"probeid"}
    ,{"disable",	CAPTURE_DISABLE,	"probeid"}
    ,{"query",		CAPTURE_QUERY,		"<probeid or -1>"}
    ,{"query_subnets",	CAPTURE_QUERY_SUBNETS,	"<probeid or -1>"}
    ,{"query_vcdfile",  CAPTURE_QUERY_VCDFILE,""}
    ,{"query_power",    CAPTURE_QUERY_POWER,    "probeid"}
    ,{"query_power_group",    CAPTURE_QUERY_POWER_GROUP,    "probeid"}
    ,{"query_power_source",    CAPTURE_QUERY_POWER_SOURCE,    "probeid"}
    ,{"query_total_power_id", CAPTURE_QUERY_TOTAL_POWER_ID, ""}
    ,{"query_timestamp",CAPTURE_QUERY_TIMESTAMP,""}
    ,{0}
  };

  enum CaptureCmds command;
  int capture_num = 0;
  int index;

  if (objc == 1) goto wrongArgs;
  if (probe_xactor == 0) {
    Tcl_SetResult (interp, (char *) "Cannot use capture command with emulation initialization", TCL_STATIC );
    return TCL_ERROR;
  }

  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], cmds_str, sizeof(cmd_struct),
                                          "command", 0, &index ) ) {
    return TCL_ERROR;
  }
  command = (enum CaptureCmds) cmds_str[index].enumcode;

  if (objc < 3) {
    capture_num = -1;
  } else {
    if (Tcl_GetIntFromObj(interp, objv[2], &capture_num) != TCL_OK) {
      return TCL_ERROR;
    }
  }

  switch (command) {
    case CAPTURE_ENABLE:
      {
	if (objc != 3) goto wrongArgs;

	probe_xactor->enableByProbeNum(capture_num);
	break;
      }
    case CAPTURE_DISABLE:
      {
	if (objc != 3) goto wrongArgs;

	if (probe_xactor->enabledByProbeNum(capture_num))
	  probe_xactor->disableByProbeNum(capture_num);

	break;
      }
    case CAPTURE_QUERY:
      {
	if (capture_num == -1) {

	  Tcl_Obj* objs[1];
	  objs[0] = Tcl_NewIntObj(probe_xactor->getNumberOfProbes());
	  Tcl_Obj* list_obj = Tcl_NewListObj(1, objs);
	  Tcl_SetObjResult(interp, list_obj);
	} else if (capture_num >= (int)probe_xactor->getNumberOfProbes()) {
          return TCL_ERROR;
        } else {
   
	  Tcl_Obj* objs[7];
	  string xactor_name = probe_xactor->getLabel(capture_num);
	  string dump_file = "";
	  unsigned int probe_num = probe_xactor->getProbeDefFromIndex(capture_num)->getProbeNum();
	  const char *type = probe_xactor->getBSVType(capture_num);
	  const char *path = probe_xactor->getPath(capture_num);
	  objs[0] = Tcl_NewStringObj(xactor_name.c_str(), xactor_name.length());
	  objs[1] = Tcl_NewStringObj(dump_file.c_str(), dump_file.length());
	  objs[2] = Tcl_NewStringObj(type, strlen(type));
	  objs[3] = Tcl_NewIntObj(probe_xactor->enabledByProbeNum(probe_num));
	  objs[4] = Tcl_NewIntObj((long) probe_xactor->getProbeDefFromIndex(capture_num)->getProbeType());
	  objs[5] = Tcl_NewIntObj(probe_num);
	  objs[6] = Tcl_NewStringObj(path, strlen(path));
	  Tcl_Obj* list_obj = Tcl_NewListObj(7, objs);
	  Tcl_SetObjResult(interp, list_obj);
	}
	break;
      }
    case CAPTURE_QUERY_SUBNETS:
      {
	if (capture_num < 0) {
          return TCL_ERROR;
        } else {
	  std::string sn;
	  string subnets;
	  Tcl_Obj* objs[2];
	  sn = probe_xactor->getParamSubNetsFromProbeNum(capture_num);
	  if (sn != "") {
	    objs[0] = Tcl_NewIntObj(1);
	    subnets = sn;
	    objs[1] = Tcl_NewStringObj(subnets.c_str(), subnets.length());
	  } else {
	    objs[0] = Tcl_NewIntObj(0);
	    objs[1] = Tcl_NewStringObj("", 0);
	  }
	  Tcl_Obj* list_obj = Tcl_NewListObj(2, objs);
	  Tcl_SetObjResult(interp, list_obj);
	}
	break;
      }
    case CAPTURE_QUERY_VCDFILE:
      {
	if (objc != 2) goto wrongArgs;

	string filename;
	Tcl_Obj* objs[1];
	filename = probe_xactor->vcdFileName();
	if (filename != "") {
	  objs[0] = Tcl_NewStringObj(filename.c_str(), filename.length());
	} else {
	  objs[0] = Tcl_NewStringObj("", 0);
	}
	Tcl_Obj* list_obj = Tcl_NewListObj(1, objs);
	Tcl_SetObjResult(interp, list_obj);

	break;
      }
    case CAPTURE_QUERY_POWER:
      {
	if (objc != 3) goto wrongArgs;

	Tcl_Obj* objs[1];

	if (probe_xactor->isPowerProbe(capture_num))
	  objs[0] = Tcl_NewIntObj(1);
	else
	  objs[0] = Tcl_NewIntObj(0);

	Tcl_Obj* list_obj = Tcl_NewListObj(1, objs);
	Tcl_SetObjResult(interp, list_obj);

	break;
      }
    case CAPTURE_QUERY_POWER_GROUP:
      {
	if (objc != 3) goto wrongArgs;

	Tcl_Obj* objs[1];

	if (probe_xactor->isPowerGroupProbe(capture_num))
	  objs[0] = Tcl_NewIntObj(1);
	else
	  objs[0] = Tcl_NewIntObj(0);

	Tcl_Obj* list_obj = Tcl_NewListObj(1, objs);
	Tcl_SetObjResult(interp, list_obj);

	break;
      }
    case CAPTURE_QUERY_POWER_SOURCE:
      {
	if (objc != 3) goto wrongArgs;

	Tcl_Obj* objs[1];

	if (probe_xactor->isPowerSourceProbe(capture_num))
	  objs[0] = Tcl_NewIntObj(1);
	else
	  objs[0] = Tcl_NewIntObj(0);

	Tcl_Obj* list_obj = Tcl_NewListObj(1, objs);
	Tcl_SetObjResult(interp, list_obj);

	break;
      }
    case CAPTURE_QUERY_TOTAL_POWER_ID:
      {
	if (objc != 2) goto wrongArgs;

	Tcl_Obj* objs[1];

	int num = probe_xactor->getTotalPowerProbeNum();
	objs[0] = Tcl_NewIntObj(num);

	Tcl_Obj* list_obj = Tcl_NewListObj(1, objs);
	Tcl_SetObjResult(interp, list_obj);

	break;
      }
    case CAPTURE_QUERY_TIMESTAMP:
      {
	if (objc != 2) goto wrongArgs;

	probe_xactor->queryTimestamp();

	break;
      }
  }

  return TCL_OK;

wrongArgs:
   dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));
   return TCL_ERROR;
}

void Capture_Cmd_Delete(ClientData clientData)
{
}


// Forward declarations of underlying function
///////////////////////////////////////////////////////////////////////////////
// Memory Xactor interface
///////////////////////////////////////////////////////////////////////////////

TCL_STATIC_FUNC_DECL(memXactor_readQ);
TCL_STATIC_FUNC_DECL(memXactor_writeQ);
TCL_STATIC_FUNC_DECL(memXactor_fillQ);
TCL_STATIC_FUNC_DECL(memXactor_dumpQ);
TCL_STATIC_FUNC_DECL(memXactor_responseQ);

int MemXActor_Cmd(ClientData clientData,
                  Tcl_Interp *interp,
                  int objc,
                  Tcl_Obj *const objv[])
{
    // Command table
  static const cmd_struct_funptr_2 cmds_str[] = {
    // CMD Name,        Help Str,                   arg cnt,  function name
    {"readQ",		"<addr>"			,1  ,memXactor_readQ}
    ,{"writeQ",		"<addr> <data>"			,2  ,memXactor_writeQ}
    ,{"fillQ",		"<lo> <cnt> <dat> <incr>"	,4  ,memXactor_fillQ}
    ,{"dumpQ",		"<lo> <cnt>"			,2  ,memXactor_dumpQ}
    ,{"responseQ",	"" 				,0  ,memXactor_responseQ}
    ,{0}                        // MUST BE LAST
  };

  if (clientData == 0) {
    Tcl_SetResult (interp, (char *) "Cannot use Memory Xactor command in this context", TCL_STATIC );
    return TCL_ERROR;
  }

  // Extract sub command
  int index;
  tclfunptr command ;
  int stat;

  if (objc == 1) {
    dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));
    return TCL_ERROR;
  }
  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], cmds_str, sizeof(cmd_struct_funptr_2),
                                           "dut command", TCL_EXACT, &index ) ) {
    return TCL_ERROR;
  }
  if (cmds_str[index].numargs + 2 != objc) {
    dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));
    return TCL_ERROR;
  }

  command = cmds_str[index].function;
  stat = command(clientData, interp, objc, objv);
  return stat;

}
void MemXActor_Cmd_Delete(ClientData clientData)
{
  // Nothing to do.
}

static int memXactor_readQ (ClientData clientData,
                            Tcl_Interp *interp,       // Current interpreter
                            int objc,                 // Number of arguments
                            Tcl_Obj *const objv[]          // Argument strings
	    )
{
  MemXactor *memxactor = *(MemXactor **) clientData;
  int stat ;
  // Get Arguments
  int addr;
  stat = Tcl_GetIntFromObj(interp, objv[2], &addr);
  RETURN_IF_ERR(stat);

  memxactor->readQ(addr);
  Tcl_Obj *r = Tcl_GetObjResult(interp);
  addTclList(interp, r, "Queued: read");
  addTclList(interp, r, "0x%08x", addr);

  return stat;
}

// dut writeQ <addr> <data>
static int memXactor_writeQ (ClientData clientData,
                             Tcl_Interp *interp,       // Current interpreter
                             int objc,                 // Number of arguments
                             Tcl_Obj *const objv[]          // Argument strings
                             )
{
  MemXactor *memxactor = *(MemXactor **) clientData;

  int stat ;
  int addr;
  Tcl_WideInt data;
  Tcl_Obj *r = Tcl_GetObjResult(interp);

  // Get arguments
  stat = Tcl_GetIntFromObj(interp, objv[2], &addr);
  RETURN_IF_ERR(stat);
  stat = Tcl_GetWideIntFromObj(interp, objv[3], &data);
  RETURN_IF_ERR(stat);

  memxactor->writeQ(addr, data);
  addTclList(interp, r, "Queued: write");
  addTclList(interp, r, "0x%08x", addr);
  addTclList(interp, r, "0x%016llx", data);

  return stat;
}

// dut fill <lo> <cnt> <data> <incr>
static int memXactor_fillQ (ClientData clientData,	// &(GlobalXactor.m_busmaster)
                            Tcl_Interp *interp,       // Current interpreter
                            int objc,                 // Number of arguments
                            Tcl_Obj *const objv[]          // Argument strings
                            )
{
  MemXactor *memxactor = *(MemXactor **) clientData;
  int stat = TCL_ERROR;
  int loaddr, wcnt;
  Tcl_WideInt data, incr;
  Tcl_Obj *r = Tcl_GetObjResult(interp);

  // Get arguments
  stat = Tcl_GetIntFromObj(interp, objv[2], &loaddr);
  RETURN_IF_ERR(stat);
  stat = Tcl_GetIntFromObj(interp, objv[3], &wcnt);
  RETURN_IF_ERR(stat);
  stat = Tcl_GetWideIntFromObj(interp, objv[4], &data);
  RETURN_IF_ERR(stat);
  stat = Tcl_GetWideIntFromObj(interp, objv[5], &incr);
  RETURN_IF_ERR(stat);

  memxactor->fillQ(loaddr, wcnt, data, incr);
  addTclList(interp, r, "Queued: fill");
  addTclList(interp, r, "0x%08x", loaddr);
  addTclList(interp, r, "%d", wcnt);
  addTclList(interp, r, "0x%016llx", data);
  addTclList(interp, r, "0x%016llx", incr);
  stat = TCL_OK;
  
  return stat;
}


// dut fill <lo> <cnt>
static int memXactor_dumpQ (ClientData clientData,	// &(GlobalXactor.m_busmaster)
                            Tcl_Interp *interp,       // Current interpreter
                            int objc,                 // Number of arguments
                            Tcl_Obj *const objv[]          // Argument strings
                            )
{
  MemXactor *memxactor = *(MemXactor **) clientData;

  int stat;
  int loaddr, wcnt;
  Tcl_Obj *r = Tcl_GetObjResult(interp);

  // Get arguments
  stat = Tcl_GetIntFromObj(interp, objv[2], &loaddr);
  RETURN_IF_ERR(stat);
  stat = Tcl_GetIntFromObj(interp, objv[3], &wcnt);
  RETURN_IF_ERR(stat);

  memxactor->dumpQ(loaddr, wcnt);
  stat = TCL_OK;
  addTclList(interp, r, "Queued: dump");
  addTclList(interp, r, "0x%08x", loaddr);
  addTclList(interp, r, "%d", wcnt);

  return stat;
}

// responseQ
static int memXactor_responseQ (ClientData clientData,	// &(GlobalXactor.m_busmaster)
                                Tcl_Interp *interp,       // Current interpreter
                                int objc,                 // Number of arguments
                                Tcl_Obj *const objv[]          // Argument strings
                                )
{
  static std::vector<MemXactor::MemData> vdata;

  MemXactor *memxactor = *(MemXactor **) clientData;
  Tcl_Obj *r = Tcl_GetObjResult(interp);

  unsigned addr;
  bool stat = memxactor->getResponseQ(addr, vdata);
  if (stat) {
    addTclList(interp, r, "0x%08x", addr );
    addTclList(interp, r, "%016llx",  vdata );
  } else {
  }

  return TCL_OK;
}

///////////////////////////////////////////////////////////////////////////////
// Memory Xactor interface class
///////////////////////////////////////////////////////////////////////////////

MemXactor::MemXactor ( const std::string & hier, const std::string & instname, SceMi *scemi) 
  : m_request    (hier, instname + "_host_client_req", scemi)
  , m_response   (hier, instname + "_host_client_resp", scemi)
{
}

// Destructor
MemXactor::~MemXactor()
{
}

// Return a vector or read responses.
bool MemXactor::getResponseQ (MemAddr &addr, std::vector<MemData> &datav )
{
  Response_T response;
  MemData data;
  bool stat;
  bool dataValid = false;

  while ( (stat = m_response.getMessageNonBlocking(response)) ) {
    data = response.get64();
    m_data.push_back(data);

    if (m_data.size() >= m_outstanding.front().words) {
      addr = m_outstanding.front().addr;
      m_outstanding.pop_front();

      // Copy data to return vector
      datav.clear();
      datav.insert (datav.begin(), m_data.begin(), m_data.end());
      m_data.clear();
      dataValid = true;
      break;
    }
  }

  return dataValid;
}

void MemXactor::readCore (const MemAddr addr)
{
  Request_T request ;
  request.m_address = addr;
  request.m_read    = true;

  m_request.sendMessage(request);

}
void MemXactor::readQ (const MemAddr addr)
{
  readCore(addr);
  class QueueData d;
  d.addr  = addr;
  d.words = 1;
  m_outstanding.push_back(d);
}


void MemXactor::writeQ (const MemAddr addr, const MemData data)
{
  Request_T request ;
  request.m_address = addr;
  request.m_read    = false;
  request.m_data    = data;

  m_request.sendMessage(request);
}

void MemXactor::fillQ (const MemAddr lo_addr,
                       const MemAddr words,
                       const MemData data_in,
                       const MemData incr)
{
  MemAddr addr;
  MemAddr wc;
  MemData d;
  for (addr = lo_addr, d = data_in, wc = 0; wc < words; ++wc, addr += 1, d = d + incr ) {
    writeQ(addr,d);
  }
}

void MemXactor::dumpQ (const MemAddr lo_addr,
                       const MemAddr words
                       )
{
  MemAddr addr, wc;

  class QueueData d;
  d.addr = lo_addr;
  d.words = words;
  m_outstanding.push_back(d);
  for (addr = lo_addr, wc = 0; wc < words; ++wc, addr += 1 ) {
    readCore(addr);
  }
}


///////////////////////////////////////////////////////////////////////////////
// 
///////////////////////////////////////////////////////////////////////////////
