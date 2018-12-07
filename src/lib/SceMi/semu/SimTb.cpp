// Copyright Bluespec Inc. 2009-2010

#include <iostream>
#include <stdexcept>
#include <string>
#include <cstdlib>
#include <cstring>

#include <pthread.h>

// Bluespec's version -- $BLUESPECDIR/tcllib/include
#include "tcl.h"

#include "SceMiHeaders.h"

// Bluespec common code
#include "bsdebug_common.h"

#include "designtcl.h"

#include "Design.hpp"
#include "ReadBackControl.h"

extern std::ofstream *rdback_log_file;
static unsigned int cmdlog = 0;

using namespace std;

// the package name and namespace for this extension

#define PKG_NAME    "BSDebug"
#define NS          "bsdebug"
#define PKG_VERSION "1.0"

// static extension global data
class SceMiGlobalData {
public:
  bool			      m_initialized ;
  SceMi                     * m_scemi;
  SceMiServiceThread        * m_serviceThread;
  RdBack::SimulationControl * m_simControl;
  SimulationControl         * m_tbsimControl;
  Design                    * m_design;
  RdBack::VCDWriter         * m_vcdWriter;

  // Simple initializer invoked when the extension is loaded
  SceMiGlobalData ()
    : m_initialized(false)
    , m_scemi(0)
      //    , m_busmaster(0)
    , m_serviceThread(0)
    , m_simControl(0)
    , m_tbsimControl(0)
  {}

  ~SceMiGlobalData ()
  {
    if (m_initialized) {
      destroy();
    }
  }

  // Initialization -- call from bsdebug::scemi init <param>
  void init (const char *paramFile) {

    if (m_initialized) throw std::runtime_error ("scemi is already initialized");

    SceMiEC* ec = NULL;
    SceMi *scemi = SceMi::Pointer(ec);

    if (scemi == NULL) {
      //      fprintf(stderr, "BSDEBUG INIT\n");
      int sceMiVersion = SceMi::Version( SCEMI_VERSION_STRING );
      SceMiParameters parameters(paramFile);
      if (!parameters.loaded()) {
	fprintf(stderr, "Error starting scemi.\n");
    	exit(1);
      }
      scemi = SceMi::Init(sceMiVersion, &parameters);
      if (scemi == NULL) {
    	fprintf(stderr, "Error starting scemi.\n");
    	exit(1);
      }
    }

    m_scemi = scemi;

    m_simControl   = new RdBack::SimulationControl ("", "scemi_simControl" ,m_scemi);
    m_tbsimControl = new SimulationControl ("", "scemi_tbsimControl" ,m_scemi);

    if (m_scemi->GetServiceOwner() == NONE) {
      // Start a SceMiService thread;
      m_serviceThread = new SceMiServiceThread  (m_scemi);
      m_scemi->SetServiceOwner(TB);
    } else {
      fprintf(stderr, "SERVICE THREAD ALREADY EXISTS!\n");
    }

    m_vcdWriter = new RdBack::VCDWriter("dump1.vcd");

    m_design = new Design();
    m_design->setVCDWriter(m_vcdWriter);
    RdBackControl *simRdBackControl = m_simControl; // Cast to base class
    m_design->setControl(simRdBackControl);

    // fprintf(stderr, "TURN ON TB SIM CONTROL\n");
    unsigned int maxcount = 1 << 30;
    m_tbsimControl->sendCommand(Edges, maxcount-1);

    m_initialized = true ;
  }

  // Destruction -- called from bsdebug::scemi delete
  void destroy () {
    m_initialized = false ;

    delete m_vcdWriter;

    // Stop the simulation side
    //    if (m_busmaster) m_busmaster->shutdown();

    // Stop and join with the service thread, then shut down scemi --
    if (m_serviceThread) {
      m_serviceThread->stop();
      m_serviceThread->join();
      delete m_serviceThread;  m_serviceThread = 0;
    }

    // Delete the simulation control
    delete m_simControl; m_simControl = 0;

    // Shutdown SceMi
    if (m_scemi) {
      SceMi::Shutdown(m_scemi);
      m_scemi = 0;
    }
  }

} SceMiGlobal;




// forward declarations of C functions which are called by tcl
extern "C" {

  // Package intialization  and cleanup
  extern int Bsdebug_Init (Tcl_Interp * interp);
  extern int Bsdebug_Unload (Tcl_Interp * interp,  int flags);
  extern void Bsdebug_ExitHandler (ClientData clientData);

  extern int SceMi_Cmd(ClientData clientData,
                       Tcl_Interp *interp,
                       int objc,
                       Tcl_Obj *const objv[]);

  extern int Dut_Cmd(ClientData clientData,
                     Tcl_Interp *interp,
                     int objc,
                     Tcl_Obj * objv[]);

} // extern "C"



// Function called if/when the dynamic library is unloaded
// Function name must match package library name
int Bsdebug_Unload (Tcl_Interp * interp,  int flags)
{
  if (flags & TCL_UNLOAD_DETACH_FROM_PROCESS) {
    SceMiGlobalData *pglobal = & SceMiGlobal;
    pglobal->destroy();
    Tcl_DeleteExitHandler ( Bsdebug_ExitHandler, &SceMiGlobal);
  }
  return TCL_OK;
}

// Exit handler called during exit.
void Bsdebug_ExitHandler (ClientData clientData)
{
  SceMiGlobalData *pglobal = (SceMiGlobalData *) clientData;
  pglobal->destroy();
}

// Package initialization function -- called during package load/require
// function name must match package library name
int Bsdebug_Init(Tcl_Interp *interp)
{
  Tcl_Namespace* nsptr = NULL;

  try {
    // Dynmaic binding of this extension to tcl
    // (This must be called before any other 'Tcl_*' function.)
    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
      return TCL_ERROR;
    }

    // register the exit handler
    Tcl_CreateExitHandler( Bsdebug_ExitHandler, &SceMiGlobal);

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
    // A top-level tcl bsdebug::scemi command -- application specific boilerplate
    Tcl_CreateObjCommand(interp,
			 NS "::scemi",
			 (Tcl_ObjCmdProc *) SceMi_Cmd,
			 (ClientData) &(SceMiGlobal),
                         0);
    Tcl_Export(interp, nsptr, "scemi", 0);

    // Bluespec emulation control command
    Tcl_CreateObjCommand(interp,
                         NS "::emu",
                         (Tcl_ObjCmdProc *) Emu_Cmd,
                         (ClientData) &(SceMiGlobal.m_tbsimControl),
                         (Tcl_CmdDeleteProc *) Emu_Cmd_Delete);
    Tcl_Export(interp, nsptr, "emu", 0);

    // Bluespec emulation control command with readback
    Tcl_CreateObjCommand(interp,
                         NS "::rdbk",
                         (Tcl_ObjCmdProc *) RdBk_Cmd,
                         (ClientData) &(SceMiGlobal.m_simControl),
                         (Tcl_CmdDeleteProc *) RdBk_Cmd_Delete);
    Tcl_Export(interp, nsptr, "rdbk", 0);

    Tcl_CreateObjCommand(interp,
    			 NS "::netlist",
    			 (Tcl_ObjCmdProc *) llbits_netlist_cmd,
    			 (ClientData) &(SceMiGlobal.m_design),
    			 (Tcl_CmdDeleteProc *) llbits_netlist_cmd_delete);
    Tcl_Export(interp, nsptr, "netlist", 0);

    // Other command can go here

  } catch (const exception & error) {
    Tcl_AppendResult(interp, error.what()
                     ,"\nCould not initialize bsdebug tcl package"
                     ,(char *) NULL);
    return TCL_ERROR;
  }

  return TCL_OK;
}




// implementation of the scemi command ensemble
// at the tcl level, the command will be
// bsdebug::scemi init <params file>
// bsdebug::scemi delete
extern "C" int SceMi_Cmd(ClientData clientData,    	//  &(GlobalXactor),
                     Tcl_Interp *interp,      	// Current interpreter
                     int objc,               	// Number of arguments
                     Tcl_Obj *const objv[]   	// Argument strings
         )
{
  // Command table
  enum ScemiCmds { scemi_init, scemi_delete };
  static const cmd_struct cmds_str[] = {
    {"init",		scemi_init,		"<params file>"}
    ,{"delete",		scemi_delete,		""}
    ,{0}                        // MUST BE LAST
  };

  // Cast client data to proper type
  SceMiGlobalData *pglobal = (SceMiGlobalData *) clientData;

  // Extract sub command
  ScemiCmds command;
  int index;
  if (objc == 1) goto wrongArgs;
  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], cmds_str, sizeof(cmd_struct),
                                           "command", 0, &index ) ) {
    return TCL_ERROR;
  }


  command = (enum ScemiCmds) cmds_str[index].enumcode;
  switch (command) {
    case scemi_init:
    {
	if (objc != 3) goto wrongArgs;
	char *paramfile = Tcl_GetString(objv[2]);
	try {
          pglobal->init(paramfile);
	} catch (const exception & error) {
          Tcl_AppendResult(interp, error.what()
                           ,"\nCould not initialize emulation"
                           ,(char *) NULL);
	  return TCL_ERROR;
	}
	//	fprintf(stderr, "Scemi Pointer: %llx\n", (unsigned long long) pglobal->m_scemi);
	std::string cmd = "set ::scemiVar ";
	std::stringstream ss;
	ss << (unsigned long long) pglobal->m_scemi;
	cmd.append(ss.str());
	//	fprintf(stderr, "COMMAND: %s\n", cmd.c_str());
	int r = Tcl_Eval(interp, cmd.c_str());
	break;
    }
    case scemi_delete:
        pglobal->destroy();
        break;
  }
  return TCL_OK;

wrongArgs:
  dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));
  return TCL_ERROR;
}

