/// Copyright (c) 2014, Bluespec Inc.  ALL RIGHTS RESERVED
#include <iostream>
#include <stdexcept>
#include <string>
#include <cstdlib>
#include <cstring>
#include <set>

#include "tcl.h"
#include "bstcl_base.h"
#include "designtcl.h"

#include "Design.hpp"

#include "fpga.h"
#include "cable.h"
#include "jtag.h"
#include "interface.h"
#include "log.h"
#include "control.h"

using namespace std;

#define PKG_NAME    "BSDebug"
#define NS          "bsdebug"
#define PKG_VERSION "1.0"

ofstream *rdback_log_file;
extern ofstream *rdback_log_file;

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

string 
convert_binstr_to_hexstr(const string &s)
{
  int numofchar;
  int32_t index;
  uint8_t byteNibble = 0;
  string outstr, tmp;

  if (s.empty())
    return string("");

  // process each group of 4 bits 
  numofchar = (s.length()+3) / 4;
  index = s.length() - 1;
  for(int i = 0; i < numofchar; i++) {
    if (s[index] == '1') byteNibble |= 1;
    index--;
    if (index >= 0) {
      if (s[index] == '1') byteNibble |= 2;
      index--;
    }
    if (index >= 0) {
      if (s[index] == '1') byteNibble |= 4;
      index--;
    }
    if (index >= 0) {
      if (s[index] == '1') byteNibble |= 8;
      index--;
    }
    
    if (byteNibble < 10) {
      tmp += byteNibble + '0';
      outstr.insert(0, tmp);
    } else {
      tmp += byteNibble - 10 + 'A';
      outstr.insert(0, tmp);
    }
    byteNibble = 0;
    tmp = "";
  }
  return outstr;
}


class SemuLiteGlobalData
{
public:
  bool                             m_initialized;
  JtagRdBackControl               *m_pControl;
  Design                          *m_pDesign;
  RdBack::VCDWriter               *m_pVCDWriter;

public:
  SemuLiteGlobalData()
    : m_initialized(false)
    , m_pControl(0)
    , m_pDesign(0)
    , m_pVCDWriter(0)
  {
  }

  ~SemuLiteGlobalData()
  {
    if (m_initialized) {
      destroy();
    }
  }

  void init(const string &cbl, const uint32_t &device)
  {
    set<string> cables = interface_registry<cable>::shared_instance().get_entries();
    set<string>::iterator str;
    string realcable = "";

    // auto-detect cable
    if (cbl == "") {
      for(str = cables.begin(); str != cables.end(); str++) {
	cable *c = interface_registry<cable>::shared_instance().create_object_of(*str);
	if (c->is_present()) {
	  cout << "Using " << c->description.c_str() << endl;
	  realcable = str->c_str();
	}
	delete c;
      }
      if (realcable == "") {
	throw string("Failed to locate a supported JTAG cable!");
      }
    } else {
      realcable = cbl;
    }

    // Allocate
    try {
      m_pControl    = new JtagRdBackControl(realcable, device);
      m_pDesign     = new Design();
      m_pVCDWriter  = new RdBack::VCDWriter("dump1.vcd");

      // Connect
      RdBackControl *pRdBackControl = m_pControl; // cast to the base class
      m_pDesign->setControl(pRdBackControl);
      m_pDesign->setVCDWriter(m_pVCDWriter);
      //m_pDesign->syncConfig();
    
      m_initialized = true;
    } catch (...) {
      throw;
    }
  }

  void destroy ()
  {
    if (m_pDesign) {
      delete m_pDesign;
      m_pDesign = 0;
    }

    if (m_pVCDWriter) {
      delete m_pVCDWriter;
      m_pVCDWriter = 0;
    }

    if (m_pControl) {
      delete m_pControl;
      m_pControl = 0;
    }

    fflush(stdout);
    m_initialized = false;
  }

  void do_readback ()
  {
    int status;

    if (!m_initialized) {
      throw string("Cannot perform readback until library is initialized!");
    }

    status = m_pControl->readState();
    if (false)
      cout << "Read request: " << (status ? "success" : "error") << endl;
    
    m_pDesign->flushVCD();
  }

  string query(const string &signal)
  {
    stringstream ss;
    if (!m_initialized) {
      throw string("Cannot perform signal query until library is initialized!");
    }

    Signal *pSignal = m_pDesign->findSignal(RTL, signal.c_str());
    ss << pSignal->getAvail() << " " 
       << convert_binstr_to_hexstr(pSignal->getValueStr(false)) << " "
       << pSignal->getValueStr(false);
    cout << pSignal->getFullName() << " "
	 << ss.str() << endl;
    return ss.str();
  }

} SemuLiteGlobal ;

// forward declarations of C functions which are called by tcl
extern "C" {
  extern int Semulite_Init(Tcl_Interp *interp);
  extern int Semulite_Unload(Tcl_Interp *interp, int flags);
  extern void Semulite_ExitHandler(ClientData clientData);
  static int SemuLite_Cmd(ClientData clientData,
			  Tcl_Interp *interp,
			  int objc,
			  Tcl_Obj *const objv[]);
  static void SemuLite_Cmd_Delete(ClientData clientData);
}

// Function called if/when the dynamic library is unloaded
// Function name must match package library name
int Semulite_Unload (Tcl_Interp *interp, int flags)
{
  if (flags & TCL_UNLOAD_DETACH_FROM_PROCESS) {
    SemuLiteGlobalData *pglobal = &SemuLiteGlobal;
    pglobal->destroy();
    Tcl_DeleteExitHandler(Semulite_ExitHandler, &SemuLiteGlobal);
  }
  return TCL_OK;
}

// Exit handler called during exit
void Semulite_ExitHandler (ClientData clientData)
{
  SemuLiteGlobalData *pglobal = (SemuLiteGlobalData*) clientData;
  pglobal->destroy();
}

// Package initialization function -- called during package load/require
// function name must match package library name
int Semulite_Init(Tcl_Interp *interp)
{
  Tcl_Namespace *nsptr = NULL;

  try {
    // Dynamic binding of this extension to tcl
    // (This must be called before any other 'Tcl_*' function.)
    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
      return TCL_ERROR;
    }

    // register the exit handler
    Tcl_CreateExitHandler(Semulite_ExitHandler, &SemuLiteGlobal);

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
    // A top-level tcl bsdebug::semulite command 
    Tcl_CreateObjCommand(interp,
			 NS "::semulite",
			 (Tcl_ObjCmdProc *)SemuLite_Cmd,
			 (ClientData) &(SemuLiteGlobal),
			 (Tcl_CmdDeleteProc *) SemuLite_Cmd_Delete);
    Tcl_Export(interp, nsptr, "semulite", 0);

    // Bluespec probe netlist command
    Tcl_CreateObjCommand(interp,
			 NS "::netlist",
			 (Tcl_ObjCmdProc *) llbits_netlist_cmd,
			 (ClientData) &(SemuLiteGlobal.m_pDesign),
			 (Tcl_CmdDeleteProc *) llbits_netlist_cmd_delete);
    Tcl_Export(interp, nsptr, "netlist", 0);
  } catch (const exception &err) {
    Tcl_AppendResult(interp, err.what(), "\nCould not initialize semulite tcl package", (char*)NULL);
    return TCL_ERROR;
  }
  return TCL_OK;
}

// implementation of the semulite commands
// bsdebug::semulite init <cablename> <deviceindex>
// bsdebug::semulite readback
// bsdebug::semulite sigval <signal>
// bsdebug::semulite delete
static int SemuLite_Cmd(ClientData  clientData,          // &(SemuLiteGlobal),
			Tcl_Interp *interp,              // current interpreter
			int         objc,                // Number of arguments
			Tcl_Obj *const objv[]            // Argument strings
			)
{
  string cablename, signalname, res;
  int device = 0;

  if (clientData == 0) {
    Tcl_AppendResult(interp, "Tcl command, ", 
		     Tcl_GetString(objv[0]),
		     ", was called without proper initialization of clientData",
		     (char*)NULL);
    return TCL_ERROR;
  }

  // Command table
  enum SemuLiteCmds {
    SEMULITE_INIT,
    SEMULITE_READBACK,
    SEMULITE_SIGNALVAL,
    SEMULITE_DELETE
  };

  static const cmd_struct cmds_str[] = {
    {  "init",            SEMULITE_INIT,               "<cablename> <device index>" }
    ,{ "readback",        SEMULITE_READBACK,           "" } 
    ,{ "sigval",          SEMULITE_SIGNALVAL,          "signal name" }
    ,{ "delete",          SEMULITE_DELETE,             "" }
    ,{ 0 }
  };

  // Cast clientdata into proper type
  SemuLiteGlobalData *pGlobal = reinterpret_cast<SemuLiteGlobalData*>(clientData);

  SemuLiteCmds command;
  int index;
  if (objc == 1) goto wrongArgs;

  if (TCL_OK != Tcl_GetIndexFromObjStruct(interp, objv[1], cmds_str, sizeof(cmd_struct), "command", 0, &index)) {
    return TCL_ERROR;
  }

  command = static_cast<enum SemuLiteCmds>(cmds_str[index].enumcode);

  switch(command) {
    case SEMULITE_INIT:
      // process options if given
      switch(objc) {
	case 4: 
	  cablename = Tcl_GetString(objv[3]);
	case 3: 
	  if (Tcl_GetIntFromObj(interp, objv[2], &device) != TCL_OK) {
	    return TCL_ERROR;
	  }
	default:
	  if (objc != 2 && objc != 3 && objc != 4) 
	    goto wrongArgs;
      }

      // Initialize readback
      try {
	pGlobal->init(cablename, device);
      } catch (const exception &err) {
	Tcl_AppendResult(interp, err.what(), "\nCould not initialize semulite", (char*)NULL);
	return TCL_ERROR;
      } catch (const string &err) {
	Tcl_AppendResult(interp, err.c_str(), "\nCould not initialize semulite", (char*)NULL);
	return TCL_ERROR;
      }
      break;

    case SEMULITE_READBACK: 
      try {
	pGlobal->do_readback();
      } catch (const exception &err) {
	Tcl_AppendResult(interp, err.what(), "\nCould not perform readback", (char*)NULL);
	return TCL_ERROR;
      } catch (const string &err) {
	Tcl_AppendResult(interp, err.c_str(), "\nCould not perform readback", (char*)NULL);
	return TCL_ERROR;
      }
      break;

    case SEMULITE_SIGNALVAL:
      if (objc != 3) goto wrongArgs;

      signalname = Tcl_GetString(objv[2]);
      try {
	res = pGlobal->query(signalname);
      } catch (const exception &err) {
	Tcl_AppendResult(interp, err.what(), "\nCould not perform query", (char*)NULL);
	return TCL_ERROR;
      } catch (const string &err) {
	Tcl_AppendResult(interp, err.c_str(), "\nCould not perform query", (char*)NULL);
	return TCL_ERROR;
      }
      Tcl_SetObjResult(interp, Tcl_NewStringObj(res.c_str(), -1));
      break;

    case SEMULITE_DELETE:
      pGlobal->destroy();
      break;
  }

  return TCL_OK;

 wrongArgs:
  dumpArguments(interp, cmds_str, Tcl_GetString(objv[0]));
  return TCL_ERROR;
}

static void SemuLite_Cmd_Delete(ClientData clientData)
{
}
