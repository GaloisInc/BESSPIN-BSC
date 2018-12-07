#!/usr/bin/python

# Copyright (c) 2105 Bluespec, Inc.  All Rights Reserved
# Author: J Schwartz

# ================================================================

# Usage:    Generate_TestBench.py  pin_file  emu_scripts_dir  cpp_src_dir  [usertb]

# XXX ...

# ================================================================
# Standard Python lib imports

import sys
import os

# ================================================================
# Project imports

from ParsePin import *    # The parser for .pin files

# ================================================================
# Generate TclTb.cpp

def genTclTb (output_dir, dut_ifc, new_module_name, usertb):

  TclTb_filename = os.path.join (output_dir, "TclTb.cpp")
  try:
    f_TclTb = open (TclTb_filename, "w")
  except:
    print "Error opening file: ", TclTb_filename
    return 1

  print "Generating file: ", TclTb_filename
  genTclTbFile (f_TclTb, dut_ifc, new_module_name, usertb)
  f_TclTb.close()

  return 0


def genTclTbFile (f_out, dut_ifc, new_module_name, usertb):

  f_out.write ("// Copyright Bluespec Inc. 2010-2011\n")
  f_out.write ("// By: GenTestbench tool\n\n")

  f_out.write ("#include <iostream>\n")
  f_out.write ("#include <stdexcept>\n")
  f_out.write ("#include <string>\n")
  f_out.write ("#include <cstdlib>\n")
  f_out.write ("#include <cstring>\n\n")
  
  f_out.write ("#include <pthread.h>\n\n")
  
  f_out.write ("// Bluespec's version -- $BLUESPECDIR/tcllib/include\n")
  f_out.write ("#include \"tcl.h\"\n\n")
  
  f_out.write ("#include \"%sXactor.h\"\n" % new_module_name)
  f_out.write ("#include \"SceMiHeaders.h\"\n\n")
  
  f_out.write ("// Bluespec common code\n")
  f_out.write ("#include \"bsdebug_common.h\"\n")
  f_out.write ("#include \"designtcl.h\"\n\n")
  f_out.write ("// TBXactor\n")
  f_out.write ("#include \"TBXactor.h\"\n\n")
  
  f_out.write ("using namespace std;\n\n")

  f_out.write ("// the package name and namespace for this extension\n")

  f_out.write ("#define PKG_NAME    \"BSDebug\"\n")
  f_out.write ("#define NS          \"bsdebug\"\n")
  f_out.write ("#define PKG_VERSION \"1.0\"\n\n")

  f_out.write ("extern std::ofstream *rdback_log_file;\n")
  if (usertb):
    f_out.write ("extern int test_usertb();\n")
    f_out.write ("extern int reset_usertb();\n")
    f_out.write ("extern void destroy_usertb();\n")
  f_out.write ("static unsigned int cmdlog = 0;\n\n")

  f_out.write ("// static extension global data\n")
  f_out.write ("class SceMiGlobalData {\n")
  f_out.write ("public:\n")
  f_out.write ("  bool             m_initialized;\n")
  f_out.write ("  %sXactor *m_dutXactor;\n" % new_module_name)
  f_out.write ("  TBXactor        *m_tbXactor;\n")
  f_out.write ("  SceMiServiceThread        * m_serviceThread;\n")
  f_out.write ("  RdBack::SimulationControl * m_simControl;\n")
  f_out.write ("  SimulationControl         * m_tbsimControl;\n")
  f_out.write ("  ProbesXactor              * m_probeControl;\n")
  f_out.write ("  Design                    * m_design;\n")
  f_out.write ("  VCDWriter                 * m_vcdWriter;\n")
  f_out.write ("\n")
  f_out.write ("  // Simple initializer invoked when the extension is loaded\n")
  f_out.write ("  SceMiGlobalData()\n")
  f_out.write ("    : m_dutXactor(0)\n")
  f_out.write ("    , m_tbXactor(0)\n")
  f_out.write ("    , m_serviceThread(0)\n")
  f_out.write ("    , m_simControl(0)\n")
  f_out.write ("    , m_tbsimControl(0)\n")
  f_out.write ("    , m_design(0)\n")
  f_out.write ("    , m_vcdWriter(0)\n")
  f_out.write ("  {}\n")
  f_out.write ("\n")
  f_out.write ("  ~SceMiGlobalData ()\n")
  f_out.write ("  {\n")
  f_out.write ("    if (m_initialized) {\n")
  f_out.write ("      destroy();\n")
  f_out.write ("    }\n")
  f_out.write ("  }\n")
  f_out.write ("\n")
  f_out.write ("  // Initialization -- call from bsdebug::scemi init <param>\n")
  f_out.write ("  void init (const char *paramfile)\n")
  f_out.write ("  {\n")
  f_out.write ("    // Instantiate TestBench Xactor\n")
  f_out.write ("    m_tbXactor = TBXactor::getOrCreate();\n")
  f_out.write ("\n")
  f_out.write ("    // Start all the services\n")
  f_out.write ("    m_tbXactor->startAllServices(paramfile);\n")
  f_out.write ("    m_simControl = m_tbXactor->getSimulationControl();\n")
  f_out.write ("    m_tbsimControl = m_tbXactor->getTbSimulationControl();\n")
  f_out.write ("    m_probeControl = m_tbXactor->getProbeControl();\n")
  f_out.write ("    m_design = m_tbXactor->getDesign();\n\n")
  f_out.write ("    // Create the transactor for the dut\n")
  f_out.write ("    m_dutXactor = %sXactor::init(m_tbXactor->getSceMi());\n" % new_module_name)
  f_out.write ("    m_initialized = true;\n\n")
  f_out.write ("  }\n")
  f_out.write ("\n")
  f_out.write ("  // Destruction -- called from bsdebug::scemi delete\n")
  f_out.write ("  void destroy () {\n")
  f_out.write ("\n")
  f_out.write ("    m_initialized = false;\n\n")
  if (usertb):
    f_out.write ("    destroy_usertb();\n\n")
  f_out.write ("    // Stop the simulation side\n")
  f_out.write ("    if (m_dutXactor) m_dutXactor->shutdown();\n\n")
  f_out.write ("    if (m_dutXactor) m_dutXactor->destroy();\n")
  f_out.write ("    if (m_tbXactor) m_tbXactor->destroy();\n")
  f_out.write ("    fflush(stdout);\n")
  f_out.write ("  }\n")
  f_out.write ("} SceMiGlobal;\n")

  f_out.write ("\n\n")
  f_out.write ("// forward declarations of C functions which are called by tcl\n")
  f_out.write ("extern \"C\" {\n")
  f_out.write ("\n")
  f_out.write ("  // Package intialization  and cleanup\n")
  f_out.write ("  extern int Bsdebug_Init (Tcl_Interp * interp);\n")
  f_out.write ("  extern int Bsdebug_Unload (Tcl_Interp * interp,  int flags);\n")
  f_out.write ("  extern void Bsdebug_ExitHandler (ClientData clientData);\n")
  f_out.write ("\n")
  f_out.write ("  static int SceMi_Cmd(ClientData clientData,\n")
  f_out.write ("                       Tcl_Interp *interp,\n")
  f_out.write ("                       int objc,\n")
  f_out.write ("                       Tcl_Obj *const objv[]);\n")
  f_out.write ("  static void SceMi_Cmd_Delete(ClientData clientData);\n")
  f_out.write ("\n")
  f_out.write ("\n")
  f_out.write ("  static int Dut_Cmd(ClientData clientData,\n")
  f_out.write ("                     Tcl_Interp *interp,\n")
  f_out.write ("                     int objc,\n")
  f_out.write ("                     Tcl_Obj *const objv[]);\n")
  f_out.write ("  static void Dut_Cmd_Delete(ClientData clientData);\n")
  f_out.write ("\n")
  f_out.write ("} // extern \"C\"\n")
  f_out.write ("\n")
  f_out.write ("\n")
  f_out.write ("\n")
  f_out.write ("// Function called if/when the dynamic library is unloaded\n")
  f_out.write ("// Function name must match package library name\n")
  f_out.write ("int Bsdebug_Unload (Tcl_Interp * interp,  int flags)\n")
  f_out.write ("{\n")
  f_out.write ("  if (flags & TCL_UNLOAD_DETACH_FROM_PROCESS) {\n")
  f_out.write ("    SceMiGlobalData *pglobal = & SceMiGlobal;\n")
  f_out.write ("    pglobal->destroy();\n")
  f_out.write ("    Tcl_DeleteExitHandler ( Bsdebug_ExitHandler, &SceMiGlobal);\n")
  f_out.write ("  }\n")
  f_out.write ("  return TCL_OK;\n")
  f_out.write ("}\n")
  f_out.write ("\n")
  f_out.write ("// Exit handler called during exit.\n")
  f_out.write ("void Bsdebug_ExitHandler (ClientData clientData)\n")
  f_out.write ("{\n")
  f_out.write ("  SceMiGlobalData *pglobal = (SceMiGlobalData *) clientData;\n")
  f_out.write ("  pglobal->destroy();\n")
  f_out.write ("}\n")
  f_out.write ("\n")
  f_out.write ("// Package initialization function -- called during package load/require\n")
  f_out.write ("// function name must match package library name\n")
  f_out.write ("int Bsdebug_Init(Tcl_Interp *interp)\n")
  f_out.write ("{\n")
  f_out.write ("  Tcl_Namespace* nsptr = NULL;\n")
  f_out.write ("\n")
  f_out.write ("  try {\n")
  f_out.write ("    // Dynmaic binding of this extension to tcl\n")
  f_out.write ("    // (This must be called before any other 'Tcl_*' function.)\n")
  f_out.write ("    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {\n")
  f_out.write ("      return TCL_ERROR;\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
  f_out.write ("    // register the exit handler\n")
  f_out.write ("    Tcl_CreateExitHandler( Bsdebug_ExitHandler, &SceMiGlobal);\n")
  f_out.write ("\n")
  f_out.write ("    // Create a namespace NS\n")
  f_out.write ("    nsptr = (Tcl_Namespace*) Tcl_CreateNamespace(interp, NS, NULL, NULL);\n")
  f_out.write ("    if (nsptr == NULL) {\n")
  f_out.write ("      return TCL_ERROR;\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
  f_out.write ("    // Provide the tcl package\n")
  f_out.write ("    if (Tcl_PkgProvide(interp, PKG_NAME, PKG_VERSION) != TCL_OK) {\n")
  f_out.write ("      return TCL_ERROR;\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
  f_out.write ("    // Register commands to this tcl extension\n")
  f_out.write ("    // A top-level tcl bsdebug::scemi command -- application specific boilerplate\n")
  f_out.write ("    Tcl_CreateObjCommand(interp,\n")
  f_out.write ("			 NS \"::scemi\",\n")
  f_out.write ("			 (Tcl_ObjCmdProc *) SceMi_Cmd,\n")
  f_out.write ("			 (ClientData) &(SceMiGlobal),\n")
  f_out.write ("			 (Tcl_CmdDeleteProc *) SceMi_Cmd_Delete);\n")
  f_out.write ("    Tcl_Export(interp, nsptr, \"scemi\", 0);\n")
  f_out.write ("\n")
  f_out.write ("    // A top-level tcl dut command -- application specific\n")
  f_out.write ("    Tcl_CreateObjCommand(interp,\n")
  f_out.write ("			 NS \"::dut\",\n")
  f_out.write ("			 (Tcl_ObjCmdProc *) Dut_Cmd,\n")
  f_out.write ("			 (ClientData) &(SceMiGlobal.m_dutXactor),\n")
  f_out.write ("			 (Tcl_CmdDeleteProc *) Dut_Cmd_Delete);\n")
  f_out.write ("    Tcl_Export(interp, nsptr, \"dut\", 0);\n")
  f_out.write ("\n")
  f_out.write ("    // Bluespec emulation control command\n")
  f_out.write ("    Tcl_CreateObjCommand(interp,\n")
  f_out.write ("                         NS \"::emu\",\n")
  f_out.write ("                         (Tcl_ObjCmdProc *) Emu_Cmd,\n")
  f_out.write ("                         (ClientData) &(SceMiGlobal.m_tbsimControl),\n")
  f_out.write ("                         (Tcl_CmdDeleteProc *) Emu_Cmd_Delete);\n")
  f_out.write ("    Tcl_Export(interp, nsptr, \"emu\", 0);\n")
  f_out.write ("\n")
  f_out.write ("    // Bluespec emulation control command with readback\n")
  f_out.write ("    Tcl_CreateObjCommand(interp,\n")
  f_out.write ("                         NS \"::rdbk\",\n")
  f_out.write ("                         (Tcl_ObjCmdProc *) RdBk_Cmd,\n")
  f_out.write ("                         (ClientData) &(SceMiGlobal.m_simControl),\n")
  f_out.write ("                         (Tcl_CmdDeleteProc *) RdBk_Cmd_Delete);\n")
  f_out.write ("    Tcl_Export(interp, nsptr, \"rdbk\", 0);\n")
  f_out.write ("\n")
  f_out.write ("    // Bluespec probe capture command\n")
  f_out.write ("    Tcl_CreateObjCommand(interp,\n")
  f_out.write ("                         NS \"::probe\",\n")
  f_out.write ("                         (Tcl_ObjCmdProc *) Capture_Cmd,\n")
  f_out.write ("                         (ClientData) &(SceMiGlobal.m_probeControl),\n")
  f_out.write ("                         (Tcl_CmdDeleteProc *) Capture_Cmd_Delete);\n")
  f_out.write ("    Tcl_Export(interp, nsptr, \"probe\", 0);\n")
  f_out.write ("\n")
  f_out.write ("    // Bluespec probe netlist command\n")
  f_out.write ("    Tcl_CreateObjCommand(interp,\n")
  f_out.write ("                         NS \"::netlist\",\n")
  f_out.write ("                         (Tcl_ObjCmdProc *) llbits_netlist_cmd,\n")
  f_out.write ("                         (ClientData) &(SceMiGlobal.m_design),\n")
  f_out.write ("                         (Tcl_CmdDeleteProc *) llbits_netlist_cmd_delete);\n")
  f_out.write ("    Tcl_Export(interp, nsptr, \"netlist\", 0);\n")
  f_out.write ("\n")
  f_out.write ("    // Other command can go here\n")
  f_out.write ("\n")
  f_out.write ("  } catch (const exception & error) {\n")
  f_out.write ("    Tcl_AppendResult(interp, error.what()\n")
  f_out.write ("                     ,\"\\nCould not initialize bsdebug tcl package\"\n")
  f_out.write ("                     ,(char *) NULL);\n")
  f_out.write ("    return TCL_ERROR;\n")
  f_out.write ("  }\n")

  f_out.write ("  return TCL_OK;\n")
  f_out.write ("}\n")

  f_out.write ("\n\n")
  f_out.write ("// implementation of the scemi command ensemble\n")
  f_out.write ("// at the tcl level, the command will be\n")
  f_out.write ("// bsdebug::scemi init <params file>\n")
  f_out.write ("// bsdebug::scemi delete\n")
  f_out.write ("static int SceMi_Cmd(ClientData clientData,    	//  &(GlobalXactor),\n")
  f_out.write ("                     Tcl_Interp *interp,      	// Current interpreter\n")
  f_out.write ("                     int objc,               	// Number of arguments\n")
  f_out.write ("                     Tcl_Obj *const objv[]   	// Argument strings\n")
  f_out.write ("         )\n")
  f_out.write ("{\n")
  f_out.write ("  // Command table\n")
  f_out.write ("  enum ScemiCmds { scemi_init, scemi_delete };\n")
  f_out.write ("  static const cmd_struct cmds_str[] = {\n")
  f_out.write ("    {\"init\",		scemi_init,		\"<params file>\"}\n")
  f_out.write ("    ,{\"delete\",		scemi_delete,		\"\"}\n")
  f_out.write ("    ,{0}                        // MUST BE LAST\n")
  f_out.write ("  };\n")
  f_out.write ("\n")
  f_out.write ("  // Cast client data to proper type\n")
  f_out.write ("  SceMiGlobalData *pglobal = (SceMiGlobalData *) clientData;\n")
  f_out.write ("\n")
  f_out.write ("  // Extract sub command\n")
  f_out.write ("  ScemiCmds command;\n")
  f_out.write ("  int index;\n")
  f_out.write ("  if (objc == 1) goto wrongArgs;\n")
  f_out.write ("  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], cmds_str, sizeof(cmd_struct),\n")
  f_out.write ("                                           \"command\", 0, &index ) ) {\n")
  f_out.write ("    return TCL_ERROR;\n")
  f_out.write ("  }\n")
  f_out.write ("\n")
  f_out.write ("\n")
  f_out.write ("  command = (enum ScemiCmds) cmds_str[index].enumcode;\n")
  f_out.write ("  switch (command) {\n")
  f_out.write ("    case scemi_init:\n")
  f_out.write ("    {\n")
  f_out.write ("	if (objc != 3) goto wrongArgs;\n")
  f_out.write ("	char *paramfile = Tcl_GetString(objv[2]);\n")
  f_out.write ("	try {\n")
  f_out.write ("          pglobal->init(paramfile);\n")
  f_out.write ("	} catch (const exception & error) {\n")
  f_out.write ("          Tcl_AppendResult(interp, error.what()\n")
  f_out.write ("                           ,\"\\nCould not initialize emulation\"\n")
  f_out.write ("                           ,(char *) NULL);\n")
  f_out.write ("	  return TCL_ERROR;\n")
  f_out.write ("	}\n")
  f_out.write ("      break;\n")
  f_out.write ("    }\n")
  f_out.write ("    case scemi_delete:\n")
  f_out.write ("        pglobal->destroy();\n")
  f_out.write ("        break;\n")
  f_out.write ("  }\n")
  f_out.write ("  return TCL_OK;\n")
  f_out.write ("\n")
  f_out.write ("wrongArgs:\n")
  f_out.write ("  dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));\n")
  f_out.write ("  return TCL_ERROR;\n")
  f_out.write ("}\n")
  f_out.write ("\n")
  f_out.write ("static void SceMi_Cmd_Delete(ClientData clientData)\n")
  f_out.write ("{\n")
  f_out.write ("}\n")
  f_out.write ("\n")
  f_out.write ("\n")
  f_out.write ("\n")
  f_out.write ("// implementation of the Dut command ensemble\n")
  f_out.write ("// dut request <int>\n")
  f_out.write ("// dut response\n")
  f_out.write ("static int Dut_Cmd(ClientData clientData,    	// &(GlobalXactor.m_dutXactor)\n")
  f_out.write ("                   Tcl_Interp *interp,     	// Current interpreter\n")
  f_out.write ("                   int objc,               	// Number of arguments\n")
  f_out.write ("                   Tcl_Obj *const objv[]   	// Argument strings\n")
  f_out.write ("         )\n")
  f_out.write ("{\n")

  # Declare the types
  #  BitT<16> request_put_bits;
  #  BitT<32> response_get_bits;
  #  BitT<8> cin_c_bits;
  #  BitT<8> dout_bits;

  input_counts = 0
  output_counts = 0
  enable_counts = 0
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      input_counts += 1
      f_out.write ("  BitT<%d> %s_bits;\n" % (xactor.field_width, xactor_name))
    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      output_counts += 1
      f_out.write ("  BitT<%d> %s_bits;\n" % (xactor.field_width, xactor_name))
    elif isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      enable_counts += 1
      for j in range (len (xactor.verilog_names)):
        f_out.write ("  BitT<%d> %s_bits;\n" % (xactor.field_widths [j], xactor.verilog_names [j]))
    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      enable_counts += 1
      for j in range (len (xactor.verilog_names)):
        f_out.write ("  BitT<%d> %s_bits;\n" % (xactor.field_widths [j], xactor.verilog_names [j]))
    # Memories are not connected to the test bench

  f_out.write ("\n")
  f_out.write ("  // Command table\n")

  f_out.write ("  enum DutCmds {")
  # whether this is the first enum (and doesn't need a preceeding comma)
  first = 1
  # enums for Get/Put
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      if (first == 0):
        f_out.write (", ")
      first = 0
      f_out.write ("Dut_Response_%s" % xactor_name)
    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      if (first == 0):
        f_out.write (", ")
      first = 0
      f_out.write ("Dut_Request_%s" % xactor_name)
  # enum for inputs
  if (input_counts > 0):
    if (first == 0):
      f_out.write (", ")
    first = 0
    f_out.write ("Dut_Request")
  # enum for outputs
  if (output_counts > 0):
    if (first == 0):
      f_out.write (", ")
    first = 0
    f_out.write ("Dut_Response")
  # user testbench
  if (usertb):
    if (first == 0):
      f_out.write (", ")
    first = 0
    f_out.write ("Dut_Test, Dut_Reset")
  # other commands
  if (first == 0):
    f_out.write (", ")
  f_out.write ("Dut_CmdLog};\n")
 
  f_out.write ("  static const cmd_struct cmds_str[] = {\n")
  first = 1
  # command entries for Get/Put
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      if (first == 0):
        f_out.write ("    ,")
      else:
        f_out.write ("    ")
      first = 0
      f_out.write ("{\"response_%s\", Dut_Response_%s, \"\"}\n" % (xactor_name, xactor_name))
    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      if (first == 0):
        f_out.write ("    ,")
      else:
        f_out.write ("    ")
      first = 0
      f_out.write ("{\"request_%s\", Dut_Request_%s, \"" % (xactor_name, xactor_name))
      for j in range (len (xactor.verilog_names)):
        if (j > 0):
          f_out.write (", ")
        f_out.write ("int")
      f_out.write ("\"}\n")
  # command entry for inputs
  if (input_counts > 0):
    if (first == 0):
      f_out.write ("    ,")
    else:
      f_out.write ("    ")
    first = 0
    f_out.write ("{\"request\", Dut_Request, \"")
    for j in range (input_counts):
      if (j > 0):
        f_out.write (", ")
      f_out.write ("int")
    f_out.write ("\"}\n")
  # command entry for outputs
  if (output_counts > 0):
    if (first == 0):
      f_out.write ("    ,")
    else:
      f_out.write ("    ")
    first = 0
    f_out.write ("{\"response\", Dut_Response, \"\"}\n")
  # command entry for user testbench
  if (usertb):
    if (first == 0):
      f_out.write ("    ,")
    else:
      f_out.write ("    ")
    first = 0
    f_out.write ("{\"test\", Dut_Test, \"\"}\n")
    f_out.write ("    ,{\"reset\", Dut_Reset, \"\"}\n")
  # command entry for other commands
  if (first == 0):
    f_out.write ("    ,")
  f_out.write ("{\"cmdlog\",	Dut_CmdLog, \"val\"}\n")

  f_out.write ("    ,{0}                        // MUST BE LAST\n")
  f_out.write ("  };\n")
  f_out.write ("\n")
  f_out.write ("  // Check that client data has been set\n")
  f_out.write ("  %sXactor *dutx = *(%sXactor **) clientData;\n" % (new_module_name, new_module_name))
  f_out.write ("  if (dutx == 0) {\n")
  f_out.write ("    Tcl_SetResult (interp, (char *) \"Cannot use dut command before emulation initialization\", TCL_STATIC );\n")
  f_out.write ("    return TCL_ERROR;\n")
  f_out.write ("  }\n")
  f_out.write ("\n")
  f_out.write ("  // Extract sub command\n")
  f_out.write ("  DutCmds command;\n")
  f_out.write ("  int index;\n")
  f_out.write ("  if (objc == 1) goto wrongArgs;\n")
  f_out.write ("  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], cmds_str, sizeof(cmd_struct),\n")
  f_out.write ("                                           \"command\", 0, &index ) ) {\n")
  f_out.write ("    return TCL_ERROR;\n")
  f_out.write ("  }\n")
  f_out.write ("  command = (enum DutCmds) cmds_str[index].enumcode;\n")
  f_out.write ("\n")

  # variables to hold Put data
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      enable_counts += 1
      for j in range (len (xactor.verilog_names)):
        f_out.write ("  long %s;\n" % xactor.verilog_names [j])
  # variables to hold input/output data
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("  long %s;\n" % xactor_name)
    elif isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("  long %s;\n" % xactor_name)

  f_out.write ("  unsigned int off;\n")
  f_out.write ("  bool sent;\n")
  f_out.write ("\n")
  f_out.write ("  switch (command) {\n")

  # case arms for Get/Put
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("    case Dut_Response_%s:\n" % xactor_name)
      f_out.write ("      {\n")
      f_out.write ("        Tcl_Obj *r, *res;\n")
      f_out.write ("        if (objc != 2) goto wrongArgs;\n")
      f_out.write ("        bool gotit;\n")
      f_out.write ("\n")
      f_out.write ("        gotit = dutx->get_%s(" % xactor_name)
      first = 1
      for verilog_name in xactor.verilog_names:
        if (first == 0):
          f_out.write (", ")
        first = 0
        f_out.write ("%s_bits" % verilog_name)
      f_out.write (");\n\n")
      
      f_out.write ("        if (gotit) {\n")
      f_out.write ("          // Convert result to a object and return\n")

      first = 1
      for verilog_name in xactor.verilog_names:
        if (first == 1):
          f_out.write ("          long %s;\n" % verilog_name)
          f_out.write ("          r = Tcl_NewIntObj(1);\n")
          f_out.write ("          Tcl_SetObjResult(interp, r );\n\n")
          f_out.write ("          %s = %s_bits;\n" % (verilog_name, verilog_name))
          f_out.write ("          res = Tcl_NewLongObj(%s);\n" % verilog_name)
          f_out.write ("          Tcl_ListObjAppendElement(interp, r, res);\n\n")
          first = 0
        else:
          f_out.write ("          long %s;\n" % verilog_name)
          f_out.write ("          %s = %s_bits;\n" % (verilog_name, verilog_name))
          f_out.write ("          res = Tcl_NewLongObj(%s);\n" % verilog_name)
          f_out.write ("          Tcl_ListObjAppendElement(interp, r, res);\n\n")

      f_out.write ("        } else {\n")
      f_out.write ("          r = Tcl_NewIntObj(0);\n")
      f_out.write ("          Tcl_SetObjResult(interp, r );\n")
      f_out.write ("        }\n\n")
      f_out.write ("        // Log this function call\n")
      f_out.write ("        if (cmdlog)\n")
      f_out.write ("          {\n")
      f_out.write ("            (*rdback_log_file) << \"waitForResponse \\\"bsdebug::dut response_%s\\\" 100\" << std::endl;\n" % xactor_name)
      f_out.write ("          }\n")
      f_out.write ("        break;\n")
      f_out.write ("      }\n")

    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("    case Dut_Request_%s:\n" % xactor_name)
      f_out.write ("      {\n")
      f_out.write ("        if (objc != %d) goto wrongArgs;\n\n" % (len(xactor.verilog_names) + 2))

      i = 2
      first = 1
      params = ""
      for verilog_name in xactor.verilog_names:
	f_out.write ("        if (Tcl_GetLongFromObj(interp, objv[%d], &%s) != TCL_OK) {\n" % (i,verilog_name))
	f_out.write ("          return TCL_ERROR;\n")
	f_out.write ("        }\n")
	f_out.write ("        %s_bits = %s;\n\n" % (verilog_name, verilog_name))
	if (first):
	  params = " << "
	  first = 0
	else:
	  params += " << \" \" << "
	params += verilog_name
        i += 1

      f_out.write ("\n")
      f_out.write ("        sent = dutx->put_%s(" % xactor_name)
      first_vname = 1
      for verilog_name in xactor.verilog_names:
        if first_vname:
          first_vname = 0
        else:
          f_out.write (", ")
        f_out.write ("%s_bits" % verilog_name)
      f_out.write (");\n\n")
      
      f_out.write ("        Tcl_Obj *r = Tcl_NewBooleanObj( sent );\n")
      f_out.write ("        Tcl_SetObjResult(interp, r );\n\n")
      f_out.write ("        // Log this function call\n")
      f_out.write ("        if (cmdlog)\n")
      f_out.write ("          {\n")
      f_out.write ("            (*rdback_log_file) << \"bsdebug::dut request_%s \"%s << std::endl;\n" % (xactor_name, params))
      f_out.write ("          }\n")
      f_out.write ("        break;\n")
      f_out.write ("      }\n")

  # case arms for inputs
  if (input_counts > 0):
    f_out.write ("    case Dut_Request:\n")
    f_out.write ("      {\n")
    f_out.write ("        if (objc != %d) goto wrongArgs;\n\n" % (input_counts + 2))

    i = 2
    first = 1
    params = ""
    for xactor_name in dut_ifc.xactors:
      xactor = dut_ifc.xactors [xactor_name]
      if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
	f_out.write ("        if (Tcl_GetLongFromObj(interp, objv[%d], &%s) != TCL_OK) {\n" % (i, xactor_name))
	f_out.write ("          return TCL_ERROR;\n")
	f_out.write ("        }\n")
	f_out.write ("        %s_bits = %s;\n" % (xactor_name, xactor_name))
	if (first == 1):
	  params = " << "
	  first = 0
        else:
	  params += " << \\\" \\\" << "
	params += xactor_name
	f_out.write ("\n")
	i += 1

    first = 1
    for xactor_name in dut_ifc.xactors:
      xactor = dut_ifc.xactors [xactor_name]
      if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
	if (first == 1):
	  f_out.write ("        sent = dutx->send_%s(" % xactor_name)
	  f_out.write ("%s_bits);\n" % xactor_name)
	  first = 0
	else:
	  f_out.write ("        sent &= dutx->send_%s(" % xactor_name)
	  f_out.write ("%s_bits);\n" % xactor_name)
	  first = 0

    f_out.write ("        Tcl_Obj *r = Tcl_NewBooleanObj( sent );\n")
    f_out.write ("        Tcl_SetObjResult(interp, r );\n")
    f_out.write ("        // Log this function call\n")
    f_out.write ("        if (cmdlog)\n")
    f_out.write ("          {\n")
    f_out.write ("            (*rdback_log_file) << \"bsdebug::dut request %s\" << std::endl;\n" % params)
    f_out.write ("          }\n")
    f_out.write ("        break;\n")
    f_out.write ("      }\n")

  # case arms for outputs
  if (output_counts > 0):
    f_out.write ("    case Dut_Response:\n")
    f_out.write ("      {\n")
    f_out.write ("        Tcl_Obj *r, *res;\n")

    first = 1
    for xactor_name in dut_ifc.xactors:
      xactor = dut_ifc.xactors [xactor_name]
      if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
	if (first == 1):
	  f_out.write ("        long %s" % xactor_name)
	  first = 0
        else:
	  f_out.write (", %s" % xactor_name)
    if (first == 0):
      f_out.write (";\n")
    f_out.write ("        if (objc != 2) goto wrongArgs;\n")
    f_out.write ("        bool gotit;\n")

    first = 1
    for xactor_name in dut_ifc.xactors:
      xactor = dut_ifc.xactors [xactor_name]
      if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
	if (first == 1):
	  f_out.write ("        gotit = dutx->receive_%s(" % xactor_name)
	  first = 0
	else:
	  f_out.write ("        gotit |= dutx->receive_%s(" % xactor_name)
	f_out.write ("%s_bits);\n" % xactor_name)
    
    f_out.write ("        if (gotit) {\n")
    f_out.write ("          // Convert result to a object and return\n")

    first = 1
    for xactor_name in dut_ifc.xactors:
      xactor = dut_ifc.xactors [xactor_name]
      if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
	if (first == 1):
	  f_out.write ("          r = Tcl_NewIntObj(1);\n")
	  f_out.write ("          Tcl_SetObjResult(interp, r );\n\n")
	  f_out.write ("          %s = %s_bits;\n" % (xactor_name, xactor_name))
	  f_out.write ("          res = Tcl_NewLongObj(%s);\n" % xactor_name)
	  f_out.write ("          Tcl_ListObjAppendElement(interp, r, res);\n")
	  first = 0
	else:
	  f_out.write ("          %s = %s_bits;\n" % (xactor_name, xactor_name))
	  f_out.write ("          res = Tcl_NewLongObj(%s);\n" % xactor_name)
	  f_out.write ("          Tcl_ListObjAppendElement(interp, r, res);\n")

    f_out.write ("        } else {\n")
    f_out.write ("          r = Tcl_NewIntObj(0);\n")
    f_out.write ("          Tcl_SetObjResult(interp, r );\n")
    f_out.write ("        }\n\n")

    f_out.write ("        // Log this function call\n")
    f_out.write ("        if (cmdlog)\n")
    f_out.write ("          {\n")
    f_out.write ("            (*rdback_log_file) << \"waitForResponse \\\"bsdebug::dut response\\\" 100\" << std::endl;\n")
    f_out.write ("          }\n")
    f_out.write ("        break;\n")
    f_out.write ("      }\n")

  # case arms for user testbench
  if (usertb):
    f_out.write ("   case Dut_Test:\n")
    f_out.write ("     {\n")
    f_out.write ("       //if (objc != 2) goto wrongArgs;\n\n")
    f_out.write ("       // **Example how you receive a parameter from the caller below**\n")
    f_out.write ("       //std::string filename;\n")
    f_out.write ("       //filename = Tcl_GetString(objv[2]);\n\n")
    f_out.write ("       // Redirect cout to our stringstream buffer or any other ostream\n")
    f_out.write ("       setvbuf(stdout, NULL, _IONBF, 0);\n")
    f_out.write ("       std::stringstream buffer;\n")
    f_out.write ("       std::streambuf *sbuf = std::cout.rdbuf();\n")
    f_out.write ("       test_usertb(); // ** This is where the call to customed function is made.**\n")
    f_out.write ("       // When done redirect cout to its old self\n")
    f_out.write ("       std::cout.rdbuf(sbuf);\n\n")
    f_out.write ("       Tcl_Obj *r = Tcl_NewStringObj(buffer.str().c_str(), buffer.str().size());\n")
    f_out.write ("       Tcl_SetObjResult(interp, r);\n")
    f_out.write ("       break;\n")
    f_out.write ("     }\n")
    f_out.write ("   case Dut_Reset:\n")
    f_out.write ("     {\n")
    f_out.write ("       //if (objc != 2) goto wrongArgs;\n\n")
    f_out.write ("       // **Example how you receive a parameter from the caller below**\n")
    f_out.write ("       //std::string filename;\n")
    f_out.write ("       //filename = Tcl_GetString(objv[2]);\n\n")
    f_out.write ("       // Redirect cout to our stringstream buffer or any other ostream\n")
    f_out.write ("       setvbuf(stdout, NULL, _IONBF, 0);\n")
    f_out.write ("       std::stringstream buffer;\n")
    f_out.write ("       std::streambuf *sbuf = std::cout.rdbuf();\n")
    f_out.write ("       reset_usertb(); // ** This is where the call to customed function is made.**\n")
    f_out.write ("       // When done redirect cout to its old self\n")
    f_out.write ("       std::cout.rdbuf(sbuf);\n\n")
    f_out.write ("       Tcl_Obj *r = Tcl_NewStringObj(buffer.str().c_str(), buffer.str().size());\n")
    f_out.write ("       Tcl_SetObjResult(interp, r);\n")
    f_out.write ("       break;\n")
    f_out.write ("     }\n")

  # case arms for other commands
  f_out.write ("    case Dut_CmdLog:\n")
  f_out.write ("      {\n")
  f_out.write ("        std::string val;\n")
  f_out.write ("        if (objc >= 3)\n")
  f_out.write ("	     {\n")
  f_out.write ("	       val = Tcl_GetString(objv[2]);\n")
  f_out.write ("	     }\n")
  f_out.write ("\n")
  f_out.write ("	   if (val == \"on\")\n")
  f_out.write ("	     cmdlog = 1;\n")
  f_out.write ("      }\n")
  f_out.write ("  }\n")
  f_out.write ("  return TCL_OK;\n")
  f_out.write ("\n")
  f_out.write ("wrongArgs:\n")
  f_out.write ("  dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));\n")
  f_out.write ("  return TCL_ERROR;\n")
  f_out.write ("}\n")

  f_out.write ("static void Dut_Cmd_Delete(ClientData clientData)\n")
  f_out.write ("{\n")
  f_out.write ("}\n")


# ================================================================
# Generate Bsv<DUT>Xactor.{h,cpp}, capi.h

def genDutXactor (output_dir, dut_ifc, new_module_name):

  if genDutXactorHeader (output_dir, dut_ifc, new_module_name):
    return 1

  if genDutXactorImpl (output_dir, dut_ifc, new_module_name):
    return 1

  if genDutXactorCAPI (output_dir, dut_ifc, new_module_name):
    return 1

  return 0


def generatePutSendHeaders (f_out, prefix, xactor_name, xactor):

  params = ""
  pipeparams = ""

  first = 1
  for j in range (len (xactor.verilog_names)):
    if (first == 1):
      first = 0
    else:
      params += ", "
      pipeparams += ", "

    params += "BitT<%d> &%s" % (xactor.field_widths[j], xactor.verilog_names[j])
    pipeparams += "std::vector<BitT<%d> > &%s" % (xactor.field_widths[j], xactor.verilog_names[j])

  f_out.write ("  bool %sput_%s(" % (prefix, xactor_name))
  f_out.write (params)
  f_out.write (");\n")
  f_out.write ("  bool %sputB_%s(" % (prefix, xactor_name))
  f_out.write (params)
  f_out.write (");\n")

  if isinstance (xactor, Xactor_PIPEPUT_IFC):
    f_out.write ("  bool %svector_put_%s(" % (prefix, xactor_name))
    f_out.write (pipeparams)
    f_out.write (");\n")

    f_out.write ("  bool %svector_putB_%s(" % (prefix, xactor_name))
    f_out.write (pipeparams)
    f_out.write (");\n")

    f_out.write ("  bool %svector_putAck_%s(" % (prefix, xactor_name))
    f_out.write (pipeparams)
    f_out.write (");\n")

  f_out.write ("\n")


def generateInputSendHeaders (f_out, prefix, emu_type, xactor_name, xactor):

  typ = "BitT<%d>" % xactor.field_width
  f_out.write ("  bool %ssend_%s(" % (prefix, xactor_name))
  f_out.write ("%s &%s" % (typ, xactor_name))
  f_out.write ("_data);\n")
  f_out.write ("  bool %ssendB_%s(" % (prefix, xactor_name))
  f_out.write ("%s &%s" % (typ, xactor_name))
  f_out.write ("_data);\n")
  f_out.write ("  bool %ssendBAck_%s(" % (prefix, xactor_name))
  f_out.write ("%s &%s" % (typ, xactor_name))
  f_out.write ("_data);\n")

  if isinstance (xactor, Xactor_PIPE_IN_IFC):
    f_out.write ("  bool %svector_send_%s(std::vector<" % (prefix, xactor_name))
    f_out.write ("%s > &%s" % (typ, xactor_name))
    f_out.write ("_data);\n")

    f_out.write ("  bool %svector_sendB_%s(std::vector<" % (prefix, xactor_name))
    f_out.write ("%s > &%s" % (typ, xactor_name))
    f_out.write ("_data);\n")

    f_out.write ("  bool %svector_sendAck_%s(std::vector<" % (prefix, xactor_name))
    f_out.write ("%s > &%s" % (typ, xactor_name))
    f_out.write ("_data);\n")

  if (emu_type == 1):
    emu_param = "EmulationPortType t"
  else:
    emu_param = "BitT<1> &t"
  f_out.write ("  bool %sset_emulation_type_%s(%s);\n" % (prefix, xactor_name, emu_param))
  f_out.write ("\n")


def generateGetRecvHeaders (f_out, prefix, xactor_name, xactor):

  params = ""
  pipeparams = ""

  first = 1
  for j in range (len (xactor.verilog_names)):
    if (first == 1):
      first = 0
    else:
      params += ", "
      pipeparams += ", "

    params += "BitT<%d> &%s" % (xactor.field_widths[j], xactor.verilog_names[j])
    pipeparams += "std::vector<BitT<%d> > &%s" % (xactor.field_widths[j], xactor.verilog_names[j])

  f_out.write ("  bool %sget_%s(" % (prefix, xactor_name))
  f_out.write (params)
  f_out.write (");\n")
  f_out.write ("  bool %sgetB_%s(" % (prefix, xactor_name))
  f_out.write (params)
  f_out.write (");\n")
  f_out.write ("\n")

  if isinstance (xactor, Xactor_PIPEGET_IFC):
    f_out.write ("  unsigned %svector_get_%s(" % (prefix, xactor_name))
    f_out.write (pipeparams)
    f_out.write (", unsigned minReturned=0, unsigned maxReturned=0);\n")
    f_out.write ("\n")


def generateOutputRecvHeaders (f_out, prefix, emu_type, xactor_name, xactor):

  typ = "BitT<%d>" % xactor.field_width
  f_out.write ("  bool %sreceive_%s(" % (prefix, xactor_name))
  f_out.write ("%s &%s_data" % (typ, xactor_name))
  f_out.write (");\n")
  f_out.write ("  bool %sreceiveB_%s(" % (prefix, xactor_name))
  f_out.write ("%s &%s_data" % (typ, xactor_name))
  f_out.write (");\n")

  if isinstance (xactor, Xactor_PIPE_OUT_IFC):
    f_out.write ("  unsigned %svector_receive_%s(std::vector<" % (prefix, xactor_name))
    f_out.write ("%s > &%s_data, unsigned minReturned=0, unsigned maxReturned=0" % (typ, xactor_name))
    f_out.write (");\n")

  if (emu_type == 1):
    emu_param = "EmulationPortType t"
  else:
    emu_param = "BitT<1> &t"
  f_out.write ("  bool %sset_emulation_type_%s(%s);\n" % (prefix, xactor_name, emu_param))
  f_out.write ("\n")


def genDutXactorHeader (output_dir, dut_ifc, new_module_name):
  header_filename = os.path.join (output_dir, "%sXactor.h" % new_module_name)

  try:
    f_header = open (header_filename, "w")
  except:
    print "Error opening file: ", header_filename
    return 1

  print "Generating file: ", header_filename
  genDutXactorHeaderFile (f_header, dut_ifc, new_module_name)
  f_header.close()

  return 0


def genDutXactorHeaderFile (f_out, dut_ifc, new_module_name):

  f_out.write ("// Copyright Bluespec Inc. 2012-2013\n")
  f_out.write ("// By: GenTestbench tool\n\n")
  f_out.write ("#pragma once\n")
  f_out.write ("\n")
  f_out.write ("// Include Bluespec's SceMi C++ api\n")
  f_out.write ("#include \"DutXactor.h\"\n")
  f_out.write ("#include \"SceMiHeaders.h\"\n")
  f_out.write ("\n")

  # ******************
  # typedef StampedT<>
  # ******************

  # Get/Put
#  for xactor_name in dut_ifc.xactors:
#    xactor = dut_ifc.xactors [xactor_name]
#    if (isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC) or
#        isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC)):
#      f_out.write ("typedef StampedT<%s_%s> S%s_%s;\n" % (new_module_name, xactor_name, new_module_name, xactor_name))

  # outputs
#  for xactor_name in dut_ifc.xactors:
#    xactor = dut_ifc.xactors [xactor_name]
#    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
#      f_out.write ("typedef StampedT<%s_%s> S%s_%s;\n" % (new_module_name, xactor_name, new_module_name, xactor_name))

#  f_out.write ("\n")

  # ********************
  # class Bsv<mod>Xactor
  # ********************

  f_out.write ("// Define a class for the top-level transactor\n")
  f_out.write ("class %sXactor : public DutXactor {\n\n" % new_module_name)
  f_out.write (" protected:\n\n")
  f_out.write ("  static %sXactor *m_xactor;\n\n" % new_module_name)
  f_out.write ("  // Data members include transactors contained in the model\n")
  f_out.write ("  // Data Xactors\n")

  # *******
  # proxies
  # *******

  # Get/Put
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("  OutProxyT < %s_%s > m_%s;\n" % (new_module_name, xactor_name, xactor_name))
    elif isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("  InProxyT < %s_%s > m_%s;\n" % (new_module_name, xactor_name, xactor_name))

  # inputs
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
	f_out.write ("  InProxyT < %s_%s > m_%s;\n" % (new_module_name, xactor_name, xactor_name))
	f_out.write ("  InProxyT < BitT<1> > m_%s_ctrl;\n" % xactor_name)

  # outputs
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
	f_out.write ("  OutProxyT < %s_%s > m_%s;\n" % (new_module_name, xactor_name, xactor_name))
	f_out.write ("  InProxyT < BitT<1> > m_%s_ctrl;\n" % xactor_name)

  f_out.write ("\n")
  f_out.write ("  // Constructor\n")
  f_out.write ("  %sXactor (SceMi *scemi);\n" % new_module_name)
  f_out.write ("\n")
  f_out.write ("  // Destructor\n")
  f_out.write ("  ~%sXactor();\n\n" % new_module_name)
  f_out.write (" public:\n")
  f_out.write ("\n")
  f_out.write ("  // Initialize transactor\n")
  f_out.write ("  static %sXactor *init(SceMi *scemi);\n" % new_module_name)
  f_out.write ("  static %sXactor *get() { return m_xactor; }\n\n" % new_module_name)
  f_out.write ("  // Destroy transactor\n")
  f_out.write ("  void destroy();\n\n")
  f_out.write ("  // Public interface .....\n")

  # *******************
  # bool sendB and send
  # *******************

  f_out.write ("  // Put/Send\n")

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      generatePutSendHeaders (f_out, "", xactor_name, xactor)

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      generateInputSendHeaders(f_out, "", 0, xactor_name, xactor)

  # *****************
  # bool getB and get
  # *****************

  f_out.write ("  // Get/Receive\n")

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      generateGetRecvHeaders (f_out, "", xactor_name, xactor)

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      generateOutputRecvHeaders(f_out, "", 0, xactor_name, xactor)

  f_out.write ("};\n")


def generatePutSendMethods (f_out, prefix, new_module_name, xactor_name, xactor):

  params = ""
  pipeparams = ""
  firstvector = ""

  first = 1
  for j in range (len (xactor.verilog_names)):
    if (first == 1):
      first = 0
      firstvector = xactor.verilog_names[j]
    else:
      params += ", "
      pipeparams += ", "

    params += "BitT<%d> &%s" % (xactor.field_widths[j], xactor.verilog_names[j])
    pipeparams += "std::vector<BitT<%d> > &%s" % (xactor.field_widths[j], xactor.verilog_names[j])

  f_out.write ("bool %sXactor::%sput_%s(%s)\n" % (new_module_name, prefix, xactor_name, params))
  f_out.write ("{\n")
  f_out.write ("  %s_%s data;\n\n" % (new_module_name, xactor_name))
  for verilog_name in xactor.verilog_names:
    f_out.write ("  data.m_field_%s = %s;\n" % (verilog_name, verilog_name))
  f_out.write ("\n")
  f_out.write ("  bool sent = m_%s.sendNB(data);\n" % xactor_name)
  f_out.write ("  return sent;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("bool %sXactor::%sputB_%s(%s)\n" % (new_module_name, prefix, xactor_name, params))
  f_out.write ("{\n")
  f_out.write ("  %s_%s data;\n\n" % (new_module_name, xactor_name))
  for verilog_name in xactor.verilog_names:
    f_out.write ("  data.m_field_%s = %s;\n" % (verilog_name, verilog_name))
  f_out.write ("\n")
  f_out.write ("  m_%s.send(data);\n" % xactor_name)
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  if isinstance (xactor, Xactor_PIPEPUT_IFC):
    f_out.write ("bool %sXactor::%svector_put_%s(%s)\n" % (new_module_name, prefix, xactor_name, pipeparams))
    f_out.write ("{\n")
    f_out.write ("  std::vector<%s_%s> data;\n\n" % (new_module_name, xactor_name))
    f_out.write ("  data.resize(%s.size());\n" % firstvector)
    f_out.write ("  for (int i=0; i<%s.size(); i++) {\n" % firstvector)
    for verilog_name in xactor.verilog_names:
      f_out.write ("    data[i].m_field_%s = %s[i];\n" % (verilog_name, verilog_name))
    f_out.write ("  };\n")
    f_out.write ("\n")
    f_out.write ("  return m_%s.sendNB(data);\n" % xactor_name)
    f_out.write ("}\n")
    f_out.write ("\n")

    f_out.write ("bool %sXactor::%svector_putB_%s(%s)\n" % (new_module_name, prefix, xactor_name, pipeparams))
    f_out.write ("{\n")
    f_out.write ("  std::vector<%s_%s> data;\n\n" % (new_module_name, xactor_name))
    f_out.write ("  data.resize(%s.size());\n" % firstvector)
    f_out.write ("  for (int i=0; i<%s.size(); i++) {\n" % firstvector)
    for verilog_name in xactor.verilog_names:
      f_out.write ("    data[i].m_field_%s = %s[i];\n" % (verilog_name, verilog_name))
    f_out.write ("  };\n")
    f_out.write ("\n")
    f_out.write ("  m_%s.send(data);\n" % xactor_name)
    f_out.write ("  return true;\n")
    f_out.write ("}\n")
    f_out.write ("\n")

    f_out.write ("bool %sXactor::%svector_putAck_%s(%s)\n" % (new_module_name, prefix, xactor_name, pipeparams))
    f_out.write ("{\n")
    f_out.write ("  std::vector<%s_%s> data;\n\n" % (new_module_name, xactor_name))
    f_out.write ("  data.resize(%s.size());\n" % firstvector)
    f_out.write ("  for (int i=0; i<%s.size(); i++) {\n" % firstvector)
    for verilog_name in xactor.verilog_names:
      f_out.write ("    data[i].m_field_%s = %s[i];\n" % (verilog_name, verilog_name))
    f_out.write ("  };\n")
    f_out.write ("\n")
    f_out.write ("  m_%s.sendAcknowledge(data);\n" % xactor_name)
    f_out.write ("  return true;\n")
    f_out.write ("}\n")
    f_out.write ("\n")


def generateInputSendMethods (f_out, prefix, emu_type, new_module_name, xactor_name, xactor):

  typ = "BitT<%d>" % xactor.field_width
  f_out.write ("bool %sXactor::%ssend_%s(%s &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  bool sent = m_%s.sendNB(%s_data);\n" % (xactor_name, xactor_name))
  f_out.write ("  return sent;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("bool %sXactor::%ssendB_%s(%s &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  m_%s.send(%s_data);\n" % (xactor_name, xactor_name))
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("bool %sXactor::%ssendBAck_%s(%s &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  m_%s.sendAcknowledge(%s_data);\n" % (xactor_name, xactor_name))
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  if isinstance (xactor, Xactor_PIPE_IN_IFC):
    f_out.write ("bool %sXactor::%svector_send_%s(std::vector<%s > &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
    f_out.write ("{\n")
    f_out.write ("  return m_%s.sendNB(%s_data);\n" % (xactor_name, xactor_name))
    f_out.write ("}\n")
    f_out.write ("\n")

    f_out.write ("bool %sXactor::%svector_sendB_%s(std::vector<%s > &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
    f_out.write ("{\n")
    f_out.write ("  m_%s.send(%s_data);\n" % (xactor_name, xactor_name))
    f_out.write ("  return true;\n")
    f_out.write ("}\n")
    f_out.write ("\n")

    f_out.write ("bool %sXactor::%svector_sendAck_%s(std::vector<%s > &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
    f_out.write ("{\n")
    f_out.write ("  m_%s.sendAcknowledge(%s_data);\n" % (xactor_name, xactor_name))
    f_out.write ("  return true;\n")
    f_out.write ("}\n")
    f_out.write ("\n")

  if (emu_type == 1):
    emu_param = "EmulationPortType t"
  else:
    emu_param = "BitT<1> &t"
  f_out.write ("bool %sXactor::%sset_emulation_type_%s(%s)\n" % (new_module_name, prefix, xactor_name, emu_param))
  f_out.write ("{\n")
  f_out.write ("  m_%s_ctrl.sendAcknowledge(t);\n" % xactor_name)
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")


# This handles both PIPE and non-PIPE
def generateGetRecvMethods (f_out, prefix, new_module_name, xactor_name, xactor):

  params = ""
  pipeparams = ""
  resizeparams = ""

  first = 1
  for j in range (len (xactor.verilog_names)):
    if (first == 1):
      first = 0
    else:
      params += ", "
      pipeparams += ", "

    params += "BitT<%s> &%s" % (xactor.field_widths[j], xactor.verilog_names[j])
    pipeparams += "std::vector<BitT<%s> > &%s" % (xactor.field_widths[j], xactor.verilog_names[j])
    resizeparams += "  %s.resize(nitems);\n" % xactor.verilog_names[j]

  f_out.write ("bool %sXactor::%sget_%s(%s)\n" % (new_module_name, prefix, xactor_name, params))
  f_out.write ("{\n")
  if isinstance (xactor, Xactor_GET_IFC):
    f_out.write ("  %s_%s data;\n\n" % (new_module_name, xactor_name))
    f_out.write ("  bool gotone = m_%s.receiveNB(data);\n" % xactor_name)
    f_out.write ("  if (gotone) {\n")
    for verilog_name in xactor.verilog_names:
      f_out.write ("    %s = data.m_field_%s;\n" % (verilog_name, verilog_name))
    f_out.write ("  }\n")
  else:
    # PIPEGET
    f_out.write ("  %s_%s data;\n\n" % (new_module_name, xactor_name))
    f_out.write ("  bool gotone = m_%s.receiveNB(data);\n" % xactor_name)
    for verilog_name in xactor.verilog_names:
      f_out.write ("  %s = data.m_field_%s;\n" % (verilog_name, verilog_name))
  f_out.write ("  return gotone;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("bool %sXactor::%sgetB_%s(%s)\n" % (new_module_name, prefix, xactor_name, params))
  f_out.write ("{\n")
  if isinstance (xactor, Xactor_GET_IFC):
    f_out.write ("  %s_%s data;\n\n" % (new_module_name, xactor_name))
    f_out.write ("  m_%s.receive(data);\n" % xactor_name)
    for verilog_name in xactor.verilog_names:
      f_out.write ("  %s = data.m_field_%s;\n" % (verilog_name, verilog_name))
  else:
    # PIPEGET
    f_out.write ("  %s_%s data;\n\n" % (new_module_name, xactor_name))
    f_out.write ("  m_%s.receive(data);\n" % xactor_name)
    for verilog_name in xactor.verilog_names:
      f_out.write ("  %s = data.m_field_%s;\n" % (verilog_name, verilog_name))
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  if isinstance (xactor, Xactor_PIPEGET_IFC):
    f_out.write ("unsigned %sXactor::%svector_get_%s(%s" % (new_module_name, prefix, xactor_name, pipeparams))
    f_out.write (", unsigned minReturned, unsigned maxReturned)\n")
    f_out.write ("{\n")
    f_out.write ("  unsigned nitems;\n")
    f_out.write ("  std::vector<%s_%s> data;\n\n" % (new_module_name, xactor_name))
    f_out.write ("  nitems = m_%s.receive(data, minReturned, maxReturned);\n" % xactor_name)
    f_out.write ("  if (nitems == 0)\n")
    f_out.write ("    return nitems;\n\n")
    f_out.write ("%s\n" % resizeparams)
    f_out.write ("  for (int i=0; i<nitems; i++) {\n")
    for verilog_name in xactor.verilog_names:
      f_out.write ("    %s[i] = data[i].m_field_%s;\n" % (verilog_name, verilog_name))
    f_out.write ("  }\n")
    f_out.write ("  return nitems;\n")
    f_out.write ("}\n")
    f_out.write ("\n")

  f_out.write ("\n")


def generateOutputRecvMethods(f_out, prefix, emu_type, new_module_name, xactor_name, xactor):

  typ = "BitT<%d>" % xactor.field_width
  f_out.write ("bool %sXactor::%sreceive_%s(%s &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  %s_%s data;\n" % (new_module_name, xactor_name))
  f_out.write ("  bool gotone = m_%s.receiveNB(data);\n" % xactor_name)
  f_out.write ("  if (gotone) {\n")
  f_out.write ("    %s_data = data;\n" % xactor_name)
  f_out.write ("  }\n")
  f_out.write ("  return gotone;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("bool %sXactor::%sreceiveB_%s(%s &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  %s_%s data;\n" % (new_module_name, xactor_name))
  f_out.write ("  m_%s.receive(data);\n" % xactor_name)
  f_out.write ("  %s_data = data;\n" % xactor_name)
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("bool %sXactor::%sset_emulation_type_%s(BitT<1> &t)\n" % (new_module_name, prefix, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  m_%s_ctrl.sendAcknowledge(" << "t);\n" % xactor_name)
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")


def generateOutputPipeRecvMethods(f_out, prefix, emu_type, new_module_name, xactor_name, xactor):

  typ = "BitT<%d>" % xactor.field_width
  f_out.write ("bool %sXactor::%sreceive_%s(%s &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  bool gotone = m_%s.receiveNB(%s_data);\n" % (xactor_name, xactor_name))
  f_out.write ("  return gotone;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("bool %sXactor::%sreceiveB_%s(%s &%s_data)\n" % (new_module_name, prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  m_%s.receive(%s_data);\n" % (xactor_name, xactor_name))
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("unsigned %sXactor::%svector_receive_%s(std::vector<%s > &%s_data" % (new_module_name, prefix, xactor_name, typ, xactor_name))
  f_out.write (", unsigned minReturned, unsigned maxReturned)\n")
  f_out.write ("{\n")
  f_out.write ("  unsigned nitems = m_%s.receive(%s_data, minReturned, maxReturned);\n" % (xactor_name, xactor_name))
  f_out.write ("  return nitems;\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("bool %sXactor::%sset_emulation_type_%s(BitT<1> &t)\n" % (new_module_name, prefix, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  m_%s_ctrl.sendAcknowledge(t);\n" % xactor_name)
  f_out.write ("  return true;\n")
  f_out.write ("}\n")
  f_out.write ("\n")


def genDutXactorImpl (output_dir, dut_ifc, new_module_name):
  impl_filename = os.path.join (output_dir, "%sXactor.cpp" % new_module_name)

  try:
    f_impl = open (impl_filename, "w")
  except:
    print "Error opening file: ", impl_filename
    return 1

  print "Generating file: ", impl_filename
  genDutXactorImplFile (f_impl, dut_ifc, new_module_name)
  f_impl.close()

  return 0


def genDutXactorImplFile (f_out, dut_ifc, new_module_name):

  f_out.write ("// Copyright Bluespec Inc. 2012-2013\n")
  f_out.write ("// By: GenTestbench tool\n\n")
  f_out.write ("#include <iostream>\n")
  f_out.write ("#include \"%sXactor.h\"\n" % new_module_name)
  f_out.write ("\n")
  f_out.write ("using namespace std;\n")
  f_out.write ("\n")
  f_out.write ("%sXactor *%sXactor::m_xactor = NULL;\n\n" % (new_module_name, new_module_name))
  f_out.write ("%sXactor *%sXactor::init(SceMi *scemi)\n" % (new_module_name, new_module_name))
  f_out.write ("{\n")
  f_out.write ("  if (m_xactor != NULL)\n")
  f_out.write ("    return m_xactor;\n\n")
  f_out.write ("  m_xactor = new %sXactor(scemi);\n\n" % new_module_name)
  f_out.write ("  return m_xactor;\n")
  f_out.write ("}\n\n")
  f_out.write ("void %sXactor::destroy()\n" % new_module_name)
  f_out.write ("{\n")
  f_out.write ("  delete m_xactor;\n")
  f_out.write ("  m_xactor = NULL;\n")
  f_out.write ("}\n\n")
  f_out.write ("%sXactor::%sXactor(SceMi *scemi)\n" % (new_module_name, new_module_name))
  f_out.write ("  : DutXactor(scemi)\n")

  # **************************
  # Contructor initialization
  # **************************

  first = 1

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_PUT_IFC):
      f_out.write ("  , m_%s (\"\", \"scemi_put_%s_inport\", XactorAdapter::InPort)\n" % (xactor_name, xactor_name))
    elif isinstance (xactor, Xactor_PIPEPUT_IFC):
      f_out.write ("  , m_%s (\"\", \"scemi_put_%s_inpipe\", XactorAdapter::InPipe)\n" % (xactor_name, xactor_name))

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC):
      f_out.write ("  , m_%s (\"\", \"scemi_get_%s_outport\", XactorAdapter::OutPort)\n" % (xactor_name, xactor_name))
    elif isinstance (xactor, Xactor_PIPEGET_IFC):
      f_out.write ("  , m_%s (\"\", \"scemi_get_%s_outpipe\", XactorAdapter::OutPipe)\n" % (xactor_name, xactor_name))

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC):
      f_out.write ("  , m_%s (\"\", \"scemi_put_%s_inport\", XactorAdapter::InPort)\n" % (xactor_name, xactor_name))
      f_out.write ("  , m_%s_ctrl (\"\", \"scemi_put_%s_ctrl_in\", XactorAdapter::InPort)\n" % (xactor_name, xactor_name))
    elif isinstance (xactor, Xactor_PIPE_IN_IFC):
      f_out.write ("  , m_%s (\"\", \"scemi_put_%s_inpipe\", XactorAdapter::InPipe)\n" % (xactor_name, xactor_name))
      f_out.write ("  , m_%s_ctrl (\"\", \"scemi_put_%s_ctrl_in\", XactorAdapter::InPort)\n" % (xactor_name, xactor_name))

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_Out_IFC):
      f_out.write ("  , m_%s (\"\", \"scemi_get_%s_outport\", XactorAdapter::OutPort)\n" % (xactor_name, xactor_name))
      f_out.write ("  , m_%s_ctrl (\"\", \"scemi_get_%s_ctrl_in\", XactorAdapter::InPort)\n" % (xactor_name, xactor_name))
    elif isinstance (xactor, Xactor_PIPE_OUT_IFC):
      f_out.write ("  , m_%s (\"\", \"scemi_get_%s_outpipe\", XactorAdapter::OutPipe)\n" % (xactor_name, xactor_name))
      f_out.write ("  , m_%s_ctrl (\"\", \"scemi_get_%s_ctrl_in\", XactorAdapter::InPort)\n" % (xactor_name, xactor_name))

  f_out.write ("{\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  # **************************
  # Destructor
  # **************************

  f_out.write ("%sXactor::~%sXactor()\n" % (new_module_name, new_module_name))
  f_out.write ("{\n")
  f_out.write ("}\n")
  f_out.write ("\n")

  # ******
  # send
  # ******

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      generatePutSendMethods (f_out, "", new_module_name, xactor_name, xactor)

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      generateInputSendMethods(f_out, "", 0, new_module_name, xactor_name, xactor)

  # *****
  # get
  # *****

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      generateGetRecvMethods (f_out, "", new_module_name, xactor_name, xactor)

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_Out_IFC):
      generateOutputRecvMethods(f_out, "", 0, new_module_name, xactor_name, xactor)
    elif isinstance (xactor, Xactor_PIPE_OUT_IFC):
      generateOutputPipeRecvMethods(f_out, "", 0, new_module_name, xactor_name, xactor)


def generateInlinePutSend (f_out, prefix, new_module_name, xactor_name, xactor):

  params = ""
  params2 = ""
  pipeparams = ""

  first = 1
  for j in range (len (xactor.verilog_names)):
    if (first == 1):
      first = 0
      firstvector = xactor.verilog_names[j]
    else:
      params += ", "
      params2 += ", "
      pipeparams += ", "

    params += "BitT<%d> &%s" % (xactor.field_widths[j], xactor.verilog_names[j])
    params2 += xactor.verilog_names[j]
    pipeparams += "std::vector<BitT<%d> > &%s" % (xactor.field_widths[j], xactor.verilog_names[j])

  f_out.write ("inline bool %sput_%s(%s)\n" % (prefix, xactor_name, params))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->put_%s(%s);\n" % (new_module_name, xactor_name, params2))
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("inline bool %sputB_%s(%s)\n" % (prefix, xactor_name, params))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->putB_%s(%s);\n" % (new_module_name, xactor_name, params2))
  f_out.write ("}\n")
  f_out.write ("\n")

  if isinstance (xactor, Xactor_PIPEPUT_IFC):
    f_out.write ("inline bool %svector_put_%s(%s)\n" % (prefix, xactor_name, pipeparams))
    f_out.write ("{\n")
    f_out.write ("  return %sXactor::get()->vector_put_%s(%s);\n" % (new_module_name, xactor_name, params2))
    f_out.write ("}\n")
    f_out.write ("\n")

    f_out.write ("inline bool %svector_putB_%s(%s)\n" % (prefix, xactor_name, pipeparams))
    f_out.write ("{\n")
    f_out.write ("  return %sXactor::get()->vector_putB_%s(%s);\n" % (new_module_name, xactor_name, params2))
    f_out.write ("}\n")
    f_out.write ("\n")

    f_out.write ("inline bool %svector_putAck_%s(%s)\n" % (prefix, xactor_name, pipeparams))
    f_out.write ("{\n")
    f_out.write ("  return %sXactor::get()->vector_putAck_%s(%s);\n" % (new_module_name, xactor_name, params2))
    f_out.write ("}\n")
    f_out.write ("\n")


def generateInlineInputSend (f_out, prefix, new_module_name, xactor_name, xactor):

  typ = "BitT<%d>" % xactor.field_width
  f_out.write ("inline bool %ssend_%s(%s &%s_data)\n" % (prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->send_%s(%s_data);\n" % (new_module_name, xactor_name, xactor_name))
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("inline bool %ssendB_%s(%s &%s_data)\n" % (prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->sendB_%s(%s_data);\n" % (new_module_name, xactor_name, xactor_name))
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("inline bool %ssendBAck_%s(%s &%s_data)\n" % (prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->sendBAck_%s(%s_data);\n" % (new_module_name, xactor_name, xactor_name))
  f_out.write ("}\n")
  f_out.write ("\n")

  if isinstance (xactor, Xactor_PIPE_IN_IFC):
    f_out.write ("inline bool %svector_send_%s(std::vector<%s > &%s_data)\n" % (prefix, xactor_name, typ, xactor_name))
    f_out.write ("{\n")
    f_out.write ("  return %sXactor::get()->vector_send_%s(%s_data);\n" % (new_module_name, xactor_name, xactor_name))
    f_out.write ("}\n")
    f_out.write ("\n")

    f_out.write ("inline bool %svector_sendB_%s(std::vector<%s > &%s_data)\n" % (prefix, xactor_name, typ, xactor_name))
    f_out.write ("{\n")
    f_out.write ("  return %sXactor::get()->vector_sendB_%s(%s_data);\n" % (new_module_name, xactor_name, xactor_name))
    f_out.write ("}\n")
    f_out.write ("\n")

    f_out.write ("inline bool %svector_sendAck_%s(std::vector<%s > &%s_data)\n" % (prefix, xactor_name, typ, xactor_name))
    f_out.write ("{\n")
    f_out.write ("  return %sXactor::get()->vector_sendAck_%s(%s_data);\n" % (new_module_name, xactor_name, xactor_name))
    f_out.write ("}\n")
    f_out.write ("\n")

  f_out.write ("inline bool %sset_emulation_type_%s(EmulationPortType t)\n" % (prefix, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  BitT<1> data = t;\n")
  f_out.write ("  return %sXactor::get()->set_emulation_type_%s(data);\n" % (new_module_name, xactor_name))
  f_out.write ("}\n")
  f_out.write ("\n")


def generateInlineGetRecv (f_out, prefix, new_module_name, xactor_name, xactor):

  params = ""
  params2 = ""
  pipeparams = ""

  first = 1
  for j in range (len (xactor.verilog_names)):
    if (first == 1):
      first = 0
    else:
      params += ", "
      params2 += ", "
      pipeparams += ", "

    params += "BitT<%s> &%s" % (xactor.field_widths[j], xactor.verilog_names[j])
    params2 += xactor.verilog_names[j]
    pipeparams += "std::vector<BitT<%s> > &%s" % (xactor.field_widths[j], xactor.verilog_names[j])

  f_out.write ("inline bool %sget_%s(%s)\n" % (prefix, xactor_name, params))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->get_%s(%s);\n" % (new_module_name, xactor_name, params2))
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("inline bool %sgetB_%s(%s)\n" % (prefix, xactor_name, params))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->getB_%s(%s);\n" % (new_module_name, xactor_name, params2))
  f_out.write ("}\n")
  f_out.write ("\n")

  if isinstance (xactor, Xactor_PIPEGET_IFC):
    f_out.write ("inline unsigned %svector_get_%s(%s" % (prefix, xactor_name, pipeparams))
    f_out.write (", unsigned minReturned, unsigned maxReturned)\n")
    f_out.write ("{\n")
    f_out.write ("  unsigned nitems;\n\n")
    f_out.write ("  nitems = %sXactor::get()->vector_get_%s(%s, minReturned, maxReturned);\n" % (new_module_name, xactor_name, params2))
    f_out.write ("  return nitems;\n")
    f_out.write ("}\n")
    f_out.write ("\n")


def generateInlineOutputRecv (f_out, prefix, new_module_name, xactor_name, xactor):

  typ = "BitT<%d>" % xactor.field_width
  f_out.write ("inline bool %sreceive_%s(%s &%s_data)\n" % (prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->receive_%s(%s_data);\n" % (new_module_name, xactor_name, xactor_name))
  f_out.write ("}\n")
  f_out.write ("\n")

  f_out.write ("inline bool %sreceiveB_%s(%s &%s_data)\n" % (prefix, xactor_name, typ, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->receiveB_%s(%s_data);\n" % (new_module_name, xactor_name, xactor_name))
  f_out.write ("}\n")
  f_out.write ("\n")

  if isinstance (xactor, Xactor_PIPE_OUT_IFC):
    f_out.write ("inline unsigned %svector_receive_%s(std::vector<%s > &%s_data, unsigned minReturned, unsigned maxReturned)\n" % (prefix, xactor_name, typ, xactor_name))
    f_out.write ("{\n")
    f_out.write ("  return %sXactor::get()->vector_receive_%s(%s_data, minReturned, maxReturned);\n" % (new_module_name, xactor_name, xactor_name))
    f_out.write ("}\n")
    f_out.write ("\n")

  f_out.write ("inline bool %sset_emulation_type_%s(EmulationPortType t)\n" % (prefix, xactor_name))
  f_out.write ("{\n")
  f_out.write ("  BitT<1> data = t;\n")
  f_out.write ("  return %sXactor::get()->set_emulation_type_%s(data);\n" % (new_module_name, xactor_name))
  f_out.write ("}\n")
  f_out.write ("\n")


def genDutXactorCAPI (output_dir, dut_ifc, new_module_name):
  capi_filename = os.path.join (output_dir, "capi.h")

  try:
    f_capi = open (capi_filename, "w")
  except:
    print "Error opening file: ", capi_filename
    return 1

  print "Generating file: ", capi_filename
  genDutXactorCAPIFile (f_capi, dut_ifc, new_module_name)
  f_capi.close()

  return 0


def genDutXactorCAPIFile (f_out, dut_ifc, new_module_name):
  
  f_out.write ("// Copyright Bluespec Inc. 2012-2013\n")
  f_out.write ("// By: GenTestbench tool\n\n")
  f_out.write ("#pragma once\n")
  f_out.write ("\n")
  f_out.write ("#include \"semu_capi.h\"\n")
  f_out.write ("#include \"%sXactor.h\"\n\n" % new_module_name)

  f_out.write ("using namespace std;\n")

  f_out.write ("//\n")
  f_out.write ("// This is Bluespec C API for communicating with the DUT\n")
  f_out.write ("//\n\n")

  f_out.write ("#ifdef __cplusplus\n")
  f_out.write ("extern \"C\" {\n")
  f_out.write ("#endif\n\n")

  f_out.write ("// Below is capi for interfacing with scemi controlled clocks\n")
  f_out.write ("// All functions return 1 - success, 0 - failed.\n\n")
  f_out.write ("/*\n")
  f_out.write ("// This is how you advance controlled clock 1 cycle\n")
  f_out.write ("if (semu_advance_controlled_clock(1)) do_something();\n\n")

  f_out.write ("// Same as above, except the call is blocked until the number of cycles are done\n")
  f_out.write ("if (semu_advance_controlled_clockB(cycles)) do_something();\n\n")

  f_out.write ("// This is how you start free-running controlled clock\n")
  f_out.write ("if (semu_start_controlled_clock()) do_something;\n\n")

  f_out.write ("// This is how you stop controlled clock\n")
  f_out.write ("if (semu_stop_controlled_clock()) do_something();\n\n")

  f_out.write ("// This is how you get current cycle of the controlled clock\n")
  f_out.write ("SceMiU64 clock_cycles;\n")
  f_out.write ("if (semu_get_current_controlled_clock_cycle(clock_cycles)) do_something();\n\n")

  f_out.write ("// This is how you assert controlled reset for 8 cycles\n")
  f_out.write ("int number_of_cycles = 8;\n")
  f_out.write ("if (semu_assert_reset(number_of_cycles)) do_something();\n\n")
  f_out.write ("*/\n\n")

  f_out.write ("  // Reset\n")
  f_out.write ("  bool semu_assert_reset(unsigned short number_of_cycles, const char *reset_name=NULL);\n\n")

  # *******************
  # bool sendB and send
  # *******************

  f_out.write ("  // Below is capi for interfacing with the DUT\n")
  f_out.write ("  // The calls with 'B' denotes blocking calls\n\n")
  f_out.write ("  // send/put (non-blocking) and sendB/putB (blocking) to the DUT\n")

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      generatePutSendHeaders (f_out, "semu_", xactor_name, xactor)

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      generateInputSendHeaders(f_out, "semu_", 1, xactor_name, xactor)

  f_out.write ("\n")

  # *****************
  # bool getB and get
  # *****************

  f_out.write ("  // receive/get (non-blocking) and receiveB/getB (blocking) from the DUT\n")

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      generateGetRecvHeaders (f_out, "semu_", xactor_name, xactor)

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      generateOutputRecvHeaders(f_out, "semu_", 1, xactor_name, xactor)

  f_out.write ("#ifdef __cplusplus\n")
  f_out.write ("};\n")
  f_out.write ("#endif\n\n")

  # **********************
  # Inline send implementations
  # **********************

  f_out.write ("// send/put\n")

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      generateInlinePutSend(f_out, "semu_", new_module_name, xactor_name, xactor)

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      generateInlineInputSend(f_out, "semu_", new_module_name, xactor_name, xactor)

  # **********************
  # Inline get implementations
  # **********************

  f_out.write ("// receive/get\n")

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      generateInlineGetRecv(f_out, "semu_", new_module_name, xactor_name, xactor)

  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      generateInlineOutputRecv(f_out, "semu_", new_module_name, xactor_name, xactor)

  f_out.write ("inline bool semu_assert_reset(unsigned short number_of_cycles, const char *reset_name)\n")
  f_out.write ("{\n")
  f_out.write ("  return %sXactor::get()->assertReset(number_of_cycles);\n" % new_module_name)
  f_out.write ("}\n")
  

# ================================================================
# Generate gui_dut.tcl, gui_tb.tcl, gui_top.tcl

def genTclGui (output_dir, dut_ifc, new_module_name, usertb):

  if generateGuiDutTcl(output_dir, dut_ifc, new_module_name, usertb):
    return 1

  # The remaining files are for 'manualtb', so skip them if 'usertb'
  if usertb:
    return 0

  if generateGuiTbTcl(output_dir):
    return 1

  if generateGuiTopTcl(output_dir):
    return 1

  # XXX should this go in 'output_dir'?
  if generatePkgIndexTcl(""):
    return 1

  return 0


def generateGuiDutTcl (output_dir, dut_ifc, new_module_name, usertb):
  gui_dut_filename = os.path.join (output_dir, "gui_dut.tcl")

  try:
    f_gui_dut = open (gui_dut_filename, "w")
  except:
    print "Error opening file: ", gui_dut_filename
    return 1
  print "Generating file: ", gui_dut_filename
  generateGuiDutTclFile(f_gui_dut, dut_ifc, new_module_name, usertb)
  f_gui_dut.close()

  return 0


def generateGuiDutTclFile (f_out, dut_ifc, new_module_name, usertb):
  f_out.write ("package require Tk\n")
  f_out.write ("namespace import bsdebug::dut\n")
  f_out.write ("\n")
  f_out.write ("namespace eval GuiDut {\n")
  f_out.write ("    variable dut\n")

  # Generate variables "e<n>" for ports

  # Start with the Get/Put ports
  #
  start_rei_port_index = 1
  i = 1
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if (isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC) or
        isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC)):
      for j in range (len (xactor.verilog_names)):
        f_out.write ("    variable e%d 0\n" % i)
        i += 1

  # Follow with the Input/Output ports
  #
  start_reg_port_index = i
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if (isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC) or
        isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC)):
      f_out.write ("    variable e%d 0\n" % i)
      i += 1

  f_out.write ("    variable updatetime 500\n")
  f_out.write ("\n")

  if (usertb):
    f_out.write ("    # Below is a sample code (commented out) for the generated manually input testbench\n")

  def tbcomment_open():
    if (usertb):
      f_out.write ("        if {0} {\n")
  def tbcomment_close():
    if (usertb):
      f_out.write ("        }\n")

  f_out.write ("    proc mkDutControl { frame } {\n")
  f_out.write ("        variable top\n")
  tbcomment_open()
  f_out.write ("        variable putbut\n")
  f_out.write ("        variable getbut\n")
  tbcomment_close()
  f_out.write ("\n")
  f_out.write ("        set top $frame\n")
  f_out.write ("\n")

  tbcomment_open()
  f_out.write ("        ## Button Frame\n")
  f_out.write ("        set button_frame [ttk::frame $top.button_frame]\n")
  f_out.write ("        set inputs_label [ttk::labelframe $top.button_frame.label]\n")
  f_out.write ("        set inputs_text [ttk::label $top.blabel -justify center -text \"Inputs/Outputs\"]\n")
  f_out.write ("        pack $inputs_text\n")
  f_out.write ("        grid $inputs_label -row 1 -column 0\n")
  tbcomment_close()

  # Loop over the Get/Put ports again
  #
  i = start_rei_port_index
  row = 2
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      tbcomment_open()
      col = 1
      for j in range (len (xactor.verilog_names)):
        buf = i
        i += 1
	if ((col % 7) == 0):
	  row += 1
	  col = 1
        pn = xactor_name.lower()
	f_out.write ("        set %s_text [ttk::label $button_frame.%s_text -text %s -justify left -width 0 -anchor e]\n" % (pn, pn, pn))
	f_out.write ("        set e%d [ttk::entry $button_frame.%s_entry -textvariable GuiDut::e%d -width 8 -validate key -validatecommand \"GuiDut::chk_num %%P\"]\n" % (buf, pn, buf))
	f_out.write ("        grid $%s_text -pady 5 -padx 5 -row %d -column %d\n" % (pn, row, col))
        col += 1
	f_out.write ("        grid $e%d -pady 5 -padx 5 -row %d -column %d\n\n" % (buf, row, col))
        col += 1
      # XXX what if there are not ports? buf won't be set
      f_out.write ("        set putbut%d [ttk::button $button_frame.put%d -text \"Put\" -command \"GuiDut::do_put_%s\"]\n" % (buf, buf, xactor_name))
      f_out.write ("        grid $putbut%d -pady 5 -padx 5 -row %d -column %d\n\n" % (buf, row, col))
      #col += 1
      row += 1
      tbcomment_close()
    
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      tbcomment_open()
      col = 1
      for j in range (len (xactor.verilog_names)):
        buf = i
        i += 1
	if ((col % 7) == 0):
	  row += 1
	  col = 1
        pn = xactor_name.lower()
	f_out.write ("        set %s_text [ttk::label $button_frame.%s_text -text %s -justify left -width 0 -anchor e]\n" % (pn, pn, pn))
	f_out.write ("        set e%d [ttk::entry $button_frame.%s_entry -textvariable GuiDut::e%d -width 8 -validate key -validatecommand \"GuiDut::chk_num %%P\" -state disabled]\n" % (buf, pn, buf))
	f_out.write ("        grid $%s_text -pady 5 -padx 5 -row %d -column %d\n" % (pn, row, col))
        col += 1
	f_out.write ("        grid $e%d -pady 5 -padx 5 -row %d -column %d\n\n" % (buf, row, col))
        col += 1
      # XXX what if there are not ports? buf won't be set
      f_out.write ("        set getbut%d [ttk::button $button_frame.get%d -text \"Get\" -command \"GuiDut::do_get_%s\"]\n" % (buf, buf, xactor_name))
      f_out.write ("        grid $getbut%d -pady 5 -padx 5 -row %d -column %d\n\n" % (buf, row, col))
      row += 1
      tbcomment_close()

  first = 1
  col = 1
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
        tbcomment_open()
        if ((col % 7) == 0):
          row += 1
          col = 1
        pn = xactor_name.lower()
	first = 0
	f_out.write ("        set %s_text [ttk::label $button_frame.%s_text -text %s -justify left -width 0 -anchor e]\n" % (pn, pn, pn))
	f_out.write ("        set e%d [ttk::entry $button_frame.%s_entry -textvariable GuiDut::e%d -width 8 -validate key -validatecommand \"GuiDut::chk_num %%P\"]\n" % (i, pn, i))
	f_out.write ("        grid $%s_text -pady 5 -padx 5 -row %d -column %d\n" % (pn, row, col))
        col += 1
	f_out.write ("        grid $e%d -pady 5 -padx 5 -row %d -column %d\n\n" % (i, row, col))
        col += 1
        i += 1
        tbcomment_close()

  if (first == 0):
    tbcomment_open()
    f_out.write ("        set putbut [ttk::button $button_frame.put -text \"Send\" -command \"GuiDut::do_send\"]\n")
    f_out.write ("        grid $putbut -pady 5 -padx 5 -row %d -column %d\n\n" % (row, col))
    tbcomment_close()

  col = 1
  first = 1
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      tbcomment_open()
      if ((col % 7) == 0):
        row += 1
        col = 1

      if (first) :
	  row += 1
	  first = 0
      pn = xactor_name.lower()
      f_out.write ("        set %s_text [ttk::label $button_frame.%s_text -text %s -justify left -width 0 -anchor e]\n" % (pn, pn, pn))
      f_out.write ("        set e%d [ttk::entry $button_frame.%s_entry -textvariable GuiDut::e%d -width 8 -validate key -validatecommand \"GuiDut::chk_num %%P\" -state disabled]\n" % (i, pn, i))
      f_out.write ("        grid $%s_text -pady 5 -padx 5 -row %d -column %d\n" % (pn, row, col))
      col += 1
      f_out.write ("        grid $e%d -pady 5 -padx 5 -row %d -column %d\n\n" % (i, row, col))
      col += 1
      i += 1
      tbcomment_close()
    
  if (first == 0):
    tbcomment_open()
    f_out.write ("        set getbut [ttk::button $button_frame.get -text \"Receive \" -command \"GuiDut::do_get\"]\n")
    f_out.write ("        grid $getbut -pady 5 -padx 5 -row %d -column %d\n" % (row, col))
    tbcomment_close()
    
  f_out.write ("\n")
  tbcomment_open()
  f_out.write ("        pack $button_frame -anchor n -side top\n")
  tbcomment_close()
  f_out.write ("\n")
  f_out.write ("        return $top\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
 
  # Put send
  #
  i = start_rei_port_index
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_PUT_IFC) or isinstance (xactor, Xactor_PIPEPUT_IFC):
      params = ""
      f_out.write ("    proc do_put_%s {} {\n" % xactor_name)
      for j in range (len (xactor.verilog_names)):
	f_out.write ("        variable e%d;\n" % i)
	params += " $e%d" % i
	i += 1
      f_out.write ("        set msg \"Blocked\"\n")
      f_out.write ("\n")
      f_out.write ("        set sent [dut request_%s%s]\n" % (xactor_name, params))
      f_out.write ("        if {$sent} {\n")
      f_out.write ("          set msg \"sending: %s\"\n" % params)
      f_out.write ("        } else {\n")
      f_out.write ("          set msg \"sending failed.\"\n")
      f_out.write ("        }\n")
      f_out.write ("        puts \"$msg\"\n")
      f_out.write ("    }\n\n")

  # Get recv
  #
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_GET_IFC) or isinstance (xactor, Xactor_PIPEGET_IFC):
      params = ""
      f_out.write ("    proc do_get_%s {} {\n" % xactor_name)
      f_out.write ("        set res [dut response_%s]\n" % xactor_name)
      f_out.write ("        set found [lindex $res 0]\n")
      f_out.write ("        if {$found != 0} {\n")
      start_rei_outport_index = i
      for j in range (len (xactor.verilog_names)):
	f_out.write ("          set GuiDut::e%d [lindex $res %d]\n" % (i, j + 1))
        i += 1

      i = start_rei_outport_index
      for j in range (len (xactor.verilog_names)):
        if (j == 0):
	  f_out.write ("          set hex [format 0x%%x $GuiDut::e%d]\n" % i)
	  f_out.write ("          set msg \"Response: %s = $GuiDut::e%d (hex: $hex)\n" % (xactor.verilog_names[j], i))
	else:
	  f_out.write (" %s = $GuiDut::e%d" % (xactor.verilog_names[j], i))
        i += 1
      f_out.write ("\"\n")
      f_out.write ("        } else {\n")
      f_out.write ("          set msg \"No response received.\"\n")
      f_out.write ("        }\n")
      f_out.write ("        puts \"$msg\"\n")
      f_out.write ("    }\n\n")

  # Input sends
  #
  i = start_reg_port_index
  first = 1
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]

    if isinstance (xactor, Xactor_Raw_In_IFC) or isinstance (xactor, Xactor_PIPE_IN_IFC):
      if (first == 1):
        params = ""
        f_out.write ("    proc do_send {} {\n")
        f_out.write ("        variable e%d;\n" % i)
        params += " $e%d" % i
        first = 0
      else:
        f_out.write ("        variable e%d;\n" % i)
        params += " $e%d" % i
      i += 1

  if (first == 0):
    f_out.write ("        set msg \"Blocked\"\n")
    f_out.write ("\n")
    f_out.write ("        set sent [dut request%s]\n" % params)
    f_out.write ("        if {$sent} {\n")
    f_out.write ("          set msg \"sending: %s\"\n" % params)
    f_out.write ("        } else {\n")
    f_out.write ("          set msg \"sending failed.\"\n")
    f_out.write ("        }\n")
    f_out.write ("        puts \"$msg\"\n")
    f_out.write ("    }\n\n")

  # Output recvs
  #
  j = 1
  start_reg_outport_index = i
  first = 1
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      if (first == 1):
        f_out.write ("    proc do_get {} {\n")
        f_out.write ("\n")
        f_out.write ("        set res [dut response]\n")
        f_out.write ("        set found [lindex $res 0]\n")
        f_out.write ("        if { $found != 0 } {\n")
        f_out.write ("          set GuiDut::e%d [lindex $res %d]\n" % (i, j))
        j += 1
        first = 0
      else:
        f_out.write ("          set GuiDut::e%d [lindex $res %d]\n" % (i, j))
        j += 1
      i += 1

  j = 1
  i = start_reg_outport_index
  for xactor_name in dut_ifc.xactors:
    xactor = dut_ifc.xactors [xactor_name]
    if isinstance (xactor, Xactor_Raw_Out_IFC) or isinstance (xactor, Xactor_PIPE_OUT_IFC):
      if (j == 1):
        f_out.write ("          set msg \"Response: %s = $GuiDut::e%d" % (xactor_name, i))
        j = 0
      else:
        f_out.write (" %s = $GuiDut::e%d" % (xactor_name, i))
      i += 1

  if (first == 0):
    f_out.write ("\"\n")
    f_out.write ("        } else {\n")
    f_out.write ("          set msg \"No response received.\"\n")
    f_out.write ("        }\n")
    f_out.write ("        puts \"$msg\"\n")
    f_out.write ("    }\n\n")

  # continue

  f_out.write ("    proc getLoop {} {\n")
  f_out.write ("        if { [catch getLoopInternal err] } {\n")
  f_out.write ("            puts stderr \"Status loop failed,  interface will not respond\"\n")
  f_out.write ("            puts stderr $err\n")
  f_out.write ("        }\n")
  f_out.write ("    }\n\n")
  f_out.write ("    # Loop watching status\n")
  f_out.write ("    proc getLoopInternal {} {\n")
  f_out.write ("        variable updatetime\n")
  f_out.write ("\n")
  f_out.write ("        set res [dut response]\n")
  f_out.write ("        while { $res != \"\" } {\n")
  f_out.write ("            puts \"$res\"\n")
  f_out.write ("            set res [dut response]\n")
  f_out.write ("	}\n")
  f_out.write ("	after $updatetime GuiDut::getLoop\n")
  f_out.write ("        update\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
  f_out.write ("# ======\n")
  f_out.write ("\n")
  f_out.write ("    proc chk_num {s} {\n")
  f_out.write ("        #string is integer $s\n")
  f_out.write ("        return true;\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
  f_out.write ("    proc do_test {} {\n")
  if (usertb):
    f_out.write ("        set msg \"Blocked\"\n")
    f_out.write ("        set sent [dut test]\n")
    f_out.write ("        if {$sent != \"\"} {\n")
    f_out.write ("            set msg \"start testing...\"\n")
    f_out.write ("            puts \"$msg\"\n")
    f_out.write ("            puts \"$sent\"\n")
    f_out.write ("        }\n")
  f_out.write ("    }\n")
  f_out.write ("    proc do_reset {} {\n")
  if (usertb):
    f_out.write ("        set msg \"Blocked\"\n")
    f_out.write ("        set sent [dut reset]\n")
    f_out.write ("        if {$sent != \"\"} {\n")
    f_out.write ("            set msg \"reset testbench...\"\n")
    f_out.write ("            puts \"$msg\"\n")
    f_out.write ("            puts \"$sent\"\n")
    f_out.write ("        }\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
  f_out.write ("}\n")


def generateGuiTbTcl (output_dir):
  gui_tb_filename = os.path.join (output_dir, "gui_tb.tcl")

  try:
    f_gui_tb = open (gui_tb_filename, "w")
  except:
    print "Error opening file: ", gui_tb_filename
    return 1
  print "Generating file: ", gui_tb_filename
  generateGuiTbTclFile(f_gui_tb)
  f_gui_tb.close()

  return 0


def generateGuiTbTclFile (f_out):
  f_out.write ("package require Tk\n")
  f_out.write ("namespace import bsdebug::dut\n")
  f_out.write ("\n")
  f_out.write ("namespace eval GuiDut {\n")
  f_out.write ("    variable updatetime 500\n\n")

  f_out.write ("    #If you are going to build a DUT UI put it in this proc\n")
  f_out.write ("    #Build the window frame in top\n")
  f_out.write ("    proc mkDutControl { frame } {\n")
  f_out.write ("        variable top\n\n")
  f_out.write ("        set top $frame\n")
  f_out.write ("        # set button_frame [ttk::frame $top.button_frame]\n")
  f_out.write ("        return $top\n")
  f_out.write ("    }\n")
  f_out.write ("    proc getLoop {} {\n")
  f_out.write ("        if { [catch getLoopInternal err] } {\n")
  f_out.write ("            puts stderr \"Status loop failed,  interface will not respond\"\n")
  f_out.write ("            puts stderr $err\n")
  f_out.write ("        }\n")
  f_out.write ("    }\n")
  f_out.write ("    proc getLoop {} {\n")
  f_out.write ("        if { [catch getLoopInternal err] } {\n")
  f_out.write ("            puts stderr \"Status loop failed,  interface will not respond\"\n")
  f_out.write ("            puts stderr $err\n")
  f_out.write ("        }\n")
  f_out.write ("    }\n\n")
  f_out.write ("    # Loop watching status\n")
  f_out.write ("    proc getLoopInternal {} {\n")
  f_out.write ("        variable updatetime\n")
  f_out.write ("\n")
  f_out.write ("        set res [dut response]\n")
  f_out.write ("        while { $res != \"\" } {\n")
  f_out.write ("	       puts \"$res\"\n")
  f_out.write ("            set res [dut response]\n")
  f_out.write ("	}\n")
  f_out.write ("	after $updatetime GuiDut::getLoop\n")
  f_out.write ("     update\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
  f_out.write ("# ======\n")
  f_out.write ("\n")
  f_out.write ("    proc chk_num {s} {\n")
  f_out.write ("        #string is integer $s\n")
  f_out.write ("        return true\n")
  f_out.write ("    }\n")
  f_out.write ("\n")
  f_out.write ("}\n")


def generateGuiTopTcl (output_dir):
  gui_top_filename = os.path.join (output_dir, "gui_top.tcl")

  try:
    f_gui_top = open (gui_top_filename, "w")
  except:
    print "Error opening file: ", gui_top_filename
    return 1
  print "Generating file: ", gui_top_filename
  generateGuiTopTclFile(f_gui_top)
  f_gui_top.close()

  return 0


def generateGuiTopTclFile (f_out):

  f_out.write ("lappend auto_path .\n")
  f_out.write ("package require BSDebug\n")
  f_out.write ("#package require ReadBackGui\n")
  f_out.write ("#package require ProbeGui\n")
  f_out.write ("\n")
  f_out.write ("fonts::set_colours\n")
  f_out.write ("fonts::initialize\n")
  f_out.write ("\n")
  f_out.write ("# Display the window\n")
  f_out.write ("\n")
  f_out.write ("proc buildEmulationWin { {win .emu} top} {\n")
  f_out.write ("    if { $win != \".\" } {\n")
  f_out.write ("        toplevel $win\n")
  f_out.write ("    }\n")
  f_out.write ("    wm title $win \"Bluespec Emulation\"\n")
  f_out.write ("    set paned [ttk::panedwindow $win.paned -orient vertical]\n")
  f_out.write ("    pack $paned -side top -expand yes -fill both -pady 5 -padx 2\n")
  f_out.write ("\n")
  f_out.write ("    set c [ReadBackGui::mkEmulationControlPanels $win true $top]\n")
  f_out.write ("    $paned add $c -weight 0\n")
  f_out.write ("\n")
  f_out.write ("\n")
  f_out.write ("    set d [GuiDut::mkDutControl $win]\n")
  f_out.write ("    $paned add $d -weight 1\n")
  f_out.write ("\n")
  f_out.write ("#    set p [ProbeGui::mkProbePanel $win \"scemi_test.vcd\"]\n")
  f_out.write ("#    $paned add $p -weight 1\n")
  f_out.write ("\n")
  f_out.write ("    # start the status loop here\n")
  f_out.write ("    after 500 ReadBackGui::statusLoop\n")
  f_out.write ("#    after 500 ProbeGui::statusLoop\n")
  f_out.write ("    #after 500 GuiDut::getLoop\n")
  f_out.write ("}\n")


def generatePkgIndexTcl (output_dir):
  pkgIndex_filename = os.path.join (output_dir, "pkgIndex.tcl")

  try:
    f_pkgIndex = open (pkgIndex_filename, "w")
  except:
    print "Error opening file: ", pkgIndex_filename
    return 1
  print "Generating file: ", pkgIndex_filename
  generatePkgIndexTclFile(f_pkgIndex)
  f_pkgIndex.close()

  return 0


def generatePkgIndexTclFile (f_out):
  f_out.write (" # pkgIndex.tcl -- tells Tcl how to load my package.\n")
  f_out.write (" package ifneeded \"BSDebug\" 1.0	\\\n")
  f_out.write ("    [list load [file join $dir libbsdebug.so]]\n")


# ================================================================
# Main program when executed from the cmd line

def main (argv = None):
  if ((len (sys.argv) < 4) or (len (sys.argv) > 5)):
    print "Usage:  %s  <pin_file_name>  <emu_scripts_dir>  <cpp_src_dir>  [<usertb>]" % os.path.basename(sys.argv [0])
    return 1

  pin_filename = sys.argv [1]
  emu_scripts_dir = sys.argv [2]
  cpp_src_dir = sys.argv [3]

  usertb = False
  if (len (sys.argv) > 4):
    usertb = (sys.argv [4]).lower() in ("yes", "true", "t", "1")

  if not os.path.exists(cpp_src_dir):
    os.makedirs(cpp_src_dir)
  if not os.path.exists(emu_scripts_dir):
    os.makedirs(emu_scripts_dir)

  (dut_name, ext) = os.path.splitext (os.path.basename (pin_filename))
  BsvDUT = "Bsv" + dut_name

  try:
    f_pin = open (pin_filename, "r")
  except:
    print "Error opening pin file: ", pin_filename
    return 1

  dut_ifc = parse_pin_file (f_pin, pin_filename)
  #if isinstance (dut_ifc, DUT_IFC):
  #  dut_ifc.show ("  ")

  if genTclTb (cpp_src_dir, dut_ifc, BsvDUT, usertb):
    # error message will have been displayed
    return 1

  if genDutXactor (cpp_src_dir, dut_ifc, BsvDUT):
    # error message will have been displayed
    return 1

  if genTclGui(emu_scripts_dir, dut_ifc, BsvDUT, usertb):
    # error message will have been displayed
    return 1

  return 0

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main ())
