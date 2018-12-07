// Copyright 2010 Bluespec Inc. All rights reserved
#pragma once

#include "Types.h"
#include "tcl.h"
#include "VeriModule.h"

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif


// Find the module starting from the "from" module by the given path "path"
//  found is the returned value of the path
VeriModule *tc_findModuleTcl  (Tcl_Interp *interp, VeriModule *from, const BString &path, BString & found);
VeriModule *tc_findModuleByNameTcl (Tcl_Interp *interp, const BString &name);

// Find the Instance given the path in "o"
//  "i" is the returned value of the path found
int tc_extractInstance   (Tcl_Interp *interp, Tcl_Obj * o, instHandle & i);

// Find the Signal given the path in "o"
//  "sig" is the returned value of the path found
int tc_extractSignal     (Tcl_Interp *interp, Tcl_Obj * o, netHandle &sig);

int tc_extractSignals    (Tcl_Interp *interp, Tcl_Obj * o, const instHandle &inst,
			  netCollection & sigs, BString &errMsg, unsigned int &width);
int tc_extractSignalsFromPattern (Tcl_Interp *interp, Tcl_Obj * o, const instHandle &inst,
				  netCollection & exprs, netCollection & sigs, BString &errMsg,
				  unsigned int &width, SignalType signaltype);
int tc_extractPathsFromPattern (Tcl_Interp *interp, const BString &pattern, BStringList &paths);

int tc_extractSignalsOfModule (Tcl_Interp *interp, Tcl_Obj * o, const BString &modname, netCollection & sigs, BString &errMsg);

int tc_extractSignal     (Tcl_Interp *interp, Tcl_Obj * o, const instHandle &inst, BString & signaly, unsigned int &width);

int tc_extractPort       (Tcl_Interp *interp, const instHandle &inst, portHandle &port);

int tc_extractSimpleName (Tcl_Obj * o, BString & n);

int tc_extractFileName (Tcl_Interp *interp, Tcl_Obj * o, BString & n);

int tc_extractSimpleNumber (Tcl_Obj * o, int & n);

int tc_extractSimplePositiveNumber (Tcl_Obj * o, int & n);

int tc_extractSignalType (Tcl_Obj * o, SignalType & t);
