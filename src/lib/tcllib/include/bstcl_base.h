// Copyright Bluespec Inc. 2009-2010

#pragma once

#include <tcl.h>
#include <vector>

#define TCL_STATIC_FUNC_DECL(name) static int name(ClientData, Tcl_Interp *, int objc, struct Tcl_Obj *const objv[])
#define RETURN_IF_ERR(var) if ((var) != TCL_OK) { return var; }


// Command decoding structure
struct cmd_struct {
  const char * cname;                 // MUST be first
  int enumcode;
  const char * help;
};


typedef int (*tclfunptr) (ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]);

// Command decode structure pointer to function
struct cmd_struct_funptr {
  const char * cname;                 // MUST be first
  const char * help;
  tclfunptr    function;
};

// Command decode structure pointer to function and argument count
struct cmd_struct_funptr_2 {
  const char * cname;                 // MUST be first
  const char * help;
  int          numargs;
  tclfunptr    function;
};


