// Copright Bluespec Inc,   2010.  All rights reserved
#pragma once

#include "Types.h"


struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const int i) ;
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const char *str) ;
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const BString &s) ;
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const BStringList &sl) ;
struct Tcl_Obj * toTclObj (Tcl_Interp *interp, const std::pair<BString,BString> sp);
struct Tcl_Obj * toTclObj (Tcl_Interp *interp, const std::list<std::pair<BString,BString> > sp);
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const void *) ;

// Covert these to a sub list
struct Tcl_Obj * toTclList (struct Tcl_Interp *interp, struct Tcl_Obj *o1, struct Tcl_Obj *o2=0, struct Tcl_Obj *o3=0) ;


// Some Verilog related types
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const Parameter &) ;
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const ParameterList &) ;
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const ModuleTerminal &) ;
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const ModuleTerminalList &) ;
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const PushedPort &) ;
struct Tcl_Obj * toTclObj (struct Tcl_Interp *interp, const AddedPortList &) ;

// Conversion to TCL results
void toTclResult (struct Tcl_Interp *interp, const int ) ;
void toTclResult (struct Tcl_Interp *interp, const char *str) ;
void toTclResult (struct Tcl_Interp *interp, const BString &str) ;
void toTclResult (struct Tcl_Interp *interp, const BStringList &sl) ;
void toTclResult (struct Tcl_Interp *interp, struct Tcl_Obj *o) ;
void toTclResult (struct Tcl_Interp *interp, const void *v) ;

// Appending to tcl result -- result is treated as a list
void appendTclResult (struct Tcl_Interp *interp, const char *str) ;
void appendTclResult (struct Tcl_Interp *interp, const BString &str) ;
void appendTclResult (struct Tcl_Interp *interp, struct Tcl_Obj *o) ;

// Add an error message to the tcl result
void appendTclResultError (struct Tcl_Interp *interp, const BString & str);
void appendTclResultError (struct Tcl_Interp *interp, const char * str);


// Structure for decoding tcl command to enums
struct cmd_struct {
  const char * cname;                 // MUST be first
  int enumcode;
  const char * help;
};

typedef  void * ClientData;
// Structure for decoding tcl command to funciton pointers
typedef int (*TclFunPtr) (ClientData clientData, struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);

// Command decode structure pointer to function
struct cmd_struct_funptr {
  const char * cname;                 // MUST be first
  const char * help;
  TclFunPtr    function;
};


void dumpArguments (struct Tcl_Interp *interp, const cmd_struct cmds[], const char * given );
void dumpArguments (struct Tcl_Interp *interp, const cmd_struct_funptr cmds[], const char * given );

// Pulling data from a Tcl_Obj
int getInts (struct Tcl_Interp *interp, Tcl_Obj *obj, IntList &intlistout);
int getInts (struct Tcl_Interp *interp, Tcl_Obj *obj, IntSet  &intsetout )
;int getString (struct Tcl_Interp *interp, Tcl_Obj *obj, BString &stringout );
int getStringList (struct Tcl_Interp *interp, Tcl_Obj *obj, BStringList &stringout );
