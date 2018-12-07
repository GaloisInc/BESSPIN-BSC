#include "Tstring.h"
#include <tcl.h>
#include <string.h>

#define TCLARGS ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[]

int TclError(Tcl_Interp* interp, const Tstring& msg);
int TclOk(Tcl_Interp* interp, const Tstring& msg);

// coversion routines
int tclobj_to_int(Tcl_Interp* interp, Tcl_Obj* obj);
long tclobj_to_long(Tcl_Interp* interp, Tcl_Obj* obj);
char* tclobj_to_charp(Tcl_Interp* interp, Tcl_Obj* obj);

char* int_to_charp(int* i);
char* charp_to_charp(char** c);
char* char_to_charp(char* c);

Tstring int_to_Tstring(const int& i);
Tstring charp_to_Tstring(const char* c);
Tstring char_to_Tstring(const char& c);
Tstring Tstring_to_Tstring(const Tstring& s);
