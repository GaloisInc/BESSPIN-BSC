#pragma once

#include <tcl.h>

EXTERN Tcl_Interp*           TTcl_CreateInterp (void);
EXTERN int                   TTcl_Eval (Tcl_Interp * interp, CONST char * script);
EXTERN CONST84_RETURN char * TTcl_GetStringResult (Tcl_Interp * interp);
EXTERN int                   TTcl_Init (Tcl_Interp * interp);
EXTERN int                   TTcl_LinkVar (Tcl_Interp * interp, CONST char * varName, char * addr, int type);
EXTERN int                   TTcl_UpdateLinkedVar (Tcl_Interp * interp, CONST char * varName);
