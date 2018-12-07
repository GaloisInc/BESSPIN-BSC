#include <tcl.h>
#include <tni.h>
#include <string.h>
#include <stdlib.h>
#include "Tstring.h"

int TclError(Tcl_Interp* interp, const Tstring& msg) 
{
  Tcl_SetStringObj(Tcl_GetObjResult(interp), (char*) msg.data(), -1);
  return TCL_ERROR;
}

int TclOk(Tcl_Interp* interp, const Tstring& msg) 
{
  Tcl_SetStringObj(Tcl_GetObjResult(interp), (char*) msg.data(), -1);
  return TCL_OK;
}

char* 
tclobj_to_charp(Tcl_Interp* interp, Tcl_Obj* obj)
{
  char* result;
  int len;
  
  if ((result = Tcl_GetStringFromObj(obj, &len)) == NULL) {
    throw "Cannot convert string";
  } else {
    return result;
  }
}

int
tclobj_to_int(Tcl_Interp* interp, Tcl_Obj* obj)
{
  int result;
  if (Tcl_GetIntFromObj(interp, obj, &result) == TCL_ERROR) {
    throw "cannot convert int";
  } else {
    return result;
  }
}

long
tclobj_to_long(Tcl_Interp* interp, Tcl_Obj* obj)
{
  long result;
  if (Tcl_GetLongFromObj(interp, obj, &result) == TCL_ERROR) {
    throw "cannot convert long";
  } else {
    return result;
  }
}

char *
int_to_charp(int* i)
{
  char *buf;
  buf = (char*) malloc(sizeof(char) * 20);
  sprintf(buf, "%d", *i);
  return buf;
}

char *
charp_to_charp(char** c)
{
  if(*c == NULL)
    return "";
  else
    return strdup(*c);
}

char *
char_to_charp(char* c)
{
  char *buf;
  buf = (char*) malloc(sizeof(char) * 2);
  buf[0] = *c;
  buf[1] = '\0';
  return buf;
}

Tstring
int_to_Tstring(const int& i)
{
  char buf[20];
  sprintf(buf, "%d", i);
  return Tstring(buf);
}

Tstring
charp_to_Tstring(const char* c)
{
  return Tstring(c);
}

Tstring
char_to_Tstring(const char& c)
{
  Tstring buf;
  buf += c;
  return buf;
}

Tstring
Tstring_to_Tstring(const Tstring& s)
{
  return Tstring(s);
}
