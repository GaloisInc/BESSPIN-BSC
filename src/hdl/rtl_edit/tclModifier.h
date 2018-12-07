// Copyright 2010 Bluespec Inc. All rights reserved

#include "tcl.h"

extern "C" {
  int HhdlEditNL_Cmd(ClientData clientData,
                     Tcl_Interp *interp,
                     int objc,
                     Tcl_Obj * objv[]);

  void HhdlEditNL_Cmd_Delete(ClientData clientData);
}
