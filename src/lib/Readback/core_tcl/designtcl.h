// -*- c++ -*-

#ifndef __DESIGNTCL_H__
#define __DESIGNTCL_H__

extern "C" {

  int llbits_netlist_cmd(
			 ClientData clientData,
			 Tcl_Interp *interp,
			 int objc,
			 Tcl_Obj *const objv[]
			 );

  void llbits_netlist_cmd_delete(ClientData clientData);

}

#endif
