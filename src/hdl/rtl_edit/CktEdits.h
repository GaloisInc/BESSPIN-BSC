// Copyright 2009 Bluespec Inc. All rights reserved
#pragma once

#include "Types.h"
#include "CktMod.h"
#include "TclUtils.h"
#include "Set.h"
#include <set>
#include <map>
#include <string>

// Container class for managing a set of edits to a netlist
// this is the main class the tcl layer talks with.
class CktEdits {
 public:
  typedef std::list<CktMod *> ModList_t;
  typedef ModList_t::iterator ModListIter_t;

private:
  static  ModList_t s_modifications;
  static  std::set<BString>              s_probe_names; // Probe names in use
  static  std::map<unsigned int,BString> s_probe_uid;  // UniqueId to probename

  static bool duplicateProbeName (struct Tcl_Interp *interp, const BString &pname) ;
  static void registerProbeName   (unsigned int uniqueId, const BString &pname);
  static void unRegisterId        (unsigned int uniqueId);
  static void unRegisterAllIds    ();
  static void rmCktEdit           (unsigned int index);

public:
  static bool s_trace;
  static bool s_got_partition;
  static unsigned int s_partition_uid;
 
protected:

public:

  static int addProbe (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int addBsvProbes (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int addCosim  (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int addCapture (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int addTrigger (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int addPort (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int rmEdit   (struct Tcl_Interp *interp, const IntList &, struct Tcl_Obj **res);
  static struct Tcl_Obj * dump (struct Tcl_Interp *, bool useronly, const IntSet & ids);
  static struct Tcl_Obj * replay (struct Tcl_Interp *, const IntSet &ids) ;

  static int crossReference (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int compile (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int apply   (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int params   (struct Tcl_Interp *interp, const BString &infile, const BString &outfile);
  static int drawin   (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int drawout  (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int checkBsvType(struct Tcl_Interp *interp, BString &bsvtype, BString &path,
			  netCollection &signals, unsigned int width);
  static int partition (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int genInitSceMi (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int genFinalSceMi (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static int genTestbench (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[]);
  static void resetEditList();
};




