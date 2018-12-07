// Copyright 2009 Bluespec Inc. All rights reserved

#include <iostream>
#include <dirent.h>
#include <sys/stat.h>

#include "CktEdits.h"
#include "CktMod.h"
#include "TclUtils.h"
#include "TclModelUtils.h"
#include "TclDumper.h"
#include "EditCompile.h"
#include "ApplyChanges.h"
#include "CrossReference.h"
#include "HdlUtils.h"
#include "TestBenchGenerator.h"
#include "Strings.h"

using namespace std;

std::list<CktMod *> 		CktEdits::s_modifications;
std::set<BString> 	 	CktEdits::s_probe_names;
std::map<unsigned int, BString> CktEdits::s_probe_uid; // Map from m_uniqueId to probeName
bool                            CktEdits::s_trace = false;
bool                            CktEdits::s_got_partition = false;
unsigned int                    CktEdits::s_partition_uid = 0;

#define MAX_LINE_SIZE 1023

void toLowerCase(std::string &str);

void CktEdits::registerProbeName (const unsigned int uniqueId, const BString &pname)
{
  s_probe_names.insert(pname);
  s_probe_uid[uniqueId] = pname;
}

void CktEdits::unRegisterId      (unsigned int uniqueId)
{
  std::map<unsigned int,BString>::iterator  iter = s_probe_uid.find(uniqueId);
  if (iter != s_probe_uid.end() ) {
    s_probe_names.erase ((*iter).second);
  }
  s_probe_uid.erase(uniqueId);
}

void CktEdits::rmCktEdit (unsigned int index)
{
  for (ModListIter_t iter = s_modifications.begin(); iter != s_modifications.end(); ++iter) {
    if (index == (*iter)->getUniqueId()) {
      
      unRegisterId ((*iter)->getUniqueId());
      s_modifications.erase(iter);
      delete *iter;
      break;                  // iterator is no longer valid
    }
  }
}

void CktEdits::unRegisterAllIds      ()
{
  std::map<unsigned int,BString>::iterator  iter;

  for (iter = s_probe_uid.begin();
       iter != s_probe_uid.end();
       iter++) {
    s_probe_names.erase ((*iter).second);
    s_probe_uid.erase ((*iter).first);
  }
}

// removes compile generated edits and renumbers the keys starting at 0.
void CktEdits::resetEditList ()
{
  ModListIter_t cktmodItr;
  ModListIter_t currItr;
  CktMod *cktmod;
  CktMod::s_nextkey = 0;

  for (cktmodItr = s_modifications.begin();
       cktmodItr != s_modifications.end();
       ) {
    currItr = cktmodItr;
    cktmod = *currItr;
    ++cktmodItr;

    if (! cktmod->isUserEdit() ) {
      delete cktmod;
      s_modifications.erase(currItr);
      continue;
    }
    cktmod->setKey(++CktMod::s_nextkey);
  }
}

bool CktEdits::duplicateProbeName (Tcl_Interp *interp, const BString &pname)
{
  if (s_probe_names.end() != s_probe_names.find(pname)) {
      BString errMsg = "Duplicate probe or trigger name `";
      errMsg += pname + "'";
      toTclResult (interp, errMsg);
      return true;
  }
  return false;
}


int CktEdits::addProbe (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  BString probename ;
  instHandle insth ;
  netCollection exprs;
  netCollection sigs;
  BString clock, enable, bsvtype;
  BString errMsg;
  unsigned int width;           // width of signal in probes
  unsigned int temp_width;
  VeriModule *module;
  // Check name
  if (TCL_OK != tc_extractSimpleName(objv[2], probename) ) {
    errMsg = "Probe names must be alpha-numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[2]);
    return TCL_ERROR;
  }

  // Check duplicate probe name
  if ( duplicateProbeName (interp, probename) ) {
    return TCL_ERROR;
  }

  // Check path and module
  char *s = Tcl_GetStringFromObj (objv[3], NULL);
  BString temp(s);
  BString found;
  module = tc_findModuleTcl(interp, NULL, temp, found);
  if (module == NULL)
    return TCL_ERROR;

  // Check clock
  if (TCL_OK != tc_extractSignal(interp, objv[5], found, clock, temp_width)) {
    return TCL_ERROR;
  }
  if (temp_width != 1) {
    errMsg = "Clock signal " + clock + " must have bit width of 1 in add probe command" ;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  enable = Tcl_GetString(objv[6]);
  /*  TODO  enable can be a const or any expression    need to enable this parsing
  // Check enable
  if (TCL_OK != tc_extractSignal(interp, objv[6], found, enable, temp_width)) {
    return TCL_ERROR;
  }
  if (temp_width != 1) {
    errMsg = "Enable signal " + enable + " must have bit width of 1 in add probe command" ;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }
  */

  // Signal type
  SignalType type;
  if (objc > 8) {
    if (TCL_OK != tc_extractSignalType(objv[8], type)) {
      errMsg += "Valid signal type are { Any Flop Reg Input Output }";
      toTclResult (interp, errMsg);
      return TCL_ERROR;
    }
  } else
    type = CktModAny;
  
  // Check signals
  if (TCL_OK != tc_extractSignalsFromPattern(interp, objv[4], found, exprs, sigs, errMsg,
					     width, type)) {
    errMsg += " in add probe command" ;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  if (objc > 7)
    bsvtype = Tcl_GetString(objv[7]);

  if (CktEdits::checkBsvType(interp, bsvtype, temp, sigs, width) == TCL_ERROR) {
    char buf[256];
    snprintf(buf, 255, "%s/%s", temp.c_str(), exprs.front().c_str());
    errMsg = "Invalid BSVType ";
    errMsg += bsvtype + " for signal ";
    errMsg += buf;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  // If there is already a partition, then we have to delete it.
  if (s_got_partition == true)
    rmCktEdit(s_partition_uid);

  AddProbe * p = new AddProbe(found, probename, exprs, sigs, width, clock, enable, bsvtype, type);
  s_modifications.push_back(p);
  toTclResult (interp, p->getUniqueId());
  registerProbeName (p->getUniqueId(), probename);
  return TCL_OK;
}

int CktEdits::checkBsvType(struct Tcl_Interp *interp, BString &bsvtype, BString &path,
			   netCollection &signals, unsigned int width)
{
  int retval = TCL_OK;
  string net = getSignalName(signals.front()).c_str();

  // Now get the signaltype from the tcl SignalTypes package
  if (signals.size() == 1) {

    string tclcmd = "getActualPath ";
    tclcmd += path;
    retval = Tcl_Eval(interp, tclcmd.c_str());
    if (retval == TCL_OK) {
      path = Tcl_GetStringResult(interp);
    }
    tclcmd = "SignalTypes::lookupSig [join [concat ";
    tclcmd += "{" + path + "} {" + net + "}] \"/\"]";
    retval = Tcl_Eval(interp, tclcmd.c_str());
    if (retval == TCL_OK) {
      string temptype = Tcl_GetStringResult(interp);
      if (temptype != "") {
	bsvtype = temptype;
	removeAllSpaces(bsvtype);
      }
    }
    //else
    //  bsvtype = "";
  }

  // If there are subnets (multiple signals) then we need to use "Bit#(n)"??
  if ((bsvtype == "")) { // || (signals.size() > 1)) {

    //printf("checkBsvType1 %s %d\n", bsvtype.c_str(), signals.size());
    char buf[32];

    snprintf( buf, 31, "Bit#(%d)", width);
    //if (bsvtype != "") {
    //  fprintf(stderr, "Warning: BSVType field %s for signal %s is changed to %s\n",
    //          bsvtype.c_str(), net.c_str(), buf);
    //}
    bsvtype = buf ;
    retval = TCL_OK;
  }

  //printf("checkBsvType2 %s\n", bsvtype.c_str());
  return retval;
}

int CktEdits::addBsvProbes (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  BString probename ;
  instHandle insth ;
  SignalType type;
  BString errMsg;

  //printf("In CktEdits::addBsvProbes\n");
  // Check name
  if (TCL_OK != tc_extractSimpleName(objv[2], probename) ) {
    errMsg = "Probe names must be alpha-numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[2]);
    return TCL_ERROR;
  }

  // Check duplicate probe name
  if ( duplicateProbeName (interp, probename) ) {
    return TCL_ERROR;
  }

  char *s = Tcl_GetStringFromObj (objv[3], NULL);
  BString temp(s);
  // Check path pattern
  temp = s;
  BStringList paths;
  //printf("In CktEdits::addBsvProbes extractPathsFromPattern %s\n", temp.c_str());
  if (TCL_OK != tc_extractPathsFromPattern(interp, temp, paths)) {
    errMsg += "Could not find a module for the given hierarchical pattern " ;
    errMsg += temp;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  // Signal type
  //printf("In CktEdits::addBsvProbes extractSignalType\n");
  if (TCL_OK != tc_extractSignalType(objv[5], type)) {
    BString t = Tcl_GetStringFromObj(objv[5], NULL);
    errMsg += "Valid signal type are { Any Flop Reg Input Output }";
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  // If there is already a partition, then we have to delete it.
  if (s_got_partition == true)
    rmCktEdit(s_partition_uid);

  // Add bsv probes object, use "/" for dummy top as the insthandle
  //printf("In CktEdits::addBsvProbes new\n");
  BString pat = Tcl_GetStringFromObj(objv[4], NULL);
  AddBsvProbes * p = new AddBsvProbes("/", probename, temp, pat, paths, type);

  //printf("In CktEdits::addBsvProbes register\n");
  s_modifications.push_back(p);
  toTclResult (interp, p->getUniqueId());
  registerProbeName (p->getUniqueId(), probename);
  //printf("In CktEdits::addBsvProbes return\n");
  return TCL_OK;
}

int CktEdits::addCosim (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  BString probename ;
  instHandle insth ;
  netCollection signals;
  BString clock, uclock, trigger, flavor;
  BString errMsg;
  unsigned int width;           // width of signal in probes
  unsigned int temp_width;
  VeriModule *module;
  // Check name
  if (TCL_OK != tc_extractSimpleName(objv[2], probename) ) {
    errMsg = "Probe names must be alpha-numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[2]);
    return TCL_ERROR;
  }

  // Check duplicate probe name
  if ( duplicateProbeName (interp, probename) ) {
    return TCL_ERROR;
  }

  // Check path and module
  char *s = Tcl_GetStringFromObj (objv[3], NULL);
  BString temp(s);
  BString path = "";
  module = tc_findModuleTcl(interp, NULL, temp, path);
  if (module == NULL) 
    return TCL_ERROR;

  // Check clock
  if (TCL_OK != tc_extractSignal(interp, objv[4], path, clock, temp_width)) {
    return TCL_ERROR;
  }
  if (temp_width != 1) {
    errMsg = "Clock signal " + clock + " must have bit width of 1 in addCosim command" ;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  uclock = "";
  if (objc > 5) {
    uclock = Tcl_GetString(objv[5]);

    if (uclock.length() > 0) {

      // Check uclock
      if (TCL_OK != tc_extractSignal(interp, objv[5], path, uclock, temp_width)) {
	return TCL_ERROR;
      }
      if (temp_width != 1) {
	errMsg = "UClock signal " + uclock + " must have bit width of 1 in addCosim command" ;
	toTclResult (interp, errMsg);
	return TCL_ERROR;
      }
    }
  }

  trigger = "1'b1";
  if (objc > 6) {

    trigger = Tcl_GetString(objv[6]);

  //   // Check trigger
//     if (TCL_OK != tc_extractSignal(interp, objv[5], path, trigger, temp_width)) {
//       return TCL_ERROR;
//     }

//     if (temp_width != 1) {
//       errMsg = "Trigger signal " + trigger + " must have bit width of 1 in addCosim command" ;
//       toTclResult (interp, errMsg);
//       return TCL_ERROR;
//     }
  }

  width = 32;
  int temp_int;
  if (objc > 7) {
    // width of scan
    if ( TCL_ERROR == Tcl_GetIntFromObj(interp, objv[7], & temp_int) ) {
      return TCL_ERROR;
    } else if (temp_int < 1) {
      errMsg = "Width value for a scan probe must be greater than 0";
      toTclResult (interp, errMsg);
      return TCL_ERROR ;
    } else {
      width = temp_int;
    }
  }

  flavor = "OBSERVE";
  if (objc > 8) {
    flavor = Tcl_GetString(objv[8]);
  }

  // If there is already a partition, then we have to delete it.
  if (s_got_partition == true)
    rmCktEdit(s_partition_uid);

  CosimFlavor f = Observe;
  FromString(f, flavor);

  AddCosim * p = new AddCosim(path, probename, width, clock, uclock, trigger, f);
  s_modifications.push_back(p);
  toTclResult (interp, p->getUniqueId());
  registerProbeName (p->getUniqueId(), probename);
  return TCL_OK;
}

///////////////////////////////////////////////////////////////////////////////////
int CktEdits::addCapture (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  BString probename ;
  instHandle insth ;
  netCollection exprs;
  netCollection signals;
  BString clock, trigger, bsvtype, enable;
  BString errMsg;
  unsigned int width;           // width of signal in probes
  unsigned int temp_width;
  int temp_int;
  unsigned int depth;
  unsigned int runwidth;
  unsigned int delay;
  VeriModule *module;
  SignalType type;
  // Check name
  if (TCL_OK != tc_extractSimpleName(objv[2], probename) ) {
    errMsg = "Probe names must be alpha-numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[2]);
    return TCL_ERROR;
  }

  if (duplicateProbeName (interp, probename)) {
    return TCL_ERROR;
  }

  // Check path and module
  char *s = Tcl_GetStringFromObj (objv[3], NULL);
  BString temp(s);
  BString found;
  module = tc_findModuleTcl(interp, NULL, temp, found);
  if (module == NULL) { return TCL_ERROR; }

  // Check clock
  if (TCL_OK != tc_extractSignal(interp, objv[5], found, clock, temp_width)) {
    return TCL_ERROR;
  }
  if (temp_width != 1) {
    errMsg = "Clock signal " + clock + " must have bit width of 1 in add capture command" ;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  enable =  Tcl_GetString(objv[6]);
  trigger =  Tcl_GetString(objv[7]);
  // TODO Check that these are expressions

  // depth of capture
  if ( TCL_ERROR == Tcl_GetIntFromObj(interp, objv[8], & temp_int) ) {
    return TCL_ERROR;
  } else if (temp_int <= 1) {
    errMsg = "Depth for a capture probe must be greater than 1";
    toTclResult (interp, errMsg);
    return TCL_ERROR ;
  } else {
    depth = temp_int;
  }

  // run width of capture
  if ( TCL_ERROR == Tcl_GetIntFromObj(interp, objv[9], & temp_int) ) {
    return TCL_ERROR;
  } else if (temp_int < 1) {
    errMsg = "Run width for a capture probe must be greater than 0";
    toTclResult (interp, errMsg);
    return TCL_ERROR ;
  } else {
    runwidth = temp_int;
  }

  // run width of capture
  if ( TCL_ERROR == Tcl_GetIntFromObj(interp, objv[10], & temp_int) ) {
    return TCL_ERROR;
  } else if (temp_int < 0) {
    errMsg = "Delay from trigger to dump in a capture probe must be greater than 0";
    toTclResult (interp, errMsg);
    return TCL_ERROR ;
  } else {
    delay = temp_int;
  }

  // Signal type
  if (objc > 12) {
    if (TCL_OK != tc_extractSignalType(objv[12], type)) {
      BString t = Tcl_GetStringFromObj(objv[12], NULL);
      errMsg += "Valid signal type are { Any Flop Reg Input Output }";
      toTclResult (interp, errMsg);
      return TCL_ERROR;
    }
  } else
    type = CktModAny;

  // Check signals
  if (TCL_OK != tc_extractSignalsFromPattern(interp, objv[4], found, exprs, signals,
					     errMsg, width, type)) {
    errMsg += " in add capture command" ;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  if (objc > 11)
    bsvtype = Tcl_GetString(objv[11]);

  if (CktEdits::checkBsvType(interp, bsvtype, temp, signals, width) == TCL_ERROR) {
    char buf[256];
    snprintf( buf, 255, "%s/%s", temp.c_str(), exprs.front().c_str());
    errMsg = "Invalid BSVType ";
    errMsg += bsvtype + " for signal ";
    errMsg += buf;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  // If there is already a partition, then we have to delete it.
  if (s_got_partition == true)
    rmCktEdit(s_partition_uid);

  AddCapture * p = new AddCapture(found, probename, exprs, signals, enable, trigger, width,
				  clock, depth, runwidth, delay, bsvtype, type);
  s_modifications.push_back(p);
  toTclResult (interp, p->getUniqueId());
  registerProbeName (p->getUniqueId(), probename);
  return TCL_OK;
}

///////////////////////////////////////////////////////////////////////////////////
int CktEdits::addTrigger (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  BString triggername ;
  instHandle insth ;
  BString expr;
  BString clock, trigger;
  BString errMsg;
  unsigned int temp_width;
  VeriModule *module;

  // Check name
  if (TCL_OK != tc_extractSimpleName(objv[2], triggername) ) {
    errMsg = "Trigger names must be alpha-numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[2]);
    return TCL_ERROR;
  }

  if (duplicateProbeName (interp, triggername)) {
    return TCL_ERROR;
  }

  // Check path and module
  char *s = Tcl_GetStringFromObj (objv[3], NULL);
  BString temp(s);
  BString found;
  module = tc_findModuleTcl(interp, NULL, temp, found);
  if (module == NULL) { return TCL_ERROR; }

  // Check trigger expr
  if (TCL_OK != tc_extractSignal(interp, objv[4], found, expr, temp_width)) {
    return TCL_ERROR;
  }
  if (temp_width != 1) {
    errMsg = "Trigger expression " + expr + " must have bit width of 1" ;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  // Check clock
  if (TCL_OK != tc_extractSignal(interp, objv[5], found, clock, temp_width)) {
    return TCL_ERROR;
  }
  if (temp_width != 1) {
    errMsg = "Clock signal " + clock + " must have bit width of 1 in trigger command" ;
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  BStringList captures;
  if (TCL_OK != getStringList( interp, objv[6], captures)) {
    return TCL_ERROR;
  }
  if (captures.empty() ) {
     errMsg = "Triggers must specific at least one capture" ;
     toTclResult (interp, errMsg);
     return TCL_ERROR;
   }

  // If there is already a partition, then we have to delete it.
  if (s_got_partition == true)
    rmCktEdit(s_partition_uid);

  AddTrigger * p = new AddTrigger(found, triggername, expr, clock, captures);
  s_modifications.push_back(p);
  registerProbeName (p->getUniqueId(), triggername);

  toTclResult (interp, p->getUniqueId());
  return TCL_OK;
}

int CktEdits::addPort (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  instHandle insth ;
  BString portname ;
  BString porttypeSt;
  DirectionE porttype;
  int width;
  BString errMsg;
  if (TCL_OK != tc_extractSimpleName(objv[3], portname) ) {
    errMsg = "Port names must be alpha-numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[2]);
    return TCL_ERROR;
  }
  // Check instance
  if (TCL_OK != tc_extractInstance(interp, objv[2], insth) ){
    return TCL_ERROR;
  }
  // Check if port already exist
  if (TCL_OK == tc_extractPort(interp, insth, portname) ) {
    errMsg = "Port already exist";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[3]);
    return TCL_ERROR;
  }
  // Check porttype
  if (TCL_OK != tc_extractSimpleName(objv[4], porttypeSt) ) {
    errMsg = "Port type must be alpha-numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[4]);
    return TCL_ERROR;
  }
  if (porttypeSt == "input")
    porttype = d_input;
  else if (porttypeSt == "output")
    porttype = d_output;
  else if (porttypeSt == "inout")
    porttype = d_inout;
  else {
    errMsg = "Port type must be one of (input,output,inout)";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[4]);
    return TCL_ERROR;
  }
  // Check expression
  if (TCL_OK != tc_extractSimpleName(objv[5], porttypeSt) ) {
    errMsg = "Expression must be alpha-numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[5]);
    return TCL_ERROR;
  }
  // Check width
  if (TCL_OK != tc_extractSimplePositiveNumber(objv[6], width) ) {
    errMsg = "Port width must be positive numeric";
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[6]);
    return TCL_ERROR;
  }

  AddPort * p = new AddPort(insth, portname, porttype, width);
  s_modifications.push_back(p);
  toTclResult (interp, p->getUniqueId());
  return TCL_OK;
}

Tcl_Obj * CktEdits::dump (Tcl_Interp *interp, bool useronly, const IntSet & ids)
{
  Tcl_Obj *lo = Tcl_NewListObj (0, NULL);
  TclDumperVisitor v(interp);
  s_modifications.sort (CktMod::keyIsLessThan);

  for (ModListIter_t it =  s_modifications.begin(); it !=  s_modifications.end();  ++it) {
    if (useronly && !(*it)->isUserEdit()) continue; // Only show user edits
    if (! ids.empty() && ( ids.find ((*it)->getUniqueId()) == ids.end()))  continue;

    if (0 != (*it)->accept (&v)) {
      Tcl_ListObjAppendElement (interp, lo, v.getElement() );
    }
  }
  return lo;
}

// returns a tcl list of string with commands to regenerate the edits.
Tcl_Obj * CktEdits::replay (Tcl_Interp *interp, const IntSet &ids)
{
  Tcl_Obj *lo = Tcl_NewListObj (0, NULL);
  TclReplayVisitor v(interp);
  s_modifications.sort (CktMod::keyIsLessThan);

  for (ModListIter_t it =  s_modifications.begin(); it !=  s_modifications.end();  ++it) {
    if (!(*it)->isUserEdit()) continue; // Only show user edits
    if (! ids.empty() && ( ids.find ((*it)->getUniqueId()) != ids.end()))  continue;

    if (0 != (*it)->accept (&v)) {
      Tcl_ListObjAppendElement (interp, lo, v.getElement() );
    }
  }
  return lo;
}

// Remove an edit from the global collection
// returns tcl status  and result if the element if found and deleted
int CktEdits::rmEdit (Tcl_Interp *interp, const IntList & ids, Tcl_Obj **result)
{
  *result = Tcl_NewListObj (0, NULL);

  int ret = TCL_OK;
  for (IntList::const_iterator iter = ids.begin(); iter != ids.end(); ++ iter ) {
    int index = *iter;

    for (ModListIter_t iter = s_modifications.begin(); iter != s_modifications.end(); ++iter) {
      if ((unsigned int) index == (*iter)->getUniqueId()) {

        Tcl_Obj *x = Tcl_NewIntObj( index ); 
        Tcl_ListObjAppendElement (interp, *result, x );

        unRegisterId ((*iter)->getUniqueId());
        s_modifications.erase(iter);
	delete *iter;
        break;                  // iterator is no longer valid
      }
    }
  }
  return ret;
}

int CktEdits::crossReference (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  int ret = CrossReference::applyCrossReferences ( interp, s_modifications );
  return ret;
}

int CktEdits::compile (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  // Reassign the keys to keep runs consistent
  resetEditList();

  // Sort edits by instance to do a depth first tranversal
  int ret = CrossReference::applyCrossReferences ( interp, s_modifications );
  if (ret != TCL_OK) return ret;

  BString errMsg ;
  ret = EditCompile::compileChanges (s_modifications, errMsg );

  if (ret == TCL_OK) {
    IntSet unused;
    Tcl_Obj *sorted = CktEdits::dump (interp, false, unused);
    toTclResult (interp, sorted);
  }
  else {
    toTclResult (interp, errMsg );
  }

  return ret;
}

int CktEdits::apply (Tcl_Interp *interp, int objc, Tcl_Obj * objv[])
{
  string foundPath;
  VeriModule *module;
  BStringList writtenFiles;

  if (s_trace) {
    printf("In CktEdits::apply\n");
  }

  char *outputDir = Tcl_GetStringFromObj (objv[2], NULL);

  // Set apply changes to text based mode
  ApplyChanges action(outputDir);
  //action.setTextBased(0);

  //s_modifications.sort (CktMod::keyIsLessThan);

  // iterate over all modsets
  for(int i = 0; i <= 1 ;i++) {
    
    ModSet modset = (ModSet) i;

    // Put all the modifications in a multiset so we can traverse a set of modifications
    //  for each particular inst at a time
    ModInstSet_t m_instSet;
    for (CktEdits::ModList_t::iterator iter = s_modifications.begin();
	 iter != s_modifications.end(); ++iter) {
      if ((*iter)->getModSet() == modset) {
	m_instSet.insert(*iter);
	BString iname    = (*iter)->getInstName();
      }
    }

    // From multiset get the range of each instance given by the path and apply changes
    ModInstSetIter_t iter;
    for (iter = m_instSet.begin(); iter != m_instSet.end(); iter = m_instSet.upper_bound(*iter) ) {
      BString iname    = (*iter)->getInstName();

      // Skip unreal changes (aka AddBsvProbes)
      if (iname == "/") continue;

      ModInstSetPairIter_t  thisinst = m_instSet.equal_range(*iter); // Pair of range

      // Find the module from the instance name
      module = HdlUtils::findModuleFromPath(NULL, iname, foundPath);
      if (module == NULL) {
	BString errMsg("Module not found");
	toTclResult (interp, errMsg);
	appendTclResult (interp, iname.c_str());
	continue;
      }

      BString::size_type delim  = iname.rfind("/");
      bool is_top = delim == 0 || delim == BString::npos;

      // Parsed tree based
      // Get the original module which is to be edited
      // If not found then something is wrong, the module was not copied before elaboration
      VeriModule * origmodule = HdlUtils::getOriginalUnelabModule(module);
      if (origmodule == 0) {
	BString errMsg("Could not find original module");
	toTclResult (interp, errMsg);
	appendTclResult (interp, module->Name());
	origmodule = module;
      }

      if (!action.getTextBased()) {
	string newName = origmodule->GetName();
	size_t pos = newName.find("_orig");
	newName = newName.substr(0, pos);
	//printf("New context module: %s\n", newName.c_str());
	newName += "_context";
	VeriModule *newModule = HdlUtils::copyModule(origmodule, newName.c_str(),
						     HdlUtils::getTempLibrary());
	//cerr << "Apply changes on instance: " << iname 
	//     << " to original copied module " << newModule->Name() << endl;

	// Mark original module for write (for testing purposes only)
	action.markModuleForWrite(origmodule);

	// Set the module context
	action.setModuleContext(newModule);
      }
      else { // Text based
	//cerr << "Apply changes on instance: " << iname 
	//     << " to original copied module " << origmodule->Name() << endl;

	// Clear any previous modifications on this module
	action.clearModifications(origmodule);

	// Set the module context
	action.setModuleContext(origmodule);
      }

      // Then apply changes to the newly copied module
      while (action.morePasses()) {
	for (ModInstSetIter_t inst_iter = thisinst.first; inst_iter != thisinst.second;
	     ++inst_iter ) {
	  (*inst_iter)->accept(&action);
	}
	// Take another pass on the module
	action.nextPass();
      }

      // If this is a text based, we have to write each module out to a file
      // right after the changes because the modifications could be cleared
      // if the same module is edited again in a different way.
      if (action.getTextBased()) {
 	BString path = iname;
	writtenFiles.push_back(action.writeTextBasedChanges(action.getModuleContext(), path, modset, is_top));
      }
    }
  }

  // Write new verilog files to the outputDir
  if (!action.getTextBased())
    action.writeParsedTreeChanges(writtenFiles);

  // write out the scan.map file
  action.writePathInfo();

  std::list<BString>::iterator itr;
  for (itr = writtenFiles.begin(); itr != writtenFiles.end(); itr++) {
    appendTclResult (interp, (*itr));
  }
  
  return TCL_OK;                // TODO return to TCL the list of files created.
}

// "downpath toppath signal port"
int CktEdits::drawin   (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[])
{
  BString downpath, toppath, signal, port;
  // Check instance
  if (TCL_OK != tc_extractInstance(interp, objv[2], downpath) ){
    return TCL_ERROR;
  }
  if (TCL_OK != tc_extractInstance(interp, objv[3], toppath) ){
    return TCL_ERROR;
  }
  unsigned temp_width;
  if (TCL_OK != tc_extractSignal(interp, objv[4], downpath, signal, temp_width)) {
    return TCL_ERROR;
  }
  if (TCL_OK != tc_extractSimpleName(objv[5], port) ) {
    BString errMsg("Port name must be alpha-numeric");
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[5]);
    return TCL_ERROR;
  }
  if (toppath.empty()) { toppath = "/";}
  DrawInSignal * p = new DrawInSignal(downpath, toppath, signal, port, temp_width);
  s_modifications.push_back(p);
  toTclResult (interp, p->getUniqueId());
  return TCL_OK;
}

// "downpath toppath signal port"
int CktEdits::drawout   (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[])
{
  BString downpath, toppath, signal, port;
  // Check instance
  if (TCL_OK != tc_extractInstance(interp, objv[2], downpath) ){
    return TCL_ERROR;
  }
  if (TCL_OK != tc_extractInstance(interp, objv[3], toppath) ){
    return TCL_ERROR;
  }
  unsigned temp_width;
  if (TCL_OK != tc_extractSignal(interp, objv[4], downpath, signal, temp_width)) {
    return TCL_ERROR;
  }
  if (TCL_OK != tc_extractSimpleName(objv[5], port) ) {
    BString errMsg("Port name must be alpha-numeric");
    toTclResult (interp, errMsg);
    appendTclResult (interp, objv[5]);
    return TCL_ERROR;
  }
  if (toppath.empty()) { toppath = "/";}
  DrawOutSignal * p = new DrawOutSignal(downpath, toppath, signal, port, temp_width);
  s_modifications.push_back(p);
  toTclResult (interp, p->getUniqueId());
  return TCL_OK;
}

int CktEdits::partition (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[])
{
  BString bspec, mspec;
  if (TCL_OK == tc_extractSimpleName(objv[2], bspec)) {
    if (bspec != "7002" && bspec != "7006" && bspec != "7406")
      if (TCL_OK != tc_extractFileName(interp, objv[2], bspec) ){
	return TCL_ERROR;
      }
  } else {
    return TCL_ERROR;
  }

  if (TCL_OK != tc_extractFileName(interp, objv[3], mspec) ){
    return TCL_ERROR;
  }

  // TODO Other cheks are needed here.
  // Should not be a top module

  // If there is already a partition edit then error
  if (s_got_partition == true) {
    BString errMsg("Cannot partition more than once.");
    toTclResult (interp, errMsg);
    return TCL_ERROR;
  }

  // If there are other task then we can't do partition
  if (s_modifications.size()) {
    return TCL_OK;
  }

  Partition * p = new Partition(bspec, mspec);
  s_modifications.push_back(p);
  toTclResult (interp, p->getUniqueId());
  s_got_partition = true;
  s_partition_uid = p->getUniqueId();

  return TCL_OK;
}

int CktEdits::genInitSceMi (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[])
{
  BString cfgfilename, pinfilename, temp;
  BString errMsg;
  // Check path and module
  BString found;
  BString outdir;
  int stat;

  if (TCL_OK != tc_extractFileName(interp, objv[2], cfgfilename) ){
    return TCL_ERROR;
  }

  if (TCL_OK != tc_extractSimpleName(objv[3], pinfilename) ){
    return TCL_ERROR;
  }

  if (TCL_OK != tc_extractSimpleName(objv[4], outdir) ){
    return TCL_ERROR;
  }
    
  TestBenchGenerator *tbgen = new TestBenchGenerator();
  stat = tbgen->readSpecFile(cfgfilename.c_str());
  if (!stat) {
    tbgen->getErrorMessage(errMsg);
    appendTclResultError (interp, errMsg);
    delete tbgen;
    return TCL_ERROR;
  }
  
  // Process output dir to point to bsv source
  size_t loc = outdir.find_last_of('/');
  outdir = outdir.substr(0, loc+1) + "bsv";
  tbgen->setBsvOutputDir(outdir.c_str());

  // Read pin file
  BString modname, newmodname;
  modname = tbgen->moduleName();
  HdlUtils *hdlUtils = HdlUtils::init();
  hdlUtils->generatePinFile(pinfilename.c_str(), modname.c_str());
  if (hdlUtils->readPinFile(pinfilename.c_str()) == 0)
    return TCL_ERROR;

  // Now do some bookeeping and then generate bsv wrappers
  newmodname = tbgen->bsvModuleName();
  outdir = tbgen->bsvOutputDir();

  netCollection signals;
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  std::map<string, string>::iterator signalItr;
  BString sig;

  for (signalItr = map->begin(); signalItr != map->end(); signalItr++) {
    sig = signalItr->first + ":" + signalItr->second;
    signals.push_back(sig);
  }

  tbgen->setSceMiControlSignals(signals);

  /*
  std::list<netHandle>::iterator netItr;
  string s, m;
  for (netItr = signals.begin(); netItr != signals.end(); netItr++) {

    s = *netItr;
    m = "";
    size_t pos = s.find(':');
    if (pos != string::npos) {
      m = s.substr(0, pos);
      s = s.substr(pos+1);
      (*map)[s] = m;
    }
    //printf("Mapping %s to %s\n", s.c_str(), m.c_str());
    (*map)[s] = m;
  }
  */

  GenSceMi * g = new GenSceMi("/", 0, modname, newmodname, outdir, signals);
  tbgen->getSceMiClockName(sig);
  g->addClockSignal(sig);
  tbgen->getSceMiResetName(sig);
  g->addResetSignal(sig);
  s_modifications.push_back(g);
  toTclResult (interp, g->getUniqueId());

  //delete tbgen;
  
  return TCL_OK;
}

int CktEdits::genFinalSceMi (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[])
{
  BString cfgfilename, temp, pinfilename;
  BString errMsg;
  // Check path and module
  BString path, found;
  BString outdir;
  int stat;

  //printf("Start genFinalSceMi\n");
  
  if (TCL_OK != tc_extractFileName(interp, objv[2], cfgfilename) ){
    return TCL_ERROR;
  }

  if (TCL_OK != tc_extractSimpleName(objv[3], pinfilename) ){
    return TCL_ERROR;
  }

  if (TCL_OK != tc_extractSimpleName(objv[4], outdir) ){
    return TCL_ERROR;
  }
    
  TestBenchGenerator *tbgen = new TestBenchGenerator();
  stat = tbgen->readSpecFile(cfgfilename.c_str());
  if (!stat) {
    tbgen->getErrorMessage(errMsg);
    appendTclResultError (interp, errMsg);
    delete tbgen;
    return TCL_ERROR;
  }

  // Set control signals
  HdlUtils *hdlUtils = HdlUtils::init();
  if (hdlUtils->readPinFile(pinfilename.c_str()) == 0)
    return TCL_ERROR;

  netCollection signals;
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  std::map<string, string>::iterator signalItr;
  BString sig;
  for (signalItr = map->begin(); signalItr != map->end(); signalItr++) {
    sig = signalItr->first + ":" + signalItr->second;
    signals.push_back(sig);
  }
  tbgen->setSceMiControlSignals(signals);

  path = tbgen->verilogPath();
  tc_findModuleTcl(interp, NULL, path, found);

  BString modname, newmodname;

  modname = tbgen->moduleName();
  newmodname = tbgen->bsvModuleName();
  //outdir = tbgen->cppOutputDir();


  // If there is already a partition, then we have to delete it.
  if (s_got_partition == true)
    rmCktEdit(s_partition_uid);

  GenSceMi * g = new GenSceMi(found, 1, modname, newmodname, outdir, signals);
  tbgen->getDutClockName(sig);
  g->addClockSignal(sig);
  tbgen->getDutResetName(sig);
  g->addResetSignal(sig);
  s_modifications.push_back(g);
  toTclResult (interp, g->getUniqueId());

  std::map<std::string,std::string>::iterator outportItr;
  for (outportItr = tbgen->beginOutPortNames();
       outportItr != tbgen->endOutPortNames();
       outportItr++) {

    //printf("Add RmSimpleAssign %s\n", outportItr->first.c_str());
    g->addOutputPort(outportItr->first.c_str());
  }

  //printf("End genFinalSceMi %s %s %s\n", modname.c_str(), newmodname.c_str(), outdir.c_str());
  return TCL_OK;
}

int CktEdits::genTestbench (struct Tcl_Interp *interp, int objc, struct Tcl_Obj * objv[])
{
  BString filename, outdir, temp, pinfilename;
  BString errMsg;
  // Check path and module
  BString found;
  int stat;

  if (TCL_OK != tc_extractFileName(interp, objv[2], filename) ){
    return TCL_ERROR;
  }

  if (TCL_OK != tc_extractSimpleName(objv[3], pinfilename) ){
    return TCL_ERROR;
  }

  if (TCL_OK != tc_extractSimpleName(objv[4], outdir) ){
    return TCL_ERROR;
  }
    
  TestBenchGenerator *tbgen = new TestBenchGenerator();
  stat = tbgen->readSpecFile(filename.c_str());
  if (!stat) {
    tbgen->getErrorMessage(errMsg);
    appendTclResultError (interp, errMsg);
    delete tbgen;
    return TCL_ERROR;
  }
  netCollection signals;
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  std::map<string, string>::iterator signalItr;
  BString sig;
  for (signalItr = map->begin(); signalItr != map->end(); signalItr++) {
    sig = signalItr->first + ":" + signalItr->second;
    signals.push_back(sig);
  }
  tbgen->setSceMiControlSignals(signals);

  // If there is already a partition, then we have to delete it.
  if (s_got_partition == true)
    rmCktEdit(s_partition_uid);

  // Generate the testbench code
  stat = tbgen->generateTestBench(outdir.c_str());
  if (!stat) {
    tbgen->getErrorMessage(errMsg);
    appendTclResultError (interp, errMsg);
    delete tbgen;
    return TCL_ERROR;
  }

  return TCL_OK;
}
