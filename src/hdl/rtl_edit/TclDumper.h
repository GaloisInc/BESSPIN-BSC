// Copyright 2010 Bluespec Inc. All rights reserved
#pragma once

#include <string.h>             /* strlen*/
#include "tcl.h"
#include "CktMod.h"

// Visitor Implementation for CktMod classes to dump to tcl
class TclDumperVisitor : public CktModVisitor {
protected:
  Tcl_Interp * m_interp;
  Tcl_Obj    * m_newObj;
public:
  TclDumperVisitor (Tcl_Interp *interp)
    : m_interp(interp)
    , m_newObj(0)
  {}
  Tcl_Obj *getElement () { return m_newObj;}
protected:
  // Utility functions
  void newList () {
    m_newObj = Tcl_NewListObj (0,NULL);
  }
  void newString ( const BString &str) {
    m_newObj = Tcl_NewStringObj(str.c_str(), -1);
  }

  void addListElem (Tcl_Obj *o) {
    Tcl_ListObjAppendElement (m_interp,m_newObj, o);
  }
  void addListElem (const BString &s) {
    addListElem (toTclObj(m_interp, s));
  }
  void addListElem (const BStringList &s) {
    addListElem (toTclObj(m_interp, s));
  }
  void addListElem (const char *s) {
    addListElem (toTclObj(m_interp, s));
  }
  void addListElem (const int i) {
    addListElem (toTclObj(m_interp, i));
  }
  void addListElem (const ModuleTerminalList &termlist) {
    addListElem (toTclObj(m_interp, termlist));
  }
  void addListElem (const ParameterList &paramlist) {
    addListElem (toTclObj(m_interp, paramlist));
  }
  void addListElem (const AddedPortList &portlist) {
    addListElem (toTclObj(m_interp, portlist));
  }

  void joinString (const BString &s) {
    joinString (s.c_str());
  }
  void joinString (const char *s) {
    if (0==strlen(s)) {
      Tcl_AppendStringsToObj(m_newObj, " {}", NULL);
    }
    else {
      Tcl_AppendStringsToObj(m_newObj, " " , s, NULL);
    }
  }
  void joinString (const BStringList &l) {
    Tcl_Obj *sl = toTclObj (m_interp, l);
    joinString (Tcl_GetString (sl));
  }
  void doCommon (CktMod *cm, const char * typestr) {
    newList();
    addListElem ("Key");      addListElem (cm->getKey());
    addListElem ("UniqueId"); addListElem (cm->getUniqueId());
    addListElem ("Path");     addListElem (cm->getInstName());
    addListElem ("Type");     addListElem (typestr);
  }
public:
  int novisitor (CktMod *cm) {
    fprintf (stderr, "TclDumperVisitor for %s %u, %u is not defined!\n", cm->getInstName().c_str(), cm->getKey(), cm->getUniqueId() );
    return 0;
  }
  int visit (ChangeModuleName *cm) {
    doCommon (cm, "ChangeModuleName");
    addListElem ("Name");     addListElem (cm->getName());
    return 1;
  }
  int visit (ChangeInstModuleName *cm) {
    doCommon (cm, "ChangeInstModuleName");
    addListElem ("Name");     addListElem (cm->getName());
    addListElem ("SubInst") ; addListElem ( cm->getContainingInst() );
    return 1;
  }
  int visit (AddProbe *cm) {
    doCommon (cm, "SimpleProbe");
    addListElem ("Name");     addListElem (cm->getName());
    addListElem ("Width");    addListElem (cm->getWidth());
    addListElem ("Clock");    addListElem (cm->getClock());
    addListElem ("Enable");   addListElem (cm->getEnable());
    addListElem ("BSVType");  addListElem (cm->getBSVType());

    addListElem ("Patterns");
    BStringList patNames;
    cm->getPatterns(patNames);
    Tcl_Obj *sl = toTclObj(m_interp,patNames);
    addListElem (sl);

    addListElem ("Signals");
    BStringList sigNames;
    cm->getSignalNames(sigNames);
    sl = toTclObj(m_interp,sigNames);
    addListElem (sl);

    BString typeNames;
    cm->getSignalTypeName(typeNames);
    addListElem ("SignalType"); addListElem (typeNames.c_str());

    return 1;
  }
  int visit (AddBsvProbes *cm) {
    doCommon (cm, "BSVProbe");
    addListElem ("Name");     addListElem (cm->getName());
    addListElem ("HierPattern"); addListElem (cm->getHierPattern());
    addListElem ("Pattern"); addListElem (cm->getPattern());
    BString typeNames;
    cm->getSignalTypeName(typeNames);
    addListElem ("SignalType"); addListElem (typeNames.c_str());

    return 1;
  }
  int visit (AddCosim *cm) {
    doCommon (cm, "CosimProbe");
    addListElem ("Name");     addListElem (cm->getName());
    addListElem ("Width");    addListElem (cm->getWidth());
    addListElem ("Clock");    addListElem (cm->getClock());
    addListElem ("UClock");   addListElem (cm->getUClock());
    addListElem ("Trigger");  addListElem (cm->getTrigger());
    addListElem ("Flavor");   addListElem (cm->getFlavorName());

    return 1;
  }
  int visit (AddScan *cm) {
    doCommon (cm, "Scan");

    return 1;
  }
  int visit (CosimFill *cm) {
    doCommon (cm, "CosimFill");

    return 1;
  }
  int visit (ChangeFragmentFile *cm) {
    doCommon (cm, "ChangeFragmentFile");
    addListElem ("InputDir");    addListElem (cm->getInputDir());
    addListElem ("OldName");     addListElem (cm->getOldName());
    addListElem ("NewNamer");    addListElem (cm->getNewName());
    return 1;
  }
  int visit (AddCapture *cm) {
    doCommon (cm, "CaptureProbe");
    addListElem ("Name");     addListElem (cm->getName());
    addListElem ("Width");    addListElem (cm->getWidth());
    addListElem ("Clock");    addListElem (cm->getClock());
    addListElem ("Enable");   addListElem (cm->getEnable());
    addListElem ("Trigger");  addListElem (cm->getTrigger());
    addListElem ("BSVType");  addListElem (cm->getBSVType());

    addListElem ("Depth");     addListElem (cm->getDepth());
    addListElem ("DumpDelay");  addListElem (cm->getDumpDelay());
    addListElem ("RunWidth");  addListElem (cm->getRunWidth());

    addListElem ("Patterns");
    BStringList patNames;
    cm->getPatterns(patNames);
    Tcl_Obj *sl = toTclObj(m_interp,patNames);
    addListElem (sl);

    addListElem ("Signals");
    BStringList sigNames;
    cm->getSignalNames(sigNames);
    sl = toTclObj(m_interp,sigNames);
    addListElem (sl);

    BString typeNames;
    cm->getSignalTypeName(typeNames);
    addListElem ("SignalType"); addListElem (typeNames.c_str());

    return 1;
  }
  int visit (AddTrigger *cm) {
    doCommon (cm, "ProbeTrigger");
    addListElem ("Name");     addListElem (cm->getName());
    addListElem ("Clock");    addListElem (cm->getClock());
    addListElem ("Expr");     addListElem (cm->getExpr());
    addListElem ("Captures"); addListElem (cm->getCaptures());
    // addListElem ("CaptureKeys");  addListElem (cm->getCaptureKeys());
    return 1;
  }
  int visit (AddInstance *cm) {
    doCommon (cm, "AddInstance");
    addListElem ("LocalInst");  addListElem ( cm->getLocalInstanceName() );
    addListElem ("Module");     addListElem (cm->getModuleName() );
    addListElem ("Param");      addListElem (cm->getParamList() );
    addListElem ("Ports");      addListElem (cm->getPortList() );
    return 1;
  }
  int visit (RmInstance *cm) {
    doCommon (cm, "RmInstance");
    addListElem ("LocalInst");  addListElem (cm->getLocalInstanceName() );
    return 1;
  }
  int visit (RmBody *cm) {
    doCommon (cm, "RmBody");
    return 1;
  }
  int visit (RmReg *cm) {
    doCommon (cm, "RmReg");
    addListElem ("RegName");  addListElem (cm->getRegName() );
    return 1;
  }
  int visit (RmNet *cm) {
    doCommon (cm, "RmNet");
    addListElem ("NetName");  addListElem (cm->getNetName() );
    return 1;
  }
  int visit (RmSimpleAssign *cm) {
    doCommon (cm, "RmSimpleAssign");
    addListElem ("NetName");  addListElem (cm->getNetName() );
    return 1;
  }
  int visit (ChangeAssignRHS *cm) {
    doCommon (cm, "ChangeAssignRHS");
    addListElem ("LHS");  addListElem (cm->getLHS() );
    addListElem ("RHS");  addListElem (cm->getRHS() );
    addListElem ("NewRHS");  addListElem (cm->getNewRHS() );
    return 1;
  }
  int visit (ProbeMux *cm) {
    doCommon (cm, "ProbeMux");
    addListElem ("Src1") ;     addListElem ( cm->getSrc1() );
    addListElem ("Src2");      addListElem (cm->getSrc2() );
    return 1;
  }
  int visit (ProbeBus *cm) {
    doCommon (cm, "ProbeBus");
    addListElem ("Src") ;    addListElem ( cm->getSrc() );
    return 1;
  }
  int visit (ProbeConnection *cm) {
    doCommon (cm, "ProbeXConnection");
    addListElem ("Src") ;    addListElem ( cm->getSrc() );
    return 1;
  }
  int visit (ProbedInstance *cm) {
    doCommon (cm, "ProbedInstance");
    addListElem ("SubInst")  ; addListElem ( cm->getContainingInst() );
    addListElem ("NewPorts") ; addListElem ( cm->getAddedPorts() );
    return 1;
  }
  int visit (ScannedInstance *cm) {
    doCommon (cm, "ScannedInstance");
    addListElem ("SubInst")  ; addListElem ( cm->getContainingInst() );
    addListElem ("NewPorts") ; addListElem ( cm->getAddedPorts() );
    return 1;
  }
  int visit (AddNet *cm) {
    doCommon (cm, "AddNet");
    addListElem ("Name") ;    addListElem ( cm->getName() );
    addListElem ("Width") ;   addListElem ( cm->getWidth() );
    return 1;
  }
  int visit (AddPort *cm) {
    doCommon (cm, "AddPort");
    addListElem ("Port") ;    addListElem ( cm->getPortName() );
    addListElem ("Width") ;   addListElem ( cm->getWidth() );
    addListElem ("Dir") ;     addListElem ( cm->getDirectionStr() );
    return 1;
  }
  int visit (UpdateParam *cm) {
    doCommon (cm, "UpdateParam");
    addListElem ("Name") ;    addListElem ( cm->getName() );
    addListElem ("Value") ;   addListElem ( cm->getValue() );
    return 1;
  }
  int visit (AddInstanceConnection *cm) {
    doCommon (cm, "AddInstanceConnection");
    addListElem ("SubInst");  addListElem ( cm->getContainingInst() );
    addListElem ("Port") ;    addListElem ( cm->getPortName() );
    addListElem ("Expr");     addListElem ( cm->getPortExpr() );
    return 1;
  }
  int visit (RenameInstanceConnection *cm) {
    doCommon (cm, "RenameInstanceConnection");
    addListElem ("SubInst");  addListElem ( cm->getContainingInst() );
    addListElem ("Port") ;    addListElem ( cm->getPortName() );
    addListElem ("Expr");     addListElem ( cm->getPortExpr() );
    return 1;
  }
  int visit (RmCode *cm) {
    doCommon (cm, "RmCode");
    //    addListElem ("LHS");      addListElem ( cm->getLHS() );
    return 1;
  }
  int visit (AddSimpleAssign *cm) {
    doCommon (cm, "AddAssign");
    addListElem ("Lhs");  addListElem ( cm->getLhs() );
    addListElem ("Rhs");  addListElem ( cm->getRhs() );
    return 1;
  }
  int visit (DrawInSignal *cm) {
    doCommon (cm, "DrawIn");
    addListElem ("TopPath");   addListElem ( cm->getTopPath() );
    addListElem ("DownPath");  addListElem ( cm->getDownPath() );
    addListElem ("Signal");    addListElem ( cm->getSignal() );
    addListElem ("Port");      addListElem ( cm->getPortName() );
    addListElem ("Width") ;   addListElem ( cm->getWidth() );
    return 1;
  }
  int visit (DrawOutSignal *cm) {
    doCommon (cm, "DrawOut");
    addListElem ("TopPath");   addListElem ( cm->getTopPath() );
    addListElem ("DownPath");  addListElem ( cm->getDownPath() );
    addListElem ("Signal");    addListElem ( cm->getSignal() );
    addListElem ("Port");      addListElem ( cm->getPortName() );
    addListElem ("Width") ;   addListElem ( cm->getWidth() );
    return 1;
  }
 int visit (ReplaceModule *cm) {
    doCommon (cm, "ReplaceModule");
    addListElem ("Name") ;    addListElem ( cm->getName() );
    return 1;
  }
  int visit (Partition *cm) {
    doCommon (cm, "Partition");
    addListElem ("BoardSpec"); addListElem (cm->getBoardSpecFile());
    addListElem ("ModuleSpec"); addListElem (cm->getModuleSpecFile());
    return 1;
  }
  int visit (GenSceMi *cm) {
    doCommon (cm, "GenSceMi");
    addListElem ("Module"); addListElem (cm->getModuleName());
    addListElem ("NewModuleName"); addListElem (cm->getNewModuleName());

    addListElem ("ClockNames");
    BStringList sigNames;
    cm->getClockSignals(sigNames);
    Tcl_Obj *sl = toTclObj(m_interp,sigNames);
    addListElem (sl);

    addListElem ("ResetNames");
    cm->getResetSignals(sigNames);
    sl = toTclObj(m_interp,sigNames);
    addListElem (sl);

    return 1;
  }
};

// Class to dump a CktMod as a command string for replay
class TclReplayVisitor : public TclDumperVisitor {
 public:
  TclReplayVisitor (Tcl_Interp *interp)
    :TclDumperVisitor(interp) {}

  void addCommand (const char * cmd) {
    newList();
    addListElem (cmd);
  }
  int novisitor (CktMod *cm) {
    fprintf (stderr, "TclReplayVisitor for %s %u, %u is not defined!\n", cm->getInstName().c_str(), cm->getKey(), cm->getUniqueId());
    return 0;
  }
  // addprobe <name> <inst> <pattern>
  int visit (AddProbe *cm) {
    addCommand ("addprobe");
    addListElem (cm->getName() );
    addListElem (cm->getInstName() );
    BStringList patNames;
    cm->getPatterns(patNames);
    addListElem (patNames);
    //BStringList sigNames;
    //cm->getSignalNames(sigNames);
    //addListElem (sigNames);
    addListElem (cm->getClock() );
    addListElem (cm->getEnable() );
    addListElem (cm->getBSVType() );
    BString typeNames;
    cm->getSignalTypeName(typeNames);
    addListElem (typeNames.c_str());
    return 1;
  }
  // addbsvprobes <name> <inst> <pattern>
  int visit (AddBsvProbes *cm) {
    addCommand ("addbsvprobes");
    addListElem (cm->getName() );
    //addListElem (cm->getInstName() );
    addListElem (cm->getHierPattern() );
    addListElem (cm->getPattern() );
    BString typeNames;
    cm->getSignalTypeName(typeNames);
    addListElem (typeNames.c_str());
    return 1;
  }
  int visit (AddCosim *cm) {
    addCommand ("addcosim");
    addListElem (cm->getName() );
    addListElem (cm->getInstName() );
    addListElem (cm->getClock() );
    addListElem (cm->getUClock() );
    addListElem (cm->getTrigger() );
    addListElem (cm->getWidth() );
    addListElem (cm->getFlavorName() );
    return 1;
  }
  int visit (AddCapture *cm) {
    addCommand ("addcapture");
    addListElem (cm->getName() );
    addListElem (cm->getInstName() );
    BStringList patNames;
    cm->getPatterns(patNames);
    addListElem (patNames);
    //BStringList sigNames;
    //cm->getSignalNames(sigNames);
    //addListElem (sigNames);
    addListElem (cm->getClock() );
    addListElem (cm->getEnable());
    addListElem (cm->getTrigger());
    addListElem (cm->getDepth() );
    addListElem (cm->getRunWidth() );
    addListElem (cm->getDumpDelay());
    addListElem (cm->getBSVType() );
    BString typeNames;
    cm->getSignalTypeName(typeNames);
    addListElem (typeNames.c_str());
    return 1;
  }
  int visit (AddTrigger *cm) {
    addCommand ("addtrigger");
    addListElem (cm->getName() );
    addListElem (cm->getInstName() );
    addListElem (cm->getExpr() );
    addListElem (cm->getClock() );
    addListElem (cm->getCaptures());
    return 1;
  }
  int visit (DrawOutSignal *cm) {
    addCommand ("drawout");
    addListElem (cm->getDownPath());
    addListElem (cm->getTopPath());
    addListElem (cm->getSignal());
    addListElem (cm->getPortName());
    return 1;
  }
  int visit (DrawInSignal *cm) {
    addCommand ("drawin");
    addListElem (cm->getDownPath());
    addListElem (cm->getTopPath());
    addListElem (cm->getSignal());
    addListElem (cm->getPortName());
    return 1;
  }
  int visit (Partition *cm) {
    addCommand ("partition");
    addListElem(cm->getBoardSpecFile());
    addListElem(cm->getModuleSpecFile());
    return 1;
  }
  int visit (GenSceMi *cm) {
    addCommand ("genSceMi");
    addListElem(cm->getModuleName());
    addListElem(cm->getNewModuleName());
    BStringList sigNames;
    cm->getClockSignals(sigNames);
    addListElem (sigNames);
    cm->getResetSignals(sigNames);
    addListElem (sigNames);
    return 1;
  }

};
