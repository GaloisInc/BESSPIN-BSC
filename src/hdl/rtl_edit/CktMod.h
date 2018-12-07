// Copyright 2009 Bluespec Inc., All rights reserved
#pragma once



/*
  The CktMod Class defines a set of transformations on a particular
  instance of a module in a design hierarchy.
 */

#include "Types.h"
#include <iostream>
#include <algorithm>
#include <list>
#include <stdexcept>

#include "ScanPath.h"
#include "VeriModuleItem.h"

// Visitor Pattern for traversals of the CktMod class hierarchy
class CktModVisitor {
 protected:
  virtual int novisitor (class CktMod *) =0;
public:
  virtual int visit(class ChangeModuleName *cm) {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ChangeInstModuleName *cm) {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddPort *cm)          {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddInstanceConnection *cm) {return this->novisitor((CktMod *) cm);}
  virtual int visit(class RenameInstanceConnection *cm) {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddNet *cm)           {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddSimpleAssign *cm)  {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddInstance *cm)      {return this->novisitor((CktMod *) cm);}
  virtual int visit(class RmInstance *cm)       {return this->novisitor((CktMod *) cm);}
  virtual int visit(class RmBody *cm)           {return this->novisitor((CktMod *) cm);}
  virtual int visit(class RmReg *cm)            {return this->novisitor((CktMod *) cm);}
  virtual int visit(class RmNet *cm)            {return this->novisitor((CktMod *) cm);}
  virtual int visit(class RmSimpleAssign *cm)   {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ChangeAssignRHS *cm)  {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddProbe *cm)         {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddBsvProbes *cm)     {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddCapture *cm)       {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddTrigger *cm)       {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ProbeMux *cm)         {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ProbeBus *cm)         {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ProbedInstance *cm)   {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ProbeConnection *cm)  {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddCosim  *cm)        {return this->novisitor((CktMod *) cm);}
  virtual int visit(class AddScan  *cm)         {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ScannedInstance *cm)  {return this->novisitor((CktMod *) cm);}
  virtual int visit(class CosimFill *cm)        {return this->novisitor((CktMod *) cm);}
  virtual int visit(class DrawOutSignal *cm)    {return this->novisitor((CktMod *) cm);}
  virtual int visit(class DrawInSignal *cm)     {return this->novisitor((CktMod *) cm);}
  virtual int visit(class UpdateParam *cm)      {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ReplaceModule *cm)    {return this->novisitor((CktMod *) cm);}
  virtual int visit(class RenameSignal *cm)     {return this->novisitor((CktMod *) cm);}
  virtual int visit(class Partition *cm)        {return this->novisitor((CktMod *) cm);}
  virtual int visit(class GenSceMi *cm)        {return this->novisitor((CktMod *) cm);}
  virtual int visit(class ChangeFragmentFile *cm) {return this->novisitor((CktMod *) cm);}
  virtual int visit(class RmCode *cm)           {return this->novisitor((CktMod *) cm);}
  
};

// Virtual base class describing an CktMod to a instance
class CktMod {
private:
  static unsigned int s_nextkey;
  static unsigned int s_nextUniqueId;

  friend class CktEdits;

protected:
  // This is the instance which is to be changed
  instHandle   m_inst ;
  // Unique key  reset for each apply
  unsigned int m_key;
  // Unique Key used in GUI  do not change.
  unsigned int m_uniqueId;
  bool   m_userEdit;
  ModSet m_modset;

protected:
  CktMod(const instHandle &ih)
    : m_inst(ih)
    , m_key(++s_nextkey)
    , m_uniqueId(++s_nextUniqueId)
    , m_userEdit(true)
    , m_modset(ProbeMods)
  {}

  virtual ~CktMod ()
    {
    };
public:
  const BString & getInstName() const { return m_inst ; }
  void setInstName(const BString &in) { m_inst = in; }
  void   setKey(unsigned int k) { m_key = k; }
  unsigned int getKey() const {return m_key;}
  unsigned int getUniqueId() const {return m_uniqueId;}
  bool   isUserEdit() const { return m_userEdit;}
  void   setCompiled() { m_userEdit = false; }
  void   setModSet(ModSet value) { m_modset = value; }
  ModSet getModSet() { return m_modset; }
  void getSignalTypeName (SignalType sig_type, BString &name);

  // Visitor pattern
  virtual int accept(CktModVisitor *v) = 0;

  // Sort by the key
  static bool keyIsLessThan (const CktMod *l, const CktMod *r) {
    return l->getKey() <  r->getKey();
  }

  // sort by instance name  highest first.
  static bool instIsLessThan (const CktMod *l, const CktMod *r) {
    return l->getInstName() <  r->getInstName();
  }
  // deepest instance firstC
  static bool instIsGreaterThan (const CktMod *l, const CktMod *r) {
    return r->getInstName() <  l->getInstName();
  }

};

// Functor class for comparing CktMod pointer by instance name
class CompareInstName {
 public:
  bool operator() (const CktMod *l, const CktMod *r) {
    return CktMod::instIsGreaterThan(l,r);
  }
};

///////////////////////////////////////////////////////////////////////////
// Change the module name
class ChangeModuleName : public CktMod {
protected:
  BString m_newname;
  unsigned int m_range_key;
public:
  ChangeModuleName(const instHandle &ih, const BString &newname, const unsigned int &range_key)
    : CktMod(ih)
    , m_newname(newname)
    , m_range_key(range_key)
  {}
  const BString & getName() const {return m_newname;}
  void            setName(const BString value) { m_newname = value; }
  const unsigned int & rangeKey() const {return m_range_key;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};

///////////////////////////////////////////////////////////////////////////
// Change the module name for an instance
class ChangeInstModuleName : public CktMod {
protected:
  BString m_newname;
  BString m_subinstance;
  unsigned int m_suffix;
  bool         m_newinst;
public:
 ChangeInstModuleName(const instHandle &ih, const BString & subinst,  const BString &newname)
    : CktMod(ih)
    , m_newname(newname)
    , m_subinstance(subinst)
    , m_suffix(0)
    , m_newinst(false)
   {}
 ChangeInstModuleName(const instHandle &ih, const BString & subinst,  const BString &newname, unsigned int suffix)
    : CktMod(ih)
    , m_newname(newname)
    , m_subinstance(subinst)
    , m_suffix(suffix)
    , m_newinst(false)
   {}
 ChangeInstModuleName(const instHandle &ih, const BString & subinst,  
		      const BString &newname, unsigned int suffix, bool newinst)
    : CktMod(ih)
    , m_newname(newname)
    , m_subinstance(subinst)
    , m_suffix(suffix)
    , m_newinst(newinst)
   {}
  const BString & getName() const {return m_newname;}
  const BString & getContainingInst() const { return m_subinstance; }
  const unsigned int & getSuffix() const { return m_suffix; }
  const bool & newInst() const { return m_newinst; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};

///////////////////////////////////////////////////////////////////////////
class AddPort : public CktMod {
protected:
  BString m_portname;
  DirectionE m_dir;
  unsigned int m_width;
public:
  AddPort(const instHandle &ih, const BString &pname, DirectionE dir, unsigned int w)
    : CktMod(ih)
    , m_portname(pname)
    , m_dir(dir)
    , m_width(w)
  {}
  DirectionE getDirection () const { return m_dir ;}
  const char * getDirectionStr () const { return getStrFromDir (m_dir) ;}
  const BString & getPortName() const { return m_portname;}
  unsigned int getWidth() const { return m_width;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};

///////////////////////////////////////////////////////////////////////////
//  New connections to the subinst
class AddInstanceConnection : public CktMod {
protected:
  BString m_subinstance;        // The instance within this module
  BString m_portname;           // Name of port on the instance
  BString m_portExpr;           // This could be an expression....

public:
  AddInstanceConnection(const instHandle &ih,const BString &subinst, 
                        const BString &pname, const BString &portExpr)
    : CktMod(ih)
    , m_subinstance(subinst)
    , m_portname(pname)
    , m_portExpr(portExpr)
  {}
  const BString & getContainingInst() const { return m_subinstance; }
  const BString & getPortName() const { return m_portname;}
  const BString & getPortExpr() const { return m_portExpr;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};


///////////////////////////////////////////////////////////////////////////
//  Rename connection of the subinst
class RenameInstanceConnection : public CktMod {
protected:
  BString m_subinstance;        // The instance within this module
  BString m_portname;           // Name of port on the instance
  BString m_portExpr;           // This could be an expression....

public:
  RenameInstanceConnection(const instHandle &ih,const BString &subinst, 
                        const BString &pname, const BString &portExpr)
    : CktMod(ih)
    , m_subinstance(subinst)
    , m_portname(pname)
    , m_portExpr(portExpr)
  {}
  const BString & getContainingInst() const { return m_subinstance; }
  const BString & getPortName() const { return m_portname;}
  const BString & getPortExpr() const { return m_portExpr;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};


///////////////////////////////////////////////////////////////////////////
// Adds a net to the instance.
// do we need to spec wire vs reg?
class AddNet : public CktMod {
protected:
  BString m_netname;
  unsigned int m_width;
  bool m_conditional;
public:
  AddNet(const instHandle &ih, const BString &nname, unsigned int w, bool c=false)
    : CktMod(ih)
    , m_netname(nname)
    , m_width(w)
    , m_conditional(c)
  {}
  const BString & getName() const { return m_netname;}
  unsigned int getWidth() const { return m_width;}
  bool getConditional() const { return m_conditional;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};

///////////////////////////////////////////////////////////////////////////
// Add a simple assign.  lhs and rhs are simple nets
// NOT UESED YET!!!
class AddSimpleAssign : public CktMod {
protected:
  BString m_lhs;
  BString m_rhs;
public:
  AddSimpleAssign(const instHandle &ih, const BString &lhs, const BString &rhs)
    : CktMod(ih)
    , m_lhs(lhs)
    , m_rhs(rhs)
  {
    if (lhs.empty()) {
      throw std::runtime_error ("Assignment created with empty lhs");
    }
    if (rhs.empty()) {
      throw std::runtime_error ("Assignment created with empty rhs");
    }
  }
  BString &getLeftHandSide() { return m_lhs; }
  BString &getRightHandSide() { return m_rhs; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
  const BString & getLhs() const { return m_lhs; }
  const BString & getRhs() const { return m_rhs; }
};

///////////////////////////////////////////////////////////////////////////
// Add an instance within this instance
class AddInstance : public CktMod {
protected:
  BString m_iname;
  BString m_mname;
  ParameterList m_plist;
  ModuleTerminalList m_mtlist;
  unsigned int m_suffix;
  bool    m_one_port_per_line;  // Output 1 port per line

public:
  AddInstance(const instHandle &ih, const BString &iname, const BString &mname, ParameterList &plist, ModuleTerminalList &mtlist)
    : CktMod(ih)
    , m_iname(iname)
    , m_mname(mname)
    , m_plist(plist)
    , m_mtlist(mtlist)
    , m_suffix(0)
    , m_one_port_per_line(false)
  {}
  AddInstance(const instHandle &ih, const BString &iname, const BString &mname, ParameterList &plist, ModuleTerminalList &mtlist, unsigned int suffix)
    : CktMod(ih)
    , m_iname(iname)
    , m_mname(mname)
    , m_plist(plist)
    , m_mtlist(mtlist)
    , m_suffix(suffix)
  {}
  const BString & getLocalInstanceName() const { return m_iname; }
  const BString & getModuleName() { return m_mname; }
  ParameterList & getParamList() { return m_plist;}
  ModuleTerminalList & getPortList() { return m_mtlist;}
  void setOnePortPerLine(bool v) { m_one_port_per_line = v; }
  bool getOnePortPerLine() { return m_one_port_per_line; }
  const unsigned int & getSuffix() { return m_suffix; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
  int addPort( const BString &portName, const BString &netName, DirectionE dir, int width)
  {
    m_mtlist.push_back (ModuleTerminal(portName, netName, dir, width) );
    // TODO check that port is not already connected...
    return 0;                   // OK status
  }
};

///////////////////////////////////////////////////////////////////////////
// Remove an instance within this instance
class RmInstance : public CktMod {
protected:
  BString m_iname;
public:
  RmInstance(const instHandle &ih, const BString &iname)
    : CktMod(ih)
    , m_iname(iname)
  {}
  const BString & getLocalInstanceName() const { return m_iname; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Remove body of verilog
class RmBody : public CktMod {
protected:
  BString m_iname;
public:
  RmBody(const instHandle &ih)
    : CktMod(ih)
  {}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Remove an reg signal
class RmReg : public CktMod {
protected:
  BString m_regname;
public:
  RmReg(const instHandle &ih, const BString &regname)
    : CktMod(ih)
    , m_regname(regname)
  {}
  const BString & getRegName() const { return m_regname; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Remove a wire
class RmNet : public CktMod {
protected:
  BString m_netname;
public:
  RmNet(const instHandle &ih, const BString &netname)
    : CktMod(ih)
    , m_netname(netname)
  {}
  const BString & getNetName() const { return m_netname; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Remove an assignment statement
class RmSimpleAssign : public CktMod {
protected:
  BString m_netname;
public:
  RmSimpleAssign(const instHandle &ih, const BString &netname)
    : CktMod(ih)
    , m_netname(netname)
  {}
  const BString & getNetName() const { return m_netname; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Change signal name
class ChangeAssignRHS : public CktMod {
protected:
  BString m_lhs;
  BString m_rhs;
  BString m_new_rhs;
public:
  ChangeAssignRHS(const instHandle &ih, const BString &lhs,
		  const BString &rhs, const BString &new_rhs)
    : CktMod(ih)
    , m_lhs(lhs)
    , m_rhs(rhs)
    , m_new_rhs(new_rhs)
  {}
  const BString & getLHS() const { return m_lhs; }
  const BString & getRHS() const { return m_rhs; }
  const BString & getNewRHS() const { return m_new_rhs; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

class RmCode : public CktMod {
protected:
  Verific::VeriModuleItem* m_node;
public:
  RmCode(const instHandle &ih, Verific::VeriModuleItem* node)
    : CktMod(ih)
    , m_node(node)
  {}
  const Verific::VeriModuleItem* getItem() { return m_node; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

class ScanData {
private:
  static unsigned int s_nextid;
protected:
  AddedPortList    m_addedPorts;
  bool             m_scan_inst_found;
  AddSimpleAssign* m_last_assign    ; // the last scan assignment (we only add the last one)
  ScanFlavor       m_flavor;
  unsigned         m_probe; // probe number
  unsigned         m_chain; // chain number
  unsigned         m_width; // width of the scan chain
  unsigned         m_id;    // unique id of the scan chain
  ScanPath*        m_path;
public:
  ScanData (const ScanFlavor & flavor, const unsigned int probe, const unsigned int chain, const unsigned int width)
    : m_addedPorts()
    , m_scan_inst_found(false)
    , m_last_assign(0)
    , m_flavor(flavor)
    , m_probe(probe)
    , m_chain(chain)
    , m_width(width)
    , m_id(++s_nextid)
    , m_path(0)
    {}

  const AddedPortList & getAddedPorts() const { return m_addedPorts; }
  const BString    & genPortName (const BString & root, bool broadcast, unsigned int width);
  const unsigned   & getProbe()  { return m_probe; }
  const unsigned   & getChain()  { return m_chain; }
  const unsigned   & getWidth()  { return m_width; }
  const unsigned   & getId()     { return m_id; }
  void               setPath(ScanPath* path) { m_path = path; }
  ScanPath*          getPath()  { return m_path; }
  const unsigned     getLength();
  const ScanFlavor & getFlavor() const { return m_flavor; }
  const BString    & mkProbeId(const BString & root);
  const BString    & mkChainId(const BString & root);
  const BString    & mkId(const BString & root);
  bool isFirstScan();
  AddSimpleAssign* getLastAssign() const { return m_last_assign; }
  void  setLastAssign (AddSimpleAssign* assign) { if (m_last_assign) delete m_last_assign; m_last_assign = assign; }
  void  reset () {
    AddedPortList* empty = new AddedPortList();
    m_last_assign = 0;
    m_scan_inst_found = false;
    m_addedPorts = *empty;
  }

}; 


typedef std::list<ScanData*> ScanDataList;

///////////////////////////////////////////////////////////////////////////
// Abstract model of a probe
class AddProbe : public CktMod {
protected:
  BString m_name;
  netCollection m_patterns;
  netCollection m_nets;
  unsigned int  m_width;
  BString       m_clock;
  BString       m_enable;
  BString       m_bsvtype;
  IntList       m_widths;  // Where the probe is a bundle of signals, list of widths for subnets
  SignalType    m_sig_type;
public:
  AddProbe (const instHandle & ih, const BString &name, netCollection & patterns,
	    netCollection & nets,
	    const unsigned int width, const BString &clock, const BString &enable,
	    const BString &bsvtype, SignalType type)
    : CktMod(ih)
    , m_name(name)
    , m_patterns(patterns)
    , m_nets(nets)
    , m_width(width)
    , m_clock(clock)
    , m_enable(enable)
    , m_bsvtype(bsvtype)
    , m_sig_type(type)
  {}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
  const BString & getName() const { return m_name ; }
  void getPatterns (BStringList &sl) const {
    sl = m_patterns;
  }
  void getSignals (BStringList &sl) const {
    sl = m_nets;
  }
  void getSignalNames (BStringList &sl) const {
    sl.clear();
    transform (m_nets.begin(), m_nets.end(), back_inserter(sl),
               getSignalName);
  }
  void getWidths (BStringList &sl) const {
    sl.clear();
    transform (m_nets.begin(), m_nets.end(), back_inserter(sl),
               getWidthString);
  }
  unsigned int getWidth() const { return m_width; };
  const BString & getClock() const { return m_clock;}
  const BString & getEnable() const { return m_enable;}
  const BString & getBSVType() const { return m_bsvtype; }
  void setBSVType(const BString &type ) { m_bsvtype = type; }
  SignalType getSignalType () const {
    return m_sig_type;
  }
  void getSignalTypeName (BString &name) { CktMod::getSignalTypeName(m_sig_type, name); }
};

///////////////////////////////////////////////////////////////////////////
// Abstract model of a probe
class AddBsvProbes : public CktMod {
protected:
  BString    m_name;
  BString    m_hier_pattern;
  BString    m_pattern;
  BStringList m_paths;
  SignalType m_sig_type;
public:
  AddBsvProbes (const instHandle & ih, const BString &name, const BString &hier_pat,
		const BString &pat, BStringList &paths, SignalType type)
    : CktMod(ih)
    , m_name(name)
    , m_hier_pattern(hier_pat)
    , m_pattern(pat)
    , m_paths(paths)
    , m_sig_type(type)
  {}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
  const BString & getName() const { return m_name ; }
  const BString & getHierPattern() const { return m_hier_pattern ; }
  const BString & getPattern() const { return m_pattern ; }
  void getPathNames (BStringList &pl) const {
    pl = m_paths;
  }
  SignalType getSignalType () const {
    return m_sig_type;
  }
  void getSignalTypeName (BString &name) { CktMod::getSignalTypeName(m_sig_type, name); }
};

///////////////////////////////////////////////////////////////////////////
// Abstract model of a scan probe

class AddCosim : public CktMod {
protected:
  BString       m_name;
  unsigned int  m_width;
  BString       m_clock;
  BString       m_uclock;
  CosimFlavor   m_flavor;
  ScanDataList* m_datalist;
  BString       m_inst;
  BString       m_def;
  BString       m_trigger;
  
public:
  AddCosim (const instHandle & ih, const BString &name,
            const unsigned int width, const BString &clock, const BString &uclock, const BString &trigger, const CosimFlavor &flavor)
    : CktMod(ih)
    , m_name(name)
    , m_width(width)
    , m_clock(clock)
    , m_uclock(uclock)
    , m_flavor(flavor)
    , m_datalist(0)
    , m_inst("")
    , m_def("")
    , m_trigger(trigger)
    {
      ScanDataList* l = new ScanDataList();
      m_datalist = l;
    }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
  const BString & getName() const { return m_name ; }
  void  setInst (BString value) { m_inst = value; }
  const BString & getInst() const { return m_inst ; }
  void  setDef  (BString value) { m_def = value; }
  const BString & getDef() const { return m_def ; }
  unsigned int getWidth() const { return m_width; };
  const BString & getClock() const { return m_clock;}
  const BString & getUClock() const { return m_uclock;}
  const BString & getTrigger() const { return m_trigger;}
  const CosimFlavor & getFlavor() const { return m_flavor; }
  const BString       getFlavorName() const {return ToString(m_flavor);}
  ScanDataList* getData() { return m_datalist; } // One for each scan chain
};


///////////////////////////////////////////////////////////////////////////
// Abstract model of a scan sample reg
class AddScan : public CktMod {
protected:
  ScanData&     m_data;
  tIdList        m_input_ids;
  tIdList        m_output_ids;
  bool          m_top;

public:
  AddScan (const instHandle & ih, ScanData& data, const tIdList &input_ids, bool top)
    : CktMod(ih)
    , m_data(data)
    , m_input_ids(input_ids)
    , m_output_ids()
    , m_top(top)

  {}
 AddScan (const instHandle & ih, ScanData& data, const tIdList &input_ids, const tIdList &output_ids, bool top)
    : CktMod(ih)
    , m_data(data)
    , m_input_ids(input_ids)
    , m_output_ids(output_ids)
    , m_top(top)

  {}
  const tIdList   & getInputIds()   const { return m_input_ids; }
  const tIdList   & getOutputIds()  const { return m_output_ids; }
  const bool     & scanTop() const { return m_top; }
  ScanData & getData() { return m_data; }

  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Abstract model of a Capture probe
class AddCapture : public CktMod {
protected:
  BString m_name;
  netCollection m_patterns;
  netCollection m_nets;
  BString       m_enable;
  unsigned int  m_width;
  BString       m_clock;
  BString       m_trigger;
  unsigned int  m_depth;
  unsigned int  m_runwidth;
  unsigned int  m_dumpdelay;
  BString       m_bsvtype;
  SignalType m_sig_type;
public:
  AddCapture (const instHandle & ih, const BString &name,
	      netCollection & patterns, netCollection & nets,
              const BString & enable, const BString & trigger,
              const unsigned int width, const BString &clock,
              const unsigned int depth, const unsigned int runwidth,
              const unsigned int dumpdelay,
              const BString &bsvtype, SignalType type)
    : CktMod(ih)
    , m_name(name)
    , m_patterns(patterns)
    , m_nets(nets)
    , m_enable(enable)
    , m_width(width)
    , m_clock(clock)
    , m_trigger(trigger)
    , m_depth(depth)
    , m_runwidth(runwidth)
    , m_dumpdelay(dumpdelay)
    , m_bsvtype(bsvtype)
    , m_sig_type(type)
  {}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
  const BString & getName() const { return m_name ; }
  void getPatterns (BStringList &sl) const {
    sl = m_patterns;
  }
  void getSignals (BStringList &sl) const {
    sl = m_nets;
  }
  void getSignalNames (BStringList &sl) const {
    sl.clear();
    transform (m_nets.begin(), m_nets.end(), back_inserter(sl),
               getSignalName);
  }
  const BString & getEnable() const { return m_enable;}
  unsigned int getWidth() const { return m_width; }
  const BString & getClock() const { return m_clock;}
  const BString & getBSVType() const { return m_bsvtype; }
  unsigned int getDepth() const { return m_depth; }
  unsigned int getRunWidth() const { return m_runwidth; }
  unsigned int getDumpDelay() const { return m_dumpdelay; }
  const BString & getTrigger() const { return m_trigger; }

  void setBSVType(const BString &type ) { m_bsvtype = type; }
  SignalType getSignalType () const {
    return m_sig_type;
  }
  void getSignalTypeName (BString &name) { CktMod::getSignalTypeName(m_sig_type, name); }
};

///////////////////////////////////////////////////////////////////////////
// Abstract model of a Trigger for a capture probe
class AddTrigger : public CktMod {
protected:
  BString   m_name;
  BString   m_expr;
  BString   m_clock;
  BStringList m_captures;       // One trigger fires many captures
  UIntSet   m_captureKeys;
public:
  AddTrigger (const instHandle & ih, const BString &name, const BString & expr,
              const BString &clock, const BStringList &caps)
    : CktMod(ih)
    , m_name(name)
    , m_expr(expr)
    , m_clock(clock)
    , m_captures(caps)
    , m_captureKeys()
  {}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
  const BString & getName() const { return m_name ; }
  const BString & getExpr() const { return m_expr ; }
  const BString & getClock() const { return m_clock;}
  const BStringList & getCaptures () const { return m_captures;}
  const UIntSet & getCaptureKeys () const { return m_captureKeys ;}

  void clearKeys() { m_captureKeys.clear() ;}
  void addKey(unsigned int x) { m_captureKeys.insert(x) ; }
};

///////////////////////////////////////////////////////////////////////////
// Abstract class representing a module which muxes 2 probe buses into 1
class ProbeMux : public CktMod {
protected:
  unsigned int m_src1;          // Id of 1 source
  unsigned int m_src2;          // Id of 1 source2
public:
  ProbeMux (const instHandle & ih, unsigned int src1, unsigned int src2)
    : CktMod(ih)
    , m_src1(src1)
    , m_src2(src2)
  {}
  unsigned int getSrc1() const { return m_src1; }
  unsigned int getSrc2() const { return m_src2; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }

};

///////////////////////////////////////////////////////////////////////////
// A refernce to an instance that has a probe bus sticking out
class ProbedInstance : public CktMod {
protected:
  BString m_subinstance;
  AddedPortList m_addedPorts;

public:
  ProbedInstance (const instHandle & ih, const BString & subinst, const AddedPortList & newPorts)
    : CktMod(ih)
    , m_subinstance(subinst)
    , m_addedPorts(newPorts)
  {}
  const BString & getContainingInst() const { return m_subinstance; }
  const AddedPortList & getAddedPorts() const { return m_addedPorts; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// 
class ScannedInstance : public CktMod {
protected:
  BString       m_subinstance;
  ScanData&     m_data;

public:
  ScannedInstance (const instHandle & ih,  ScanData & data, const BString & subinst)
    : CktMod(ih)
    , m_subinstance(subinst)
    , m_data(data)
  {}
  const BString & getContainingInst() const { return m_subinstance; }
  const AddedPortList & getAddedPorts() const { return m_data.getAddedPorts(); }
  ScanData & getData() { return m_data; }

  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Connector from a probemodule, probemux of probeinstance to the bus
class ProbeBus : public CktMod {
protected:
  unsigned int m_src;           // Either: probe, Probemux, or probeinst
public:
  ProbeBus (const instHandle & ih, unsigned int src)
    : CktMod(ih)
    , m_src(src)
  {}
  unsigned int getSrc() const { return m_src; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Connector from a probemodule, probemux of probeinstance to a instance of probe xactor
class ProbeConnection : public CktMod {
protected:
  unsigned int m_src;           // Either: probe, Probemux, or probeinst
public:
  ProbeConnection (const instHandle & ih, unsigned int src)
    : CktMod(ih)
    , m_src(src)
  {}
  unsigned int getSrc() const { return m_src; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

//////////////////////////////////////////////////////////////////
// Connects signal from downpath to toppath using port as the port.
// The instHandle is used as downpath
class DrawOutSignal : public CktMod {
protected:
  BString m_topPath;
  BString m_signal;
  BString m_port;
  unsigned m_width;
public:
  DrawOutSignal (const BString & src, const BString &top, const BString &sig, const BString &port, unsigned width)
    : CktMod(src)
    , m_topPath(top)
    , m_signal(sig)
    , m_port(port)
    , m_width(width)
  {}
  const BString & getDownPath() const {return m_inst;}
  const BString & getTopPath() const {return m_topPath;}
  const BString & getPortName () const {return m_port;}
  const BString & getSignal () const { return m_signal;}
  unsigned getWidth() const { return m_width;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

//////////////////////////////////////////////////////////////////
// Connects signal from srcpath to destpath using port as the port.
// The instHandle is used as dst path
// TODO combine with above class....
class DrawInSignal : public CktMod {
protected:
  BString m_topPath;
  BString m_signal;
  BString m_port;
  unsigned m_width;
public:
  DrawInSignal  (const BString & src, const BString &top, const BString &sig, const BString &port, unsigned width)
    : CktMod(src)
    , m_topPath(top)
    , m_signal(sig)
    , m_port(port)
    , m_width(width)
  {}
  const BString & getDownPath() const {return m_inst;}
  const BString & getTopPath() const {return m_topPath;}
  const BString & getPortName () const {return m_port;}
  const BString & getSignal () const { return m_signal;}
  unsigned getWidth() const { return m_width;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Change a Parameter value on this module
class UpdateParam : public CktMod {protected:
  BString m_param;
  BString m_value;              // Image() call from Verific
public:
  UpdateParam (const instHandle & ih, const BString &param, const BString & value)
    : CktMod(ih)
    , m_param(param)
    , m_value(value)
  {}
  const BString & getName() const {return m_param;}
  const BString & getValue() const { return m_value; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

class ReplaceModule : public CktMod {protected:
  BString m_newname;
public:
  ReplaceModule (const instHandle & ih, const BString &name)
    : CktMod(ih)
    , m_newname(name)
  {}
  const BString & getName() const {return m_newname;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

class CosimFill : public CktMod {protected:
public:
  CosimFill (const instHandle & ih)
    : CktMod(ih)
  {}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};


class RenameSignal : public CktMod {
protected:
  BString m_name;
  BString m_name_new;
  bool    m_include_dcls;
public:
  RenameSignal(const instHandle &ih, const BString &name, const BString &name_new, const bool & include_dcls = true)
    : CktMod(ih)
    , m_name(name)
    , m_name_new(name_new)
    , m_include_dcls(include_dcls)
  {}
  const BString & getName() const { return m_name; }
  const BString & getNameNew() const { return m_name_new; }
  const bool    & includeDecls() const { return m_include_dcls; }
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};

class PartitionSpec;

typedef std::map<std::string, std::string> FixedTerminalTable;
typedef std::map<std::string, std::string> SingleEndedTerminals;

// Partion this instance to another fpga
class Partition : public CktMod {
protected:
  BString m_partspec;
  BString m_boardspec;
  BString m_modspec;
  PartitionSpec *m_partition_spec;
public:
  //
  Partition (const BString &boardspec, const BString &modspec);
  const char *getBoardSpecFile() const { return m_boardspec.c_str();}
  const char *getModuleSpecFile() const { return m_modspec.c_str();}
  PartitionSpec *getPartitionSpec() { return m_partition_spec; }

  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

// Generate scemi and hook pure verilog dut to it
class GenSceMi : public CktMod {
protected:
  int m_phase; // 0-init, 1-final
  BString m_module;
  BString m_new_mod_name;
  BString m_output_dir;
  netCollection m_clk_signals;
  netCollection m_rst_signals;
  netCollection m_clk_rst_signals;
  netCollection m_output_ports;
public:
  GenSceMi (const instHandle & ih, int phase, const BString &module, const BString &new_mod_name,
	    const BString &dir, netCollection &clk_rst_sigs);

  int getPhase() const { return m_phase; }
  const char *getModuleName() const { return m_module.c_str();}
  const char *getNewModuleName() const { return m_new_mod_name.c_str();}
  const char *getOutputDir() const { return m_output_dir.c_str();}
  void getClkRstSignals (BStringList &sl) const {
    sl = m_clk_rst_signals;
  }
  void addClockSignal (BString &sl) {
    m_clk_signals.push_back(sl);
  }
  void addResetSignal (BString &sl) {
    m_rst_signals.push_back(sl);
  }
  void getClockSignals(netCollection &sl) {
    sl = m_clk_signals;
  }
  void getResetSignals(netCollection &sl) {
    sl = m_rst_signals;
  }
  BStringListIterator clockSignalBegin() {
    return m_clk_signals.begin();
  }
  BStringListIterator clockSignalEnd() {
    return m_clk_signals.begin();
  }
  BStringListIterator resetSignalBegin() {
    return m_rst_signals.begin();
  }
  BStringListIterator resetSignalEnd() {
    return m_rst_signals.begin();
  }
  
  void addOutputPort(const char *portname) { m_output_ports.push_back(portname); }
  void getOutputPorts (BStringList &sl) const {
    sl = m_output_ports;
  }
  

  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  }
};

///////////////////////////////////////////////////////////////////////////
// Change fragment file
class ChangeFragmentFile : public CktMod {
protected:
  BString m_input_dir;
  BString m_oldname;
  BString m_newname;
public:
 ChangeFragmentFile(const instHandle &ih, const BString &indir,
		    const BString &oldname, const BString &newname)
    : CktMod(ih)
    , m_input_dir(indir)
    , m_oldname(oldname)
    , m_newname(newname)
  {}
  const BString & getInputDir() const {return m_input_dir;}
  const BString & getOldName() const {return m_oldname;}
  const BString & getNewName() const {return m_newname;}
  virtual int accept(CktModVisitor *v) {
    return v->visit(this);
  };
};


