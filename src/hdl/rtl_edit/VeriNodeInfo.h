// Copyright 2011 Bluespec Inc. All rights reserved
#pragma once

#include <string>
#include <list>
#include <map>
#include "Set.h"
#include "string.h"
#include "VeriVisitor.h"
#include "veri_file.h"
#include "VeriModule.h"
#include "VeriMisc.h"
#include "VeriId.h"
#include "Array.h"
#include "Types.h"
#include "VeriExpression.h"
#include "VeriStatement.h"
#include "VeriMisc.h"

using namespace std;

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif

class VeriNodeInfo;

typedef std::map<std::string, VeriNodeInfo*> VeriNodeInfoMap;
typedef std::map<std::string, VeriNodeInfo*>::iterator VeriNodeInfoIterator;
typedef std::list<VeriNodeInfo*> VeriNodeInfoList;
typedef std::list<VeriNodeInfo*>::iterator VeriNodeInfoListIterator;


class NodeInfoVisitor : public VeriVisitor
{
 public:
  NodeInfoVisitor();
  virtual ~NodeInfoVisitor();
  
  // The following Visit methods need to be redefined for our purpose :
  
  // The following class definitions can be found in VeriModuleItem.h
  virtual void Visit(VeriModuleInstantiation &node);
  virtual void Visit(VeriAlwaysConstruct &node);
  virtual void Visit(VeriContinuousAssign &node);
  
  // Accessor methods
  Set* GetAssignStmts()          { return &_assign_stmts ; }
  Set* GetAlwaysConstructs()     { return &_always_constructs ; }
  Set* GetModuleInsts()          { return &_module_insts ; }
  
 private:
  Set _assign_stmts ;      // Container (Set) of VeriAssign*'s
  Set _always_constructs ; // Container (Set) of VeriAlwaysConstruct*'s
  Set _module_insts ;      // Container (Set) of VeriModuleInstantiation*'s
  
  // Prevent the compiler from implementing the following
  NodeInfoVisitor(NodeInfoVisitor &node);
  NodeInfoVisitor& operator=(const NodeInfoVisitor &rhs);
};

class AssignmentVisitor : public VeriVisitor
{
 public :
 AssignmentVisitor() : VeriVisitor(), _assigned_ids(POINTER_HASH) { } ;
  ~AssignmentVisitor() {} ;
  
  // Collect blocking assignments :
  virtual void Visit(VeriBlockingAssign &node) {
    // Go to the assigned target (expression) :
    VeriExpression *target = node.GetLVal() ;
    // Find the assigned identifier(s) :
    VeriIdDef *target_id = (target) ? target->GetId() : 0 ;
    if (target_id) _assigned_ids.Insert(target_id) ;
  }
  // Collect non-blocking assignments :
  virtual void Visit(VeriNonBlockingAssign &node) {
    // Go to the assigned target (expression) :
    VeriExpression *target = node.GetLVal() ;
    // Find the assigned identifier(s) :
    VeriIdDef *target_id = (target) ? target->GetId() : 0 ;
    if (target_id) _assigned_ids.Insert(target_id) ;
  }
  
  // Accessor method
  Set* GetAssignIds()          { return &_assigned_ids ; }
  
 private :
  Set _assigned_ids ; // Set of VeriIdDef *'s (to avoid overlap in targets), collected by this visitor
} ;


class AssignVisitor : public VeriVisitor
{
 public :
 AssignVisitor() : VeriVisitor(), _blocking_assigns(POINTER_HASH),
    _nonblocking_assigns(POINTER_HASH) { } ;

  ~AssignVisitor() {} ;
  
  // Collect blocking assignments :
  virtual void Visit(VeriBlockingAssign &node) {
    _blocking_assigns.Insert(&node) ;
  }
  
  // Collect blocking assignments :
  virtual void Visit(VeriNonBlockingAssign &node) {
    _nonblocking_assigns.Insert(&node) ;
  }
  
  // Accessor method
  Set* GetBlockingAssigns() { return &_blocking_assigns ; }
  Set* GetNonBlockingAssigns() { return &_nonblocking_assigns ; }
  
 private :
  Set _blocking_assigns ;
  Set _nonblocking_assigns ;
} ;


class SignalVisitor : public VeriVisitor
{
 public:
 SignalVisitor() : _signals(2) { }
  virtual ~SignalVisitor() { }
  
 private:
  // Prevent compiler from defining the following
  SignalVisitor(const SignalVisitor &) ;            // Purposely leave unimplemented
  SignalVisitor& operator=(const SignalVisitor &) ; // Purposely leave unimplemented
  
 public:
  void         PrintInfo() ;
  void         ResetSignals() { _signals.Reset() ; }
  unsigned     NumSignals() { return _signals.Size(); }
  
  // Accessor methods
  Array* GetSignals()          { return &_signals ; }
  
 public:
  virtual void VERI_VISIT(VeriIdRef, node);
  virtual void VERI_VISIT(VeriIndexedId, node);
  virtual void VERI_VISIT(VeriSelectedName, node);
  virtual void VERI_VISIT(VeriIndexedMemoryId, node);
  
 protected:
  Array _signals ;
  
}; // class SignalVisitor


enum VeriNodeType { NodeInfoNoDriver, NodeInfoAssign, NodeInfoExpression, NodeInfoConstant,
		    NodeInfoAlwaysBlockAssign, NodeInfoModuleInput, NodeInfoModuleOutput,
		    NodeInfoInstInput, NodeInfoInstOutput, NodeInfoExpressionSrc };

// This is a class that keep tracks of each node or wire in the verilog.
// Each node is basically a source and sink pair that can represent:
//  - assignment statement
//  -

class VeriNodeInfo
{
 private:

  BString m_source_name;
  BString m_sink_name;
  VeriNodeType m_type;
  VeriNode *m_node;
  VeriNodeInfo *m_virtual_source;
  VeriNodeInfoList m_sinks;
  std::map<std::string, int> m_expr_inputs;

 protected:

  VeriNodeInfo()
    { m_node = NULL; m_type = NodeInfoNoDriver; m_virtual_source = NULL; }

 public:

  // Constructor
  VeriNodeInfo(const char *source_name, const char *sink_name, VeriNodeType t, VeriNode *node)
    { m_source_name = source_name; m_sink_name = sink_name; m_type = t;
      m_virtual_source = NULL; m_node = node; }

  // Destructor
  ~VeriNodeInfo() {};

  const char *getSourceName() { return m_source_name.c_str(); }
  const char *getSinkName() { return m_sink_name.c_str(); }
  VeriNodeType getType() { return m_type; }
  VeriNode *getVeriNode() { return m_node; }

  void setSourceName(const char *n) { m_source_name = n; }
  void setType(VeriNodeType t) { m_type = t; }
  void setVeriNode(VeriNode *n) { m_node = n; }


  void setVirtualSource(VeriNodeInfo *node) { m_virtual_source = node; }
  VeriNodeInfo *getVirtualSource() { return m_virtual_source; }
  void addSink(VeriNodeInfo *sink);
  void sortUniqueSink();

  VeriNodeInfoListIterator sinkBegin() { return m_sinks.begin(); }
  VeriNodeInfoListIterator sinkEnd() { return m_sinks.end(); }

  int addExpressionInput(const char *name);
  int addExpressionInput(BString &name) { return addExpressionInput(name.c_str()); }
  int isAnExpressionInput(const char *name) { return m_expr_inputs[name]; }
  int isAnExpressionInput(const BString &name) { return m_expr_inputs[name]; }
  
  std::map<std::string, int>::iterator expressionInputBegin() { return m_expr_inputs.begin(); }
  std::map<std::string, int>::iterator expressionInputEnd() { return m_expr_inputs.end(); }

  void dump();
};

class VeriNodeInfoTable
{
 private:

  VeriNodeInfoList m_info;
  VeriNodeInfoMap m_sink_info;
  VeriNodeInfoMap m_source_info;
  VeriNodeInfoList m_virtual_nets;
  int m_processed;
  std::map<std::string, int> m_widthmap;

 protected:

  static std::map<VeriModule*, VeriNodeInfoTable*> theVeriNodeInfoTableMap;

  void buildSourceSubGraph(VeriNodeInfo *node);
  VeriNodeInfo *recursiveFindSource(VeriNodeInfo *sink);
  VeriNodeInfo *recursiveFindExprSource(VeriNodeInfo *node, const BString &sname);

 public:

  // Constructor
  VeriNodeInfoTable() { m_processed = 0; }

  // Destructor
  ~VeriNodeInfoTable();

  // Accessor
  //VeriNodeInfo *getSinkVeriNodeInfo(const char *sink_name) { return m_sink_info[sink_name]; }
  //VeriNodeInfo *getSinkVeriNodeInfo(BString &sink_name) { return m_sink_info[sink_name]; }
  //VeriNodeInfo *getSourceVeriNodeInfo(const char *source_name)
  //{ return m_source_info[source_name]; }
  //VeriNodeInfo *getSourceVeriNodeInfo(BString &source_name)
  //{ return m_source_info[source_name]; }

  // Add methods
  VeriNodeInfo *addVeriNodeInfo(const char *source_name, const char *sink_name,
				VeriNodeType t, VeriNode *node);
  VeriNodeInfo *addVeriNodeInfoExpr(const char *source_name, const char *sink_name,
				    VeriNodeType t, VeriNode *node);
  VeriNodeInfo *addVeriNodeInfo(BString &source_name, BString &sink_name, VeriNodeType t,
				VeriNode *node)
  { return addVeriNodeInfo(source_name.c_str(), sink_name.c_str(), t, node); }
  VeriNodeInfo *addVeriNodeInfoExpr(BString &source_name, BString &sink_name, VeriNodeType t,
				    VeriNode *node)
  { return addVeriNodeInfoExpr(source_name.c_str(), sink_name.c_str(), t, node); }
  
  // Process
  void processAndCreateVirtualNets();
  static VeriNodeInfoTable *load(VeriModule *module);
  static VeriNodeInfoTable *getTable(VeriModule *module) { return theVeriNodeInfoTableMap[module]; }
  void loadWidthsDefinition(VeriModule *module);
  void loadDeclaration(VeriModule *module);
  void loadModulePorts(VeriModule *module);
  void loadAssignment(NodeInfoVisitor &visitor);
  void loadAssignment(VeriModule *module);
  void loadModuleInstantiation(NodeInfoVisitor &visitor);
  void loadAlwaysBlock(NodeInfoVisitor &visitor);

  // Traverse all sinks
  VeriNodeInfoIterator sinkBegin()
  { return m_sink_info.begin(); }
  VeriNodeInfoIterator sinkEnd()
  { return m_sink_info.end(); }

  // Traverse all sources
  VeriNodeInfoIterator sourceBegin()
  { return m_source_info.begin(); }
  VeriNodeInfoIterator sourceEnd()
  { return m_source_info.end(); }

  // Traverse all nodes
  VeriNodeInfoListIterator nodeBegin()
  { return m_info.begin(); }
  VeriNodeInfoListIterator nodeEnd()
  { return m_info.end(); }

  // Traverse all virtual nets
  VeriNodeInfoListIterator virtualNetsBegin();
  VeriNodeInfoListIterator virtualNetsEnd()
  { return m_virtual_nets.end(); }

  // Find virtual net from a signal
  VeriNodeInfo *findVirtualSource(const char *signal);

  // Find a node
  VeriNodeInfo *findSource(const char *signal)
  { return m_source_info[signal]; }
  VeriNodeInfo *findSink(const char *signal)
  { return m_sink_info[signal]; }

  static bool isDeadEndNode(VeriNodeInfo *node);
  static bool isModuleInputNode(VeriNodeInfo *node);
  static bool isModuleOutputNode(VeriNodeInfo *node);

  // Dump
  void dump();
};

class OccCell;

typedef std::map<std::string, OccCell*> OccCellSet;
typedef std::map<std::string, OccCell*>::iterator OccCellIterator;

// Class for representing occurrence of a module in a design hierarchy

class OccCell {

 protected:

  static OccCellSet         TopCells;  // Top of design tree

  OccCell                  *Parent;    // Parent
  VeriModule               *Module;    // Module
  VeriModuleInstantiation  *ModuleInst;// ModuleInst
  VeriInstId               *Inst;      // Inst
  OccCellSet                OccCells;  // Children
  string                    HierName;  // Hierarchical Name
  bool                      Loaded;
  bool                      Top;

  // Model building methods
  OccCell();
  OccCell(OccCell *parent, VeriModuleInstantiation *modinst, VeriInstId *inst);

  // Load the internal of a cell
  void load();

 private:

 public:

  // Destructor
  ~OccCell();

  // Create
  static OccCell *createTopCell(VeriModule *module);

  // Get top cell
  static OccCell *getTopCell(const char *name);

  // Parse input string and get the first token delimited by "/"
  static void getNextLevel(const char *path, std::string &next, std::string &leftover);

  // The defining instance
  VeriModule *module() { return Module; }
  VeriModuleInstantiation *moduleInst() { return ModuleInst; }
  VeriInstId *inst() { return Inst; }

  // Name
  const char *name();
  const char *moduleName();
  const char *hierName() { return HierName.c_str(); }

  // Access
  bool isTop() { return Top; }
  OccCell *parent() { return Parent; }

  // Views set
  OccCellIterator childBegin();
  OccCellIterator childEnd();
  OccCell *locateChildByName(const char *name);

  OccCell *recursiveFindOccCell(const char *path);

  void dump();
};

