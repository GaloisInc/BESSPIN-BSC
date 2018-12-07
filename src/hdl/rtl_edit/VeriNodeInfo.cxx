// Copyright 2010 Bluespec Inc. All rights reserved

#include "VeriNodeInfo.h"
#include "HdlUtils.h"
#include "Map.h"
#include "VeriModule.h"     // Definition of a VeriModule and VeriPrimitive
#include "VeriId.h"         // Definitions of all identifier definition tree nodes
#include "VeriExpression.h" // Definitions of all verilog expression tree nodes
#include "VeriModuleItem.h" // Definitions of all verilog module item tree nodes
#include "VeriStatement.h"  // Definitions of all verilog statement tree nodes
#include "VeriTreeNode.h"
#include "veri_tokens.h"
#include "VeriScope.h"      // Symbol table of locally declared identifiers

// Info table
std::map<VeriModule*, VeriNodeInfoTable*> VeriNodeInfoTable::theVeriNodeInfoTableMap;

// Top cells
OccCellSet OccCell::TopCells;

int hasBracket(const char *name, BString &rootname)
{
  char *c;

  rootname = name;
  
  if ((c = strchr((char*)name, '['))) {
    rootname = rootname.substr(0, c-name);
    return 1;
  }
  return 0;
}

void VeriNodeInfo::addSink(VeriNodeInfo *sink)
{
  m_sinks.push_back(sink);
}

void VeriNodeInfo::sortUniqueSink()
{
  m_sinks.sort();
  m_sinks.unique();
}

int VeriNodeInfo::addExpressionInput(const char *name)
{
  if ((m_sink_name == name) || m_expr_inputs[name])
    return 0;

  m_expr_inputs[name] = 1;

  BString nbname;
  hasBracket(name, nbname);
  if (nbname != name)
    m_expr_inputs[nbname] = 1;

  return 1;
}

void VeriNodeInfo::dump()
{
  BString vsource, type, vtype;

  if (getType() == NodeInfoNoDriver)
    type = "NodeInfoNoDriver";
  else if (getType() == NodeInfoAssign)
    type = "NodeInfoAssign";
  else if (getType() == NodeInfoExpression)
    type = "NodeInfoExpression";
  else if (getType() == NodeInfoConstant)
    type = "NodeInfoConstant";
  else if (getType() == NodeInfoAlwaysBlockAssign)
    type = "NodeInfoAlwaysBlockAssign";
  else if (getType() == NodeInfoModuleInput)
    type = "NodeInfoModuleInput";
  else if (getType() == NodeInfoModuleOutput)
    type = "NodeInfoModuleOutput";
  else if (getType() == NodeInfoInstInput)
    type = "NodeInfoInstInput";
  else if (getType() == NodeInfoInstOutput)
    type = "NodeInfoInstOutput";
  else if (getType() == NodeInfoExpressionSrc)
    type = "NodeInfoExpressionSrc";
  else
    type = "NONE";

  if (m_virtual_source) {
    vsource = m_virtual_source->m_source_name;
    if (m_virtual_source->getType() == NodeInfoNoDriver)
      vtype = "NodeInfoNoDriver";
    else if (m_virtual_source->getType() == NodeInfoAssign)
      vtype = "NodeInfoAssign";
    else if (m_virtual_source->getType() == NodeInfoExpression)
      vtype = "NodeInfoExpression";
    else if (m_virtual_source->getType() == NodeInfoConstant)
      vtype = "NodeInfoConstant";
    else if (m_virtual_source->getType() == NodeInfoAlwaysBlockAssign)
      vtype = "NodeInfoAlwaysBlockAssign";
    else if (m_virtual_source->getType() == NodeInfoModuleInput)
      vtype = "NodeInfoModuleInput";
    else if (m_virtual_source->getType() == NodeInfoModuleOutput)
      vtype = "NodeInfoModuleOutput";
    else if (m_virtual_source->getType() == NodeInfoInstInput)
      vtype = "NodeInfoInstInput";
    else if (m_virtual_source->getType() == NodeInfoInstOutput)
      vtype = "NodeInfoInstOutput";
    else if (m_virtual_source->getType() == NodeInfoExpressionSrc)
      vtype = "NodeInfoExpressionSrc";
    else
      vtype = "NONE";
  }
  else
    vtype = vsource = "None";
  int nsinks = 0;
  nsinks = m_sinks.size();

  BString instname;
  if ((getType() == NodeInfoInstInput) || (getType() == NodeInfoInstOutput))
    instname = ((VeriInstId *)getVeriNode())->Name();
  else
    instname = "";
  printf("VeriNodeInfo: [%s] Source: %s Sink: %s Virtual Source: %s [%s] Num Sinks: %d %p inst: %s\n",
  	 type.c_str(), m_source_name.c_str(), m_sink_name.c_str(), vsource.c_str(), vtype.c_str(),
  	 nsinks, this, instname.c_str());
}

VeriNodeInfoTable::~VeriNodeInfoTable()
{
  VeriNodeInfoListIterator sinkItr;

  for (sinkItr = m_info.begin();
       sinkItr != m_info.end();
       sinkItr++) {

    delete *sinkItr;
  }
}

VeriNodeInfo *VeriNodeInfoTable::addVeriNodeInfo(const char *source_name, const char *sink_name,
						 VeriNodeType t, VeriNode *node)
{
  VeriNodeInfo *sinfo;
  BString noBracketNameSrc, noBracketNameSink;

  //printf("AddVeriNodeInfo src:%s sink:%s %d\n", source_name, sink_name, t);

  if ((sink_name == NULL) || !strcmp(sink_name, ""))
    return NULL;

  m_processed = 0;

  if (t == NodeInfoModuleOutput) {
    hasBracket(sink_name, noBracketNameSink);
    hasBracket(source_name, noBracketNameSrc);

    sinfo = new VeriNodeInfo(noBracketNameSrc.c_str(), noBracketNameSink.c_str(), t, node);
    //printf("Add module output info src:%s sink:%s %p\n", sinfo->getSourceName(), sinfo->getSinkName(), sinfo);
    m_info.push_back(sinfo);
  }
  else {
    
    hasBracket(sink_name, noBracketNameSink);
    hasBracket(source_name, noBracketNameSrc);

    sinfo = m_sink_info[noBracketNameSink];
    //printf("Sink Info1 %s %p\n", noBracketNameSink.c_str(), sinfo);
    if (sinfo) {
      //if (t != NodeInfoInstInput)
      //cerr << "Error VeriNodeInfoTable::addSourceInfo(): something wrong sink "
      //     << sink_name << " " << noBracketNameSink << " already exists." << endl;
      return sinfo;
    }
    
    sinfo = new VeriNodeInfo(noBracketNameSrc.c_str(), noBracketNameSink.c_str(), t, node);
    m_info.push_back(sinfo);
    //if (t == NodeInfoInstInput)
    //printf("Add sink info src:%s sink:%s %s %s\n", sinfo->getSourceName(), sinfo->getSinkName(),
    //	   noBracketNameSrc.c_str(), noBracketNameSink.c_str());
    m_sink_info[noBracketNameSink] = sinfo;
    m_source_info[noBracketNameSrc] = sinfo;
  }
  
  return sinfo;
}

VeriNodeInfo *VeriNodeInfoTable::addVeriNodeInfoExpr(const char *source_name, const char *sink_name,
						     VeriNodeType t, VeriNode *node)
{
  VeriNodeInfo *sinfo;
  BString noBracketNameSrc, noBracketNameSink;

  if ((sink_name == NULL) || !strcmp(sink_name, ""))
    return NULL;

  m_processed = 0;

  if (t == NodeInfoModuleOutput) {
    sinfo = new VeriNodeInfo(source_name, sink_name, t, node);
    m_info.push_back(sinfo);
  }
  else {
    
    sinfo = m_sink_info[sink_name];
    //printf("Sink Info2 %s %p\n", sink_name, sinfo);
    if (sinfo) {
      //cerr << "Error VeriNodeInfoTable::addSourceInfoExpr(): something wrong sink "
      //   << sink_name << " already exists." << endl;
      return NULL;
    }
    
    sinfo = new VeriNodeInfo(source_name, sink_name, t, node);
    m_info.push_back(sinfo);
    //printf("Add exp sink info %s %p\n", sinfo->getSourceName(), sinfo);
    m_sink_info[sink_name] = sinfo;
  }
  
  return sinfo;
}

bool VeriNodeInfoTable::isDeadEndNode(VeriNodeInfo *node)
{
  //printf("node %p\n", node);
  if ((node->getType() != NodeInfoAssign) && (node->getType() != NodeInfoModuleOutput) &&
      (node->getType() != NodeInfoInstInput) && (node->getType() != NodeInfoAlwaysBlockAssign))
    return true;

  return false;
}

bool VeriNodeInfoTable::isModuleInputNode(VeriNodeInfo *node)
{
  if (node->getType() == NodeInfoModuleInput)
    return true;

  return false;
}

bool VeriNodeInfoTable::isModuleOutputNode(VeriNodeInfo *node)
{
  if (node->getType() == NodeInfoModuleOutput)
    return true;

  return false;
}

void VeriNodeInfoTable::processAndCreateVirtualNets()
{
  VeriNodeInfoListIterator vItr;

  if (m_processed)
    return;

  //printf("Processing..\n");
  // Null out all the virtual net pointers
  for (vItr = m_info.begin();
       vItr != m_info.end();
       vItr++) {

    if (*vItr == NULL) {
      continue;
    }

    (*vItr)->setVirtualSource(NULL);
  }

  //printf("Processing2..\n");
  // Find and set virtual source
  for (vItr = m_info.begin();
       vItr != m_info.end();
       vItr++) {

    //printf ("Top node: "); 
    //(*vItr)->dump();
    buildSourceSubGraph(*vItr);
  }

  // Mark that the table has been processed
  m_processed = 1;
  //dump();
}

//int level = 0;

void VeriNodeInfoTable::buildSourceSubGraph(VeriNodeInfo *node)
{
  if ((node->getType() != NodeInfoExpression) &&
      (node->getType() != NodeInfoExpressionSrc)) {
    //level = 0;
    recursiveFindSource(node);
  }
  else {
    std::map<std::string, int>::iterator sItr;

    recursiveFindExprSource(node, node->getSourceName());
    for (sItr = node->expressionInputBegin();
	 sItr != node->expressionInputEnd();
	 sItr++) {
      recursiveFindExprSource(node, sItr->first);
    }
  }

  if (node->getVirtualSource())
    node->getVirtualSource()->sortUniqueSink();
}

VeriNodeInfo *VeriNodeInfoTable::recursiveFindSource(VeriNodeInfo *node)
{
  VeriNodeInfo *source;
  BString noBracketName;

  //level++;

  // Found the source if the node is not an assignment and not module output and not inst input
  //for (i=0; i<level;i++) printf(" ");
  //printf("recursive find source of node: ");
  //node->dump();
  if (isDeadEndNode(node)) {
    
    source = node;
  }
  else {

    hasBracket(node->getSourceName(), noBracketName);
    source = m_sink_info[noBracketName];
    //for (i=0; i<level;i++) printf(" ");
    //printf("Sink Info3 %s %p\n", noBracketName.c_str(), source);
    if (source && (source != node) && (source->getType() != NodeInfoAlwaysBlockAssign)) {
      //for (i=0; i<level;i++) printf(" ");
      //printf("source1: ");
      //source->dump();
      source = recursiveFindSource(source);
      source->addSink(node);
    }
    else {
      source = m_source_info[noBracketName];
      if (source == NULL) {
	m_source_info[noBracketName] = node;
	source = node;
	//for (i=0; i<level;i++) printf(" ");
	//printf("source2: ");
	//source->dump();
      }
    }
  }
  
  node->setVirtualSource(source);
  if (source != node)
    source->addSink(node);
  //for (i=0; i<level;i++) printf(" ");
  //printf("Set virtual source for: ");
  //node->dump();
  m_virtual_nets.push_back(source);
  return source;
}

VeriNodeInfo *VeriNodeInfoTable::recursiveFindExprSource(VeriNodeInfo *node, const BString &sname)
{
  VeriNodeInfo *source = node;

  //printf("recursive find expr source of node on source %s: ", sname.c_str());
  //node->dump();
  if ((node->getType() != NodeInfoExpression) &&
      (node->getType() != NodeInfoExpressionSrc)) {
    cerr << "Error VeriNodeInfoTable::recursiveFindExprSource(): node not an expression" << endl;
    node->dump();
    return NULL;
  }

  if ((sname == node->getSourceName()) || (node->isAnExpressionInput(sname) == 1)) {

    source = m_sink_info[sname];
    //printf("Sink Info4 %s %p\n", sname.c_str(), source);
    if (source) {
      //printf("source: ");
      //source->dump();
      source = recursiveFindSource(source);
      if (source != node)
        source->addSink(node);
    }
    else
      source = node;
  }

  node->setVirtualSource(source);
  m_virtual_nets.push_back(source);

  return source;
}

VeriNodeInfoListIterator VeriNodeInfoTable::virtualNetsBegin()
{
  if (!m_processed)
    processAndCreateVirtualNets();

  return m_virtual_nets.begin();
}

void VeriNodeInfoTable::loadWidthsDefinition(VeriModule *module)
{
  if (module == NULL) return;

  m_widthmap.clear();

  // Get the scope of this module :
  VeriScope *module_scope = module->GetScope() ;
 
  // Now iterate over the declared identifiers ((VeriIdDef*s) in the hash table (scope->DeclArea()) in this scope :
  int m,l;
  MapIter mi ;
  VeriIdDef *id ;
  char *id_name ;
  int width;

  FOREACH_MAP_ITEM(module_scope->DeclArea(), mi, &id_name, &id) {
    // Rule out all identifiers declared here, except for 'nets' :
    if (!(id->IsNet() || id->IsReg())) continue ;
    
    // Here, 'id' is a 'net', declared in the module
    // Use the extended VhdlIdDef class API (in file VeriId.h) to do what you want
    string name = id->Name();
    //printf("name %s\n", name.c_str());
    width = id->GetPackedWidth(&m, &l);

    if (m_widthmap[name] == 0)
      m_widthmap[name] = width;
  }
}

VeriNodeInfoTable *VeriNodeInfoTable::load(VeriModule *module)
{
  if (module == NULL) return NULL;

  VeriNodeInfoTable *table = theVeriNodeInfoTableMap[module];
  if (table == NULL) {
    table = new VeriNodeInfoTable();
    theVeriNodeInfoTableMap[module] = table;
  }
  else
    return table;

  // Traverse the verific looking for statements to build
  // the node info table

  //printf("Load module %s\n", module->Name());

  NodeInfoVisitor visitor;

  module->Accept(visitor);

  //loadDeclaration(module);
  //printf("loadModulePorts\n");
  table->loadModulePorts(module);
  //printf("loadAlwaysBlock\n");
  table->loadAlwaysBlock(visitor);
  //printf("loadAssignment1\n");
  //table->loadAssignment(visitor);
  //printf("loadAssignment2\n");
  table->loadAssignment(module);
  //printf("loadModuleInstantiation\n");
  table->loadModuleInstantiation(visitor);
  //printf("Done load\n");

  table->processAndCreateVirtualNets();
  //printf("Done process\n");

  return table;
}

void VeriNodeInfoTable::loadDeclaration(VeriModule *module)
{
  VeriDataDecl *decl;
  unsigned i ;
  VeriIdDef *id ;

  foreachDataDecl(module, decl) {
    FOREACH_ARRAY_ITEM(decl->GetIds(), i, id) {
      if (!id) continue ;
      addVeriNodeInfo("", id->Name(), NodeInfoNoDriver, id);
    }
  }
}

void VeriNodeInfoTable::loadModulePorts(VeriModule *module)
{
  Array *port_connects = module->GetPortConnects();
  VeriExpression *expr;
  std::ostringstream stream;
  unsigned it;
  std::string portname;
  VeriIdDef *port;
  
  //printf("loadModulePorts module %s\n", module->Name());

  FOREACH_ARRAY_ITEM(port_connects, it, expr) {
    
    stream.str("");
    expr->PrettyPrint(stream, 0);
    portname = stream.str();
    port = HdlUtils::findPortOfModuleByName(module, portname);
    if (port) {
      //printf("loadModulePorts name %s\n", portname.c_str());
      if (port->IsInput()) {
	//printf("addVeriNodeInfo1 NodeInfoModuleInput source: %s sink: %s\n",
	//       port->Name(), port->Name());
	addVeriNodeInfo(port->Name(), port->Name(), NodeInfoModuleInput, port);
      } else if (port->IsOutput()) {
	//printf("addVeriNodeInfo NodeInfoModuleOutput source: %s sink: %s\n",
	//       port->Name(), port->Name());
	addVeriNodeInfo(port->Name(), port->Name(), NodeInfoModuleOutput, port);
      } else {
	//Bidirectional currently not handled
      }
    }
  }
  
}

void VeriNodeInfoTable::loadAlwaysBlock(NodeInfoVisitor &visitor)
{
  // Now let's look at all always contructs that we collected
  VeriNodeInfo *node;
  VeriAlwaysConstruct *always ;
  SetIter si ;
  BString lstring, rstring, expstring;
  std::stringstream st;
  std::string lhstring, rhstring, sig;
  VeriIdRef *ref ;

  FOREACH_SET_ITEM(visitor.GetAlwaysConstructs(), si, &always) {
    VeriStatement *stmt = always->GetStmt() ;
    // Always constructs most of the time have event controls or delay controls
    if (stmt->GetClassId() == ID_VERIDELAYCONTROLSTATEMENT) {
      VeriDelayControlStatement *delay_control = (VeriDelayControlStatement*)stmt ;
      // Delay control
      VeriExpression *delay = delay_control->GetDelay() ; (void)delay ;
      // Get statement
      //cout << "Always Block Delay Control Statement: ";
      stmt = delay_control->GetStmt() ;
      stmt->PrettyPrint(cout, 0);
      // Now do what you want with the statement's contents
    } else if (stmt->GetClassId() == ID_VERIEVENTCONTROLSTATEMENT) {
      // Event control
      VeriEventControlStatement *event_control = (VeriEventControlStatement*)stmt ;

      // Get statement
      stmt = event_control->GetStmt() ;
      //cout << "Always Block Event Control Statement: ";
      //stmt->PrettyPrint(cout, 0);

      VeriExpression *lval, *rval;
      AssignVisitor assignVisitor;
      VeriBlockingAssign *bassign;
      SetIter ai;
      SignalVisitor v;
      unsigned i;

      stmt->Accept(assignVisitor);

      FOREACH_SET_ITEM(assignVisitor.GetBlockingAssigns(), ai, &bassign) {

	lval = bassign->GetLVal();
	rval = bassign->GetValue();
	
	//cout << "Found always block assign2 lhs: ";
	//lval->PrettyPrint(cout, 0);
	//cout << " rhs: ";
	//rval->PrettyPrint(cout, 0);
	//cout << endl;
	
	// Get the lhs value
	lval->Accept(v);
	st.str("");
	lval->PrettyPrint(st, 0);
	lstring = st.str();
	v.ResetSignals();
	
	// Now do the rhs
	rval->Accept(v);
	if (v.NumSignals() < 1) { // Const case
	  if (rval->IsConst() || rval->IsConstExpr()) {
	    st.str("");
	    rval->PrettyPrint(st, 0);
	    rstring = st.str();
	    addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoConstant, rval);
	  }
	  else {
	    st.str("");
	    bassign->PrettyPrint(st, 0);
	    bassign->Info("Something wrong, unrecognized Assignment Statement: %s",
			   st.str().c_str());
	  }
	}
	else if (v.NumSignals() == 1 && !(rval->OperType())) {
	  FOREACH_ARRAY_ITEM(v.GetSignals(), i, ref) {
	    st.str("");
	    ref->PrettyPrint(st, 0);
	    rstring = st.str();
	    //printf("addVeriNodeInfo9 NodeInfoAssign source: %s sink: %s\n",
	    //	   rstring.c_str(), lstring.c_str());
	    if (rval->IsIdRef())
	      addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoAlwaysBlockAssign,
			      ref);
	    else
	      addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoExpression, ref);
	  }
	}
	else {
	  
	  st.str("");
	  rval->PrettyPrint(st, 0);
	  expstring = st.str();
	  node = addVeriNodeInfo(expstring.c_str(), lstring.c_str(), NodeInfoExpression, rval);
	  //printf("addVeriNodeInfo10 NodeInfoExpression source: %s sink: %s\n",
	  //	 expstring.c_str(), lstring.c_str());
	  
	  FOREACH_ARRAY_ITEM(v.GetSignals(), i, ref) {
	    st.str("");
	    ref->PrettyPrint(st, 0);
	    rstring = st.str();
	    node = findSink(expstring.c_str());
	    if (node == NULL) {
	      //printf("addVeriNodeInfo11 NodeInfoExpressionSrc source: %s sink: %s\n",
	      //	     rstring.c_str(), expstring.c_str());
	      node = addVeriNodeInfoExpr(rstring.c_str(), expstring.c_str(),
					 NodeInfoExpressionSrc, ref);
	    }
	    else {
	      node->addExpressionInput(rstring.c_str());
	      //printf(" addExpressionInput12 source: %s sink: %s\n",
	      //     rstring.c_str(), expstring.c_str());
	    }
	  }
	}
	v.ResetSignals() ;
      }
      
      VeriNonBlockingAssign *nbassign;
      FOREACH_SET_ITEM(assignVisitor.GetNonBlockingAssigns(), ai, &nbassign) {
	
	lval = nbassign->GetLVal();
	rval = nbassign->GetValue();
	
	//cout << "Found always block assign3 lhs: ";
	//lval->PrettyPrint(cout, 0);
	//cout << " rhs: ";
	//rval->PrettyPrint(cout, 0);
	//cout << endl;
	
	// Get the lhs value
	lval->Accept(v);
	st.str("");
	lval->PrettyPrint(st, 0);
	lstring = st.str();
	v.ResetSignals();
	
	// Now do the rhs
	rval->Accept(v);
	if (v.NumSignals() < 1) { // Const case
	  if (rval->IsConst() || rval->IsConstExpr()) {
	    st.str("");
	    rval->PrettyPrint(st, 0);
	    rstring = st.str();
	    //printf("addVeriNodeInfo13a NodeInfoConstant source: %s sink: %s\n",
	    //	   rstring.c_str(), lstring.c_str());
	    addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoConstant, rval);
	  }
	  else {
	    st.str("");
	    nbassign->PrettyPrint(st, 0);
	    nbassign->Info("Something wrong, unrecognized Assignment Statement: %s",
			   st.str().c_str());
	  }
	}
	else if (v.NumSignals() == 1 && !(rval->OperType())) {
	  FOREACH_ARRAY_ITEM(v.GetSignals(), i, ref) {
	    st.str("");
	    ref->PrettyPrint(st, 0);
	    rstring = st.str();
	    //printf("addVeriNodeInfo13 NodeInfoAssign source: %s sink: %s\n",
	    //	   rstring.c_str(), lstring.c_str());
	    if (rval->IsIdRef())
	      addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoAlwaysBlockAssign, ref);
	    else
	      addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoExpression, ref);
	  }
	}
	else {
	  
	  st.str("");
	  rval->PrettyPrint(st, 0);
	  expstring = st.str();
	  node = addVeriNodeInfo(expstring.c_str(), lstring.c_str(), NodeInfoExpression, rval);
	  //printf("addVeriNodeInfo14 NodeInfoExpression source: %s sink: %s\n",
	  //	 expstring.c_str(), lstring.c_str());
	  
	  FOREACH_ARRAY_ITEM(v.GetSignals(), i, ref) {
	    st.str("");
	    ref->PrettyPrint(st, 0);
	    rstring = st.str();
	    node = findSink(expstring.c_str());
	    if (node == NULL) {
	      //printf("addVeriNodeInfo15 NodeInfoExpressionSrc source: %s sink: %s\n",
	      //	     rstring.c_str(), expstring.c_str());
	      node = addVeriNodeInfoExpr(rstring.c_str(), expstring.c_str(),
					 NodeInfoExpressionSrc, ref);
	    }
	    else {
	      node->addExpressionInput(rstring.c_str());
	      //printf(" addExpressionInput16 source: %s sink: %s\n",
	      //     rstring.c_str(), expstring.c_str());
	    }
	  }
	}
	v.ResetSignals() ;
      }
      
    } else {
      cout << "Other Always Statement: ";
      // Get statement
      stmt->PrettyPrint(cout, 0);
      // Now do what you want with the statement's contents
    }
  }
}

void print_expression_type(VeriExpression *exp)
{
  if (exp->IsIdRef())
    cout << "EXP is an IdRef" << endl;
  else if (exp->IsConstExpr())
    cout << "EXP is a Constant Expression" << endl;
  else if (exp->IsConst())
    cout << "EXP is a Constant" << endl;
  else if (exp->IsInlineConstraint())
    cout << "EXP is an inline Constraint" << endl;
  else if (exp->IsCaseOperator())
    cout << "EXP is a Case Operator" << endl;
  else if (exp->IsIfOperator())
    cout << "EXP is an If Operator" << endl;
  else if (exp->IsMultiAssignPattern())
    cout << "EXP is a multi assign pattern" << endl;
  else if (exp->IsMultiConcat())
    cout << "EXP is a multi concat" << endl;
  else if (exp->IsConcat())
    cout << "EXP is a concat" << endl;
  else if (exp->IsStreamingConcat())
    cout << "EXP is a streaming concat" << endl;
  else if (exp->IsConcatItem())
    cout << "EXP is a concat item" << endl;
  else if (exp->IsNewExpr())
    cout << "EXP is a new expression" << endl;
  else if (exp->IsParenthesized())
    cout << "EXP is a perenthesized" << endl;
  else if (exp->IsQuestionColon())
    cout << "EXP is a question colon" << endl;
  else if (exp->IsSelectCondition())
    cout << "EXP is a select condition" << endl;
  else if (exp->IsTypeOperator())
    cout << "EXP is a type operator" << endl;
  else if (exp->IsCondPredicate())
    cout << "EXP is a cond predicate" << endl;
  else if (exp->IsRange())
    cout << "EXP is a Range" << endl;
  else if (exp->IsReal())
    cout << "EXP is a Real" << endl;
  else if (exp->IsSolveBefore())
    cout << "EXP is a Solve Before" << endl;
  else if (exp->IsUnsizedBit())
    cout << "EXP is an unsized Bit" << endl;
  else if (exp->OperType())
    cout << "EXP is an OperType" << endl;
  else if (exp->IsAnsiPortDecl())
    cout << "EXP is an AnsiPortDecl" << endl;
  else if (exp->IsAssignPattern())
    cout << "EXP is an AssignPattern" << endl;
  else if (exp->IsMultiAssignPattern())
    cout << "EXP is an MultiAssignPattern" << endl;
  else
    cout << "EXP is **Unknown**" << endl;
}

bool isSimpleSignal(VeriExpression *exp)
{
  if (exp->IsIdRef())
    return true;

  if (exp->OperType())
    return false;

  return true;
}

void VeriNodeInfoTable::loadAssignment(NodeInfoVisitor &visitor)
{
  // Now let's look at all continuous assign statements that we collected
  SetIter si ;
  VeriContinuousAssign *assign ;
  FOREACH_SET_ITEM(visitor.GetAssignStmts(), si, &assign) {
    // Get strength (if it exists)
    VeriStrength *strength = assign->GetStrength() ; (void)strength ;
    // Get delays (if they exist)
    unsigned i ;
    VeriExpression *delay ;
    FOREACH_ARRAY_ITEM(assign->GetDelay(), i, delay) {
      // Do what you want with the delay (which is a expression) here
    }
    // Get lhs and rhs of continuous assignments
    BString lstring, rstring, expstring;
    std::stringstream st;
    VeriNetRegAssign *netassign ;
    VeriExpression *lhs, *rhs  ;
    VeriNodeInfo *node;
    std::string lhstring, rhstring, sig;
    //cout << "Assign Statement: ";
    //assign->PrettyPrint(cout, 0);
    //cout << endl;

    FOREACH_ARRAY_ITEM(assign->GetNetAssigns(), i, netassign) {
      lhs = netassign->GetLValExpr() ;
      rhs = netassign->GetRValExpr() ;
      st.str("");
      lhs->PrettyPrint(st, 0);
      lhstring = st.str();
      st.str("");
      rhs->PrettyPrint(st, 0);
      rhstring = st.str();
      /*
      cout << "Assignment Statement: " << lhstring << " " << rhstring << endl;
      if (rhs->IsIdRef())
	cout << "RHS is an IdRef" << endl;
      if (rhs->IsConstExpr())
	cout << "RHS is a Constant Expression" << endl;
      if (rhs->IsConst())
	cout << "RHS is a Constant" << endl;
      if (rhs->IsInlineConstraint())
	cout << "RHS is an inline Constraint" << endl;
      if (rhs->IsCaseOperator())
	cout << "RHS is a Case Operator" << endl;
      if (rhs->IsIfOperator())
	cout << "RHS is an If Operator" << endl;
      if (rhs->IsMultiAssignPattern())
	cout << "RHS is a multi assign pattern" << endl;
      if (rhs->IsMultiConcat())
	cout << "RHS is a multi concat" << endl;
      if (rhs->IsConcat())
	cout << "RHS is a concat" << endl;
      if (rhs->IsStreamingConcat())
	cout << "RHS is a streaming concat" << endl;
      if (rhs->IsConcatItem())
	cout << "RHS is a concat item" << endl;
      if (rhs->IsNewExpr())
	cout << "RHS is a new expression" << endl;
      if (rhs->IsParenthesized())
	cout << "RHS is a perenthesized" << endl;
      if (rhs->IsQuestionColon())
	cout << "RHS is a question colon" << endl;
      if (rhs->IsSelectCondition())
	cout << "RHS is a select condition" << endl;
      if (rhs->IsTypeOperator())
	cout << "RHS is a type operator" << endl;
      if (rhs->IsCondPredicate())
	cout << "RHS is a cond predicate" << endl;
      if (rhs->IsRange())
	cout << "RHS is a Range" << endl;
      if (rhs->IsReal())
	cout << "RHS is a Real" << endl;
      if (rhs->IsSolveBefore())
	cout << "RHS is a Solve Before" << endl;
      if (rhs->IsUnsizedBit())
	cout << "RHS is an unsized Bit" << endl;
      if (rhs->OperType())
	cout << "RHS is an OperType" << endl;
      */
      // Do what you want with them ...
      if (rhs->OperType()) {

	//printf("addVeriNodeInfo NodeInfoExpression source: %s sink: %s\n",
	//       rhstring.c_str(), lhstring.c_str());
	node = addVeriNodeInfo(rhstring.c_str(), lhstring.c_str(), NodeInfoExpression, assign);
	    
	VeriExpression *left, *right;
	left = rhs->GetLeft();
	right = rhs->GetRight();
	if (left) {
	  st.str("");
	  left->PrettyPrint(st, 0);
	  sig = st.str();
	  //printf("addVeriNodeInfo1 NodeInfoExpression source: %s sink: %s\n",
	  //	 sig.c_str(), rhstring.c_str());
	  node = findSink(rhstring.c_str());
	  if (node == NULL)
	    node = addVeriNodeInfo(sig.c_str(), rhstring.c_str(), NodeInfoExpression, assign);
	  else
	    node->addExpressionInput(sig.c_str());
	  //printf(" addExpressionInput2 source: %s sink: %s\n",
	  //	 sig.c_str(), rhstring.c_str());
	}
	if (right) {
	  //cout << " Right: ";
	  //right->PrettyPrint(cout, 0);
	  st.str("");
	  right->PrettyPrint(st, 0);
	  sig = st.str();
	  node = findSink(rhstring.c_str());
	  //printf(" addExpressionInput3 source: %s sink: %s\n",
	  //	 sig.c_str(), rhstring.c_str());
	  }	  if (node == NULL)
	    node = addVeriNodeInfo(sig.c_str(), rhstring.c_str(), NodeInfoExpression, assign);
	  else {
	    node->addExpressionInput(sig.c_str());
	  }
	}
      }
      else if (rhs->IsConcat() || rhs->IsMultiConcat() || rhs->IsQuestionColon()) {
	//printf("addVeriNodeInfo NodeInfoExpression source: %s sink: %s\n",
	//     rhstring.c_str(), lhstring.c_str());
	node = addVeriNodeInfo(rhstring.c_str(), lhstring.c_str(), NodeInfoExpression, assign);

	unsigned k ;
	VeriExpression *expr ;
	Array *exprs = rhs->GetExpressions() ;
	FOREACH_ARRAY_ITEM(exprs, k, expr) {
	  st.str("");
	  expr->PrettyPrint(st, 0);
	  sig = st.str();
	  node = findSink(rhstring.c_str());
	  //printf(" addExpressionInput4 source: %s sink: %s\n",
	  //	 sig.c_str(), rhstring.c_str());
	  if (node == NULL)
	    node = addVeriNodeInfo(sig.c_str(), rhstring.c_str(), NodeInfoExpression, assign);
	  else {
	    node->addExpressionInput(sig.c_str());
	  }
	}
      }
      else if (rhs->IsConst() || rhs->IsConstExpr()) {
	//printf("addVeriNodeInfo5 NodeInfoConstant source: %s sink: %s\n",
	//     rhstring.c_str(), lhstring.c_str());
	addVeriNodeInfo(rhstring.c_str(), lhstring.c_str(), NodeInfoConstant, assign);
      }
      else if (rhs->IsIdRef()) {
	//printf("addVeriNodeInfo6 NodeInfoAssign source: %s sink: %s\n",
	//     rhstring.c_str(), lhstring.c_str());
	addVeriNodeInfo(rhstring.c_str(), lhstring.c_str(), NodeInfoAssign, assign);
      }
      else {
	cerr << "Something wrong, unrecognized Assignment Statement: " << lhstring << " " << rhstring << endl;
	
      }
  }
}

void VeriNodeInfoTable::loadAssignment(VeriModule *module)
{
  Array *items = module->GetModuleItems() ;
  unsigned it;
  VeriModuleItem *item;
  VeriIdRef *ref ;
  BString lstring, rstring, expstring;
  std::stringstream st;
  VeriNodeInfo *node;
  SignalVisitor v ;

  FOREACH_ARRAY_ITEM(items, it, item) {
    if (item == NULL) continue;
    if (item->GetClassId() == ID_VERICONTINUOUSASSIGN) {
      VeriContinuousAssign *cont_assign = dynamic_cast<VeriContinuousAssign *>(item) ;
      Array *assigns = (cont_assign) ? cont_assign->GetNetAssigns() : 0 ;
      if (!assigns) continue;
      
      unsigned i ;
      VeriNetRegAssign *assign ;
      FOREACH_ARRAY_ITEM(assigns, i, assign) {
	if (!assign) continue ;
	//printf("\nAssign statement: ");
	//assign->PrettyPrint(cout, 0);
	//cout << endl;
	VeriExpression *lval = assign->GetLValExpr() ;
	VeriExpression *rval = assign->GetRValExpr() ;
	if ((lval == NULL) || (rval == NULL))
	  continue;

	// Get the lhs value
	lval->Accept(v);
	st.str("");
	lval->PrettyPrint(st, 0);
	lstring = st.str();
	v.ResetSignals();

	// Now do the rhs
	rval->Accept(v);

	//printf("HERE numsignals %d left:%s\n", v.NumSignals(), lstring.c_str());
	//print_expression_type(rval);

	if (v.NumSignals() < 1) { // Const case
	  if (rval->IsConst() || rval->IsConstExpr()) {
	    st.str("");
	    rval->PrettyPrint(st, 0);
	    rstring = st.str();
	    addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoConstant, assign);
	  }
	  else {
	    st.str("");
	    assign->PrettyPrint(st, 0);
	    assign->Info("Something wrong, unrecognized Assignment Statement: %s",
			 st.str().c_str());
	  }
	}
	else if (v.NumSignals() == 1 && (isSimpleSignal(rval))) {
	  //printf("before addVeriNodeInfo6 NodeInfoAssign sink: %s\n", lstring.c_str());
	  FOREACH_ARRAY_ITEM(v.GetSignals(), i, ref) {
	    st.str("");
	    ref->PrettyPrint(st, 0);
	    rstring = st.str();
	    if (rval->IsIdRef()) {
	      if (m_widthmap[rstring] == m_widthmap[lstring]) {
		addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoAssign, cont_assign);
		//printf("addVeriNodeInfo6a NodeInfoAssign source: %s width: %d sink: %s width: %d\n",
		//     rstring.c_str(), m_widthmap[rstring], lstring.c_str(), m_widthmap[lstring]);
	      }
	      //else
	      //printf("Cannot addVeriNodeInfo6 NodeInfoAssign source: %s sink: %s\n",
	      //       rstring.c_str(), lstring.c_str());
	    }
	    //else {
	    //addVeriNodeInfo(rstring.c_str(), lstring.c_str(), NodeInfoExpression, cont_assign);
	    //  printf("addVeriNodeInfo6b NodeInfoExpression source: %s width: %d sink: %s width: %d\n",
	    //       rstring.c_str(), m_widthmap[rstring], lstring.c_str(), m_widthmap[lstring]);
	    //}
	  }
	}
	else {

	  st.str("");
	  rval->PrettyPrint(st, 0);
	  expstring = st.str();
	  node = addVeriNodeInfo(expstring.c_str(), lstring.c_str(), NodeInfoExpression, assign);
	  //printf("addVeriNodeInfo7 NodeInfoExpression source: %s sink: %s\n",
	  //	 expstring.c_str(), lstring.c_str());

	  FOREACH_ARRAY_ITEM(v.GetSignals(), i, ref) {
	    st.str("");
	    ref->PrettyPrint(st, 0);
	    rstring = st.str();
	    node = findSink(expstring.c_str());
	    if (node == NULL) {
	      //printf("addVeriNodeInfo8 NodeInfoExpressionSrc source: %s sink: %s\n",
	      //	     rstring.c_str(), expstring.c_str());
	      node = addVeriNodeInfoExpr(rstring.c_str(), expstring.c_str(),
					 NodeInfoExpressionSrc, assign);
	    }
	    else {
	      node->addExpressionInput(rstring.c_str());
	      //printf(" addExpressionInput3 source: %s sink: %s\n",
	      //     rstring.c_str(), expstring.c_str());
	    }
	  }
	}
	v.ResetSignals() ;
      }
    }
  }
  return;
}

void VeriNodeInfoTable::loadModuleInstantiation(NodeInfoVisitor &visitor)
{
  // Now let's look at all module instantiations that we collected
  std::ostringstream strm;
  VeriModuleInstantiation *module_inst ;
  SetIter si ;
  FOREACH_SET_ITEM(visitor.GetModuleInsts(), si, &module_inst) {
    // Get module's name
    const char *name = module_inst->GetModuleName() ; (void)name ;
    VeriModule *instantiated_module = module_inst->GetInstantiatedModule() ; (void) instantiated_module ;
    
    //printf("ModuleInstantiation %s\n", name);
    // Get strength (if it exists)
    //VeriStrength *strength = module_inst->GetStrength() ; (void) strength ;
    // Get parameter values (if they exist)
    //unsigned i ;
    //VeriExpression *param ;
    //FOREACH_ARRAY_ITEM(module_inst->GetParamValues(), i, param) {
      // Do what you want here ...
    //}
    // Get instances (identifiers) themselves
    VeriInstId *inst ;
    unsigned i;
    FOREACH_ARRAY_ITEM(module_inst->GetInstances(), i, inst) {
      // Get instance name
      //const char *inst_name = inst->Name() ; (void)inst_name ;
      // Get instance's range (if it exists)
      //VeriRange *range = inst->GetRange() ; (void)range ;
      // Get port connects (the actuals of the instantiation)
      unsigned j ;
      VeriExpression *port_connect, *value;
      VeriIdDef *port_def;
      BString portname, connname;
      FOREACH_ARRAY_ITEM(inst->GetPortConnects(), j, port_connect) {
	if (!port_connect) continue ; // open actual

	if (port_connect->GetClassId() == ID_VERIPORTCONNECT) {
	  //printf("in veriportconnect\n");
	  value = port_connect->GetConnection();
	  strm.str("");
	  value->PrettyPrint(strm, 0);
	  connname = port_connect->NamedFormal();
	  port_def = HdlUtils::findPortOfInstByName(inst, connname);
	  //printf("found by name %s\n", connname.c_str());
	} else if (port_connect->IsIdRef()) {
	  //printf("here find pos %d\n", j);
	  VeriModule *module_def = inst->GetInstantiatedModule();
	  port_def = HdlUtils::findPortOfModuleByPosition(module_def, j);
	  strm.str("");
          port_connect->PrettyPrint(strm, 0);
	  connname = strm.str();
	  //printf("found %p\n", port_def);
	}

	if (port_def == NULL)
	  continue;

	//printf("Found port_def %s\n", port_def->Name()); 
	if (port_def->IsInput()) {
	  portname = BString(inst->Name()) + "_" + port_def->Name();
	  //printf("addVeriNodeInfo NodeInfoInstInput source: %s sink: %s %p\n",
	  //	 strm.str().c_str(), portname.c_str(), inst);
	  addVeriNodeInfo(strm.str().c_str(), portname.c_str(), NodeInfoInstInput, inst);
	}
	else if (port_def->IsOutput()) {
	  portname = BString(inst->Name()) + "_" + port_def->Name();
	  //printf("addVeriNodeInfo NodeInfoInstOutput source: %s sink: %s %p table:%p\n",
	  //	 portname.c_str(), strm.str().c_str(), inst, this);
	  addVeriNodeInfo(portname.c_str(), strm.str().c_str(), NodeInfoInstOutput, inst);
	}
	else
	  // Bidirectional currently not handled
	  cerr << "Warning: VeriNodeInfoTable::loadModuleInstantiation(): Bidirectional port "
	       << port_def->Name() << " found on instance "
	       << inst->Name() << " in module " << name << ". The port is ignored." << endl;
      }
    }
  }
}

void VeriNodeInfoTable::dump()
{
  VeriNodeInfoListIterator sinkItr;

  printf ("DUMP Table:\n");

  for (sinkItr = m_info.begin();
       sinkItr != m_info.end();
       sinkItr++) {

    (*sinkItr)->dump();
  }
  printf("DONE DUMP\n");
}

VeriNodeInfo *VeriNodeInfoTable::findVirtualSource(const char *signal)
{
  VeriNodeInfo *source = NULL;
  VeriNodeInfo *sink = m_sink_info[signal];
  //printf("Sink Info5 %s %p\n", signal, sink);
  if (sink) {
    source = sink->getVirtualSource();
  }

  return source;
}

NodeInfoVisitor::NodeInfoVisitor()
  : _assign_stmts(POINTER_HASH),
    _always_constructs(POINTER_HASH),
    _module_insts(POINTER_HASH)
{
}

NodeInfoVisitor::~NodeInfoVisitor()
{
}

void NodeInfoVisitor::Visit(VeriModuleInstantiation &node)
{
  _module_insts.Insert(&node) ;  // Store this node
  // We don't need to traverse any of this class's members.
}

void NodeInfoVisitor::Visit(VeriAlwaysConstruct &node)
{
  _always_constructs.Insert(&node) ;  // Store this node
  // We don't need to traverse any of this class's members.
}

void NodeInfoVisitor::Visit(VeriContinuousAssign &node)
{
  _assign_stmts.Insert(&node) ;   // Store this node
  // We don't need to traverse any of this class's members.
}

void SignalVisitor::VERI_VISIT(VeriIdRef, node)
{
  _signals.InsertLast(&node) ;
}

void SignalVisitor::VERI_VISIT(VeriIndexedId, node)
{
  _signals.InsertLast(&node) ;
}

void SignalVisitor::VERI_VISIT(VeriSelectedName, node)
{
  _signals.InsertLast(&node) ;
}

void SignalVisitor::VERI_VISIT(VeriIndexedMemoryId, node)
{
  _signals.InsertLast(&node) ;
}

void SignalVisitor::PrintInfo()
{
  unsigned i ;
  VeriIdRef *ref ;
  FOREACH_ARRAY_ITEM(&_signals, i, ref) {
    if (ref) ref->Info("    Found signal %s", ref->GetName()) ;
  }
}

// Constructor
OccCell::OccCell()
{
  Parent = NULL;
  Module = NULL;
  ModuleInst = NULL;
  Inst = NULL;
  Loaded = false;
  Top = false;
}
OccCell::OccCell(OccCell *parent, VeriModuleInstantiation *modinst, VeriInstId *inst)
{
  Parent = parent;
  ModuleInst = modinst;
  Inst = inst;
  Module = ModuleInst->GetInstantiatedModule();
  //if (Module == NULL)
  //  printf("Found null module for inst %s\n", inst->Name());
  HierName = string(parent->hierName()) + "/" + name();
  parent->OccCells[inst->Name()] = this;
  Loaded = false;
  Top = false;
}

OccCell *OccCell::getTopCell(const char *name)
{
  return TopCells[name];
}

OccCell *OccCell::createTopCell(VeriModule *module)
{
  OccCell *cell;
  
  cell = TopCells[module->Name()];
  if (cell)
    return cell;
  
  cell = new OccCell();
  cell->Module = module;
  //cout << "Create TopCell for module " << module->Name() << endl;
  TopCells[module->Name()] = cell;
  cell->Top = true;
  cell->HierName = string("/") + module->Name();
  cell->load();

  return cell;
}

OccCell::~OccCell()
{
  OccCellIterator cellItr;
  OccCell *cell;

  for (cellItr = OccCells.begin(); cellItr != OccCells.end(); cellItr++) {

    cell = cellItr->second;
    if (cell->ModuleInst == NULL)
      OccCells[cell->moduleName()] = NULL;
    delete cell;
  }
}

void OccCell::load()
{
  VeriModule *module_def;
  InstantiationVisitor instantiation_visitor ;
  VeriModuleInstantiation *module_inst ;
  unsigned i;
  SetIter si; // a Set iterator
  VeriInstId *inst = 0;
  //OccCell *cell;

  if (Loaded) return;

  if (ModuleInst == NULL)
    module_def = Module;
  else
    module_def = ModuleInst->GetInstantiatedModule();

  if (module_def == NULL) {
    Loaded = true;
    return;
  }

  //printf("loading %s...\n", module_def->Name());

  // Create Insts from module instantiation
  module_def->Accept( instantiation_visitor ) ;
  //cout << "here1" << endl;
  FOREACH_SET_ITEM(&(instantiation_visitor._module_insts), si, &module_inst) {
    
    //cout << "here2" << endl;
    Array *inst_arr = module_inst->GetInstances() ;
    //cout << "here3" << endl;
    FOREACH_ARRAY_ITEM(inst_arr, i, inst) {

      if (module_inst->GetInstantiatedModule())
	//cout << "Create child cell " << inst->Name() << endl;
	new OccCell(this, module_inst, inst);
    }
  }
  Loaded = true;
}

const char *OccCell::name()
{
  if (Inst)
    return Inst->Name();
  else {
    return moduleName();
  }
}

const char *OccCell::moduleName()
{
  return Module->Name();
}

OccCellIterator OccCell::childBegin()
{
  if (Loaded == false)
    load();

  return OccCells.begin();
}

OccCellIterator OccCell::childEnd()
{
  return OccCells.end();
}

OccCell *OccCell::recursiveFindOccCell(const char *path)
{
  OccCell *child;
  string instname, leftover;

  getNextLevel(path, instname, leftover);

  if (instname == "")
    return this;

  child = locateChildByName(instname.c_str());

  if (leftover == "")
    return child;
  else
    return recursiveFindOccCell(leftover.c_str());
}

OccCell *OccCell::locateChildByName(const char *name)
{
  if (Loaded == false)
    load();

  return OccCells[name];
}

void OccCell::getNextLevel(const char *path, std::string &next, std::string &leftover)
{
  const string delims("/[]{}");
  std::string path_string = path;

  string::size_type start = path_string.find_first_not_of(delims);

  if (start == string::npos) {
    next = leftover = "";
    return;
  }

  string::size_type endp = path_string.find_first_of(delims, start);
  next = path_string.substr(start, endp-start); // The first token in the path

  endp = (endp == string::npos) ? path_string.size() : endp+1 ;
  leftover = path_string.substr(endp);
}


void OccCell::dump()
{
  printf("Cell: %s Module: %s\n", name(), moduleName());

  OccCellIterator itr;
  OccCell *child;

  itr = childBegin();
  while (itr != childEnd()) {
    
    child = itr->second;
    printf(" Child: %s Module: %s\n", child->name(), child->moduleName());
  }
}

