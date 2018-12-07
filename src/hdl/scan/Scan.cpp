
#include "Scan.h"
#include "VUtils.h"
#include "V.h"

#include <stack>
#include <string>
#include "Globals.h"
#include "LineFile.h"

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif

VeriStatement* createScanShiftStmt(VeriModule* module, VeriExpression* scan_expr, VeriExpression* id_expr)
{

  // Create concat {_SCAN_0,r0}
  Array *lhs_concat = new Array() ;
  lhs_concat->InsertLast(Copy(scan_expr)) ;
  lhs_concat->InsertLast(Copy(id_expr));
  VeriExpression *lhs = new VeriConcat(lhs_concat) ;

  // Create concat {r0,_SCAN_0}
  Array *rhs_concat = new Array() ;
  rhs_concat->InsertLast(Copy(id_expr));
  rhs_concat->InsertLast(Copy(scan_expr));
  VeriExpression *rhs = new VeriConcat(rhs_concat) ;

  // Now create the assignment with that :
  VeriStatement* stmt = new VeriBlockingAssign(lhs, 0, rhs);

  return stmt;

}

VeriStatement* createScanBlock(VeriModule* module, VeriStatement* expr, VeriIdDef** scan_internal, std::string* prefix, boost::regex filter, ScanPath* path, unsigned* includes_all, unsigned none_yet)
{

  tIdSet id_set = getAssignmentIds(expr);
  // No state variables
  if (id_set.size() == 0) return NULL;

  Array *shift_stmts = new Array();
  Array *concat_lhs = new Array();
  Array *concat_rhs = new Array();
  std::stack<ScanPath*> paths;

  const char* scan_next_name = getUnusedName(module, "");
  //XYZ printf("ADDING REG (%d): %s\n", none_yet, scan_next_name);

  VeriIdDef* scan_next = addSignal(module, scan_next_name, Globals::GetWidth(), VERI_REG);

  ScanPath* path_extra;
  path_extra = new ScanPath(strdup(scan_next_name), Globals::GetWidth());
  if (none_yet) {
//    
//    path->Prepend(path_reg);
  }

  // Now create the assignment with that :
//  VeriStatement* assign_stmt = new VeriBlockingAssign(ref(scan_next), 0, ref(*scan_internal));
//  shift_stmts->Insert(assign_stmt);

//  concat_lhs->InsertLast(ref(scan_next));
//  concat_rhs->InsertLast(ref(*scan_internal));

  unsigned includes_none = 1;
  foreachSetId(id_set, it) {
    VeriIdDef* id = (VeriIdDef*) *it;

    //    id->Resolve(module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV);

    const char* name = id->Name();
    std::string full_name = *prefix;
    full_name += "/";
    full_name += name;

    VeriDataType* dt = id->GetDataType();
    
    if (dt->IsIntegerType()) {
      Dbg::printf("SKIPPING INTEGER! %s\n", full_name.c_str());
      continue;
    }

    boost::match_results<std::string::const_iterator> what;
    if (0 == boost::regex_match(full_name, what, filter, boost::match_default))
      {
	includes_all = 0;
	continue;
      }

    VeriIdDef* existing = module->GetScope()->Find(id->Name());

    if (!existing) {
      //XYZ printf("WARNING: Ignoring local register: %s\n", full_name.c_str());
      continue;
    }
    //    id = existing;
    //XYZ printf("YYYYYYY %s\n", id->Name());

    includes_none = 0;

    if (id->IsMemory() && id->UnPackedDimension() > 1) {
      fprintf(stderr, "Unhandled case. Array '%s' dimension is > 1, Ignoring.\n", id->Name()); 
      continue;
    }

    if (id->IsMemory()) {

      VeriRange* r0 = id->GetDimensionAt(0);
      VeriRange* r1 = id->GetDimensionAt(1);

      r0->StaticReplaceConstantExpr(1);
      r1->StaticReplaceConstantExpr(1);

      int i = 0, min = 0, max = 0, size = 0;

      min = r1->GetLsbOfRange();
      max = r1->GetMsbOfRange();

      if (min > max) {
	min = r1->GetMsbOfRange();
	max = r1->GetLsbOfRange();
      }

      size = max - min + 1;

      min = r0->GetLsbOfRange();
      max = r0->GetMsbOfRange();

      if (min > max) {
	min = r0->GetMsbOfRange();
	max = r0->GetLsbOfRange();
      }

      for(i=min;i<=max;i++) {
	//	printf("ARR NAME: %s %d %d \n", id->Name(), i, size);
	std::string label = name;
//	label += "[";
//	label += itoa(i);
//	label += "]";
	ScanPath* path_reg = new ScanPath(strdup(label.c_str()), size, i);
	//XYZ printf("A PATH+:   %s %s %d\n", module->Name(), label.c_str(), size);
	path->Prepend(path_reg);
//	paths.push(path_reg);

	Array *idx_array = new Array(1) ;
	idx_array->InsertLast(mkExpression(i));

	VeriIndexedMemoryId* select = new VeriIndexedMemoryId(id, idx_array);
//	VeriStatement* stmt = createScanShiftStmt(module, ref(scan_next), select);
//	shift_stmts->Insert(stmt);
	concat_lhs->InsertFirst(select);
	concat_rhs->InsertFirst(select);
      }
    } else {

      VeriRange* range = id->GetDimensionAt(0);

      int size = 1;
      if (range) {
      
	range->StaticReplaceConstantExpr(1);

	int min = 0, max = 0;

	min = range->GetLsbOfRange();
	max = range->GetMsbOfRange();

	if (min > max) {
	  min = range->GetMsbOfRange();
	  max = range->GetLsbOfRange();
	}

	size = max - min + 1;
      }

      ScanPath* path_reg = new ScanPath(strdup(name), size);
      //XYZ printf("  PATH+:   %s %s %s, %d\n", module->Name(), id->Name(), name, size);
      path->Prepend(path_reg);
//      paths.push(path_reg);

//      VeriStatement* stmt = createScanShiftStmt(module, ref(scan_next), ref(id));
//      shift_stmts->Insert(stmt);
      concat_lhs->InsertFirst(ref(id));
      concat_rhs->InsertFirst(ref(id));
    }
  }

  if (includes_none) {
    return NULL;
  }

  path->Prepend(path_extra);

 //  if (none_yet) {
//     path->Prepend(path_extra);
//   }

//   while (!paths.empty()) {
//     path->Prepend(paths.top());
//     paths.pop();
//   }

  concat_lhs->InsertFirst(ref(scan_next));
  concat_rhs->InsertLast(ref(*scan_internal));

  VeriExpression *lhs = new VeriConcat(concat_lhs);
  VeriExpression *rhs = new VeriConcat(concat_rhs);

  VeriStatement* stmt = new VeriNonBlockingAssign(lhs, 0, rhs);
  shift_stmts->Insert(stmt);

  *scan_internal = scan_next;

  VeriStatement* block = new VeriSeqBlock(0,0,shift_stmts,0);

  return block;

}

// void markScanClocks(VeriModule* module, VeriEventControlStatement *event_control_stmt)
// {

//   // Look at the sensitivity list (the "@()" clause), and visit the 'edge' expressions :
//   Array *sens_list = event_control_stmt->GetAt() ;
//   unsigned it_0;
//   VeriExpression *event_expr ;
//   FOREACH_ARRAY_ITEM(sens_list, it_0, event_expr) {
//     if (!event_expr) break ; // invalid sensitivity list
//     if (event_expr->IsEdge(VERI_POSEDGE)) {
//       VeriIdDef* id = event_expr->FullId();
//       if (id) {
// 	printf("SCAN CLOCK: %s\n", id->Name());
// 	pushAttribute(module, "SCAN_CLOCK", id->Name());
// 	printf("SCAN CLOCK: %s\n", id->Name());
//       }
//     }
//     if (event_expr->IsEdge(VERI_NEGEDGE)) {
//       VeriIdDef* id = event_expr->FullId();
//       if (id) {
// 	printf("SCAN CLOCK: %s\n", id->Name());
// 	pushAttribute(module, "SCAN_CLOCK", id->Name());
// 	printf("SCAN CLOCK: %s\n", id->Name());
//       }
//     }
//   }
// }

//   if (veNodeGetObjType(alwaysStmt) == VE_DELAY_OR_EVENT)
//     {
//       veNode Control = veDelOrEventControlGetTimingControlExpr(alwaysStmt);
//       if (veNodeGetObjType(Control) == VE_EVENT_CONTROL)
// 	{
// 	  veIter ExprList = veEventControlGetExprList(Control);
// 	  veNode Expr;
// 	  while ((Expr = veListGetNextNode(ExprList)))
// 	    {
// 	      if ((veNodeGetObjType(Expr) == VE_POSE_SCALAR_EVENT_EXPR))
// 		{
// 		  veNode clk_use = vePosEdgeExprGetEventExpr (Expr);
// 		  veNode net = veScopeVariableGetParentObject(clk_use);
// 		  if (net) {
// 		    removeAttribute(module, "scanClk", (char*) veNetGetName(net));
// 		    addAttribute(module, "scanClk", veModuleCreateString(module, (char*) veNetGetName(net)), module);
// 		  }
// 		}
// 	      if ((veNodeGetObjType(Expr) == VE_NEG_SCALAR_EVENT_EXPR))
// 		{
// 		  veNode clk_use = veNegEdgeExprGetEventExpr (Expr);
// 		  veNode net = veScopeVariableGetParentObject(clk_use);
// 		  if (net) {
// 		    removeAttribute(module, "scanClk", (char*) veNetGetName(net));
// 		    addAttribute(module, "scanClk", veModuleCreateString(module, (char*) veNetGetName(net)), module);
// 		  }
// 		}
// 	    }
// 	  veFreeListMemory(ExprList);
// 	}
//     }
// }


VeriIdDef* getScanAny(VeriModule* module)
{
  return getOrAddPort(module, "SCAN_ANY", false, VERI_INPUT, 1);
}

VeriIdDef* getUClock(VeriModule* module)
{
  return getOrAddPort(module, "SCAN_UCLOCK", false, VERI_INPUT, 1);
}

unsigned connectScanInstances(VeriModule* module, VeriIdDef** scan_internal,  VeriIdDef* scan_mode, ScanPath* path)
{

  unsigned count     = 0;
  VeriModuleInstantiation *module_inst;
  foreachModuleInstance(module, module_inst) {
    const char* inst_name  = getInstanceName(module_inst);
    VeriModule* module_def = module_inst->GetInstantiatedModule();

    if (!module_def) continue;

    const char* scan_in_name      = getAttribute(module_def, "SCAN_IN");
    const char* scan_out_name     = getAttribute(module_def, "SCAN_OUT");
    const char* scan_mode_name    = getAttribute(module_def, "SCAN_MODE");
    const char* scan_length_name  = getAttribute(module_def, "SCAN_LENGTH");
    const char* scan_width_name   = getAttribute(module_def, "SCAN_WIDTH");
    const char* scan_any_name     = getAttribute(module_def, "SCAN_ANY");
    const char* scan_uclock_name  = getAttribute(module_def, "SCAN_UCLOCK");

    if (scan_in_name && scan_out_name && scan_mode_name && scan_length_name && scan_width_name) {
      //XYZ printf("CONNECTING: %s %s %s\n", inst_name, module->Name(), module_def->Name());

      count++;

      ModuleDesc* md = ModuleDescMap::GetDesc(module_def);
      ScanPath* sub_path = new ScanPath(inst_name, md);
      path->Prepend(sub_path);

      module->AddPortRef(inst_name, scan_in_name, ref(*scan_internal));

      const char* scan_next_name = getUnusedName(module, "");
      VeriIdDef*  scan_next      = addSignal(module, scan_next_name, Globals::GetWidth());

      *scan_internal = scan_next;

      VeriIdDef* scan_any    = getScanAny(module);
      VeriIdDef* scan_uclock = getUClock(module);

      //XYZ printf("JJJJJJ: %s \n", scan_next_name);

      module->AddPortRef(inst_name, scan_out_name,  ref(*scan_internal));
      module->AddPortRef(inst_name, scan_mode_name, ref(scan_mode));
      module->AddPortRef(inst_name, scan_length_name, NULL);

      // Try to remove these port refs in case they've already been connected.
      module->RemovePortRef(inst_name, scan_any_name);
      module->RemovePortRef(inst_name, scan_uclock_name);
      module->AddPortRef(inst_name, scan_any_name, ref(scan_any));
      module->AddPortRef(inst_name, scan_uclock_name, ref(scan_uclock));


    } else {
      //XYZ printf("SKIPPING: %s\n", module_def->Name());
    }
  }

  return count;

}

void connectScanOuput(VeriModule* module, VeriIdDef* scan_internal,  VeriIdDef* scan_out)
{

  VeriContinuousAssign* assign = mkContinuousAssign(module, ref(scan_out), ref(scan_internal));
  module->AddModuleItem(assign);
}

void connectScanCount(VeriModule* module, ScanPath* path,  VeriIdDef* scan_width,  VeriIdDef* scan_length)
{

  unsigned length_fixed  = path->Length(1);
  unsigned length_scaled = path->Length(0);

  VeriExpression* expr_fixed  = mkExpression(length_fixed);
  VeriExpression* expr_scaled = new VeriBinaryOperator(VERI_MUL, ref(scan_width), mkExpression(length_scaled));
  //  VeriExpression* expr_length = new VeriBinaryOperator(VERI_MIN,
  //                                                       new VeriBinaryOperator(VERI_PLUS, expr_fixed, expr_scaled),
  //                                                       mkExpression(1));
  VeriExpression* expr_length = new VeriBinaryOperator(VERI_PLUS, expr_fixed, expr_scaled);

  VeriExpression* expr_count;
  if (length_fixed == 0 && length_scaled == 0) {
    expr_count = mkExpression(0);
  } else {
    //    expr_count  = new VeriBinaryOperator(VERI_PLUS,
    //					 new VeriBinaryOperator(VERI_DIV, expr_length, ref(scan_width)),
    //					 mkExpression(1));
    expr_count = expr_length;
  }

  VeriContinuousAssign* assign = mkContinuousAssign(module, ref(scan_length), expr_count);
  module->AddModuleItem(assign);
}

unsigned scannedBefore(VeriModule* module)
{
  VeriExpression* expr = module->GetAttribute("SCANNED_BEFORE");
  if (expr) {
    return 1;
  }
  return 0;
}


VeriStatement* getScanBlockBody (VeriStatement* mblock)
{

  if (mblock->GetClassId() == ID_VERISEQBLOCK) {
    unsigned i;
    VeriSeqBlock* block = static_cast<VeriSeqBlock*>(mblock);
    VeriStatement* mstmt;
    FOREACH_ARRAY_ITEM(block->GetStatements(), i, mstmt) {
      break;
    }
    if (mstmt && mstmt->GetClassId() == ID_VERICONDITIONALSTATEMENT) {
      VeriConditionalStatement* stmt = static_cast<VeriConditionalStatement*>(mstmt);
      return stmt->GetThenStmt();
    }
  }
  fprintf(stderr, "getScanBlockBody: unexpected error.\n");
  exit(1);
}

VeriIdDef* createConcatSignal(VeriModule* module, tIdSet all_ids, std::string* prefix, boost::regex filter, ScanPath* path)
{

  Array *concat = new Array();

  bool includes_all = true;

  int total = 0;
  foreachSetId(all_ids, it) {
    VeriIdDef* id = (VeriIdDef*) *it;

    const char* name = id->Name();
    std::string full_name = *prefix;
    full_name += "/";
    full_name += name;

    VeriDataType* dt = id->GetDataType();
    
    if (dt->IsIntegerType()) {
      Dbg::printf("SKIPPING INTEGER! %s\n", full_name.c_str());
      continue;
    }

    boost::match_results<std::string::const_iterator> what;
    if (0 == boost::regex_match(full_name, what, filter, boost::match_default))
      {
	includes_all = false;
	continue;
      }

    VeriIdDef* existing = module->GetScope()->Find(id->Name());

    if (!existing) {
      printf("WARNING: Ignoring local register: %s\n", full_name.c_str());
      continue;
    }

    //XYZ printf("YYYYYYY %s\n", id->Name());

    if (id->IsMemory() && id->UnPackedDimension() > 1) {
      fprintf(stderr, "Unhandled case. Array '%s' dimension is > 1, Ignoring.\n", id->Name()); 
      continue;
    }

    if (id->IsMemory()) {

      VeriRange* r0 = id->GetDimensionAt(0);
      VeriRange* r1 = id->GetDimensionAt(1);

      r0->StaticReplaceConstantExpr(1);
      r1->StaticReplaceConstantExpr(1);

      int i = 0, min = 0, max = 0, size = 0;

      min = r1->GetLsbOfRange();
      max = r1->GetMsbOfRange();

      if (min > max) {
	min = r1->GetMsbOfRange();
	max = r1->GetLsbOfRange();
      }

      size = max - min + 1;

      min = r0->GetLsbOfRange();
      max = r0->GetMsbOfRange();

      if (min > max) {
	min = r0->GetMsbOfRange();
	max = r0->GetLsbOfRange();
      }

      for(i=min;i<=max;i++) {
	//	printf("ARR NAME: %s %d %d \n", id->Name(), i, size);
	std::string label = name;
	ScanPath* path_reg = new ScanPath(strdup(label.c_str()), size, i);
	//XYZ printf("A PATH+:   %s %s %d\n", module->Name(), label.c_str(), size);
	path->Prepend(path_reg);
	
	total += size;

	Array *idx_array = new Array(1) ;
	idx_array->InsertLast(mkExpression(i));

	VeriIndexedMemoryId* select = new VeriIndexedMemoryId(id, idx_array);
	concat->InsertFirst(select);
      }
    } else {

      VeriRange* range = id->GetDimensionAt(0);

      int size = 1;
      if (range) {
      
	range->StaticReplaceConstantExpr(1);

	int min = 0, max = 0;

	min = range->GetLsbOfRange();
	max = range->GetMsbOfRange();

	if (min > max) {
	  min = range->GetMsbOfRange();
	  max = range->GetLsbOfRange();
	}

	size = max - min + 1;
      }

      ScanPath* path_reg = new ScanPath(strdup(name), size);
      //XYZ printf("  PATH+:   %s %s %s, %d\n", module->Name(), id->Name(), name, size);
      path->Prepend(path_reg);
      concat->InsertFirst(ref(id));
      total += size;
    }
  }

  if (total > 0) {

    VeriExpression *expr = new VeriConcat(concat);

    const char* name = getUnusedName(module, "_CONCAT", 1);
    VeriIdDef* signal = addSignal(module, name, total);

    addAssign(module, signal, expr);

    ScanPath* path_extra = new ScanPath((char*) "_SCAN", Globals::GetWidth());
    path->Prepend(path_extra);

    return signal;

  } else {
    
    return NULL;
  }
}

VeriStatement* addCondition(VeriModule* module, VeriExpression* condition, VeriStatement* body)
{
  if (body) {
    VeriConditionalStatement* stmt;
    if (body->GetClassId() == ID_VERICONDITIONALSTATEMENT) {
      VeriConditionalStatement* cond_stmt = static_cast<VeriConditionalStatement*>(body);
      VeriExpression* cond      = cond_stmt->GetIfExpr();
      VeriStatement*  then_stmt = cond_stmt->GetThenStmt();
      VeriStatement*  else_stmt = cond_stmt->GetElseStmt();

      stmt = new VeriConditionalStatement(cond, addCondition(module, condition, then_stmt), addCondition(module, condition, else_stmt));
    } else { 
      stmt = new VeriConditionalStatement(condition, body, NULL);
    }
    return stmt;
  }
  return NULL;
}

void addStateChain(VeriModule* module, std::string *prefix, boost::regex filter, ScanPath* path, unsigned* includes_all, int width)
{

  //Add ports scan_in, scan_out, scan_mode;

  char*      scan_width_name = (char*) getUnusedName(module, "_WIDTH");
  VeriIdDef* scan_width      = module->AddParameter(scan_width_name, 0 /* data type */, new VeriIntVal(width) /* initial value */) ;
  Globals::SetWidth(ref(scan_width));

  char*      scan_in_name   = (char*) getUnusedName(module, "_IN");
  VeriIdDef* scan_in        = addPort(module, scan_in_name, VERI_INPUT, Globals::GetWidth());

  char*      scan_out_name  = (char*) getUnusedName(module, "_OUT");
  VeriIdDef* scan_out       = addPort(module, scan_out_name, VERI_OUTPUT, Globals::GetWidth());

  char*      scan_mode_name = (char*) getUnusedName(module, "_MODE");
  VeriIdDef* scan_mode      = addPort(module, scan_mode_name, VERI_INPUT, 1);

  VeriIdDef* scan_internal  = scan_in;
  
  unsigned   before         = scannedBefore(module);
  VeriIdDef* scan_any       = getScanAny(module);
  VeriIdDef* scan_uclock    = getUClock(module); // needed for side effects.

  addAttribute(module, "SCANNED_BEFORE", mkExpression(1));

  tIdSet all_ids;
  VeriEventControlStatement *stmt;
  foreachAlways(module, stmt) {
    if (isClocked(stmt)) {
      VeriStatement* body_orig = stmt->GetStmt() ;
      VeriStatement* body = Copy(body_orig);
      VeriStatement* block;

      if (!before) {
	VeriExpression* cond_run           = new VeriUnaryOperator(VERI_LOGNOT, ref(scan_any));
	VeriStatement* run_stmt = addCondition(module, cond_run, body);
	Array *stmts = new Array();
	stmts->Insert(run_stmt);
	block = new VeriSeqBlock(0,0,stmts,0);
	block->Resolve(module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV);
 	stmt->ReplaceChildStmt(body_orig, block);
      }

      tIdSet id_set = getAssignmentIds(body);
      foreachSetId(id_set, it) {
	VeriIdDef* id = (VeriIdDef*) *it;
	//XYZ printf("Adding: %s\n", id->Name());
	all_ids.insert(id);
      }

//      markScanClocks(module, stmt);

    }
  }

  ScanPath* path_extra = new ScanPath;
  VeriIdDef* def = createConcatSignal(module, all_ids, prefix, filter, path_extra);

  path->Prepend(path_extra);

  if (def) {
//    char* expr_name                 = (char*) getUnusedName(module, "_LENGTH");
//    VeriIdDef* scan_length    = addSignal(module, expr_name, 32);
    addSampleReg(module, def,  &scan_internal, scan_mode, NULL, "sample_state", width);
  }

  connectScanInstances(module, &scan_internal, scan_mode, path);

  connectScanOuput(module, scan_internal, scan_out);

  char*      scan_length_name  = (char*) getUnusedName(module, "_LENGTH");
  VeriIdDef* scan_length       = addPort(module, scan_length_name, VERI_OUTPUT, 32);

  connectScanCount(module, path, scan_width, scan_length);

  addAttribute(module, "SCAN_LENGTH", scan_length_name);
  addAttribute(module, "SCAN_IN",    scan_in_name);
  addAttribute(module, "SCAN_MODE",  scan_mode_name);
  addAttribute(module, "SCAN_OUT",   scan_out_name);
  addAttribute(module, "SCAN_WIDTH", scan_width_name);

}

void addSampleReg(VeriModule* module, VeriIdDef* signal, VeriIdDef** scan_internal, VeriIdDef* scan_mode, VeriIdDef* scan_length, const char* name, int width)
{

  VeriIdDef* scan_uclock = getUClock(module);
  VeriIdDef* scan_any    = getScanAny(module);

  Array* param_values = new Array();

  VeriExpression* newparam = new VeriPortConnect((char*) Copy("SCAN_WIDTH"), Globals::GetWidth());
  param_values->InsertLast(newparam);

  VeriExpression* newparam2 = new VeriPortConnect((char*) Copy("width"), mkExpression(getSignalSize(signal)));

  param_values->InsertLast(newparam2);

  const char* inst_name = getUnusedName(module, name, false);
  VeriModuleInstantiation* module_inst = addInstance(module, inst_name, "SampleReg", param_values);

  VeriModule* module_def = module_inst->GetInstantiatedModule();

  if (!module_def) {
    fprintf(stderr, "Unable to resolve `SampleReg'. Exiting.\n");
    exit(1);
  }

  VeriInstId* inst;
  foreachInstId(module_inst, inst) {

    inst->AddPortRef("SCAN_IN", ref(*scan_internal), module->GetScope());
    inst->AddPortRef("SCAN_MODE", ref(scan_mode), module->GetScope());
    if (scan_length) {
      inst->AddPortRef("SCAN_LENGTH", ref(scan_length), module->GetScope());
    } else {
      inst->AddPortRef("SCAN_LENGTH", NULL, module->GetScope());
    }
    inst->AddPortRef("SCAN_ANY", ref(scan_any), module->GetScope());
    inst->AddPortRef("D_IN", ref(signal), module->GetScope());
    inst->AddPortRef("CLK",  ref(scan_uclock), module->GetScope());

    const char* scan_next_name = getUnusedName(module, "");
    VeriIdDef*  scan_next      = addSignal(module, scan_next_name, Globals::GetWidth());
    *scan_internal = scan_next;

    inst->AddPortRef("SCAN_OUT", ref(*scan_internal), module->GetScope());


  }
}
