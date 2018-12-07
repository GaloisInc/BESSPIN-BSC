#include <stdio.h>
#include <stdarg.h>

#include <iostream>         // cout
using std::cout ;

#include "string.h"

#include "VUtils.h"
#include "V.h"

#include <boost/regex.hpp> // for exit
#include "Globals.h"

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

void collectModulesInternal (VeriModule* module, Set* mods)
{

  if (!module) {
    return;
  }

  VeriModule* saved = static_cast<class  VeriModule*>(mods->Get(module));
  if (saved) {
    return;
  }

  VeriModuleInstantiation *module_inst;
  foreachModuleInstance(module, module_inst) {
    VeriModule *module_def = module_inst->GetInstantiatedModule();
    if(module_def) {
      collectModulesInternal(module_def, mods);
    }
  }

  mods->Insert(module);

}

Set* collectModules (VeriModule* module)
{

  Set* mods = new Set(POINTER_HASH, 2);
  collectModulesInternal (module, mods);

  return mods;
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

void addAssign(VeriModule* module, VeriIdDef* id, VeriExpression* expr)
{

  VeriNetRegAssign *assign = new VeriNetRegAssign (new VeriIdRef(id), expr);
  Array* net_assign = new Array(1);
  net_assign->InsertLast(assign);
  VeriContinuousAssign  *continuous_assignment = new  VeriContinuousAssign(0, 0, net_assign) ;
  continuous_assignment->Resolve(module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV) ;
  module->AddModuleItem(continuous_assignment);

}

tItemSet getContinuousAssigns(VeriTreeNode* node)
{
  ContinuousAssignVisitor assign_visitor ;
  node->Accept( assign_visitor ) ;
  
  return assign_visitor._assigns;
}

tItemSet getBlockingAssigns(VeriTreeNode* node)
{
  BlockingAssignVisitor assign_visitor ;
  node->Accept( assign_visitor ) ;
  
  return assign_visitor._assigns;
}

tItemSet getNonBlockingAssigns(VeriTreeNode* node)
{
  NonBlockingAssignVisitor assign_visitor ;
  node->Accept( assign_visitor ) ;
  
  return assign_visitor._assigns;
}

tIdSet getAssignmentIds(VeriTreeNode* node)
{
  AssignmentIdVisitor assign_visitor ;
  node->Accept( assign_visitor ) ;
  
  return assign_visitor._assigned_ids;
}


VeriContinuousAssign* getAssign(VeriModule* module, VeriIdDef* id)
{
  ContinuousAssignVisitor assign_visitor ;
  module->Accept( assign_visitor ) ;

  tItemSet::iterator it;
  for( it = assign_visitor._assigns.begin(); it != assign_visitor._assigns.end(); it++ ) {
    VeriContinuousAssign* assign = (VeriContinuousAssign*) *it;

    unsigned i ;
    VeriNetRegAssign *nr_assign;
    FOREACH_ARRAY_ITEM(assign->GetNetAssigns(), i, nr_assign) {
      VeriExpression* lhs = nr_assign->GetLValExpr();
      if (lhs->IsIdRef()) {
	VeriIdDef* lhs_id = lhs->GetId();
	if (lhs_id == id) {
	  return assign;
	}
      }
    }
  }

  return 0;

}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

VeriExpression* mkExpression(const char* value)
{
  VeriExpression* expr = new VeriConstVal(value, VERI_STRING);
  return expr;
}

VeriExpression* mkExpression(int value)
{
  VeriExpression* expr = new VeriIntVal(value);
  return expr;
}

VeriContinuousAssign* mkContinuousAssign(VeriModule* module, VeriExpression* lhs, VeriExpression* rhs)
{
  Array *assign_list = new Array (1) ; 
  VeriNetRegAssign *assign = new VeriNetRegAssign(lhs, rhs);
  assign_list->InsertLast(assign);

  VeriContinuousAssign  *continuous_assignment = new  VeriContinuousAssign(0, 0, assign_list) ;
  // Resolve reference, links VeriIdRef to VeriIdDef
  continuous_assignment->Resolve(module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV) ;
  return continuous_assignment;
}


// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

const char* toString(tStringSet* set)
{

  std::string out ="";
  unsigned first = 1;
  foreachInStringSet(set, it) {
    char* item = (char*) (*it).c_str();
    if (!first) out += ",";
    out += item;
    first = 0;
  }
  return out.c_str();
}

tStringSet fromString(const char* value)
{
  tStringSet out;
  if (!value) {
    return out;
  }
  char* current = strdup(value);
  unsigned first = 1;
  while (1)
    {
      char* last = strchr(current, ',');
      if (last) {
	first =0;
	int i = 0;
	while (1) {
	  if (current[i] == ',') {
	    current[i] = 0;
	    break;
	  }
	  i++;
	}
	if (strlen(current) == 0) {
	  fprintf(stderr, "%s: ill-formed attribute expression '%s' exiting\n", 
		  Globals::GetExecName(), value);
		
	  exit(1);
	}
	out.insert(current);
	current = last + 1;
      } else {
	if ((strlen(current) > 0) || !first) {
	  out.insert(current);
	}
	break;
      }
    }
  return out;
}

const char* getAttribute(VeriTreeNode* node, const char *attr_name)
{

  VeriExpression* expr = node->GetAttribute(attr_name);
  //XYZ printf("FOUND ATTR: %s\n", attr_name);

  if (expr) {
    if (expr->IsConst()) {
      VeriConstVal* constant_value = static_cast<VeriConstVal*>(expr);
      char* value_with_quotes      = constant_value->Image();
      char* value                  = removeQuotes(value_with_quotes);
      //XYZ printf("VALUE: %s\n", value);
      return value;
    } else {
      fprintf(stderr, "getAttribute: unexpected error.\n");
      exit(1);
    }
  }
  return NULL;
}

tStringSet getAttributes(VeriTreeNode* node, const char *attr_name)
{
  const char* value = getAttribute(node, attr_name);
  return fromString(value);
}

void addAttribute(VeriTreeNode* node, const char *attr_name, VeriExpression* expr)
{
  Map* map = new Map(STRING_HASH, 1);
  map->Insert(strdup(attr_name), expr, 1, 0);
  node->AddAttributes(map);
}

void addAttribute(VeriTreeNode* node, const char *attr_name, const char* value)
{

  //  printf("ADDING ATTR: %s %s\n", attr_name, value);
  VeriExpression* expr = new VeriConstVal(strdup(value), VERI_STRING);
  return addAttribute(node, attr_name, expr);
}

void addAttribute(VeriTreeNode* node, const char *attr_name, tStringSet* set)
{
  const char* value = toString(set);
  return addAttribute(node, attr_name, value);
  //  const char* value2 = getAttribute(node, attr_name);
  //  printf("D0 %d %s %s %s %s\n", set->size(), ((VeriModule*) node)->Name(), attr_name, value, value2);
}

void removeAttribute(VeriTreeNode* node, const char *attr_name)
{
  Map* map = node->GetAttributes();
  map->Remove(attr_name);
}

void pushAttribute(VeriTreeNode* node, const char *attr_name, const char* value)
{
  tStringSet set = getAttributes(node, attr_name);
  set.insert(value);
  addAttribute(node, attr_name, &set);
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------


const char* getUnusedName(VeriModule* module, const char* name, bool add_prefix)
{
  std::string prefix = "";
  if (add_prefix) {
    prefix += Globals::GetPrefix();
  }
  prefix += name;

  VeriIdDef* existing = module->GetScope()->Find(prefix.c_str());
  //XYZ printf("NAME: %s %d\n", prefix.c_str(), (existing != 0));
  if (existing) {
    unsigned i = 1;
    while (1) {
      std::string name_new = prefix;
      name_new += itoa(i);
      VeriIdDef* existing = module->GetScope()->Find(name_new.c_str());
      if (existing) {
	i++;
      } else {
	return strdup(name_new.c_str());
      }
    }
  }
  return strdup(prefix.c_str());
}

const char* getUnusedName(VeriModule* module, const char* name)
{
  return getUnusedName(module, name, 1);
}

// Returns the first name this module had (before copying/modifications).
const char* getModuleNameBase(VeriModule* module)
{
  VeriExpression* expr_base = module->GetAttribute("nameBase");
  if (!expr_base) {
    const char* name_base = strdup(module->GetName());
    expr_base = new VeriConstVal(name_base, VERI_STRING);
    addAttribute(module, "nameBase", expr_base);
  }

  if (expr_base->IsConst()) {
    VeriConstVal *constant_value = static_cast<VeriConstVal*>(expr_base);
    char* name_with_quotes = constant_value->Image();
    return removeQuotes(name_with_quotes);
  } else {
    fprintf(stderr, "getModuleNameBase: unexpected error.\n");
    exit(1);
  }
}

VeriModule* findModule(const char* name)
{
  MapIter mi ;
  VeriLibrary* lib;
  FOREACH_VERILOG_LIBRARY(mi, lib) {
    break;
  }
  if (lib) {
    return lib->GetModuleAnywhere(name, 1);
  }

  return NULL;
}

std::string createParamsString(Map* params_map)
{
 std::string out;
 if (!params_map) {
   return out;
 }

 MapIter miter;
 char* name;
 VeriExpression* value;
 FOREACH_MAP_ITEM(params_map, miter, &name, &value) {

   if (value->IsConst()) {
     VeriConstVal *constant_value = static_cast<VeriConstVal*>(value);
     if (constant_value->IsString()) {
       //       char* value_string = constant_value->Image() ;
       continue;
     }
     out += "_";
     int i = constant_value->Integer();
     if (i == -1) {
       out += "X";
     } else {
       out += itoa(i);
     }
   }
 }
 return out;

}


std::string createParamsString(VeriModule* module)
{
  std::string out;
  Array *params = module->GetParameters();
  unsigned i ;
  VeriParamId *param ;
  FOREACH_ARRAY_ITEM(params, i, param) {
//XYZ    param->PrettyPrint(cout, 0);
//    const char* name      = param->GetName();
    VeriExpression* value = param->GetInitialValue();

    if (value->IsConst()) {
      VeriConstVal *constant_value = static_cast<VeriConstVal*>(value);
      if (constant_value->IsString()) {
	//       char* value_string = constant_value->Image() ;
	continue;
      }
      out += "_";
      int i = constant_value->Integer();
      if (i == -1) {
	out += "X";
      } else {
	out += itoa(i);
      }
    }
  }

  return out;
}

const char* getModuleNamePrefix(VeriModule* module)
{
  VeriExpression* expr_elab = module->GetAttribute("nameElab");
  if (!expr_elab) {
    std::string name_base = getModuleNameBase(module);
    std::string name_new = name_base;
    std::string params   = createParamsString(module);
    name_new += params;
    name_new += "_PP";
    expr_elab = new VeriConstVal(name_new.c_str(), VERI_STRING);
    addAttribute(module, "nameElab", expr_elab);
  }

  if (expr_elab->IsConst()) {
    VeriConstVal *constant_value = static_cast<VeriConstVal*>(expr_elab);
    char* name_with_quotes = constant_value->Image();
    return removeQuotes(name_with_quotes);
  } else {
    fprintf(stderr, "getModuleNamePrefix: unexpected error.\n");
    exit(1);
  }
}

const char* createNewModuleName(VeriModule* module)
{
  unsigned i = 0;
  while (1) {
    std::string name_new = getModuleNamePrefix(module);
    name_new += itoa(i);
    VeriModule* existing = findModule(name_new.c_str());
    if (existing) {
      i++;
    } else {
      return strdup(name_new.c_str());
    }
  }
}

VeriModule* moduleCopyWithNewName(VeriModule *module_orig)
{
  const char* name_new = createNewModuleName(module_orig);

  //XYZ printf("NEW NAME %s\n", name_new);

  return moduleCopyWithNewName(module_orig, name_new);
}

VeriModule* moduleCopyWithNewName(VeriModule *module_orig, const char* name_new)
{
	
  VeriMapForCopy id_map_table ; // need this for copy routine
  VeriModuleItem *item_orig = static_cast<class VeriModuleItem*>(module_orig);
  VeriModuleItem *item_new  = item_orig->CopyWithName(name_new, id_map_table, 1) ;
  VeriModule *module = static_cast<class VeriModule*>(item_new);
  if (module_orig->IsStaticElaborated()) {
    module->SetStaticElaborated();
  }
  return module;
}

const char* getInstanceName (VeriModuleInstantiation *module_inst)
{
  Array *inst_arr = module_inst->GetIds();
  VeriIdDef *inst_id ;
  unsigned j ;
  const char *inst_name ;
  FOREACH_ARRAY_ITEM(inst_arr, j,inst_id) { // Iterate through the instance array
    if (!inst_id) continue ;
    inst_name = inst_id->GetName() ; // Get the name of the instantiation
    break ;
  }
  return inst_name;
}

void removeModule (VeriModule* module)
{
  veri_file::RemoveModule(module->GetName(), module->GetLibrary()->GetName());
}

void setInstanceModule( VeriInstId* inst_id, VeriModule *module)
{
  VeriModuleInstantiation* module_inst = inst_id->GetModuleInstance();
  if (module_inst) {
    VeriName *module_name_ref = module_inst->GetModuleNameRef() ;
    if (module_name_ref) {
      // Below is equivalent to: inst->SetModuleName(module->GetName());
      module_name_ref->SetName(Strings::save(module->GetName()));
      // Set the id too:
      module_name_ref->SetId(module->GetId()) ;
    }
  }
}

void setInstanceModule(VeriModuleInstantiation *module_inst, VeriModule *module)
{
  VeriName *module_name_ref = module_inst->GetModuleNameRef() ;
  if (module_name_ref) {
    // Below is equivalent to: inst->SetModuleName(module->GetName());
    module_name_ref->SetName(Strings::save(module->GetName()));
    // Set the id too:
    module_name_ref->SetId(module->GetId()) ;
  }
}


Map* createParamsMap (VeriModuleInstantiation *module_inst)
{

  VeriModule *mod = module_inst->GetInstantiatedModule();
  Array *actuals = (module_inst) ? module_inst->GetParamValues() : 0 ;
  if (!actuals) {
    return NULL;
  }

  Map *actual_params = new Map(STRING_HASH_CASE_INSENSITIVE, actuals->Size());

  const char *name ;
  VeriExpression *actual ;

  unsigned i ;
  VeriExpression *expr ;
  FOREACH_ARRAY_ITEM(actuals, i, expr) {
      if (!expr) continue ;

      actual = expr->GetConnection() ; // The actual expression
      name = expr->NamedFormal() ; // Formal name for named connection
      if (!name) {
          // Positional connection, get parameter at pos "i":
          VeriIdDef *param = mod->GetParamAt(i) ;
          name = (param) ? param->Name() : 0 ;
      } else {
          // May want check for validity of this parameter using
          // mod->GetParam(name)
      }
      if (!name) {
          // Too many actuals: may want to error out
          continue ;
      }
      // Insert the pair into the map:
      actual_params->Insert(Strings::save(name), actual) ;
  }

  return actual_params;
}

void setParameters(VeriModule* module, Map* params_map)
{

  Array *params = module->GetParameters();
  unsigned i ;
  VeriParamId *param ;
  if (params_map) {
    FOREACH_ARRAY_ITEM(params, i, param) {
      //    param->PrettyPrint(cout, 0);
      const char* name      = param->GetName();
      VeriExpression* value = static_cast<class  VeriExpression*>(params_map->GetValue(name));
      if (value) {
	//XYZ printf("Setting %s %s\n", module->Name(), name);
	param->SetInitialValue(value);
      } else {
      }
    }
  }
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

void addBaseNames (VeriModule* module)
{
  getModuleNameBase(module);
  VeriModuleInstantiation *module_inst;
  foreachModuleInstance(module, module_inst) {

    VeriModule *module_def = module_inst->GetInstantiatedModule();
    if (module_def) {
      addBaseNames(module_def);
    }
  }
}

VeriModule* renameAllModules (VeriModule* module)
{
  VeriModuleInstantiation *module_inst;
  foreachModuleInstance(module, module_inst) {
    VeriModule *module_def = module_inst->GetInstantiatedModule();
    if (!module_def) {
      continue;
    }
    VeriModule* xx = renameAllModules(module_def);
    setInstanceModule(module_inst, xx);
  }

  const char* prefix = getModuleNamePrefix(module);
  std::string name = prefix;
  name += "0";
  VeriModule* module_renamed = findModule(name.c_str());
  //XYZ printf("A0\n");
  if (!module_renamed) {
    //XYZ printf("A1\n");
    module_renamed = moduleCopyWithNewName(module);
  }
  //XYZ printf("A2\n");
  //XYZ printf("A3 %s\n", module_renamed->Name());

  return module_renamed;
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

unsigned isClocked(VeriEventControlStatement *event_control_stmt)
{
  // Look at the sensitivity list (the "@()" clause), and check if it has 'edge' expressions :
  Array *sens_list = event_control_stmt->GetAt() ;
  unsigned it_0, is_clocked = 0 ;
  VeriExpression *event_expr ;
  FOREACH_ARRAY_ITEM(sens_list, it_0, event_expr) {
    if (!event_expr) break ; // invalid sensitivity list
    if (event_expr->IsEdge(0/*any edge (pos or neg)*/)) {
      is_clocked = 1 ;
      break ; // no need to check the other sensitivity list items. This statement is clocked !
    }
  }

  return is_clocked;
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

VeriStatement* Copy(VeriStatement* x) {
  VeriMapForCopy id_map_table ; // need this for copy routine
  VeriStatement* cp = x->CopyStatement(id_map_table);
  return cp;
}


VeriExpression* Copy(VeriExpression* x) {
  VeriMapForCopy id_map_table ; // need this for copy routine
  VeriExpression* cp = x->CopyExpression(id_map_table);
  return cp;
}

const char* Copy(const char* value)
{
  return Strings::save(value);
}

char* Copy(char* value)
{
  return Strings::save(value);
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

VeriSeqBlock* addStatement(VeriSeqBlock* block_orig, VeriStatement* stmt)
{
  Array* stmts = block_orig->GetStatements();
  stmts->Insert(stmt);
  VeriSeqBlock* block = new VeriSeqBlock(block_orig->GetLabel(),
					 block_orig->GetDeclItems(),
					 stmts,
					 block_orig->GetScope());
  return block;
					 
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

VeriIdDef* addSignal(VeriModule* module, const char* name, VeriExpression* size_expr, unsigned type)
{

  VeriRange* range = NULL;
  int size = 2;

  if (size_expr->IsConst()) {
    VeriIntVal* constant_value = static_cast<VeriIntVal*>(size_expr);
    size = constant_value->Integer();
  }

  if (size > 1) {
    VeriExpression* msb_expr = new VeriBinaryOperator(VERI_MIN,
						      Copy(size_expr),
						      mkExpression(1));
    range = new VeriRange(msb_expr, mkExpression(0));
  }

  VeriIdDef* id = module->AddSignal(strdup(name), new VeriDataType(type, 0, range), 0);
  return id;
}

VeriIdDef* addSignal(VeriModule* module, const char* name, int size, unsigned type)
{
  return addSignal(module, name, mkExpression(size), type);
}

VeriIdDef* addPort(VeriModule* module, const char* name, unsigned port_direction, VeriExpression* size_expr, unsigned type)
{

  VeriRange* range = NULL;
  int size = 2;

  if (size_expr->IsConst()) {
    VeriIntVal* constant_value = static_cast<VeriIntVal*>(size_expr);
    size = constant_value->Integer();
  }

  if (size > 1) {
    VeriExpression* msb_expr = new VeriBinaryOperator(VERI_MIN,
						      Copy(size_expr),
						      mkExpression(1));
    range = new VeriRange(msb_expr, mkExpression(0));
  }

  //XYZ printf("ADDING PORT: %s %s\n", module->Name(), name);
  VeriIdDef* id = module->AddPort(strdup(name), port_direction, new VeriDataType(type, 0, range));
  return id;
}

VeriIdDef* addPort(VeriModule* module, const char* name, unsigned port_direction, int size, unsigned type)
{
  return addPort(module, name, port_direction, mkExpression(size), type);
}

VeriIdRef* ref(VeriIdDef* id)
{

  return (new VeriIdRef(id));

}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

VeriIdDef* getOrAddPort(VeriModule* module, const char* name, bool add_prefix, unsigned port_direction, VeriExpression* size_expr, unsigned type)
{

  const char* aname = getAttribute(module, name);

  if (aname) {
    VeriIdDef* id = module->GetScope()->Find(aname);
    if (id) {
      return id;
    }

    fprintf(stderr, "getOrAddInput: unable to resolve net named '%s'.\n", aname);
    exit(1);

  } else {
    aname = (char*) getUnusedName(module, name, add_prefix);
    VeriIdDef* def = addPort(module, aname, port_direction, size_expr, type);

    addAttribute(module, name, aname);
    return def;

  }
}

VeriIdDef* getOrAddPort(VeriModule* module, const char* name, bool add_prefix,  unsigned port_direction, int size, unsigned type)
{
  return getOrAddPort(module, name, add_prefix, port_direction, mkExpression(size), type);
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

VeriModuleInstantiation* addInstance(VeriModule* module, const char* inst_name, const char* module_name, Array* param_values)
{
  VeriModuleInstantiation *module_inst;
  VeriInstId *inst = new VeriInstId(Strings::save(inst_name), NULL, NULL);
  VeriIdRef *instantiated_module =  new VeriIdRef(Strings::save(module_name));

  module_inst = new VeriModuleInstantiation(instantiated_module, 0, param_values, inst, NULL);

  if (module_inst) {
    module->AddModuleItem(module_inst);
    return module_inst;
  } else {
    fprintf(stderr, "addinstance: unexpected error.\n");
    exit(1);
  }
}

VeriModuleInstantiation* addInstance(VeriModule* module, const char* inst_name, const char* module_name)
{
  return addInstance(module, inst_name, module_name, NULL);
}


// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

VeriModule* getModuleOrig(VeriModule* module)
{

  std::string name_base = getModuleNameBase(module);
  return findModule(name_base.c_str());

}

VeriExpression* createConcat(VeriModule* module, tStringList ids)
{

  Array *concat = new Array() ;
  foreachInStringList(&ids, it) {
    VeriIdDef* id = module->GetScope()->Find((*it).c_str());
    if (id) {
       concat->InsertLast(ref(id));
    } else {
      fprintf(stderr, "createConcat: unable to resolve net named '%s'.\n", (*it).c_str());
      exit(1);
    }
  }

  VeriExpression *expr = new VeriConcat(concat) ;
  return expr;
}

VeriIdDef* createConcatSignal(VeriModule* module, tStringList ids)
{

  Array *concat = new Array() ;
  int size = 0;
  foreachInStringList(&ids, it) {
    VeriIdDef* id = module->GetScope()->Find((*it).c_str());
    if (id) {
      size += getSignalSize(id);
      concat->InsertLast(ref(id));
    } else {
      fprintf(stderr, "createConcatSignal: unable to resolve net named '%s'.\n", (*it).c_str());
      exit(1);
    }
  }

  VeriExpression *expr = new VeriConcat(concat);

  const char* name = getUnusedName(module, "_CONCAT", 1);
  VeriIdDef* signal = addSignal(module, name, size);

  addAssign(module, signal, expr);

  return signal;
}

tStringList createInputList(VeriModule* module)
{
  tStringList concat;

  VeriDataDecl* decl;
  foreachDataDecl(module, decl) {
    if (decl->GetDir() == VERI_INPUT) {
      VeriIdDef* id;
      Array* ids = decl->GetIds();
      unsigned it;
      FOREACH_ARRAY_ITEM(ids, it, id) {
	concat.push_back(id->Name());
      }
    }
  }

  return concat;
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

int getSignalSize(VeriIdDef* id)
{

  if (id->IsMemory() && id->UnPackedDimension() > 1) {
      fprintf(stderr, "getSize: Unhandled case. Array '%s' dimension is > 1. Exiting.\n", id->Name()); 
      exit(1);
  }

  int min = 0, max = 0, size = 0;
  if (id->IsMemory()) {

    VeriRange* r = id->GetDimensionAt(1);

    size = 1;
    if (r) {
      r->StaticReplaceConstantExpr(1);

      min = r->GetLsbOfRange();
      max = r->GetMsbOfRange();

      if (min > max) {
	min = r->GetMsbOfRange();
	max = r->GetLsbOfRange();
      }
      size = max - min + 1;
    }
  } else {

    VeriRange* r = id->GetDimensionAt(0);

    size = 1;
    if (r) {
      
      r->StaticReplaceConstantExpr(1);

      min = r->GetLsbOfRange();
      max = r->GetMsbOfRange();

      if (min > max) {
	min = r->GetMsbOfRange();
	max = r->GetLsbOfRange();
      }
      size = max - min + 1;
    }
  }
  return size;
}


// int getSignalSize(VeriExpression* expr)
// {

//   VeriDataType* dt = expr->GetDataType();

//   if(!dt) {
//     printf("ZZZZZZZZZ no dt\n");
//   }

//   if (dt->UnPackedDimension() > 1) {
//     fprintf(stderr, "getSize: Unhandled case. Array dimension is > 1. Exiting.\n");
//     exit(1);
//   }

//   int min = 0, max = 0, size = 0;
//   if (dt->UnPackedDimension() == 1) {

//     VeriRange* r = dt->GetDimensionAt(1);

//     size = 1;
//     if (r) {
//       r->StaticReplaceConstantExpr(1);

//       min = r->GetLsbOfRange();
//       max = r->GetMsbOfRange();

//       if (min > max) {
// 	min = r->GetMsbOfRange();
// 	max = r->GetLsbOfRange();
//       }
//       size = max - min + 1;
//     }
//   } else {

//     VeriRange* r = dt->GetDimensionAt(0);

//     size = 1;
//     if (r) {
      
//       r->StaticReplaceConstantExpr(1);

//       min = r->GetLsbOfRange();
//       max = r->GetMsbOfRange();

//       if (min > max) {
// 	min = r->GetMsbOfRange();
// 	max = r->GetLsbOfRange();
//       }
//       size = max - min + 1;
//     }
//   }
//   return size;
// }


// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

VeriIdDef* addConnection(VeriModule* module, VeriInstId* inst_0,  VeriInstId* inst_1, 
			 const char* formal_0, const char* formal_1, const char* cname)
{
  VeriModule* module_0 = inst_0->GetModuleInstance()->GetInstantiatedModule();
  VeriModule* module_1 = inst_1->GetModuleInstance()->GetInstantiatedModule();

  VeriIdDef* port_0 = module_0->GetScope()->Find(formal_0);
  VeriIdDef* port_1 = module_1->GetScope()->Find(formal_1);

  if (port_0 && port_1) {

    int size_0 = getSignalSize(port_0);
    int size_1 = getSignalSize(port_1);

    if (size_0 == size_1) {

      const char* name  = getUnusedName(module, cname, 1);
      VeriIdDef* signal;
      signal = addSignal(module, name, size_0);
      
      printf("CONNECTING %s(%s) [%d] with %s(%s) [%d] \n", inst_0->Name(), formal_0, size_0,  inst_1->Name(), formal_1, size_1);

//      module->RemovePortRef(inst_0->Name(), formal_0);
      module->RemovePortRef(inst_1->Name(), formal_1);

      module->AddPortRef(Strings::save(inst_0->Name()), Strings::save(formal_0), ref(signal));
      module->AddPortRef(Strings::save(inst_1->Name()), Strings::save(formal_1), ref(signal));

//      inst_0->AddPortRef(strdup(formal_0), ref(signal), module->GetScope());
//      inst_1->AddPortRef(strdup(formal_1), ref(signal), module->GetScope());

//      inst_0->Resolve(module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV) ;
//      inst_1->Resolve(module->GetScope(), VeriTreeNode::VERI_UNDEF_ENV) ;

      return signal;

    } else {
      fprintf(stderr, "addConnection: Ports %s(%s) size=%d and %s(%s) size=%d do not match in size. Exiting.\n", \
	      inst_0->Name(), formal_0, size_0,  inst_1->Name(), formal_1, size_1);
      exit(1);
    }
  } else {
    fprintf(stderr, "addConnection: Unable to resolve ports %s(%s) and %s(%s). Exiting.\n", \
	    inst_0->Name(), formal_0, inst_1->Name(), formal_1);
    exit(1);

  }
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

const char* GetOrigName (VeriModule* module)
{
  const char* name = module->GetOriginalModuleName();
  return name ? name : module->GetName();
}


unsigned HasPort(VeriModule *module, const char *port_name)
{
  unsigned it;
  VeriIdDef *port;
  FOREACH_ARRAY_ITEM(module->GetPorts(), it, port) {
    if (Strings::compare(port_name, port->GetName())) {
      return 1;
    }
  }
  return 0;
}

// -----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
