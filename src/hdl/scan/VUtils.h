
#include "V.h"

#include "utils.h"
#include "Dbg.h"
#include <set>

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif



typedef std::set<VeriModuleItem*> tItemSet;
typedef std::set<VeriIdDef*> tIdSet;

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------



#define foreachModule(MODULE) MapIter TOKENPASTE(_M_, __LINE__) ; FOREACH_VERILOG_MODULE( TOKENPASTE(_M_, __LINE__), MODULE) 

#define foreachModuleInstance(MODULE,INST) InstantiationVisitor TOKENPASTE(_I_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_I_, __LINE__) ); SetIter TOKENPASTE(_SI_, __LINE__); FOREACH_SET_ITEM( &(TOKENPASTE(_I_, __LINE__)._module_insts), TOKENPASTE(_SI_, __LINE__), &( INST ))

#define foreachInstId(MODULE,INST) InstIdVisitor TOKENPASTE(_I_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_I_, __LINE__) ); SetIter TOKENPASTE(_SI_, __LINE__); FOREACH_SET_ITEM( &(TOKENPASTE(_I_, __LINE__)._instids), TOKENPASTE(_SI_, __LINE__), &( INST ))

#define foreachDataDecl(MODULE,DECL) DeclVisitor TOKENPASTE(_I_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_I_, __LINE__) ); SetIter TOKENPASTE(_SI_, __LINE__); FOREACH_SET_ITEM( &(TOKENPASTE(_I_, __LINE__)._decls), TOKENPASTE(_SI_, __LINE__), &( DECL ))

#define foreachAlways(MODULE, STMT) EventControlVisitor TOKENPASTE(_E_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_E_, __LINE__) ); unsigned TOKENPASTE(_I_, __LINE__); FOREACH_ARRAY_ITEM( &(TOKENPASTE(_E_, __LINE__)._event_control_stmts), TOKENPASTE(_I_, __LINE__), STMT )

#define foreachSetItem(SET, ITER)  tItemSet::iterator ITER; for( ITER = (SET).begin(); ITER != (SET).end(); (ITER)++ )

#define foreachSetId(SET, ITER)  tIdSet::iterator ITER; for( ITER = (SET).begin(); ITER != (SET).end(); (ITER)++ )

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

// A visitor pattern, which will find the target names in instantiations in a parse tree
// Collect all instantiation targets from Blocking and Non-Blocking instantiations :
class InstantiationVisitor : public VeriVisitor
{
public :
    InstantiationVisitor() : VeriVisitor(), _module_insts(POINTER_HASH) { } ;
    ~InstantiationVisitor() {} ;

    // Collect instantiations :
  virtual void Visit(VeriModuleInstantiation &node) {
        _module_insts.Insert(&node) ;  
    }

public :
    Set _module_insts ; 
} ;

class DeclVisitor : public VeriVisitor
{
public :
    DeclVisitor() : VeriVisitor(), _decls(POINTER_HASH) { } ;
    ~DeclVisitor() {} ;

    virtual void Visit(VeriDataDecl &node) {
      _decls.Insert(&node) ;  
    }

public :
    Set _decls ; 
} ;

class InstIdVisitor : public VeriVisitor
{
public :
    InstIdVisitor() : VeriVisitor(), _instids(POINTER_HASH) { } ;
    ~InstIdVisitor() {} ;

    virtual void Visit(VeriInstId &node) {
      _instids.Insert(&node) ;  
    }

public :
    Set _instids ; 
} ;

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

// A visitor pattern, which will find the event control statements in a parse tree :
// VeriVisitor is a general purpose Verific Verilog parse tree traversal pattern.
// It is fine tuned here to collect event control statements inside always constructs
// (skip initial blocks) :
class EventControlVisitor : public VeriVisitor
{
public :
    EventControlVisitor() : VeriVisitor(), _event_control_stmts() { } ;
    ~EventControlVisitor() {} ;

    virtual void Visit(VeriInitialConstruct &node) {
      if (Dbg::Mode()) {node.Info("skipping this initial block");}
    }

    // Collect event control statements :
    virtual void Visit(VeriEventControlStatement &node) {
      if (Dbg::Mode()) {node.Info("visiting this event control statement");}
        _event_control_stmts.InsertLast(&node) ;
    }

public :
    Array _event_control_stmts ; // array of VeriEventControlStatement *'s, collected by this visitor
} ;

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

// A visitor pattern, which will find the target names in instantiations in a parse tree
// Collect all instantiation targets from Blocking and Non-Blocking instantiations :
class ContinuousAssignVisitor : public VeriVisitor
{
public :
    ContinuousAssignVisitor() : VeriVisitor(), _assigns() { } ;
    ~ContinuousAssignVisitor() {} ;

    // Collect instantiations :
  virtual void Visit(VeriContinuousAssign &node) {
        _assigns.insert(&node) ;  
    }

public :
  tItemSet _assigns ; 
} ;

class NonBlockingAssignVisitor : public VeriVisitor
{
public :
    NonBlockingAssignVisitor() : VeriVisitor(), _assigns() { } ;
    ~NonBlockingAssignVisitor() {} ;

    // Collect instantiations :
  virtual void Visit(VeriNonBlockingAssign &node) {
        _assigns.insert(&node) ;  
    }

public :
  tItemSet _assigns ; 
} ;

class BlockingAssignVisitor : public VeriVisitor
{
public :
    BlockingAssignVisitor() : VeriVisitor(), _assigns() { } ;
    ~BlockingAssignVisitor() {} ;

    // Collect instantiations :
  virtual void Visit(VeriBlockingAssign &node) {
        _assigns.insert(&node) ;  
    }

public :
  tItemSet _assigns ; 
} ;

// A visitor patteren, which will find the target ids in assignments in a parse tree
// Collect all assignment targets from Blocking and Non-Blocking assignments :
class AssignmentIdVisitor : public VeriVisitor
{
public :
    AssignmentIdVisitor() : VeriVisitor(), _assigned_ids() { } ;
    ~AssignmentIdVisitor() {} ;

  /*   // Collect blocking assignments : */
/*     virtual void Visit(VeriBlockingAssign &node) { */
/*         // Go to the assigned target (expression) : */
/*         VeriExpression *target = node.GetLVal() ; */
/*         // Find the assigned identifier(s) : */
/*         VeriIdDef *target_id = (target) ? target->GetId() : 0 ; */
/*         if (target_id) _assigned_ids.insert(target_id) ; */
/*     } */
    // Collect non-blocking assignments :
    virtual void Visit(VeriNonBlockingAssign &node) {
        // Go to the assigned target (expression) :
        VeriExpression *target = node.GetLVal() ;
        // Find the assigned identifier(s) :
        VeriIdDef *target_id = (target) ? target->GetId() : 0 ;
        if (target_id) _assigned_ids.insert(target_id) ;
    }

public :
  tIdSet _assigned_ids;
} ;


// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

Set*                  collectModules (VeriModule *module);
void                  addAssign(VeriModule* module, VeriIdDef* id, VeriExpression* expr);
VeriContinuousAssign* getAssign(VeriModule* module, VeriIdDef* id);
tItemSet              getContinuousAssigns(VeriTreeNode* node);
tItemSet              getBlockingAssigns(VeriTreeNode* node);
tItemSet              getNonBlockingAssigns(VeriTreeNode* node);
tIdSet                getAssignmentIds(VeriTreeNode* node);
VeriExpression*       mkExpression(const char* value);
VeriExpression*       mkExpression(int value);
VeriContinuousAssign* mkContinuousAssign(VeriModule* module, VeriExpression* lhs, VeriExpression* rhs);
const char*           getAttribute(VeriTreeNode* node, const char *attr_name);
tStringSet            getAttributes(VeriTreeNode* node, const char *attr_name);
void                  addAttribute(VeriTreeNode* node, const char *attr_name, VeriExpression* expr);
void                  addAttribute(VeriTreeNode* node, const char *attr_name, const char* value);
void                  addAttribute(VeriTreeNode* node, const char *attr_name, tStringSet* set);
void                  pushAttribute(VeriTreeNode* node, const char *attr_name, const char* value);
void                  removeAttribute(VeriTreeNode* node, const char *attr_name);
const char*           getUnusedName(VeriModule* module, const char* name, bool add_prefix);
const char*           getUnusedName(VeriModule* module, const char* name);
const char*           getModuleNameBase(VeriModule* module);
VeriModule*           findModule(const char* name);
std::string           createParamsString(Map* params_map);
std::string           createParamsString(VeriModule* module);
const char*           getModuleNamePrefix(VeriModule* module);
const char* createNewModuleName(VeriModule *module);
VeriModule* moduleCopyWithNewName(VeriModule *module_orig);
VeriModule* moduleCopyWithNewName(VeriModule *module_orig, const char* name_new);
const char* getInstanceName (VeriModuleInstantiation *module_inst);
void        removeModule (VeriModule* module);
void        setInstanceModule(VeriModuleInstantiation *module_inst, VeriModule *module);
void        setInstanceModule( VeriInstId* inst_id, VeriModule *module);
Map*        createParamsMap(VeriModuleInstantiation *module_inst);
void        setParameters(VeriModule* module, Map* params_map);
void        addBaseNames (VeriModule* module);
VeriModule* renameAllModules (VeriModule* module);
VeriStatement*  Copy(VeriStatement* x);
VeriExpression* Copy(VeriExpression* x);
const char*     Copy(const char* value);
char*           Copy(char* value);
unsigned    isClocked (VeriEventControlStatement *event_control_stmt);
VeriSeqBlock*         addStatement(VeriSeqBlock* block_orig, VeriStatement* stmt);
VeriIdDef*            addSignal(VeriModule* module, const char* name, VeriExpression* size_expr, unsigned type = VERI_WIRE);
VeriIdDef*            addSignal(VeriModule* module, const char* name, int size, unsigned type = VERI_WIRE);
VeriIdDef*            addPort(VeriModule* module, const char* name, unsigned port_direction, VeriExpression* size_expr, unsigned type = VERI_WIRE);
VeriIdDef*            addPort(VeriModule* module, const char* name, unsigned port_direction, int size, unsigned type = VERI_WIRE);
VeriIdDef*            getOrAddPort(VeriModule* module, const char* name, bool add_prefix, unsigned port_direction, VeriExpression* size_expr, unsigned type = VERI_WIRE);
VeriIdDef*            getOrAddPort(VeriModule* module, const char* name, bool add_prefix, unsigned port_direction, int size, unsigned type = VERI_WIRE);
VeriIdRef*            ref(VeriIdDef* id);
VeriModuleInstantiation* addInstance(VeriModule* module, const char* inst_name, const char* module_name, Array* param_values);
VeriModuleInstantiation* addInstance(VeriModule* module, const char* inst_name, const char* module_name);
VeriModule*              getModuleOrig(VeriModule* module);
VeriExpression*          createConcat(VeriModule* module, tStringList ids);
VeriIdDef*               createConcatSignal(VeriModule* module, tStringList ids);
tStringList              createInputList(VeriModule* module);
int                      getSignalSize(VeriIdDef* id);
//int                      getSignalSize(VeriExpression* expr);
VeriIdDef*               addConnection(VeriModule* module, VeriInstId* inst_0,  VeriInstId* inst_1, const char* formal_0, const char* formal_1, const char* cname = "");

const char* GetOrigName (VeriModule *module);
unsigned HasPort(VeriModule *module, const char *port_name);

