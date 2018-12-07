// Copyright 2010 Bluespec Inc. All rights reserved
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

#include "ScanPath.h"

using namespace std;

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif

class Partition;

typedef std::list<VeriInstId*> VeriInstIdList;
typedef std::list<VeriInstId*>::iterator VeriInstIdListIterator;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

#define TOKENPASTEIN(x, y) x ## y
#define TOKENPASTE(x, y) TOKENPASTEIN(x, y)
#define foreachModuleInstance(MODULE,INST) InstantiationVisitor TOKENPASTE(_I_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_I_, __LINE__) ); SetIter TOKENPASTE(_SI_, __LINE__); FOREACH_SET_ITEM( &(TOKENPASTE(_I_, __LINE__)._module_insts), TOKENPASTE(_SI_, __LINE__), &( INST ))

#define foreachAlways(MODULE, STMT) EventControlVisitor TOKENPASTE(_E_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_E_, __LINE__) ); unsigned TOKENPASTE(_I_, __LINE__); FOREACH_ARRAY_ITEM( &(TOKENPASTE(_E_, __LINE__)._event_control_stmts), TOKENPASTE(_I_, __LINE__), STMT )

#define foreachSetId(SET, ITER)  tIdSet::iterator ITER; for( ITER = (SET).begin(); ITER != (SET).end(); (ITER)++ )

#define foreachListId(SET, ITER)  tIdList::iterator ITER; for( ITER = (SET).begin(); ITER != (SET).end(); (ITER)++ )

#define foreachSetRef(SET, ITER)  tRefSet::iterator ITER; for( ITER = (SET).begin(); ITER != (SET).end(); (ITER)++ )

#define foreachSetItem(SET, ITER)  tModuleItemSet::iterator ITER; for( ITER = (SET).begin(); ITER != (SET).end(); (ITER)++ )

#define foreachInstId(MODULE,INST) InstIdVisitor TOKENPASTE(_I_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_I_, __LINE__) ); SetIter TOKENPASTE(_SI_, __LINE__); FOREACH_SET_ITEM( &(TOKENPASTE(_I_, __LINE__)._instids), TOKENPASTE(_SI_, __LINE__), &( INST ))

#define foreachDataDecl(MODULE,DECL) DeclVisitor TOKENPASTE(_I_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_I_, __LINE__) ); SetIter TOKENPASTE(_SI_, __LINE__); FOREACH_SET_ITEM( &(TOKENPASTE(_I_, __LINE__)._decls), TOKENPASTE(_SI_, __LINE__), &( DECL ))
#define foreachContAssign(MODULE,DECL) ContAssignmentVisitor TOKENPASTE(_I_, __LINE__); ( MODULE )->Accept( TOKENPASTE(_I_, __LINE__) );  tStatementSet::iterator ITER; for( ITER = (TOKENPASTE(_I_, __LINE__)._instids).begin(); ITER != (TOKENPASTE(_I_, __LINE__)._instids).end(); (ITER)++ )
////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

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
      // if (Dbg::Mode()) {node.Info("skipping this initial block");}
    }

    // Collect event control statements :
    virtual void Visit(VeriEventControlStatement &node) {
      // if (Dbg::Mode()) {node.Info("visiting this event control statement");}
        _event_control_stmts.InsertLast(&node) ;
    }

public :
    Array _event_control_stmts ; // array of VeriEventControlStatement *'s, collected by this visitor
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

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

enum VariablePrintStyle { HdlPositionalStyle, HdlKeyValuePairStyle, HdlUnknownStyle };

class PortVisitor : public VeriVisitor
{
public :
 
  PortVisitor() : VeriVisitor(), _port_connects(POINTER_HASH) { } ;
  ~PortVisitor() {} ;

  // Collect instantiations :
  virtual void Visit(VeriIdDef &node) {
    if (node.IsPort())
      _port_connects.Insert(&node) ;  
  }

public :
    Set _port_connects ; 
};

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

// A visitor pattern, which will find the target names in instantiations in a parse tree
// Collect all instantiation targets from Blocking and Non-Blocking instantiations :
class GateInstantiationVisitor : public VeriVisitor
{
public :
    GateInstantiationVisitor() : VeriVisitor(), _gate_insts(POINTER_HASH) { } ;
    ~GateInstantiationVisitor() {} ;

    // Collect instantiations :
    virtual void Visit(VeriGateInstantiation &node) {
      _gate_insts.Insert(&node) ;  
    }

public :
    Set _gate_insts ; 
} ;

struct RdyEnableInterface {
  std::string RdyPort;
  std::string EnablePort;
  std::list<std::string> DataPorts;
  std::map<std::string, std::string> BitMap;
  std::map<std::string, int> SizeMap;
  std::string Interface;
  int Type; // 0-Put, 1-Get, 2-PipePut, 3-PipeGet
  bool used;
  int Width;
  int Count;

  RdyEnableInterface() { used = false; Width = 0;}
  RdyEnableInterface(std::string &rdy, std::string &en, std::string &interface)
  { RdyPort = rdy; EnablePort = en; Interface = interface;
    Type = 0; used = false; Width = 0; Count = 0;}

  void setRdy(std::string &rdy) { RdyPort = rdy; }
  void setEn(std::string &en) { EnablePort = en; }
  void setInterface(std::string &i) { Interface = i; }
  void addData(std::string &data) { DataPorts.push_back(data); }
  void setPutType() { Type = 0; }
  void setGetType() { Type = 1; }
  void setPipePutType() { Type = 2; }
  void setPipeGetType() { Type = 3; }
  void setUsed(bool u) { used = u; }
  void setWidth(int w) { Width = w; }
  void setCount(int c) { Count = c; }
  const char *getRdy() { return RdyPort.c_str(); }
  const char *getEn() { return EnablePort.c_str(); }
  const char *getInterface() { return Interface.c_str(); }
  std::list<std::string>::iterator dataBegin() { return DataPorts.begin(); }
  std::list<std::string>::iterator dataEnd() { return DataPorts.end(); }
  void sortData() { DataPorts.sort(); }
  int numData() { return DataPorts.size(); }
  bool isPut() { return Type == 0; }
  bool isGet() { return Type == 1; }
  bool isPipePut() { return Type == 2; }
  bool isPipeGet() { return Type == 3; }
  bool getUsed() { return used; }
  int getWidth() { return Width; }
  int getCount() { return Count; }
  std::map<std::string, std::string> *getBitMap() { return &BitMap; }
  std::map<std::string, int> *getSizeMap() { return &SizeMap; }
  void getAssignedBitsString(std::string &portname, std::string &bitstring)
  { bitstring = BitMap[portname]; }
};

struct MemoryInterface {
  std::string RdyPort;
  std::string EnablePort;
  std::list<std::string> DataPorts;
  std::string Interface;
  int Type; // 0-Req, 1-Rsp
  bool used;
  int AddressWidth;
  int DataWidth;

  MemoryInterface() { used = false; AddressWidth = 0; DataWidth = 0;}
  MemoryInterface(std::string &rdy, std::string &en, std::string &interface)
  { RdyPort = rdy; EnablePort = en; Interface = interface;
    Type = 0; used = false; DataWidth = 0; AddressWidth = 0;}

  void setRdy(std::string &rdy) { RdyPort = rdy; }
  void setEn(std::string &en) { EnablePort = en; }
  void setInterface(std::string &i) { Interface = i; }
  void addData(std::string &data) { DataPorts.push_back(data); }
  void setReqType() { Type = 0; }
  void setRspType() { Type = 1; }
  void setUsed(bool u) { used = u; }
  void setDataWidth(int w) { DataWidth = w; }
  void setAddressWidth(int c) { AddressWidth = c; }
  const char *getRdy() { return RdyPort.c_str(); }
  const char *getEn() { return EnablePort.c_str(); }
  const char *getInterface() { return Interface.c_str(); }
  std::list<std::string>::iterator dataBegin() { return DataPorts.begin(); }
  std::list<std::string>::iterator dataEnd() { return DataPorts.end(); }
  void sortData() { DataPorts.sort(); }
  int numData() { return DataPorts.size(); }
  bool isReq() { return Type == 0; }
  bool isRsp() { return Type == 1; }
  bool getUsed() { return used; }
  int getDataWidth() { return DataWidth; }
  int getAddressWidth() { return AddressWidth; }
};

typedef std::map<std::string, bool> NetAliases;
typedef std::map<std::string, bool>::iterator NetAliasesIterator;

class HdlUtils
{
 private:

  static veri_file veri_reader;

  static HdlUtils *theHdlUtils;
  static VeriModule *theRootModule;
  static string _copysuffix;

  // Members for options and command line related data
  Array*   _files;
  Array*   _vfiles;
  Array*   _ydirs;
  Array*   _idirs;
  Array*   _dmacros;
  unsigned _no_elab;
  unsigned _no_copy;
  string   _regexpr;
  unsigned _printall;
  string   _topmod;
  string   _libext;
  unsigned _blackbox;
  unsigned _verilog_mode;

  int      processed_options;
  int      read_port_spec;
  int      use_fpga_memory;

  static BStringList allpaths;

  static VeriLibrary *_temp_library;
  static BString _fragment_file_path;

  // Private it can be constructed/destroyed only through init()/shutdown()
  HdlUtils();
  ~HdlUtils();
  void print_help(const char *progname);
  int processOptions(string &errstring);

  static std::map<string, string> clk_rst_map;
  static std::map<string, string> port_xactor_type;
  static std::map<string, int> port_pipe_type;
  static std::map<string, int> port_lockstep_type;
  static bool found_lockstep;

 public:

  // Options or command line related methods for setting up verific
  void addYDir(char *path) { _ydirs->InsertLast(newString(path)); }
  void addIDir(char *path) { _idirs->InsertLast(newString(path)); }
  void addVFile(char *path) { _vfiles->InsertLast(newString(path)); }
  void addFile(char *path) { _files->InsertLast(newString(path)); }
  void clearFiles();
  void addDefMacro(char *path) { _dmacros->InsertLast(newString(path)); }
  void setNoElab() { _no_elab = 1; }
  void setNoCopy() { _no_copy = 1; }
  void setSystemVerilog() { _verilog_mode = 3; }
  void setRegExpr(char *expr) { _regexpr = expr; }
  void setPrintOnlyModified() { _printall = 0; }
  void setTopMod(char *name) { _topmod = name; }
  void setCopySuffix(char *name) { _copysuffix = name; }
  void setLibExt(char *expr) { _libext = expr; }
  void setBlackboxLevel(unsigned value) { _blackbox = value; }

  Array*   getYDirs() { return _ydirs; }
  Array*   getIDirs() { return _idirs; }
  Array*   getVFiles() { return _vfiles; }
  Array*   getFiles() { return _files; }
  Array*   getDefMacros() { return _dmacros; }
  unsigned getNoElab() { return _no_elab; }
  unsigned getNoCopy() { return _no_elab; }
  unsigned getSystemVerilog() { return _verilog_mode == 3; }
  char*    getRegExpr() { return (char*)_regexpr.c_str(); }
  unsigned getPrintOnlyModified() { return !_printall; }
  const char* getTopMod() { return _topmod.c_str(); }
  static const char* getCopySuffix() { return _copysuffix.c_str(); }
  char*    getLibExt() { return (char*) _libext.c_str(); }
  char*    newString(char *s)
  { char *newstring = new char[strlen(s)+1]; strcpy(newstring, s); return newstring; }
  static std::map<string, string> &getControlSignalsMap() { return clk_rst_map; }

 public:

  // Static utility methods for traversing the verific parsetree
  static VeriModule *findModuleFromPath(VeriModule *from, const std::string &path, std::string &found);
  static VeriInstId *findInstFromPath(const std::string &path);

  // Static utility methods for traversing the verific parsetree
  static VeriModule *findModuleByName(const char *name);
  static VeriInstId *findInstInModuleByName(VeriModule *from, const std::string &instname); 
  static VeriIdDef *findSignalInModuleByName(VeriModule *from, const std::string &signame);
  static int findWidthOfSignal(VeriIdDef *iddef);
  static void findClockSignalInModule(VeriModule *from, std::string &return_clk_name);
  static void findSignalsInModuleByExpression(VeriModule *module, std::string exp,
					      netCollection &sigs, unsigned int &thiswidth,
					      SignalType signaltype=CktModAny);
  static void findBSVProbeSignalsInModule(VeriModule *module, netCollection &sigs,
					  unsigned int &thiswidth, SignalType signaltype=CktModAny);
  static void findFlopSignalsInModule(VeriModule *module, netCollection &sigs,
				      unsigned int &thiswidth);
  static void findFlopSignalsInModuleByExpression(VeriModule *module, std::string exp,
						  netCollection &sigs,
						  unsigned int &thiswidth,
						  SignalType signaltype=CktModAny,
						  std::map<BString, bool> *sigsmap=NULL);
  static VeriIdDef *findConnectedIdDef(VeriModule *module, VeriIdDef *def,
				       VeriInstId *from, VeriInstId *to);
  static void getListOfConnectedInst(VeriModule *module, VeriIdDef *def, VeriInstIdList &instlist);
  static int isSignalConnectedToInst(VeriIdDef *def, VeriInstId *inst);
  static bool isNetTypeMatched(VeriIdDef *id, SignalType signaltype);
  static void findPathsFromPattern(const BString & pattern, BStringList &paths);
  static void recursiveFindAllPaths(VeriModule *from, BStringList &paths, BString &frompath);

  static VeriIdDef *findPortOfModuleByName(VeriModule *module, std::string &portname); 
  static VeriIdDef *findPortOfInstByName(VeriInstId *inst, std::string &portname); 
  static VeriIdDef *findPortOfModuleByPosition(VeriModule *module, unsigned int pos);
  static unsigned int getNetName(VeriIdDef *netid, string &netName);
  static unsigned int findInstsByMasterName (VeriModule *mod, const std::string & mastname, std::list<VeriInstId *> &);
  static VariablePrintStyle getPortConnectsPrintStyle(VeriInstId *inst_id);
  static unsigned addPortRefKeyValue(VeriModule *module, VeriInstId *inst_id, string &portname, string &netname);
  static unsigned addPortRefPositional(VeriModule *module, VeriInstId *inst_id, string &netname);
  static VeriRange * mkVeriRangeFromWidth(unsigned int width);
  static VeriConst * mkVeriConst (const string &str);
  static bool stringToInt ( const std::string & str, int & value);
  static bool isClocked (VeriEventControlStatement *event_control_stmt, BStringSet *clock_set = NULL);
  static tIdSet getAssignmentIds(VeriTreeNode* node, VeriIdDef* rh = 0);
  static tIdSet getContAssignmentIds(VeriTreeNode* node, VeriIdDef* rh = 0);
  static VeriExpression* mkExpression(const char* value);
  static VeriExpression* mkExpression(int value);
  static VeriIdRef* ref(VeriIdDef* id);
  static VeriExpression* createConcatExpression(VeriModule* module, tIdList all_ids, ScanPath** scan_path);
  static unsigned int getSignalSize(VeriIdDef* id);
  static const char* getImage(VeriTreeNode* node);
  static void visitRefs(VeriTreeNode* node);

  // Methods for handling copying and accessing unelaborated copy of modules
  static VeriModule* copyModule(VeriModule *module_orig, const char *newName, VeriLibrary *lib);
  static int recursiveCopyModule(VeriModule *module_orig, const char *new_suffix, VeriLibrary *lib);
  static void getVerilogFileList(VeriModule *top_module, std::map<string, int> &file_list);
  static void recursiveCreateVerilogFileList(VeriModule *module, std::map<string, int> &file_list);
  static int copyAndElaborateAllStatic(const char *module_suffix);
  static VeriModule* getOriginalUnelabModule(VeriModule* module);
  static VeriLibrary* getTempLibrary() { return _temp_library; }
  static const char* getFileName(VeriModule *module);
  static bool isBlackboxed(unsigned size) { return size >= theHdlUtils->_blackbox; }
  static bool isBlackboxed(VeriIdDef* id);
  static bool isBlackboxed(VeriInstId* id, BStringSet & uclocks = *(new BStringSet()));


  static void findConnectionNameOfPort(VeriInstId *inst, BString &portname, BString &return_name);
  static void setConnectionNameOfPort(VeriModule *module, VeriInstId *inst,
				      BString &portname, BString &new_conn_name)
  { setConnectionNameOfPort(module, inst, portname.c_str(), new_conn_name.c_str()); }
  static void setConnectionNameOfPort(VeriModule *module, VeriInstId *inst, const char *portname,
				      const char *new_conn_name);
  static ModuleTerminal *locateModuleTerminal(ModuleTerminalList &mtlist, std::string &name);
  static void createModuleParameterList (Verific::VeriModule* module, ParameterList &plist);
  static void createModuleTerminalList (VeriModule* module, ModuleTerminalList &mtlist,
					const char *prefix=0, std::map<string, int> *map=0);
  static void createModuleTerminalList (VeriModule* module, ModuleTerminalList &mtlist,
					const char *prefix, std::map<string, string> *map);
  static void createModuleTerminalList2 (VeriModule* module, ModuleTerminalList &mtlist,
					 std::map<string, string> *map=0);
  static void createInstanceTerminalList (VeriInstId* inst, ModuleTerminalList &mtlist);
  static void findFragmentFilePath(BString &path);

  static void loadVeriNodeInfoTable(VeriModule *module);
  static int isPortPipe(const char *portname) { return port_pipe_type[portname]; }
  static int isPortLockstep() { return found_lockstep; }

  std::map<std::string,RdyEnableInterface*> RdyEnableIfc;
  std::map<std::string,MemoryInterface*> MemoryReqIfc;
  std::map<std::string,MemoryInterface*> MemoryRespIfc;
  std::map<std::string,MemoryInterface*> MemoryReqPortIfc;
  std::map<std::string,MemoryInterface*> MemoryRespPortIfc;

 public:

  // Initialize 
  static HdlUtils *init();

  // Shutdown 
  static void shutdown();

  int decode_options (int argc, char **argv, string &errstring);

  // Read verilog input files
  int analyze(string &errstring, char *path=0);
  int force_analyze(string &errstring, char *path=0);

  // Generate Scemi Layer
  int readPinFile(const char *filename);
  int readPortDefinitions(std::ifstream &file, int &LineNum, const char *filename);
  int readMemDefinitions(std::ifstream &file, int &LineNum, const char *filename);


  int generateSceMiLayer(VeriModule *module, const char *new_module_name,
			 const char *scemi_layer_filename,
			 const char *outdir);
  int generateSceMiLayerFile(VeriModule *module, const char *new_module_name,
			     const char *scemi_layer_filename);
  int generateLockstepSceMiLayerFile(VeriModule *module, const char *new_module_name,
				     const char *scemi_layer_filename);
  int generateDutFile(VeriModule *module, const char *new_module_name,
		      const char *filename);
  int generateLockstepDutFile(VeriModule *module, const char *new_module_name,
			      const char *filename);
  int generatePinFile(const char *filename, const char *modulename);
  static void saveFile(const char *filename);
  static int generateDutVerilog(VeriModule *module, const char *module_name, const char *inst_name,
				ParameterList &plist, ModuleTerminalList &new_terminals,
				const char *indir, const char *outdir);

  const char *getBitType(ModuleTerminal *terminal, string &returnval);
  const char *getPortNameVar(ModuleTerminal *terminal, const char *new_module_name,
			     string &returnval);
  int isRegularModuleTerminal(ModuleTerminal *terminal);

  std::map<std::string,RdyEnableInterface*> &getRdyEnableIfc()
  { return RdyEnableIfc; }

  std::map<std::string,MemoryInterface*> &getMemoryReqIfc()
  { return MemoryReqIfc; }

  std::map<std::string,MemoryInterface*> &getMemoryRespIfc()
  { return MemoryRespIfc; }

  std::map<std::string,MemoryInterface*> &getMemoryReqPortIfc()
  { return MemoryReqPortIfc; }

  std::map<std::string,MemoryInterface*> &getMemoryRespPortIfc()
  { return MemoryRespPortIfc; }

  void setRdyEnableIfcWidth(VeriModule *module);
  
  void assignRdyEnableBitMaps(RdyEnableInterface *rei, VeriModule *module);
  
  // Read verilog input files
  //int loadNetlist(const char *modulename);

  // Control if verific should ignore translate_off pragma's during parsing
  // (1 is ignore, 0 is don't-ignore (skip translate_off->translate_on blocks)
  static void setIgnoreTranslateOff(unsigned ignore_pragma)
  { veri_file::SetIgnoreTranslateOff(ignore_pragma); }
  static void setIgnoreAllPragmas(unsigned ignore_pragma)
  { veri_file::SetIgnoreAllPragmas(ignore_pragma); }

  int file_exists(char *fileName);


  // Methods for find an alias of a net/signal
  static bool findNetAliases(const char *path, const char *signame, NetAliases &hashlist);
};


class IdRefVisitor : public VeriVisitor
{
public :
    IdRefVisitor() : VeriVisitor(), _nm(BString("")), _refs(), _ids() { } ;
    ~IdRefVisitor() {} ;
    IdRefVisitor(const BString & name);

    // Collect instantiations :
  virtual void Visit(VeriIdRef & node) {
    VeriIdDef* d = node.GetId();
    if (_nm.length() == 0 || _nm.compare(d->GetName()) == 0) {
      printf("Visit: %s\n", node.GetName());
      _ids.insert(node.GetId());
      _refs.insert(&node);
    }
  }

public :
  const BString & _nm;
  tRefSet _refs;
  tIdSet _ids;
} ;

class RefVisitor : public VeriVisitor
{
public :
    RefVisitor() : VeriVisitor(), _ids() { } ;
    ~RefVisitor() {} ;

    // Collect instantiations :
    virtual void Visit(VeriIdRef &node) {
         _ids.insert(node.GetId()) ;  
    }

    // Collect instantiations :
    virtual void Visit(VeriIndexedMemoryId &node) {
//      printf("M %s\n", node.GetId()->Name());
      _ids.insert(node.GetId()) ;  
    }

public :
  tIdSet _ids;
} ;

// A visitor patteren, which will find the target ids in assignments in a parse tree
// Collect all assignment targets from Blocking and Non-Blocking assignments :
class AssignmentIdVisitor : public VeriVisitor
{
public :
    AssignmentIdVisitor() : VeriVisitor(), _rh(0), _assigned_ids() { } ;
    ~AssignmentIdVisitor() {} ;
    AssignmentIdVisitor(VeriIdDef* rh);

    // Collect blocking assignments : 
    virtual void Visit(VeriBlockingAssign &node) { 
      // Go to the assigned target (expression) : 
      VeriExpression *target = node.GetLVal() ; 
      // Find the assigned identifier(s) : 
      VeriIdDef *target_id = (target) ? target->GetId() : 0 ; 
      if (target_id && !_rh) {
	_assigned_ids.insert(target_id) ; 
      }
      if (target_id && _rh) {
	VeriExpression* rhs = node.GetValue() ; 
	RefVisitor vis;
	rhs->Accept( vis ) ;
	foreachSetId(vis._ids, it) {
	  VeriIdDef* r = (VeriIdDef*) *it;
//	  printf("F %s %d\n", r->Name(), r == _rh);
	  if (r == _rh) {
	    _assigned_ids.insert(target_id) ; 
	    break;
	  }
	}
      }
    }
    // Collect non-blocking assignments :
    virtual void Visit(VeriNonBlockingAssign &node) {
        // Go to the assigned target (expression) :
        VeriExpression *target = node.GetLVal() ;
        // Find the assigned identifier(s) :
        VeriIdDef *target_id = (target) ? target->GetId() : 0 ;
	if (target_id && !_rh) {
	  _assigned_ids.insert(target_id) ;
	}
	if (target_id && _rh) {
	  VeriExpression* rhs = node.GetValue() ; 
	  RefVisitor vis;
	  rhs->Accept( vis ) ;
	  foreachSetId(vis._ids, it) {
	    VeriIdDef* r = (VeriIdDef*) *it;
//	    printf("F %s %d\n", r->Name(), r == _rh);
	    if (r == _rh) {
	      _assigned_ids.insert(target_id) ; 
	      break;
	    }
	  }
	}
    }

public :
  VeriIdDef* _rh;
  tIdSet _assigned_ids;
} ;


class ContAssignmentIdVisitor : public VeriVisitor
{
public :
    ContAssignmentIdVisitor() : VeriVisitor(), _rh(0), _assigned_ids() { } ;
    ~ContAssignmentIdVisitor() {} ;
    ContAssignmentIdVisitor(VeriIdDef* rh);

    // Collect blocking assignments : 
    virtual void Visit(VeriNetRegAssign &node) { 
      // Go to the assigned target (expression) :
      VeriExpression *target = node.GetLValExpr() ;
      // Find the assigned identifier(s) :
      VeriIdDef *target_id = (target) ? target->GetId() : 0 ;
      if (target_id && !_rh) {
 	_assigned_ids.insert(target_id) ;
      }
      if (target_id && _rh) {
 	VeriExpression* rhs = node.GetRValExpr() ;
	RefVisitor vis;
 	rhs->Accept( vis ) ;
	foreachSetId(vis._ids, it) {
	  VeriIdDef* r = (VeriIdDef*) *it;
	  // printf("F %s %d\n", r->Name(), r == _rh);
	  if (r == _rh) {
	    _assigned_ids.insert(target_id) ;
	    break;
	  }
	}
      }
    }

public :
  VeriIdDef* _rh;
  tIdSet _assigned_ids;
} ;


class LookupByMaster : public VeriVisitor
{
  std::list<VeriInstId *> & _found;
  unsigned int _cnt;
  const std::string &_lookfor;
public:
  LookupByMaster (const std::string &lookfor, std::list<VeriInstId *> &found)
    : _found(found)
    ,_cnt(0)
    , _lookfor(lookfor)
  {}
  unsigned int getCount() const { return _cnt;}
  virtual void Visit(VeriInstId &node) {
    const char *mod = node.GetModuleReference();
    if (_lookfor == mod) {
      ++ _cnt ;
      _found.push_back(&node);
    }
  }
};

class  ContAssignmentVisitor : public VeriVisitor
{
public :
    ContAssignmentVisitor() : VeriVisitor(), _rh(0), _assigned_ids(), _assigns() { } ;
    ~ContAssignmentVisitor() {} ;
    ContAssignmentVisitor(VeriIdDef* rh);

    virtual void Visit(VeriContinuousAssign &node) { 
      unsigned it0;
      VeriNetRegAssign* nr; 
      FOREACH_ARRAY_ITEM(node.GetNetAssigns(), it0, nr) {
	VeriExpression *target = nr->GetLValExpr() ;
	RefVisitor vis0;
	target->Accept( vis0 ) ;
	foreachSetId(vis0._ids, it) {
	  VeriIdDef* lh_id = (VeriIdDef*) *it;
	  if (!_rh) {
	    _assigned_ids.insert(lh_id) ;
	  } else {
	    VeriExpression* rhs = nr->GetRValExpr() ;
	    RefVisitor vis;
	    rhs->Accept( vis ) ;
	    foreachSetId(vis._ids, it) {
	      VeriIdDef* r = (VeriIdDef*) *it;
	      if (r == _rh) {
		_assigned_ids.insert(lh_id) ;
		_assigns.insert(&node);
		break;
	      }
	    }
	  }
	}
      }
    }

public :
  VeriIdDef* _rh;
  tIdSet _assigned_ids;
  tModuleItemSet _assigns;
} ;
