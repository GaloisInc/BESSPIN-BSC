// Copyright 2010 Bluespec Inc. All rights reserved

#include "HdlUtils.h"
#include "getopt.h"
#include <sys/stat.h>
#include <boost/regex.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>
#include <sstream>
#include <limits>
#include <dirent.h>

#include "Map.h"
#include "VeriModule.h"     // Definition of a VeriModule and VeriPrimitive
#include "VeriId.h"         // Definitions of all identifier definition tree nodes
#include "VeriExpression.h" // Definitions of all verilog expression tree nodes
#include "VeriModuleItem.h" // Definitions of all verilog module item tree nodes
#include "VeriStatement.h"  // Definitions of all verilog statement tree nodes
#include "VeriMisc.h"       // Definitions of all extraneous verilog tree nodes (ie. range, path, strength, etc...)
#include "VeriConstVal.h"   // Definitions of parse-tree nodes representing constant values in Verilog.
#include "VeriScope.h"      // Symbol table of locally declared identifiers
#include "VeriLibrary.h"    // Definition of VeriLibrary
#include "VeriUtil_Stat.h"
#include "VeriCopy.h"
#include "veri_tokens.h"

#include "CktMod.h"

#define MAX_LINE_SIZE 1023

using namespace std;

#define BUFSIZE 256
#define ERR_MSG_BYTES 4096
//static char err_msg[ERR_MSG_BYTES];

// Static members of HdlUtils
veri_file HdlUtils::veri_reader;
HdlUtils *HdlUtils::theHdlUtils = NULL;
VeriModule *HdlUtils::theRootModule = NULL;
string HdlUtils::_copysuffix;
VeriLibrary *HdlUtils::_temp_library = NULL;
BStringList HdlUtils::allpaths;
BString HdlUtils::_fragment_file_path;
std::map<string, string> HdlUtils::clk_rst_map;
std::map<string, string> HdlUtils::port_xactor_type;
std::map<string, int> HdlUtils::port_pipe_type;
std::map<string, int> HdlUtils::port_lockstep_type;
bool HdlUtils::found_lockstep = false;

void toLowerCase(std::string &str);
void removeDoubleUnderscore(std::string &str);


string Token;

HdlUtils::HdlUtils()
{
  _files    = new Array();
  _vfiles   = new Array();
  _ydirs    = new Array();
  _idirs    = new Array();
  _dmacros  = new Array();
  _no_elab  = 0;
  _printall = 1;
  _verilog_mode  = 3; /*SYSTEM_VERILOG*/
  _regexpr  = ".*";
  _copysuffix = "_orig";
  processed_options = 0;
  read_port_spec = 0;
  _blackbox = numeric_limits<unsigned int>::max();
  use_fpga_memory = 0;
}

HdlUtils::~HdlUtils()
{
  unsigned it_a;
  char *st;
  FOREACH_ARRAY_ITEM(_files, it_a, st)
    delete st;
  delete _files;
  FOREACH_ARRAY_ITEM(_vfiles, it_a, st)
    delete st;
  delete _vfiles;
  FOREACH_ARRAY_ITEM(_ydirs, it_a, st)
    delete st;
  delete _ydirs;
  FOREACH_ARRAY_ITEM(_idirs, it_a, st)
    delete st;
  delete _idirs;
  delete _dmacros;
  delete _temp_library; _temp_library = NULL;
}

HdlUtils *HdlUtils::init()
{
  if (theHdlUtils != NULL)
    return theHdlUtils;

  theHdlUtils = new HdlUtils();

  // Default ignores
  HdlUtils::setIgnoreTranslateOff(0);
  //  HdlUtils::setIgnoreAllPragmas(1);

  return theHdlUtils;
}

void HdlUtils::shutdown()
{
  delete theHdlUtils;
  theHdlUtils = NULL;
  theRootModule = NULL;
}

int HdlUtils::analyze(string &errstring, char *path)
{
  //if (processed_options == 0) {
  if (!processOptions(errstring))
    return 0;

  if ((path != NULL) && !file_exists(path)) {
    errstring += "Error: file ";
    errstring += path;
    errstring += " not found\n";
    return 0;
  }

  if (path) {
    if (!veri_reader.Analyze(path, _verilog_mode)) {
      printf("cant analyze %s\n", path);
      return 0;
    }
  }
  //printf("analyzed %s\n", path);
  return 1;
}

int HdlUtils::force_analyze(string &errstring, char *path)
{
  printf("force analyze %s\n", path);

  if (path) {
    if (!veri_reader.Analyze(path, _verilog_mode)) {
      printf("cant analyze %s\n", path);
      return 0;
    }
  }
  //printf("analyzed %s\n", path);
  return 1;
}

int HdlUtils::processOptions(string &errstring)
{
  /* -------------------------------------------------------------------------- */
  /* Incorporate command line options.                                          */
  /* -------------------------------------------------------------------------- */

  boost::regex filter(getRegExpr());

  char* expr = getLibExt();

  if (expr) {

    //printf("LIB EXT: %s\n", expr);

    while (1) {
      char* next = strchr(expr, '+');
      if (next) {
	next = next + 1;
	int i = 0;
	while (1) {
	  if (expr[i] == '+') {
	    expr[i] = 0;
	    break;
	  }
	  i++;
	}
	if (strlen(expr) == 0 || strlen(next) == 0) {
	  fprintf(stderr, "%s: ill-formed +libext+ expression exiting\n", 
		  "HdlUtils");
		
	  return 0;
	}
	veri_reader.AddLibExt(expr);
	//printf("ADDING EXT: %s\n", expr + 1);
	expr = next;
      } else {
	//printf("ADDING EXT: %s\n", expr + 1);
	break;
      }
    }
  }

  unsigned it_a;
  char *path;

  FOREACH_ARRAY_ITEM(getDefMacros(), it_a, expr) {
    char* last = strchr(expr, '=');
    if (last) {
      char* value = last + 1;
      int i = 0;
      while (1) {
	if (expr[i] == '=') {
	  expr[i] = 0;
	  break;
	}
	i++;
      }
      if (strlen(expr) == 0) {
	expr[i] = '='; // put expr back like it was
	fprintf(stderr, "%s: ill-formed define macro expression '%s' exiting\n", 
		"HdlUtils", expr);
		
	return 0;
      }
      //      unsigned result = veri_reader.DefineMacro(expr, value);
      if (veri_reader.DefineMacro(expr, value)) return 0;
      //printf("ADDING DEF: %s = %s\n", expr, value);
    } else {

      if (veri_reader.DefineMacro(expr)) {
	errstring += "Error: illegal macro expression ";
	errstring += expr;
	errstring += "\n";
	return 0;
      }
      //printf("ADDING DEF: %s\n", expr);
    }
  }

  string filename;
  std::ifstream file;
  FOREACH_ARRAY_ITEM(getYDirs(), it_a, path) {
    veri_reader.AddYDir(path);
    if (_fragment_file_path == "") {
      filename = path;
      filename += "/";
      filename += "scemilink.vlog_fragment";
      file.clear();
      file.open(filename.c_str());
      if (!file.fail()) {

	_fragment_file_path = path;
	file.close();
      }
    }
  }

  FOREACH_ARRAY_ITEM(getVFiles(), it_a, path) {
    veri_reader.AddVFile(path);
  }

  FOREACH_ARRAY_ITEM(getIDirs(), it_a, path) {
    veri_reader.AddIncludeDir(path);
  }

  bool found_a_file = false;
  FOREACH_ARRAY_ITEM(getFiles(), it_a, path) {
    if (!file_exists(path)) {
      errstring += "Error: file ";
      errstring += path;
      errstring += " not found and will be ignored.\n";
    }
    found_a_file = true;
  }
  if (found_a_file)
    if (!veri_reader.AnalyzeMultipleFiles(getFiles(), _verilog_mode, "work", 0)) {
      errstring += "Error: reading one or more input file(s).";
      //errstring += path;
      errstring += "\n";
      clearFiles();
      return 0;
    }

  found_a_file = false;
  FOREACH_ARRAY_ITEM(getVFiles(), it_a, path) {
    if (path==NULL || !file_exists(path)) {
      errstring += "Error: verilog file ";
      errstring += path;
      errstring += " not found and will be ignored\n";
    }
    found_a_file = true;
  }
  if (found_a_file)
    if (!veri_reader.AnalyzeMultipleFiles(getVFiles(), _verilog_mode, "work", 0)) {
      errstring += "Error: reading verilog file.";
      //errstring += path;
      errstring += "\n";
      clearFiles();
      return 0;
    }
  clearFiles();

  // Load the rest of the design verilog files
  

  // -----------------------------------------------------------------------------
  //
  // -----------------------------------------------------------------------------

  // Create the temporary library
  _temp_library = new VeriUserLibrary("Temp");

  if (!getNoElab()) {
    if (!HdlUtils::copyAndElaborateAllStatic(getCopySuffix())) return 0;
  }
 
  // Get all the top level modules
  if (getTopMod() == NULL || (!strcmp(getTopMod(), ""))) {
   
    Array *all_top_modules = veri_file::GetTopModules() ;
    // Get a handle of the first top level module, if any.
    theRootModule = (all_top_modules && all_top_modules->Size()) ?
      (VeriModule *)all_top_modules->GetFirst() : 0 ;
    
    //fprintf(stderr, "Warning: Top module is not specified by -e, module %s will be used.\n",
    //theRootModule->Name()); 
    
    setTopMod((char*)theRootModule->Name());

  } else {

    Array *all_top_modules = veri_file::GetTopModules() ;
    unsigned i;
    string topmodname = getTopMod();
    VeriModule *module;
    theRootModule = NULL;
    FOREACH_ARRAY_ITEM(all_top_modules, i, module) {
      if (topmodname == module->Name())
	theRootModule = module;
    }
    if (theRootModule == NULL) {

      fprintf(stderr, "Error: The specified top module %s is not found in any of the given verilog files\n.", topmodname.c_str());
      return 0;
    }
  }

  if (errstring != "")
    return 0;

  return 1;
}

void HdlUtils::clearFiles()
{
  unsigned it_a;
  char *path;

  FOREACH_ARRAY_ITEM(getFiles(), it_a, path) {
    delete [] path;
  }
  _files->Reset();

  //FOREACH_ARRAY_ITEM(getVFiles(), it_a, path) {
    //delete [] path;
  //}
  _vfiles->Reset();
}

int HdlUtils::copyAndElaborateAllStatic(const char *module_suffix)
{
  VeriModule *module;

  // Create a private user library for the copied modules
  if (_temp_library == NULL)
    _temp_library = new VeriUserLibrary("Temp");

  Array *all_top_modules = veri_file::GetTopModules() ;
  unsigned i;
  FOREACH_ARRAY_ITEM(all_top_modules, i, module) {
    recursiveCopyModule(module, module_suffix, _temp_library);
  }

  // Elaborate all
  if (!veri_reader.ElaborateAllStatic()) {
    fprintf(stderr, "Error HdlUtils::copyAndElaborateAllStatic:: fail static elaboration.\n");
    return 0;
  }

  return 1;
}

int HdlUtils::recursiveCopyModule(VeriModule *module, const char *module_suffix,
				  VeriLibrary *newlib)
{
  VeriModule *new_module;

  // Append 'module_suffix' to existing name
  char *new_name = Strings::save(module->GetName(), module_suffix) ;
  

  // Already copied before, just happily return without recursing
  new_module = newlib->GetModule(new_name, 1);
  if (new_module)
    return 1;

  // Copy the module but do not add it to the original lib
  new_module = copyModule(module, new_name, newlib);
  Strings::free(new_name);

  // Recursively copy
  InstantiationVisitor instantiation_visitor ;
  SetIter si_0; // a Set iterator
  VeriModuleInstantiation *module_inst ;
  VeriModule *module_def;
  module->Accept( instantiation_visitor ) ;
  FOREACH_SET_ITEM(&(instantiation_visitor._module_insts), si_0, &module_inst) {
   
    module_def = module_inst->GetInstantiatedModule();

    if (module_def)
      recursiveCopyModule(module_def, module_suffix, newlib);
  }

  return 1;
}

const char *HdlUtils::getFileName(VeriModule *module)
{
  linefile_type lf = module->Linefile();

  if (!lf)
    return NULL;

  const char *fn = lf->GetFileName();

  return fn;
}

void HdlUtils::getVerilogFileList(VeriModule *top_module, std::map<string, int> &file_list)
{
  recursiveCreateVerilogFileList(top_module, file_list);
  return;
}

void HdlUtils::recursiveCreateVerilogFileList(VeriModule *module, std::map<string, int> &file_list)
{
  if (file_list[HdlUtils::getFileName(module)])
    return;

  // Recursively create file list
  InstantiationVisitor instantiation_visitor ;
  SetIter si_0; // a Set iterator
  VeriModuleInstantiation *module_inst ;
  VeriModule *module_def;
  module->Accept( instantiation_visitor ) ;
  FOREACH_SET_ITEM(&(instantiation_visitor._module_insts), si_0, &module_inst) {
   
    module_def = module_inst->GetInstantiatedModule();

    if (module_def)
      recursiveCreateVerilogFileList(module_def, file_list);
  }

  file_list[HdlUtils::getFileName(module)] = 1;

  return;
}

extern int opterr;
extern int optopt;

void cleanup_dirname(char *dir, string &newdir)
{
  size_t size;

  newdir = dir;

  while ((newdir[0] == ':') || (newdir[0] == '+'))
    newdir = newdir.substr(1);

  size = newdir.size()-1;
  while ((newdir[size] == ':') || (newdir[size] == '+')) {
    newdir = newdir.substr(0, size);
    size--;
  }
}

int HdlUtils::decode_options (int argc, char **argv, string &errstring)
{
  int opt_errors_encountered = 0;
  char buf[320];
  opterr = 0;
  optind = 0;

  //printf("Num args %d\n", argc);
  struct option longopts[] =
    {
      { "help",	         0,  0, 'h' },
      { "just-modified", 0,  0, 'j' },
      { "no-elab",       0,  0, 'n' },
      { "system-verilog",0,  0, 't' },
      { "regexp",        1,  0, 'r' },
      { "topmod",        1,  0, 'e' },
      { "copy_suffix",   1,  0, 's' },
      { "vdir",	         1,  0, 'v' },
      { "ydir",	         1,  0, 'y' },
      { "outdir",        1,  0, 'o' },
      { "blackbox",      1,  0, 'b' },
      { "define",        0,  0, 'D' },
      { "verbose",       0,  0, 'V' },
      { 0, 0, 0, 0 }
    };
  
  while (1)
    {
      int opt = getopt_long_only (argc, argv, "Vhjne:o:r:v:y:b:D:s:", longopts, 0);

      if (opt == EOF) {
	break;
      }

      //printf("OPT %c %s\n", opt, optarg);
      switch (opt)
	{
	case 'e':
	  if (!isalpha(optarg[0])) {
	    snprintf(buf, 320, "HdlUtils::decode_options(): invalid top module value %s for option (-e, -topmod).\n", argv[optind-1]);
	    errstring += buf;
	  } else
	    setTopMod(optarg);
          break;
	case 'h':
	  opt_errors_encountered = 1;
	  print_help("HdlUtils");
          break;
	case 'j':
	  setPrintOnlyModified();
          break;
	case 'r':
	  if (optarg[0] == '-') {
	    opt_errors_encountered = 1;
	    snprintf(buf, 320, "HdlUtils::decode_options(): invalid reg expression value %s for option (-r, -regexp).\n", argv[optind-1]);
	    errstring += buf;
	  } else
	    setRegExpr(optarg);
          break;
	case 'v':
	  if (!isalpha(optarg[0]) && optarg[0] != '/' && optarg[0] != '.') {
	    snprintf(buf, 320, "HdlUtils::decode_options()2: invalid filename %s for option (-v, -vdir).\n", argv[optind-1]);
	    errstring += buf;
	  } else {
	    //printf("AddVFile %s\n", optarg);
	    ifstream infile (optarg);
	    if (!infile) {
	      snprintf(buf, 320, "HdlUtils::decode_options(): cannot open file %s for option (-v, -vdir).\n", argv[optind-1]);
	      errstring += buf;
	      opt_errors_encountered = 1;
	    } else {
	      infile.close();
	      addVFile(optarg);
	    }
	  }
          break;
	case 'V':
	  //printf("boo\n");
	  //Dbg::Incr();
          break;
	case 'o':
          break;
	case 'y':
	  if (!isalpha(optarg[0]) && optarg[0] != '/' && optarg[0] != '.') {
	    snprintf(buf, 320, "HdlUtils::decode_options(): invalid directory value %s for option (-y, -ydir).\n", argv[optind-1]);
	    errstring += buf;
	  } else {
	    // First try to open the directory
	    DIR *dp;
	    string dirname;
	    cleanup_dirname(optarg, dirname); 
	    if((dp  = opendir(dirname.c_str())) == NULL) {
	      opt_errors_encountered = 1;
	      snprintf(buf, 320, "HdlUtils::decode_options(): cannot open directory %s for option (-y, -ydir).\n", optarg);
	      errstring += buf;
	    }
	    else {
	      addYDir(optarg);
	      closedir(dp);
	    }
	  }
          break;
	case 'D':
	  //printf("here -D %s\n", optarg);
	  addDefMacro(Strings::save(optarg));
          break;
	case 'n':
	  setNoElab();
          break;
	case 't':
	  setSystemVerilog();
          break;
	case 'b':
	  //printf("B: %s\n", optarg);
	  theHdlUtils->setBlackboxLevel(atoi(optarg));
          break;
	case 's':
	  if (!isprint(optarg[0]) && !isspace(optarg[0])) {
	    snprintf(buf, 320, "HdlUtils::decode_options(): invalid suffix value %s for option (-s, -suffix).\n", argv[optind-1]);
	    errstring += buf;
	  } else
	    setCopySuffix(optarg);
          break;
	case '?':
	  opt_errors_encountered = 1;
	  snprintf(buf, 320, "HdlUtils::decode_options(): unrecognized option %s\n", argv[optind-1]);
	  errstring += buf;
	  break;
	default:
	  printf("UNREACHABLE\n");
	  opt_errors_encountered = 1;
	  /* unreachable */
	  break;
	}
    }

  //printf("done decoding\n");
  if (opt_errors_encountered) {
    fprintf(stderr, "%s: exiting\n", "HdlUtils");
    return 0;
  }

  char *identifier;

  while (optind < argc) {

    if (!strncmp(argv[optind], "+libext+", 8)) {
      char *expr;
      expr = argv[optind] + 8;
      if (!expr || strlen(expr) == 0) {
	fprintf(stderr, "%s: ill-formed plusarg '%s' exiting\n", argv[0], argv[optind]);
	return 0;
      }
      setLibExt(Strings::save(expr));
      optind++;
      continue;
    }

    if (!strncmp(argv[optind], "+define+", 8)) {
      char *expr;
      expr = argv[optind] + 8;
      if (!expr || strlen(expr) == 0) {
	fprintf(stderr, "%s: ill-formed plusarg '%s' exiting\n", argv[0], argv[optind]);
	return 0;
      }
      addDefMacro(Strings::save(expr));
      optind++;
      continue;
    }

    if (!strncmp(argv[optind], "+incdir+", 8)) {
      identifier = argv[optind] + 8;
      if (!identifier || strlen(identifier) == 0) {
	fprintf(stderr, "%s: ill-formed plusarg '%s' exiting\n", argv[0], argv[optind]);
	return 0;
      }
      addIDir(identifier);
      optind++;
      continue;
    }
    
    //printf("Inserting %s\n", argv[optind]) ;
    char *f = Strings::save(argv[optind]);

    ifstream infile(f);
    if (!infile) {
      snprintf(buf, 320, "HdlUtils::decode_options(): cannot open file %s for option (-v, -vdir).\n", f);
      errstring += buf;
      opt_errors_encountered = 1;
    } else {
      infile.close();
      addFile(f);
    }
    optind++;
  }

  return opt_errors_encountered==0;
}

// -----------------------------------------------------------------------------
// Find an module base on path
// -----------------------------------------------------------------------------

VeriModule *HdlUtils::findModuleFromPath(VeriModule *from, const std::string &path, std::string &found)
{
  const string leadingDelims(" \t{}/:");
  const string delims("/[}");

  if (path.empty()) return from; // nothing more to find.

  if ((from == NULL) && (path == "/")) {
    return theRootModule; // return the root module
  }

  if ((path[0] == '/') && from) // Absolute path cannot be relative to a module
    return NULL;

  VeriModule *result = 0;

  // Split the path string
  string::size_type start = path.find_first_not_of(leadingDelims);
  if (start == string::npos)  return result ;

  string::size_type endp = path.find_first_of(delims,start);
  string top( path, start, endp-start); // The first token in the path

  endp = (endp == string::npos) ? path.size() : endp+1 ;
  string rest(path, endp); // The rest in the path

  //printf("findModule1: %s\n", path.c_str());
  // Absolute path
  unsigned i ;
  if ((path[0] == '/') || ((from==NULL) && (path[0] != '/'))) {
    // Get all the top level modules
    VeriModule *mod;
    Array *all_top_modules = veri_file::GetTopModules() ;
    // Get a handle of the first top level module, if any.
    FOREACH_ARRAY_ITEM(all_top_modules, i, mod) {
      //printf("Top: %s %s %s\n", top.c_str(), mod->Name(), rest.c_str());
      if (top == mod->Name()) {
        found += "/" + top ;
	//printf("Here Top: %s %s rest:%s\n", top.c_str(), mod->Name(), rest.c_str());
	return HdlUtils::findModuleFromPath(mod, rest, found);
      }
    }
    return NULL;  // nothing found
  }

  //printf("findModule2: %s\n", path.c_str());
  // Relative path
  // Traverse the instances of the module
  InstantiationVisitor instantiation_visitor ;

  from->Accept( instantiation_visitor ) ;
  
  SetIter si; // a Set iterator
  VeriModuleInstantiation *module_inst ;
  FOREACH_SET_ITEM(&(instantiation_visitor._module_insts), si, &module_inst) {
    
    //printf("findModule3: %s\n", path.c_str());
    // Get instances (the indentifiers)
    VeriInstId *inst ;
    Array *inst_arr = module_inst->GetInstances() ;
    FOREACH_ARRAY_ITEM(inst_arr, i, inst) {
      //printf("findModule4: %s\n", path.c_str());
      if (top == inst->Name()) {
        found += "/" + top;
	VeriModule *module_def = module_inst->GetInstantiatedModule();
	return HdlUtils::findModuleFromPath(module_def, rest, found);
      }
    }
  }

  //printf("Return Module: %s\n", result->Name());
  return result;
}

VeriInstId *HdlUtils::findInstFromPath(const std::string &fullpath)
{
  string path, instname, found;
  VeriInstId *inst_id;
  size_t stloc;

  stloc = fullpath.find_last_of('/');
  path = fullpath.substr(0, stloc);
  instname = fullpath.substr(stloc+1);

  VeriModule *parentmod = HdlUtils::findModuleFromPath(NULL, path, found);
  if (parentmod == NULL) {
    return NULL;
  }

  inst_id = HdlUtils::findInstInModuleByName(parentmod, instname);

  return inst_id;
}

VeriModule *HdlUtils::findModuleByName(const char *name)
{
  VeriModule *module;
  string mname = name;
  Array *all_top_modules = veri_file::GetTopModules() ;
  unsigned i;
  FOREACH_ARRAY_ITEM(all_top_modules, i, module) {
    //printf("Found Module %s\n", module->Name());
    if (mname == module->Name())
      return module;
  }
  
  return NULL;
}


VeriInstId *HdlUtils::findInstInModuleByName(VeriModule *from, const std::string &instname)
{
  InstantiationVisitor instantiation_visitor ;
  from->Accept( instantiation_visitor ) ;

  VeriInstId *inst = 0;
  SetIter si; // a Set iterator
  unsigned i ;
  VeriModuleInstantiation *module_inst = 0;

  FOREACH_SET_ITEM(&(instantiation_visitor._module_insts), si, &module_inst) {
    // Get instances (the indentifiers)
    Array *inst_arr = module_inst->GetInstances() ;
    FOREACH_ARRAY_ITEM(inst_arr, i, inst) {
      if (inst->Name() && instname == inst->Name()) {
	return inst;
      }
    }
  }

  return NULL;
}

// class static
unsigned int HdlUtils::findInstsByMasterName (VeriModule *mod, const std::string & mastername, std::list<VeriInstId *> &outlist)
{
  VeriScope *module_scope = mod->GetScope() ;

  LookupByMaster visitor(mastername, outlist);

  SetIter si;
  VeriIdDef *def;
  Set dset;
  Set *dsetp = &dset;
  module_scope->GetDeclaredIds(dset);
  FOREACH_SET_ITEM (dsetp, si, &def) {
    def->Accept(visitor);
  }
  return visitor.getCount();
}

VeriIdDef *HdlUtils::findSignalInModuleByName(VeriModule *from, const std::string &signame)
{
  // Get the scope of this module :
  VeriScope *module_scope = from->GetScope() ;
 
  // Now iterate over the declared identifiers ((VeriIdDef*s) in the hash table (scope->DeclArea()) in this scope :
  MapIter mi ;
  VeriIdDef *id ;
  char *id_name ; 
  FOREACH_MAP_ITEM(module_scope->DeclArea(), mi, &id_name, &id) {

    //printf(" find signal %s\n", id->Name());
    // Rule out all identifiers declared here, except for 'nets' :
    if (!(id->IsNet() || id->IsReg())) continue ;
    
    // Here, 'id' is a 'net', declared in the module
    // Use the extended VhdlIdDef class API (in file VeriId.h) to do what you want
    if (signame == id->Name()) {
      return id;
    }
  }

  return NULL;
}

// Connection is inferred by:
//  1. assign to_signal = from_signal && instance from(..., to_signal, ...)
//  2. or instance from(..., from_signal,..)
//  3. ???
VeriIdDef *HdlUtils::findConnectedIdDef(VeriModule *module, VeriIdDef *def,
					VeriInstId *from, VeriInstId *to)
{
  VeriIdDef *id;

  //printf("hereh1 findConnecgtedIdDef of module %s\n", module->Name());
  // Get all the assignment statements with rhs value of def
  tIdSet id_set = getAssignmentIds(module, def);
  //printf("hereh2\n");
  foreachSetId(id_set, it) {
    //printf("hereh3\n");
    id = (VeriIdDef*) *it;

    //printf("hereh4 %s %s\n", id->Name(), to->Name());
    // Condition 1:
    if (isSignalConnectedToInst(id, to))
      return id;
  }

  if (from) {
    //printf("hereh5 %s %s def %s\n", from->Name(), to->Name(), def->Name());
    // Condition 2:
    if (isSignalConnectedToInst(def, to)) {
      //printf("hereh6\n");
      return def;
    }
  }

  return NULL;
}

void HdlUtils::getListOfConnectedInst(VeriModule *module, VeriIdDef *def, VeriInstIdList &instlist)
{
  VeriIdDef *id;
  InstantiationVisitor instantiation_visitor ;
  VeriInstId *inst = 0;
  SetIter si; // a Set iterator
  unsigned i ;
  VeriModuleInstantiation *module_inst = 0;

  //printf("Getlistof2 connected inst\n");
  module->Accept( instantiation_visitor ) ;
  //printf("Getlistof2 after accept\n");
  FOREACH_SET_ITEM(&(instantiation_visitor._module_insts), si, &module_inst) {
    //printf("each module inst %p\n", module_inst);
    // Get instances (the indentifiers)
    Array *inst_arr = module_inst->GetInstances() ;
    //printf("get instances %p\n", inst_arr);
    FOREACH_ARRAY_ITEM(inst_arr, i, inst) {
      //printf("foo %p %p\n", inst, def);
      //printf("check inst2 %s is connected to signal %s\n", inst->Name(), def->Name());
      if (isSignalConnectedToInst(def, inst))
	//printf("Yes connected2\n");
	instlist.push_back(inst);
    }
  }

  //printf("Getlistof connected inst\n");
  // Get all the assignment statements with rhs value of def
  tIdSet id_set = getAssignmentIds(module, def);
  foreachSetId(id_set, it) {
    id = (VeriIdDef*) *it;
    //printf("Found rhs %s\n", id->Name());

    //module->Accept( instantiation_visitor ) ;
    FOREACH_SET_ITEM(&(instantiation_visitor._module_insts), si, &module_inst) {
      // Get instances (the indentifiers)
      Array *inst_arr = module_inst->GetInstances() ;
      FOREACH_ARRAY_ITEM(inst_arr, i, inst) {
	//printf("check inst1 %s is connected to signal %s\n", inst->Name(), id->Name());
	if (isSignalConnectedToInst(id, inst))
	  //printf("Yes connected1\n");
	  instlist.push_back(inst);
      }
    }
  }

  instlist.unique();
}

int HdlUtils::isSignalConnectedToInst(VeriIdDef *def, VeriInstId *inst)
{
  unsigned i;
  VeriExpression *expr, *value;
  Array *port_connects = inst->GetPortConnects();
  std::ostringstream strm;
  //VeriIdDef *port_def;

  FOREACH_ARRAY_ITEM(port_connects, i, expr) {
    
    strm.str("");
    //printf("isSignalConnectedToInst compare %s\n", def->Name());
    if (expr->GetClassId() == ID_VERIPORTCONNECT) {
      value = expr->GetConnection();
      value->PrettyPrint(strm, 0);
      //printf("value %s\n", strm.str().c_str());
      if (strm.str() == def->Name())
	return 1;
    } else if (expr->IsIdRef()) {
      expr->PrettyPrint(strm, 0);
      //printf("positional %s\n", strm.str().c_str());
      if (strm.str() == def->Name()) {
	//printf("FOUND connected %s\n", def->Name());
	return 1;
      }
    }
  }
  return 0;
}


void HdlUtils::findClockSignalInModule(VeriModule *from, std::string &return_clk_name)
{
  std::string exp = "[c|C][l|L][k|K].*";
  
  // Set up regex
  boost::regex e(exp);
  
  // Get the scope of this module :
  VeriScope *module_scope = from->GetScope() ;
  
  // Now iterate over the declared identifiers ((VeriIdDef*s) in the hash table (scope->DeclArea()) in this scope :
  MapIter mi ;
  VeriIdDef *id ;
  char *id_name ;
  FOREACH_MAP_ITEM(module_scope->DeclArea(), mi, &id_name, &id) {
    // Rule out all identifiers declared here, except for 'nets' :
    if (!(id->IsNet() || id->IsReg())) continue ;
    
    // Here, 'id' is a 'net', declared in the module
    // Use the extended VhdlIdDef class API (in file VeriId.h) to do what you want
    if (boost::regex_match(id->Name(), e)) {
      return_clk_name = id->Name();
      return;
    }
  }

  // If not found, let's try matching with anything in the middle of the string
  exp = ".*[c|C][l|L][k|K].*";
  
  // Set up regex
  boost::regex e2(exp);
  FOREACH_MAP_ITEM(module_scope->DeclArea(), mi, &id_name, &id) {
    // Rule out all identifiers declared here, except for 'nets' :
    if (!(id->IsNet() || id->IsReg())) continue ;
    
    // Here, 'id' is a 'net', declared in the module
    // Use the extended VhdlIdDef class API (in file VeriId.h) to do what you want
    if (boost::regex_match(id->Name(), e2)) {
      return_clk_name = id->Name();
      return;
    }
  }
}

int HdlUtils::findWidthOfSignal(VeriIdDef *id)
{
  int m, l, width;
  width = id->GetPackedWidth(&m, &l);

  return width;
}

void HdlUtils::findSignalsInModuleByExpression(VeriModule *module, std::string exp,
					       netCollection &sigs, unsigned int &thiswidth,
					       SignalType signaltype)
{
  int m,l;
  std::map<BString, bool> sigsmap;

  // backslashing special character
  qualifySpecialChar(exp);

  // Set up regex
  boost::regex e(exp);

  // Get the scope of this module :
  VeriScope *module_scope = module->GetScope() ;

  // Initialize width
  thiswidth = 0;
 
    
  // Now iterate over the declared identifiers ((VeriIdDef*s) in the hash table (scope->DeclArea()) in this scope :
  MapIter mi ;
  VeriIdDef *id ;
  char *id_name ;
  char buf[100];
  int width;
  //printf("Find exp %s\n", exp.c_str());
  FOREACH_MAP_ITEM(module_scope->DeclArea(), mi, &id_name, &id) {
 
    //printf("find signal %s\n", id->Name());
    // If net type does not match, skip it
    if (!isNetTypeMatched(id, signaltype)) continue;
    
    // Here, 'id' is a 'net', declared in the module
    // Use the extended VhdlIdDef class API (in file VeriId.h) to do what you want
    if (boost::regex_match(id->Name(), e)) {
      width = id->GetPackedWidth(&m, &l);
      snprintf(buf,99, "%s:%d", id->Name(), width);
      sigs.push_back(buf);

      // Mark the signal as has been taken in
      if (signaltype & CktModFlop)
	sigsmap[buf] = true;

      thiswidth += width;
    }
  }

  // If flop signaltype is selected then we have to go find all the flop signals
  // and make sure there is no duplicate
  if (signaltype & CktModFlop)
    findFlopSignalsInModuleByExpression(module, exp, sigs, thiswidth, signaltype, &sigsmap);

  return;
}

bool HdlUtils::isNetTypeMatched(VeriIdDef *id, SignalType signaltype) {

  // Rule out all identifiers declared here, except for 'nets' :
  if (signaltype & CktModAny) {
    if (id->IsNet() || id->IsReg()) return true;
  }

  // Check for reg type
  if ((signaltype & CktModReg) && (id->IsReg())) return true;
    
  // Check for input type
  if ((signaltype & CktModInput) && (id->IsInput() || id->IsInout())) return true;
    
  // Check for output type
  if ((signaltype & CktModOutput) && (id->IsOutput() || id->IsInout())) return true;
    
  return false;
}

void HdlUtils::findFlopSignalsInModule(VeriModule *module, netCollection &sigs,
				       unsigned int &thiswidth)
{
  int m,l,width;
  char buf[100];

  // Initialize width
  thiswidth = 0;

  VeriEventControlStatement *stmt;
  VeriIdDef *id;
  VeriStatement *body;
  foreachAlways(module, stmt) {
    if (isClocked(stmt)) {
      body = stmt->GetStmt();
      tIdSet id_set = getAssignmentIds(body);
      foreachSetId(id_set, it) {
	id = (VeriIdDef*) *it;
	width = id->GetPackedWidth(&m, &l);
	snprintf(buf,99, "%s:%d", id->Name(), width);
	sigs.push_back(buf);
	thiswidth += width;
      }
    }
  }
  return;
}

void HdlUtils::findFlopSignalsInModuleByExpression(VeriModule *module, std::string exp,
						   netCollection &sigs,
						   unsigned int &thiswidth,
						   SignalType signaltype,
						   std::map<BString, bool> *sigsMap)
{
  int m,l,width;
  char buf[100];

  // If Flop type is not specified then nothing is to be found
  if ((signaltype & CktModFlop) == 0)
    return;

  // Initialize width
  thiswidth = 0;

  // backslashing special character
  qualifySpecialChar(exp);

  // Set up regex
  boost::regex e(exp);

  VeriEventControlStatement *stmt;
  VeriIdDef *id;
  VeriStatement *body;
  foreachAlways(module, stmt) {
    if (isClocked(stmt)) {
      body = stmt->GetStmt();
      tIdSet id_set = getAssignmentIds(body);
      foreachSetId(id_set, it) {
	id = (VeriIdDef*) *it;

	if (boost::regex_match(id->Name(), e)) {
	  width = id->GetPackedWidth(&m, &l);
	  snprintf(buf,99, "%s:%d", id->Name(), width);
	  // If map table is given then we make the signal
	  // is not already in the sigs netCollection
	  if (sigsMap && ((*sigsMap)[buf] == false)) {
	    sigs.push_back(buf);
	    thiswidth += width;
	  } else {
	    sigs.push_back(buf);
	    thiswidth += width;
	  }
	}
      }
    }
  }
  return;
}

void HdlUtils::findBSVProbeSignalsInModule(VeriModule *module, netCollection &sigs,
					   unsigned int &thiswidth, SignalType signaltype)
{
  std::string pattern = ".*$PROBE";

  findSignalsInModuleByExpression(module, pattern, sigs, thiswidth, signaltype);
}

VeriIdDef *HdlUtils::findPortOfModuleByName(VeriModule *module, std::string &portname)
{
  PortVisitor visitor;
  
  module->Accept(visitor) ;

  SetIter si; // a Set iterator
  VeriIdDef *port;
  FOREACH_SET_ITEM(&(visitor._port_connects), si, &port) {

    if (portname == port->Name())
      return port;
  }

  return 0;
}

VeriIdDef *HdlUtils::findPortOfModuleByPosition(VeriModule *module, unsigned int pos)
{
  PortVisitor visitor;
  unsigned int i = 0;
  
  module->Accept(visitor) ;

  SetIter si; // a Set iterator
  VeriIdDef *port;
  FOREACH_SET_ITEM(&(visitor._port_connects), si, &port) {

    if (i == pos)
      return port;
    i++;
  }

  return 0;
}
  
VeriIdDef *HdlUtils::findPortOfInstByName(VeriInstId *inst, std::string &portname)
{
  VeriModule *module_def = inst->GetInstantiatedModule();
  
  if (module_def == NULL)
    return 0;

  Array *port_connects = module_def->GetPortConnects();
  VeriExpression *expr;
  VeriExpression *found_expr = NULL;
  VeriExpression *conn;
  std::ostringstream conn_name;
  unsigned it;
  std::string conn_string;
  FOREACH_ARRAY_ITEM(port_connects, it, expr) {
    
    if (expr->GetClassId() == ID_VERIPORTCONNECT) {
      if (portname == expr->NamedFormal()) {
	found_expr = expr;
	break;
      }
    }
    //else
    //  break;
  }

  conn_string = portname;
  if (found_expr != NULL) {
    if ((conn = expr->GetConnection()) != NULL) {
      conn->PrettyPrint(conn_name, 0);
      conn_string = conn_name.str();
    }
  }

  PortVisitor visitor;
  SetIter si; // a Set iterator
  VeriIdDef *port;

  module_def->Accept(visitor) ;
  FOREACH_SET_ITEM(&(visitor._port_connects), si, &port) {
    
    if (conn_string == port->Name()) {
      return port;
    }
  }
  
  return 0;
}

void HdlUtils::print_help (const char* progname)
{
  /* Spaces and tabs are significant in this message; they're chosen so the
     message aligns properly both in a tty and in a Windows message box.
     Please try to preserve them; otherwise the output is very hard to read
     when using vprocess.  */
  fprintf(stdout, "Usage: %s [OPTIONS] VERILOG_FILE VERILOG_FILE ...\n\
       The following OPTIONS are accepted:\n", progname);
}

unsigned int HdlUtils::getNetName(VeriIdDef *netid, std::string &netname)
{
  if (!(netid->IsNet() || netid->IsReg()))
    return 0;

  for (unsigned int i = 0; i<netid->PackedDimension() +netid->UnPackedDimension(); ++i) {
    VeriRange *range = netid->GetDimensionAt(i);
    if (range) {
      char buf[100];
      snprintf(buf,99," [%d:%d]",range->GetMsbOfRange(),  range->GetLsbOfRange() );
      netname += buf;
    }
  }

  switch (netid->Dir())
    {
    case VERI_INPUT:
      netname += " \t(input)";
      break;
    case VERI_OUTPUT:
      netname += " \t(output)";
      break;
    case VERI_INOUT:
      netname += " \t(inout)";
      break;
    }

  return 0;
} 

VeriModule* HdlUtils::copyModule(VeriModule *module, const char *newName, VeriLibrary *lib)
{
  if (newName == NULL)
    return NULL;

  // Check in the library if module with above made up name exists, if present, return that.
  if (lib == NULL)
    lib = module->GetLibrary() ;
  VeriModule *new_top = (lib) ? lib->GetModule(newName, 1) : 0 ;

  if (!new_top) {
    // Module does not already exist, copy this module with the new name:
    VeriMapForCopy old2new ; // original VeriIdDef -> copied VeriIdDef association Map

    (void) VeriTreeNode::CreateInstantiatedUnitName(module->GetId(), newName, lib, old2new) ;
    new_top = module->CopyModule(old2new) ;

    // Add the copied module to library containing the top module:
    if (lib && !lib->AddModule(new_top)) new_top = 0 ;
  }

  // Overwrite the current position with this new top
  //if (new_top) _top_modules->Insert(i, new_top) ;

  return new_top;
}

VeriModule* HdlUtils::getOriginalUnelabModule(VeriModule *module)
{
  string newname;
  if (module->GetOriginalModuleName())
    newname = module->GetOriginalModuleName();
  else
    newname = module->GetName();
  newname += getCopySuffix();

  // Check in the library if module with above made up name exists, if present, return that.
  if (HdlUtils::_temp_library == NULL)
    HdlUtils::_temp_library = new VeriUserLibrary("Temp");

  VeriModule *new_module;
  new_module = HdlUtils::_temp_library->GetModule(newname.c_str(), 1);
  
  return new_module;
}

VariablePrintStyle HdlUtils::getPortConnectsPrintStyle(VeriInstId *inst_id)
{
  unsigned i;
  VeriExpression *expr;
  Array *port_connects = inst_id->GetPortConnects();

  FOREACH_ARRAY_ITEM(port_connects, i, expr) {
  
    if (expr->IsIdRef())
      return HdlPositionalStyle;
    else if (expr->GetClassId() == ID_VERIPORTCONNECT)
      return HdlKeyValuePairStyle;
    else
      return HdlKeyValuePairStyle;
  }

  return HdlKeyValuePairStyle;
}

VeriConst *HdlUtils::mkVeriConst (const string &str)
{
  VeriConst *ret = 0;
  string::size_type tick = str.find("'");
  if (tick == string::npos) {
    // not a based number, just an int
     try {
       int val = boost::lexical_cast<int>(str);
       ret = new VeriIntVal(val);
     } catch(boost::bad_lexical_cast &) {
       // Ignore the error
     }
  }
  else {                        // 'b
    unsigned sz = 0;
    if (tick != 0) {
      int size;
      if (stringToInt (str.substr(0,tick),size) && size > 0 ) {
        sz = size;
      }
    }
    ret = new VeriConstVal (str.substr(tick).c_str(), VERI_BASED_NUM, sz);
  }
  return ret;
}

bool HdlUtils::stringToInt( const string & str, int &value)
{
  bool valid;
  try {
    int val = boost::lexical_cast<int>(str);
    valid = true;
    value = val;
  } catch(boost::bad_lexical_cast &) {
    valid = false;
  }
  return valid;
}



unsigned HdlUtils::addPortRefKeyValue(VeriModule *module, VeriInstId *inst_id,
				      string &portname, string &netname)
{
  string value;
  unsigned ret;

  value = netname;

  VeriConst *constvalue = mkVeriConst (value);
  if (constvalue != 0) {
    ret = inst_id->AddPortRef(portname.c_str(),
			      constvalue,
			      module->GetScope());
  }
  else
    ret = inst_id->AddPortRef(portname.c_str(),
			      new VeriIdRef(Strings::save(value.c_str())),
			      module->GetScope());

  return ret;
}

unsigned HdlUtils::addPortRefPositional(VeriModule *module, VeriInstId *inst_id,
					string &netname)
{
  string value;
  unsigned ret;

  value = netname;

    VeriConst *constvalue = mkVeriConst (value);
    if (constvalue != 0) {
    ret = inst_id->AddPortRef(NULL,
			      constvalue,
			      module->GetScope());
  }
  else
    ret = inst_id->AddPortRef(NULL,
			      new VeriIdRef(Strings::save(value.c_str())),
			      module->GetScope());

  return ret;
}

VeriRange * HdlUtils::mkVeriRangeFromWidth(unsigned int width)
{
  VeriRange *range = 0;
  if (width == 0) {
    cerr << "Creating a Range with 0 width!!!" << endl;
  }
  else if (width == 1) {
    range = 0;
  }
  else {
    VeriIntVal *upper = new VeriIntVal(width-1);
    VeriIntVal *lower = new VeriIntVal(0);
    range = new VeriRange(upper, lower);
  }
  return range;
}


bool HdlUtils::isClocked(VeriEventControlStatement *event_control_stmt, BStringSet *clock_set)
{
  // Look at the sensitivity list (the "@()" clause), and check if it has 'edge' expressions :
  Array *sens_list = event_control_stmt->GetAt() ;
  unsigned it_0;
  bool is_clocked = false;
  VeriExpression *event_expr ;
  FOREACH_ARRAY_ITEM(sens_list, it_0, event_expr) {
    if (!event_expr) break ; // invalid sensitivity list
    if (event_expr->IsEdge(0/*any edge (pos or neg)*/)) {
      if (clock_set == NULL) {
	is_clocked = true ;
//	printf("CLOCKED BY %s\n", event_expr->GetName());
	break ; // no need to check the other sensitivity list items. This statement is clocked !
      }
      if (clock_set->find(event_expr->GetName()) != clock_set->end()) {
//	printf("CLOCKED BY UCLOCK %s\n", event_expr->GetName());
	is_clocked = true ;
	break ; // no need to check the other sensitivity list items. This statement is clocked !
      }
    }
  }

  return is_clocked;
}

tIdSet  HdlUtils::getAssignmentIds(VeriTreeNode* node, VeriIdDef* rh)
{
  AssignmentIdVisitor assign_visitor(rh);
  node->Accept( assign_visitor ) ;
  
  return assign_visitor._assigned_ids;
}

tIdSet  HdlUtils::getContAssignmentIds(VeriTreeNode* node, VeriIdDef* rh)
{
  ContAssignmentIdVisitor assign_visitor(rh);
  node->Accept( assign_visitor ) ;
  
  return assign_visitor._assigned_ids;
}

VeriExpression* HdlUtils::mkExpression(const char* value)
{
  VeriExpression* expr = new VeriConstVal(value, VERI_STRING);
  return expr;
}

VeriExpression* HdlUtils::mkExpression(int value)
{
  VeriExpression* expr = new VeriIntVal(value);
  return expr;
}

VeriIdRef* HdlUtils::ref(VeriIdDef* id)
{
  return (new VeriIdRef(id));
}

VeriExpression* HdlUtils::createConcatExpression(VeriModule* module, tIdList all_ids, ScanPath** scan_path)
{

  Array *concat = new Array();

  ScanPath* path = *scan_path;

  int total = 0;
  foreachListId(all_ids, it) {
    VeriIdDef* id = (VeriIdDef*) *it;

    const char* name = id->Name();

    VeriDataType* dt = id->GetDataType();

    if (!dt) { // this is needed for 1-bit VERI_INPUTs ... not sure why.
      VeriRange *range = HdlUtils::mkVeriRangeFromWidth(1);
      dt = new VeriDataType(VERI_WIRE, 0, range);
    }
    
    if (dt->IsIntegerType()) {
//      Dbg::printf("SKIPPING INTEGER! %s\n", full_name.c_str());
      continue;
    }

    if (id->IsMemory() && id->UnPackedDimension() > 1) {
      fprintf(stderr, "Unhandled case. Array '%s' dimension is > 1, Ignoring.\n", id->Name()); 
      continue;
    }

    if (id->IsMemory()) {
//      fprintf(stdout, "Skipping array '%s'for state collection.\n", id->Name()); 
//      continue;

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

    ScanPath* path_extra = new ScanPath((char*) "_SCAN", mkExpression(123));
    path->Prepend(path_extra);

    return expr;

  } else {
    
    return NULL;
  }
}

unsigned int HdlUtils::getSignalSize(VeriIdDef* id)
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

const char* HdlUtils::getImage(VeriTreeNode* node)
{
  std::ostringstream outs;
  node->PrettyPrint (outs, 1);  
  return strdup(outs.str().c_str());
}

void HdlUtils::visitRefs(VeriTreeNode* node)
{

  IdRefVisitor ref_visitor;
  node->Accept( ref_visitor );

}

// -----------------------------------------------------------------------------
// Find module/paths base on pattern
// -----------------------------------------------------------------------------

void HdlUtils::findPathsFromPattern(const BString & pattern, BStringList &paths)
{
  //printf("Find path from pattern %s\n", pattern.c_str());
  if (pattern.empty()) return; // nothing to find

  if (pattern == "/") {
    paths.push_back("/");
    return; // return the root path
  }

  BString dummy;
  //printf("Calling recusriveFindAllPaths\n");
  if (allpaths.size() == 0)
    recursiveFindAllPaths(NULL, allpaths, dummy);

  // Set up regex
  boost::regex e(pattern);
  
  std::list<BString>::iterator sitr;
  for (sitr = allpaths.begin(); sitr != allpaths.end(); sitr++) {

    //printf("Matching %s with %s\n", (*sitr).c_str(), pattern.c_str());
    if (boost::regex_match(*sitr, e)) {
      paths.push_back(*sitr);
    }
  }
}

void HdlUtils::recursiveFindAllPaths(VeriModule *from, BStringList &paths, BString &frompath)
{
  unsigned i ;
  BString topath;

  //printf("recursiveFindAllPaths from %s\n", frompath.c_str());

  // Get all the top level modules
  if (from == NULL) {
    VeriModule *mod;
    Array *all_top_modules = veri_file::GetTopModules() ;
    // Get a handle of the first top level module, if any.
    FOREACH_ARRAY_ITEM(all_top_modules, i, mod) {
      //printf("Mod name %s\n", mod->Name());
      topath = frompath + "/" + mod->Name();
      HdlUtils::recursiveFindAllPaths(mod, paths, topath);
    }
    return;
  }

  // Traverse the instances of the module
  InstantiationVisitor instantiation_visitor ;

  from->Accept( instantiation_visitor ) ;

  SetIter si; // a Set iterator
  VeriModuleInstantiation *module_inst ;
  FOREACH_SET_ITEM(&(instantiation_visitor._module_insts), si, &module_inst) {
    
    // Get instances (the indentifiers)
    VeriInstId *inst ;
    Array *inst_arr = module_inst->GetInstances() ;

    FOREACH_ARRAY_ITEM(inst_arr, i, inst) {
      topath = frompath + "/" + inst->Name();
      //printf("findModule4: %s\n", topath.c_str());
      VeriModule *module_def = module_inst->GetInstantiatedModule();
      if (module_def) {
	//printf("HEREEEE module_def %s\n", module_def->Name());
	HdlUtils::recursiveFindAllPaths(module_def, paths, topath);
      }
    }
  }

  // Found a valid path
  paths.push_back(frompath);
  //printf("findModule3: %s\n", frompath.c_str());

  return;
}

bool HdlUtils::isBlackboxed(VeriIdDef* id)
{
  VeriDataType* dt = id->GetDataType();

  if (!dt) { // this is needed for 1-bit VERI_INPUTs ... not sure why.
    return false;
  }
    
  if (dt->IsIntegerType()) {
    return false;
  }

  if (id->IsMemory() && id->UnPackedDimension() > 1) {
    fprintf(stderr, "ERROR: Unhandled case. Array '%s' dimension is > 1, Ignoring.\n", id->Name()); 
    return false;
  }

  if (id->IsMemory()) {

    VeriRange* r0 = id->GetDimensionAt(0);
    VeriRange* r1 = id->GetDimensionAt(1);

    r0->StaticReplaceConstantExpr(1);
    r1->StaticReplaceConstantExpr(1);

    unsigned int min = 0, max = 0, size = 0;

    min = r0->GetLsbOfRange();
    max = r0->GetMsbOfRange();

    if (min > max) {
      min = r0->GetMsbOfRange();
      max = r0->GetLsbOfRange();
    }

    size = max - min + 1;

    if (isBlackboxed(size)) {
//      fprintf(stdout, "Array id '%s' is blackboxed (%d).\n", id->Name(), size);
      return true;
    }
  }

  return false;
}

bool HdlUtils::isBlackboxed(VeriInstId* id, BStringSet & uclocks)
{

  VeriModule* mod    = id->GetInstantiatedModule();
  VeriModule* module = getOriginalUnelabModule(mod);
  const char* name   = module->GetName();

  static const char* modNames[] = {"CrossingRegN", "CrossingRegA", "CrossingRegUN", "CrossingBypassWire", 0};

  int i;
  for(i=0; modNames[i] != 0;i++) {
    std::string mod_name = modNames[i];
    mod_name += "_orig";
    if (mod_name.compare(name) == 0) {

      Array *port_connects = id->GetPortConnects();
      unsigned it;
      VeriExpression *expr, *conn;
      std::ostringstream conn_name;
      FOREACH_ARRAY_ITEM(port_connects, it, expr) {
	if (expr->GetClassId() == ID_VERIPORTCONNECT) {
	  BString formal = expr->NamedFormal();
	  if (formal.compare("CLK") != 0 && formal.compare("sCLK") != 0) continue;
	  conn_name.str("");
	  if ((conn = expr->GetConnection()) != NULL) {
	    conn->PrettyPrint(conn_name, 0);
	  }
	  if (uclocks.end() != uclocks.find(conn_name.str())) {
//	    printf("BLACK: %s\n", id->GetName());
	    return true;
	  }
	}
      }
    }
  }
  return false;
}

int HdlUtils::file_exists (char * fileName)
{
  struct stat buf;
  int i = stat ( fileName, &buf );
  /* File found */
  if ( i == 0 )
    {
      return 1;
    }
  return 0;
}

void HdlUtils::createModuleParameterList (Verific::VeriModule* module, ParameterList &plist)
{
  Array *params = module->GetParameters();
  unsigned itema;
  VeriIdDef *para;
  FOREACH_ARRAY_ITEM (params, itema, para) {
    VeriExpression *expr = para->GetInitialValue();
    //VeriConst *exprC = expr->ConstCast();
    
    char * val = expr->Image();
    printf("Parameter %s %s\n", para->Name(), val);
    plist.push_back(Parameter (para->Name(), val));
  }
}

ModuleTerminal *HdlUtils::locateModuleTerminal(ModuleTerminalList &mtlist, std::string &name)
{
  ModuleTerminal *terminal;
  ModuleTerminalIterator mtItr;

  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    //printf(" Found terminal %s\n", terminal->m_portName.c_str());
    if (terminal->m_portName == name)
      return terminal;
  }

  return NULL;
}

void HdlUtils::createModuleTerminalList (VeriModule* module, ModuleTerminalList &mtlist,
					 const char *prefix, std::map<string, int> *map)
{
  unsigned it;
  VeriIdDef *port;
  int size;
  BString blank;
  BString pf;
  VeriRange *range;
  int size_sign, min, max;

  if (prefix) pf = prefix;
  FOREACH_ARRAY_ITEM(module->GetPorts(), it, port) {
    if (port->IsInput()) {

      range = port->GetDimensionAt(0);
      size = 1;

      if (range) {
	range->StaticReplaceConstantExpr(1);
	min = range->GetLsbOfRange();
	max = range->GetMsbOfRange();

	if (min > max) {
	  min = range->GetMsbOfRange();
	  max = range->GetLsbOfRange();
	}

	size = max - min + 1;
      }
      else {
	size_sign = port->StaticSizeSign() ;
	size = GET_CONTEXT_SIZE(size_sign) ; // Size
	//unsigned is_signed = GET_CONTEXT_SIGN(size_sign) ; // Signing
      }

      blank = "";
      if (prefix) {
	if (map && (*map)[port->Name()])
	  blank = port->Name();
	else
	  blank = pf + port->Name();
      }
      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_input, size));
    } else if (port->IsOutput()) {

      range = port->GetDimensionAt(0);
      size = 1;

      if (range) {
	range->StaticReplaceConstantExpr(1);
	min = range->GetLsbOfRange();
	max = range->GetMsbOfRange();

	if (min > max) {
	  min = range->GetMsbOfRange();
	  max = range->GetLsbOfRange();
	}

	size = max - min + 1;
      }
      else {
	size_sign = port->StaticSizeSign() ;
	size = GET_CONTEXT_SIZE(size_sign) ; // Size
	//unsigned is_signed = GET_CONTEXT_SIGN(size_sign) ; // Signing
      }

      blank = "";
      if (prefix) {
	if (map && (*map)[port->Name()])
	  blank = port->Name();
	else
	  blank = pf + port->Name();
      }
      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_output, size));

    } else {

      range = port->GetDimensionAt(0);
      size = 1;

      if (range) {
	range->StaticReplaceConstantExpr(1);
	min = range->GetLsbOfRange();
	max = range->GetMsbOfRange();

	if (min > max) {
	  min = range->GetMsbOfRange();
	  max = range->GetLsbOfRange();
	}

	size = max - min + 1;
      }
      else {
	size_sign = port->StaticSizeSign() ;
	size = GET_CONTEXT_SIZE(size_sign) ; // Size
	//unsigned is_signed = GET_CONTEXT_SIGN(size_sign) ; // Signing
      }

      blank = "";
      if (prefix) {
	if (map && (*map)[port->Name()])
	  blank = port->Name();
	else
	  blank = pf + port->Name();
      }
      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_inout, size));
    }
  }
}

void HdlUtils::createModuleTerminalList (VeriModule* module, ModuleTerminalList &mtlist,
					 const char *prefix, std::map<string, string> *map)
{
  unsigned it;
  VeriIdDef *port;
  int size;
  BString blank;
  BString pf;
  if (prefix) pf = prefix;

  FOREACH_ARRAY_ITEM(module->GetPorts(), it, port) {
    if (port->IsInput()) {
      VeriRange* range = port->GetDimensionAt(0);

      size = 1;

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
      //printf("SIZE: %d\n", size);
      //ssize = itoa(size);
      //ssize += "'b0";
      blank = "";
      if (prefix) {
	string foo = (*map)[port->Name()];
	if (map && (*map)[port->Name()] != "")
	  blank = (*map)[port->Name()];
	else
	  blank = pf + port->Name();
      }
      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_input, size));
    } else if (port->IsOutput()) {
      VeriRange* range = port->GetDimensionAt(0);

      size = 1;

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
      blank = "";
      if (prefix) {
	string foo = (*map)[port->Name()];
	if (map && (*map)[port->Name()] != "")
	  blank = (*map)[port->Name()];
	else
	  blank = pf + port->Name();
      }
      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_output, size));
    } else {
      VeriRange* range = port->GetDimensionAt(0);

      size = 1;

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

      blank = "";
      if (prefix) {
	if (map && (*map)[port->Name()] != "")
	  blank = (*map)[port->Name()];
	else
	  blank = pf + port->Name();
      }
      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_inout, size));
    }
  }
}

void HdlUtils::assignRdyEnableBitMaps(RdyEnableInterface *rei, VeriModule *module)
{
  int size;
  char stemp[64];
  std::string portname, lstr, postfix;
  unsigned it;
  VeriIdDef *port;
  int count;

  std::map<std::string, std::string> *bitMap = rei->getBitMap();
  if (bitMap->size() > 0)
    return;

  std::map<std::string, int> *sizeMap = rei->getSizeMap();

  FOREACH_ARRAY_ITEM(module->GetPorts(), it, port) {
    // Check if the port belongs to rdy/enable group
    lstr = port->Name();

    if (rei != RdyEnableIfc[lstr]) continue;

    VeriRange* range = port->GetDimensionAt(0);
    
    size = 1;
    
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

    (*sizeMap)[port->Name()] = size;
  }

  rei->setCount(rei->getWidth());

  std::list<std::string>::iterator stItr;
  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {

    portname = *stItr;
    count = rei->getCount();
    size = (*sizeMap)[portname];
    if (size > 1) {

      sprintf(stemp, "[%d:%d]", count-1, count-size);
      rei->setCount(count-size);
      postfix  = stemp;

    } else if (size == 1) {

      if (((count-1) == 0) && (rei->getWidth() == 1)) {
	stemp[0] = '\0';
      } else {
	sprintf(stemp, "[%d]", count-1);
      }
      rei->setCount(count-1);
      postfix  = stemp;
    }

    (*bitMap)[portname] = postfix;
  }
}

void HdlUtils::createModuleTerminalList2 (VeriModule* module, ModuleTerminalList &mtlist,
					  std::map<string, string> *map)
{
  unsigned it;
  VeriIdDef *port;
  int size;
  BString blank;
  BString pf;
  RdyEnableInterface *rei;
  MemoryInterface *mi;
  std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();
  //std::map<std::string,MemoryInterface*> &MemoryReqIfc = HdlUtils::init()->getMemoryReqIfc();
  //std::map<std::string,MemoryInterface*> &MemoryRespIfc = HdlUtils::init()->getMemoryRespIfc();
  std::map<std::string,MemoryInterface*> &MemoryReqPortIfc = HdlUtils::init()->getMemoryReqPortIfc();
  std::map<std::string,MemoryInterface*> &MemoryRespPortIfc = HdlUtils::init()->getMemoryRespPortIfc();
  string lstr;
  int count = 0;
  char stemp[64];
  string ifc_lowercase;

  if (map == NULL)
    map = &HdlUtils::getControlSignalsMap();

  // Initialize bookeeping
  FOREACH_ARRAY_ITEM(module->GetPorts(), it, port) {
    // Check if the port belongs to rdy/enable group
    lstr = port->Name();
    rei = RdyEnableIfc[lstr];
    if (rei) {
      rei->setCount(rei->getWidth());
    }
  }

  FOREACH_ARRAY_ITEM(module->GetPorts(), it, port) {
    // Check if the port belongs to rdy/enable group
    lstr = port->Name();
    rei = RdyEnableIfc[lstr];
    mi = MemoryReqPortIfc[lstr];
    if (mi == NULL) {
      mi = MemoryRespPortIfc[lstr];
    }
    //printf("Found REI %s port name %s\n", lstr.c_str(), port->Name());
    if (port->IsInput()) {
      VeriRange* range = port->GetDimensionAt(0);

      size = 1;

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

      //printf("SIZE: %d\n", size);
      //ssize = itoa(size);
      //ssize += "'b0";
      blank = "";
      string foo = (*map)[port->Name()];
      if (map && (*map)[port->Name()] != "") {
	blank = (*map)[port->Name()];
      }
      else {
	if (rei) {
	  ifc_lowercase = rei->getInterface();
	  toLowerCase(ifc_lowercase);
	  if (string(rei->getEn()) == port->Name()) {
	    blank = string("EN_") + ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	  }
	  else if (string(rei->getRdy()) == port->Name()) {
	    blank = string("RDY_") + ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	  }
	  else { // Data case
	    count = rei->getCount();
	    if (size > 1) {
	      sprintf(stemp, "[%d:%d]", count-1, count-size);
	      rei->setCount(count-size);
	    }
	    else if (size == 1) {
	      if (((count-1) == 0) && (rei->getWidth() == 1)) {
		stemp[0] = '\0';
	      } else {
		sprintf(stemp, "[%d]", count-1);
	      }
	      rei->setCount(count-1);
	    }
	    blank += ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	    blank += stemp;
	  }
	} else if (mi) {
	  if (mi->isRsp()) {
	    if (string(mi->getEn()) == port->Name()) {
	      blank = string("EN_") + mi->getInterface() + "_response_put";
	    } else {
	      blank = string(mi->getInterface()) + "_response_put";
	    }
	  }
	  else if (mi->isReq()) {
	    blank = string("EN_") + mi->getInterface() + "_request_get";
	  }
	}
	else {
	  if (!HdlUtils::isPortLockstep())
	    blank = string(port->Name()) + "_put";
	  else
	    blank = string(port->Name()) + "_value";
	  toLowerCase(blank);
	}
      }

      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_input, size));
      //printf("new terminal1 %s %s\n", port->GetName(), blank.c_str());
    } else if (port->IsOutput()) {
      VeriRange* range = port->GetDimensionAt(0);
      
      size = 1;
      
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

      blank = "";
      string foo = (*map)[port->Name()];
      if (map && (*map)[port->Name()] != "") {
	blank = (*map)[port->Name()];
      }
      else {
	if (rei) {
	  ifc_lowercase = rei->getInterface();
	  toLowerCase(ifc_lowercase);
	  if (string(rei->getEn()) == port->Name()) {
	    blank = string("EN_") + ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	  }
	  else if (string(rei->getRdy()) == port->Name()) {
	    blank = string("RDY_") + ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	  }
	  else { // Data case
	    count = rei->getCount();
	    if (size > 1) {
	      sprintf(stemp, "[%d:%d]", count-1, count-size);
	      rei->setCount(count-size);
	    }
	    else if (size == 1) {
	      if (((count-1) == 0) && (rei->getWidth() == 1)) {
		stemp[0] = '\0';
	      } else {
		sprintf(stemp, "[%d]", count-1);
	      }
	      rei->setCount(count-1);
	    }
	    blank += ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	    blank += stemp;
	  }
	}
	else if (mi) {
	  if (mi->isReq()) {
	    if (string(mi->getRdy()) == port->Name()) {
	      blank = string("RDY_") + mi->getInterface() + "_request_get";
	    } else {
	      blank = string(mi->getInterface()) + "_request_get";
	    }
	  } else if (mi->isRsp()) {
	    blank = string("RDY_") + mi->getInterface() + "_response_put";
	  }
	}
	else {
	  if (!HdlUtils::isPortLockstep())
	    blank = string(port->Name()) + "_get";
	  else
	    blank = string(port->Name());
	  toLowerCase(blank);
	}
      }
      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_output, size));
      //printf("new terminal2 %s %s\n", port->GetName(), blank.c_str());
    } else {
      VeriRange* range = port->GetDimensionAt(0);
      
      size = 1;
      
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

      blank = "";
      if (map && (*map)[port->Name()] != "") {
	blank = (*map)[port->Name()];
      }
      else {
	if (rei) {
	  ifc_lowercase = rei->getInterface();
	  toLowerCase(ifc_lowercase);
	  if (string(rei->getEn()) == port->Name()) {
	    blank = string("EN_") + ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	  }
	  else if (string(rei->getRdy()) == port->Name()) {
	    blank = string("RDY_") + ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	  }
	  else { // Data case
	    count = rei->getCount();
	    if (size > 1) {
	      sprintf(stemp, "[%d:%d]", count+size-1, count);
	      rei->setCount(count+size);
	    }
	    else if (size == 1) {
	      sprintf(stemp, "[%d]", count);
	      rei->setCount(count+1);
	    }
	    blank += ifc_lowercase;
	    if (rei->isPut() || rei->isPipePut())
	      blank += "_put";
	    else
	      blank += "_get";
	    blank += stemp;
	  }
	}
	else if (mi) {
	  if (mi->isRsp()) {
	    if (string(mi->getEn()) == port->Name()) {
	      blank = string("EN_") + mi->getInterface() + "_response_put";
	    } else if (string(mi->getRdy()) == port->Name()) {
	      blank = string("RDY_") + mi->getInterface() + "_response_put";
	    } else {
	      blank = string(mi->getInterface()) + "_response_put";
	    }
	  } else if (mi->isReq()) {
	    if (string(mi->getEn()) == port->Name()) {
	      blank = string("EN_") + mi->getInterface() + "_request_get";
	    } else if (string(mi->getRdy()) == port->Name()) {
	      blank = string("RDY_") + mi->getInterface() + "_request_get";
	    } else {
	      blank = string(mi->getInterface()) + "_request_get";
	    }
	  }
	}
	else {
	  if (!HdlUtils::isPortLockstep())
	    blank = string(port->Name()) + "_put";
	  else
	    blank = string(port->Name()) + "_value";
	  toLowerCase(blank);
	}
      }
      mtlist.push_back (ModuleTerminal (port->GetName(), blank, d_inout, size));
      //printf("new terminal3 %s %s\n", port->GetName(), blank.c_str());
    }
  }
}

void HdlUtils::createInstanceTerminalList (VeriInstId* inst, ModuleTerminalList &mtlist)
{
  PortVisitor visitor;
  unsigned i;
  VeriExpression *expr, *value;
  Array *port_connects = inst->GetPortConnects();
  std::ostringstream strm;
  std::string portname;
  VeriIdDef *port_def;
  BString foundpath;
  int width;

  FOREACH_ARRAY_ITEM(port_connects, i, expr) {
    
    value = expr->GetConnection();
    strm.str("");
    value->PrettyPrint(strm, 0);
    if (expr->GetClassId() == ID_VERIPORTCONNECT) {
      portname = expr->NamedFormal();
      port_def = HdlUtils::findPortOfInstByName(inst, portname);
      width = HdlUtils::findWidthOfSignal(port_def);
      if (port_def->IsInput()) {
	//printf("Found input port %s %s value %s width %d\n", port_def->Name(),
	//       portname.c_str(), strm.str().c_str(), width);
	mtlist.push_back (ModuleTerminal (port_def->Name(), strm.str(), d_input, width));
      }
      else if (port_def->IsOutput()) {
	//printf("Found output port %s %s value %s width %d\n", port_def->Name(),
	//       portname.c_str(), strm.str().c_str(), width);
	mtlist.push_back (ModuleTerminal (port_def->Name(), strm.str(), d_output, width));
      }
      else {
	fprintf(stderr, "HdlUtils::createInstanceTerminalList(): Cannot partition across on inout (positional)");
      }
    } else if (expr->IsIdRef()) {
      VeriModule *module_def = inst->GetInstantiatedModule();
      port_def = HdlUtils::findPortOfModuleByPosition(module_def, i);
      width = HdlUtils::findWidthOfSignal(port_def);
      if (width == 0) width = 1;
      if (port_def->IsInput())
	mtlist.push_back (ModuleTerminal (port_def->Name(), strm.str(), d_input, width));
      else if (port_def->IsOutput())
	mtlist.push_back (ModuleTerminal (port_def->Name(), strm.str(), d_output, width));
      else {
	fprintf(stderr, "HdlUtils::createInstanceTerminalList(): Cannot partition across on inout (positional)");
      }
    }
  }
}

void HdlUtils::findConnectionNameOfPort(VeriInstId *inst, BString &portname, BString &return_name)
{
  unsigned it;
  VeriExpression *expr, *conn;
  BString instportname;
  std::ostringstream strm;

  Array *port_connects = inst->GetPortConnects();
  FOREACH_ARRAY_ITEM(port_connects, it, expr) {
    if (expr->GetClassId() == ID_VERIPORTCONNECT) {
      instportname = expr->NamedFormal();
      if (instportname == portname) {
	if ((conn = expr->GetConnection()) != NULL) {
	  strm.str("");
	  conn->PrettyPrint(strm, 0);
	  return_name = strm.str();
	  return;
	}
      }
    }
  }

  return_name = "";
}

void HdlUtils::setConnectionNameOfPort(VeriModule *module, VeriInstId *inst, const char *portname,
				       const char *new_conn_name)
{
  unsigned it;
  VeriExpression *expr, *conn;
  BString instportname;
  std::ostringstream strm;

  VeriScope *module_scope = module->GetScope() ;
  Array *port_connects = inst->GetPortConnects();
  FOREACH_ARRAY_ITEM(port_connects, it, expr) {
    if (expr->GetClassId() == ID_VERIPORTCONNECT) {
      instportname = expr->NamedFormal();
      if (instportname == portname) {
	if ((conn = expr->GetConnection()) != NULL) {

	  // Create new port-ref
	  VeriExpression *connval = new VeriIdRef(Strings::save(new_conn_name));
	  VeriExpression *newconn = new VeriPortConnect(Strings::save(portname),
							connval);

	  newconn->Resolve(module_scope, VeriTreeNode::VERI_PORT_CONNECTION) ;
	  inst->ReplaceChildExpr(expr, newconn);

	  //conn = expr->GetConnection();
	  //strm.str("");
	  //conn->PrettyPrint(strm, 0);
	  //cout << "After setting conn is " << strm.str().c_str() << endl;
	}
      }
    }
  }
}

void HdlUtils::findFragmentFilePath(BString &returnpath)
{
  if (_fragment_file_path != "") {
    returnpath = _fragment_file_path;
    return;
  }

  unsigned it_a;
  char *path;
  std::ifstream file;
  string filename;

  FOREACH_ARRAY_ITEM(theHdlUtils->getYDirs(), it_a, path) {
    if (path) {
      filename = path;
      filename += "/";
      filename += "scemilink.vlog_fragment";
      file.clear();
      file.open(filename.c_str());
      if (!file.fail()) {
	
	returnpath = _fragment_file_path = path;
	file.close();
	return;
      }
    }
  }
}

int HdlUtils::generateSceMiLayer(VeriModule *module, const char *new_module_name,
				 const char *scemi_layer_filename, 
				 const char *outdir)
{
  BString odir = outdir;
 
  //printf("generateSceMiLayer\n");
  if (odir == "")
    odir = "bsv";
  BString fullname = odir + "/" + scemi_layer_filename; 
  
  BString filename = module->Name();
  filename += ".pin";

  // Create SceMiLayer.bsv file
  if (!HdlUtils::isPortLockstep())
    generateSceMiLayerFile(module, new_module_name, fullname.c_str());
  else
    generateLockstepSceMiLayerFile(module, new_module_name, fullname.c_str());

  // Create Dut bsv file
  string dut_filename = odir + "/" + new_module_name;
  dut_filename += ".bsv";

  if (!HdlUtils::isPortLockstep())
    generateDutFile(module, new_module_name, dut_filename.c_str());
  else
    generateLockstepDutFile(module, new_module_name, dut_filename.c_str());

  //printf("done generateSceMiLayer\n");
  return 1;
}

//
// The For loop way
void toLowerCase(std::string &str)
{
  const int length = str.length();
  for(int i=0; i < length; ++i)
    {
      str[i] = std::tolower(str[i]);
    }
}

//
// The For loop way
void removeDoubleUnderscore(std::string &str)
{
  std::string newstring;
  size_t from, to;
  from = 0;
  to = str.find("__");
  if (to != std::string::npos) {
    while (to != std::string::npos) {
      newstring += str.substr(from, to);
      from = to+1;
      to = str.find("__", to+2);
    }
    newstring += str.substr(from);
    str = newstring;
  }
}

bool isNumber(const char *st)
{
  int len = strlen(st);
  for (int i=0; i<len; i++)
    if (!isdigit(st[i]))
      return false;
  return true;
}

int HdlUtils::readPinFile(const char *filename)
{
  const char *delimiter = " \n\t{}";
  char *token;

  std::ifstream file;
  char lineBuf[MAX_LINE_SIZE+1];
  char buf[1024];
  int LineNum;
  int stat = 1;

  if (read_port_spec != 0)
    return 1;
  read_port_spec = 1;

  LineNum = 0;

  file.open(filename);
  if (file.fail()) {
    cerr << "ERROR HdlUtils::readPinFile(): unable to open portmap file "
	 << filename << endl;
  }

  printf("Reading file %s\n", filename);
  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    LineNum++;
    token = strtok(lineBuf, delimiter);

    // Check comment
    if (token == NULL) continue;
    if (strncmp(token, "//", 2) == 0) continue;
    
    if (!strcmp(token, "port")) {
      stat &= readPortDefinitions(file, LineNum, filename);
      if (stat == 0) cout << "Something wrong with port" << endl;
    } else if (!strcmp(token, "memory")) {
      stat &= readMemDefinitions(file, LineNum, filename);
      if (stat == 0) cout << "Something wrong with memory" << endl;
      use_fpga_memory = 1;
      //} else if (!strcmp(token, "pipedepth")) {
      //token = strtok(0, delimiter);
      //if (token == 0) cout << "Something wrong with pipedepth" << endl;
      //if (!isNumber(token)) cout << "Invalid value for pipedepth" << endl;
      //PipeDepth = atoi(token);
    } else {
      sprintf(buf, "%d", LineNum);
      cerr << "Error HdlUtils::readPinFile(): bad statement in pin file "
	   << filename << " on line "
	   << buf << "." << endl;
      stat = 0;

    }

    if (stat == 0) {
      cout << "Something wrong with the end" << endl;
      file.close();
      return 0;
    }
  }

  // Now sort the data of all rdy/enable interfaces
  RdyEnableInterface *rei;
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei->getUsed()) continue;
    rei->sortData();
    rei->setUsed(true);
  }

  file.close();
  printf("Done reading %s\n", filename);
  return stat;
}

int HdlUtils::readPortDefinitions(std::ifstream &file, int &LineNum, const char *filename)
{
  const char *delimiter = " \n\t{}";
  char lineBuf[MAX_LINE_SIZE+1];
  char buf[1024];
  char *token;
  BString portname, porttype, size, clkname, rdentok, ifctype, ifcname;
  BString name;
  bool found_pipe = false;

  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    LineNum++;
    token = strtok(lineBuf, delimiter);

    // Check comment
    if (token == NULL) continue;
    if (strncmp(token, "//", 2) == 0) continue;
    
    if (!strcmp(token, "input")) {
      portname = strtok(0, delimiter);
      if (portname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): bad input port in file "
	     << filename << " on line "
	     << buf << endl;
	return 0;
      }

      // Size field
      token = strtok(0, delimiter);
      size = token;
      if (size == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): port size is not given in file "
	     << filename << " on line "
	     << buf << endl;
	return 0;
      }

      porttype = "LC";

      // Clk field
      clkname = "";
      token = strtok(0, delimiter);

      if (token)
	clkname = token;
      if (clkname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): clock name is not given in file "
	     << filename << " on line "
	     << buf << endl;
	return 0;
      }
      if (clkname.substr(0, 6) == "clock:") {
	name = clkname.substr(6);
	clk_rst_map[portname] = name;
      } else if (clkname.substr(0, 6) == "reset:") {
	name = clkname.substr(6);
	clk_rst_map[portname] = name;
      }
      
      // PUT, GET, PIPEPUT, PIPEGET PIPE or LOCKSTEP
      token = strtok(0, delimiter);
      if (token == NULL) continue;
      rdentok = token;
      if (rdentok == "")
	return 1;
      else if (rdentok == "PIPE") {

	if (found_lockstep == true) {
	  cerr << "Error HdlUtils::readPortDefinitions(): Bad PIPE interface specification "
	       << "in file " << filename << " on line "
	       << buf << ". Cannot have ports with both PIPE and LOCKSTEP definition." << endl;
	  return 0;
	}
	found_pipe = true;
	port_pipe_type[portname] = 1;
	token = strtok(0, delimiter);
	if (token == NULL) continue;
	rdentok = token;
      }
      else if (rdentok == "LOCKSTEP") {

	if (found_pipe == true) {
	  cerr << "Error HdlUtils::readPortDefinitions(): Bad LOCKSTEP interface specification "
	       << "in file " << filename << " on line "
	       << buf << ". Cannot have ports with both PIPE and LOCKSTEP definition." << endl;
	  return 0;
	}
	found_lockstep = true;
	port_lockstep_type[portname] = 1;
	token = strtok(0, delimiter);
	if (token == NULL) continue;
	rdentok = token;
      }
      else if ((rdentok == "RDY") || (rdentok == "EN") || (rdentok == "DATA")) {

	if (found_lockstep == true) {
	  cerr << "Error HdlUtils::readPortDefinitions(): Bad RDY/EN/DATA interface specification "
	       << "in file " << filename << " on line "
	       << buf << ". Cannot have ports with both RDY/EN/DATA and LOCKSTEP definition." << endl;
	  return 0;
	}
      }
      else if ((rdentok != "RDY") && (rdentok != "EN") && (rdentok != "DATA")) {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): Bad RDY/EN/DATA interface specification "
	     << "in file " << filename << " on line "
	     << buf << ". Unrecognized token '" << rdentok << "'." << endl;
	return 0;
      }
      token = strtok(0, delimiter);
      if (token)
	ifctype = token;
      else
	ifctype = "";
      if (ifctype == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): port type of RDY/EN/DATA interface "
	     << "is not given in file " << filename << " on line "
	     << buf << endl;
	return 0;
      }
      else if ((ifctype != "PUT") && (ifctype != "GET") &&
	       (ifctype != "PIPEPUT") && (ifctype != "PIPEGET") &&
	       (ifctype != "MEMREQ") && (ifctype != "MEMRESP")) {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): Bad PUT/GET interface specification "
	     << "in file " << filename << " on line "
	     << buf << ". Unrecognized token '" << ifctype << "'." << endl;
	return 0;
      }
      ifcname = strtok(0, delimiter);
      int rdyenable;
      if ((ifctype == "PUT") || (ifctype == "GET")) {
	rdyenable = 1;
      }
      else if ((ifctype == "PIPEPUT") || (ifctype == "PIPEGET")) {
	rdyenable = 1;
      }
      else if ((ifctype == "MEMREQ") || (ifctype == "MEMRESP")) {
	rdyenable = 0;
      }
      if (ifcname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): interface name associated with RDY/EN/DATA is not given in file "
	     << filename << " on line "
	     << buf << endl;
	return 0;
      }

      if (rdyenable) {
	RdyEnableInterface *rei = RdyEnableIfc[ifcname];
	if (rei == NULL) {
	  rei = new RdyEnableInterface();
	  rei->setInterface(ifcname);
	  RdyEnableIfc[ifcname] = rei;
	}
	RdyEnableIfc[portname] = rei;
	if (rdentok == "RDY")
	  rei->setRdy(portname);
	else if (rdentok == "EN")
	  rei->setEn(portname);
	else if (rdentok == "DATA")
	  rei->addData(portname);

	// Set put/get type
	if (ifctype == "PUT")
	  rei->setPutType();
	else if (ifctype == "PIPEPUT")
	  rei->setPipePutType();
	else if (ifctype == "GET")
	  rei->setGetType();
	else if (ifctype == "PIPEGET")
	  rei->setPipeGetType();

      } else {
	MemoryInterface *mi;
	// Set put/get type
	if (ifctype == "MEMREQ") {
	  mi = MemoryReqIfc[ifcname];
	}
	else {
	  mi = MemoryRespIfc[ifcname];
	}
	if (mi == NULL) {
	  mi = new MemoryInterface();
	  mi->setInterface(ifcname);
	}
	if (ifctype == "MEMREQ") {
	  MemoryReqIfc[ifcname] = mi;
	  MemoryReqPortIfc[portname] = mi;
	  mi->setReqType();
	}
	else {
	  MemoryRespIfc[ifcname] = mi;
	  MemoryRespPortIfc[portname] = mi;
	  mi->setRspType();
	}

	if (rdentok == "RDY")
	  mi->setRdy(portname);
	else if (rdentok == "EN")
	  mi->setEn(portname);
	else if (rdentok == "DATA")
	  mi->addData(portname);

      }
      
    } else if (!strcmp(token, "output")) {
      portname = strtok(0, delimiter);
      if (portname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): bad output port in port map file on line "
	     << buf << endl;
	return 0;
      }

      // Size field
      token = strtok(0, delimiter);
      size = token;
      if (size == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): port size is not given in file "
	     << filename << " on line "
	     << buf << endl;
	return 0;
      }

      porttype = "LC";
      port_xactor_type[portname] = porttype;
      token = strtok(0, delimiter);
      if (token)
	clkname = token;
      else
	clkname = "";
      if (clkname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): clock name is not given in testbench.spec file on line "
	     << buf << endl;
	return 0;
      }

      token = strtok(0, delimiter);

      if (token == NULL) continue;

      rdentok = token;
      if (rdentok == "")
	return 1;
      else if (rdentok == "PIPE") {

	if (found_lockstep == true) {
	  cerr << "Error HdlUtils::readPortDefinitions(): Bad PIPE interface specification "
	       << "in file " << filename << " on line "
	       << buf << ". Cannot have ports with both PIPE and LOCKSTEP definition." << endl;
	  return 0;
	}
	found_pipe = true;
	port_pipe_type[portname] = 1;
	token = strtok(0, delimiter);
	if (token == NULL) continue;
	rdentok = token;
      }
      else if (rdentok == "LOCKSTEP") {

	if (found_pipe == true) {
	  cerr << "Error HdlUtils::readPortDefinitions(): Bad LOCKSTEP interface specification "
	       << "in file " << filename << " on line "
	       << buf << ". Cannot have ports with both PIPE and LOCKSTEP definition." << endl;
	  return 0;
	}
	found_lockstep = true;
	port_lockstep_type[portname] = 1;
	token = strtok(0, delimiter);
	if (token == NULL) continue;
	rdentok = token;
      }
      else if ((rdentok == "RDY") || (rdentok == "EN") || (rdentok == "DATA")) {

	if (found_lockstep == true) {
	  cerr << "Error HdlUtils::readPortDefinitions(): Bad RDY/EN/DATA interface specification "
	       << "in file " << filename << " on line "
	       << buf << ". Cannot have ports with both RDY/EN/DATA and LOCKSTEP definition." << endl;
	  return 0;
	}
      }
      else if ((rdentok != "RDY") && (rdentok != "EN") && (rdentok != "DATA")) {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): Bad RDY/EN/DATA interface specification "
	     << "in file " << filename << " on line "
	     << buf << endl;
	return 0;
      }
      ifctype = strtok(0, delimiter);
      if (ifctype == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): port type of RD/EN/DATA interface "
	     << "is not given in file " << filename << " on line "
	     << buf << endl;
	return 0;
      }
      else if ((ifctype != "PUT") && (ifctype != "GET") &&
	       (ifctype != "PIPEPUT") && (ifctype != "PIPEGET") &&
	       (ifctype != "MEMREQ") && (ifctype != "MEMRESP")) {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): Bad PUT/GET interface specification "
	     << "in file " << filename << " on line "
	     << buf << endl;
	return 0;
      }
      ifcname = strtok(0, delimiter);
      int rdyenable;
      if ((ifctype == "PUT") || (ifctype == "GET")) {
	rdyenable = 1;
      }
      else if ((ifctype == "PIPEPUT") || (ifctype == "PIPEGET")) {
	rdyenable = 1;
      }
      else if ((ifctype == "MEMREQ") || (ifctype == "MEMRESP")) {
	rdyenable = 0;
      }
      if (ifcname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): interface name associated with RDY/EN/DATA is not given in file "
	     << filename << " on line "
	     << buf << endl;
	return 0;
      }
      if (ifcname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error HdlUtils::readPortDefinitions(): port name is not given in file "
	     << filename << " on line "
	     << buf << endl;
	return 0;
      }

      if (rdyenable) {
	RdyEnableInterface *rei = RdyEnableIfc[ifcname];
	if (rei == NULL) {
	  rei = new RdyEnableInterface();
	  rei->setInterface(ifcname);
	  RdyEnableIfc[ifcname] = rei;
	}
	RdyEnableIfc[portname] = rei;
	if (rdentok == "RDY")
	  rei->setRdy(portname);
	else if (rdentok == "EN")
	  rei->setEn(portname);
	else if (rdentok == "DATA")
	  rei->addData(portname);

	// Set put/get type
	if (ifctype == "PUT")
	  rei->setPutType();
	else if (ifctype == "PIPEPUT")
	  rei->setPipePutType();
	else if (ifctype == "GET")
	  rei->setGetType();
	else if (ifctype == "PIPEGET")
	  rei->setPipeGetType();

      } else {
	MemoryInterface *mi;
	// Set put/get type
	if (ifctype == "MEMREQ") {
	  mi = MemoryReqIfc[ifcname];
	}
	else {
	  mi = MemoryRespIfc[ifcname];
	}
	if (mi == NULL) {
	  mi = new MemoryInterface();
	  mi->setInterface(ifcname);
	}
	if (ifctype == "MEMREQ") {
	  MemoryReqIfc[ifcname] = mi;
	  MemoryReqPortIfc[portname] = mi;
	  mi->setReqType();
	}
	else {
	  MemoryRespIfc[ifcname] = mi;
	  MemoryRespPortIfc[portname] = mi;
	  mi->setRspType();
	}

	if (rdentok == "RDY")
	  mi->setRdy(portname);
	else if (rdentok == "EN")
	  mi->setEn(portname);
	else if (rdentok == "DATA")
	  mi->addData(portname);
      }
      
    } else if (!strcmp(token, "endport")) {
      return 1;

    } else {
      sprintf(buf, "%d", LineNum);
      cerr << "Error HdlUtils::readPortDefinitions(): bad statement in port file "
	   << filename << " on line "
	   << buf << "." << endl;
      return 0;
    }
  }

  return 1;
}

int HdlUtils::readMemDefinitions(std::ifstream &file, int &LineNum, const char *filename)
{
  const char *delimiter = " \n\t:{}=";
  char lineBuf[MAX_LINE_SIZE+1];
  char buf[1024];
  char *token;
  BString ifcname;
  int addval, dataval;

  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    LineNum++;
    token = strtok(lineBuf, delimiter);
    
    // Check comment
    if (token == NULL) continue;
    if (strncmp(token, "//", 2) == 0) continue;
    
    ifcname = token;

    if (token && strcmp(token, "endmemory")) {
    // Create memory interface object

      MemoryInterface *mireq = MemoryReqIfc[ifcname];
      MemoryInterface *miresp = MemoryRespIfc[ifcname];
      if (mireq == NULL) {
	mireq = new MemoryInterface();
	mireq->setInterface(ifcname);
	MemoryReqIfc[ifcname] = mireq;
      }
      if (miresp == NULL) {
	miresp = new MemoryInterface();
	miresp->setInterface(ifcname);
	MemoryRespIfc[ifcname] = miresp;
      }
      token = strtok(0, delimiter);
      
      if (!strcmp(token, "address")) {      
	token = strtok(0, delimiter);
	if (token) {
	  addval = atoi(token);
	  mireq->setAddressWidth(addval);
	  miresp->setAddressWidth(addval);
	} else {
	  sprintf(buf, "%d", LineNum);
	  cerr << "Error HdlUtils::readMemDefinitions(): bad memory statement in pin file "
	       << filename 
	       << " on line "
	       << buf << "." << endl;
	  return 0;
	}
      }

      token = strtok(0, delimiter);
      if (!strcmp(token, "data")) {
	token = strtok(0, delimiter);
	if (token) {
	  dataval = atoi(token);
	  mireq->setDataWidth(dataval);
	  miresp->setDataWidth(dataval);
	} else {
	  sprintf(buf, "%d", LineNum);
	  cerr << "Error HdlUtils::readMemDefinitions(): bad memory statement in pin file "
	       << filename 
	       << " on line "
	       << buf << "." << endl;
	  return 0;
	}
      }

    } else if (!strcmp(token, "endmemory")) {
      return 1;
    } else {
      sprintf(buf, "%d", LineNum);
      cerr << "Error HdlUtils::readMemDefinitions(): bad statement in pin file " << filename 
	   << " on line "
	   << buf << "." << endl;
      return 0;
    }
  }
  
  return 1;
}

void HdlUtils::setRdyEnableIfcWidth(VeriModule *module)
{
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  RdyEnableInterface *rei;
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;
  string lstr;
  int width;


  HdlUtils::createModuleTerminalList(module, mtlist);

  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    rei = RdyEnableIfc[lstr];
    if (rei == NULL) continue;
    if (rei->getRdy() == terminal->m_portName ||
	rei->getEn() == terminal->m_portName)
      continue;
    width = rei->getWidth();
    width += terminal->m_width;
    rei->setWidth(width);
  }
}

int HdlUtils::generateSceMiLayerFile(VeriModule *module, const char *new_module_name,
				     const char *scemi_layer_filename)
{
  std::fstream file;
  string errstring;
  int found;

  //printf ("generateSceMiLayerFile\n");
  file.open(scemi_layer_filename, ios::out);
  if (file.fail()) {
    errstring += "Error HdlUtils::generateSceMiLayerFile(): file ";
    errstring += scemi_layer_filename;
    errstring += " not found\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "// Copyright Bluespec Inc." << endl;
  file << "// By: edithdl generate scemi utility" << endl << endl;

  file << "package SceMiLayer;" << endl << endl;
  file << "import GetPut::*;" << endl;
  file << "import ClientServer::*;" << endl;
  file << "import DefaultValue::*;" << endl;
  file << "import SceMi::*;" << endl;
  file << "import Connectable::*;" << endl;
  file << "import Readback::*;" << endl;
  if (use_fpga_memory) {
    file << "import Memory::*;" << endl;
    file << "import SceMiMemoryXactor::*;" << endl;
    file << "import Connectable::*;" << endl;
    file << "import RegFile::*;" << endl;
    file << "import FIFO::*;" << endl;
    file << "import Clocks::*;" << endl;
    file << "import BUtils::*;" << endl;
  }
  file << "import " << new_module_name << "::*;" << endl << endl;

  file << "`include \"Readback.defines\"" << endl << endl;

  MemoryInterface *mi;
  std::map<std::string,MemoryInterface*>::iterator miItr;
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL) continue;
    mi->setUsed(false);
  }
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL || mi->getUsed() == true) continue;
    mi->setUsed(true);
    file << "typedef MemoryClient#(" << mi->getAddressWidth() << ", "
	 << mi->getDataWidth() << ") SceMiLayer" << ";" << endl;
  }
  file << endl;

  if (!use_fpga_memory) {
    file << "module [SceMiModule] mkSceMiLayer();" << endl;
  } else {
    file << "`ifdef SCEMI_TCP" << endl;
    file << "module [SceMiModule] mkSceMiLayer();" << endl;
    file << "`else" << endl;
    file << "module [SceMiModule] mkSceMiLayer(SceMiLayer);" << endl;
    file << "`endif" << endl << endl;;
  }

  file << "   SceMiClockConfiguration conf = defaultValue;" << endl;
  file << "   SceMiClockPortIfc clk_port <- mkSceMiClockPort(conf);" << endl << endl;

  file << "   " << new_module_name << "Ifc dut <- buildDut(mk" 
       << new_module_name << ", clk_port);" << endl << endl;

  // Generate all the data type
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;
  
  createModuleTerminalList(module, mtlist);

  // Input data type
  string input_var, lstr;
  RdyEnableInterface *rei;
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    toLowerCase(lstr);
    rei = RdyEnableIfc[terminal->m_portName];
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      if (rei) {
	if ((terminal->m_portName != rei->getRdy()) && (terminal->m_portName != rei->getEn())) {
	  if (rei->getUsed() == false) {
	    rei->setUsed(true);
	    input_var = string("put_") + rei->getInterface();
	    removeDoubleUnderscore(input_var);
	    if (rei->isPut()) {
	      file << "   Get#(" << new_module_name << "_" << rei->getInterface() << ") "
		   << input_var << " <- mkSemuRdyEnableInPortXactor(clk_port);" << endl;
	    } else if (rei->isPipePut()) {
	      file << "   Get#(" << new_module_name << "_" << rei->getInterface() << ") "
		   << input_var << " <- mkInPipeXactor(4096, Fifo, clk_port);" << endl;
	      //<< input_var << " <- mkSemuRdyEnableInPortPipeXactor(4096, Fifo, clk_port);" << endl;
	    }
	  }
	}
      } else if (isRegularModuleTerminal(terminal)) {
	input_var = string("put_") + terminal->m_portName;
	removeDoubleUnderscore(input_var);
	if (port_pipe_type[terminal->m_portName] == 0) {
	  file << "   let " << input_var << " <- mkSemuInPortXactor(dut." << lstr << ", ";
	  if (port_xactor_type[terminal->m_portName] == "TC")
	    file << "TightlyCoupled, conf);" << endl;
	  else
	    file << "LooselyCoupled, conf);" << endl;
	} else {
	  //file << "   Get#(" << new_module_name << "_" << terminal->m_portName << ") "
	  //<< input_var << " <- mkInPipeXactor(4096, Fifo, clk_port);" << endl;
	  //<< input_var << " <- mkSemuRdyEnableInPortPipeXactor(4096, Fifo, clk_port);" << endl;
	  file << "   let " << input_var << " <- mkSemuInPortPipeXactor(dut." << lstr << ", ";
	  if (port_xactor_type[terminal->m_portName] == "TC")
	    file << "TightlyCoupled, 4096, Fifo, conf);" << endl;
	  else
	    file << "LooselyCoupled, 4096, Fifo, conf);" << endl;
	}
      }
    }
  }
  found = 0;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    toLowerCase(lstr);
    rei = RdyEnableIfc[terminal->m_portName];
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      if (rei) {
	if ((terminal->m_portName != rei->getRdy()) && (terminal->m_portName != rei->getEn())) {
	  if (rei->getUsed() == false) {
	    rei->setUsed(true);
	    input_var = string("get_") + rei->getInterface();
	    removeDoubleUnderscore(input_var);
	    if (rei->isGet()) {
	      file << "   Put#(" << new_module_name << "_" << rei->getInterface() << ") "
		   << input_var << " <- mkSemuRdyEnableOutPortXactor(clk_port);" << endl;
	    } else if (rei->isPipeGet()) {
	      file << "   Put#(" << new_module_name << "_" << rei->getInterface() << ") "
                   << input_var << " <- mkOutPipeXactor(4096, Fifo, clk_port);" << endl;
	      //<< input_var << " <- mkSemuRdyEnableOutPortPipeXactor(4096, Fifo, clk_port);" << endl;
	    }
	    found = 1;
	  }
	}
      } else if (isRegularModuleTerminal(terminal)) {
	input_var = string("get_") + terminal->m_portName;
	removeDoubleUnderscore(input_var);
	if (port_pipe_type[terminal->m_portName] == 0) {
	  file << "   let " << input_var << " <- mkSemuOutPortXactor(dut." << lstr << ", ";
	  if (port_xactor_type[terminal->m_portName] == "LC")
	    file << "LooselyCoupled, conf);" << endl;
	  else
	    file << "TightlyCoupled, conf);" << endl;
	} else {
	  //file << "   Put#(" << new_module_name << "_" << terminal->m_portName << ") "
	  //   << input_var << " <- mkSemuRdyEnableOutPortPipeXactor(4096, Fifo, clk_port);" << endl;
	  //<< input_var << " <- mkOutPipeXactor(4096, Fifo, clk_port);" << endl;
	  file << "   let " << input_var << " <- mkSemuOutPortPipeXactor(dut." << lstr << ", ";
	  if (port_xactor_type[terminal->m_portName] == "LC")
	    file << "LooselyCoupled, 4096, Fifo, conf);" << endl;
	  else
	    file << "TightlyCoupled, 4096, Fifo, conf);" << endl;
	}
      }
    }
  }

  string ifc_lowercase;
  if (found) 
    file << endl;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL) continue;
    mi->setUsed(false);
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    toLowerCase(lstr);
    rei = RdyEnableIfc[terminal->m_portName];
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      if (rei) {
	ifc_lowercase = rei->getInterface();
	toLowerCase(ifc_lowercase);
	if ((terminal->m_portName != rei->getRdy()) && (terminal->m_portName != rei->getEn())) {
	  if (rei->getUsed() == false) {
	    rei->setUsed(true);
	    input_var = string("get_") + rei->getInterface();
	    removeDoubleUnderscore(input_var);
	    file << "   mkConnection(put_" << rei->getInterface() << ", dut."
		 << ifc_lowercase << ");" << endl;
	  }
	}
      } else if (HdlUtils::isPortPipe(terminal->m_portName.c_str())) {
	//file << "   mkConnection(put_" << terminal->m_portName << ", dut."
	//     << lstr << ");" << endl;
      }
    }
  }

  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    toLowerCase(lstr);
    rei = RdyEnableIfc[terminal->m_portName];
    miItr = MemoryReqPortIfc.find(terminal->m_portName);
    mi = NULL;
    if (miItr != MemoryReqIfc.end())
      mi = miItr->second;
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      if (rei) {
	ifc_lowercase = rei->getInterface();
	toLowerCase(ifc_lowercase);
	if ((terminal->m_portName != rei->getRdy()) && (terminal->m_portName != rei->getEn())) {
	  if (rei->getUsed() == false) {
	    rei->setUsed(true);
	    input_var = string("get_") + rei->getInterface();
	    removeDoubleUnderscore(input_var);
	    file << "   mkConnection(dut." << ifc_lowercase << ", get_"
		 << rei->getInterface() << ");" << endl;
	  }
	}
      } else if (HdlUtils::isPortPipe(terminal->m_portName.c_str())) {
	//file << "   mkConnection(dut." << lstr << ", get_"
	//     << terminal->m_portName << ");" << endl;
      }
    }
  }

  int first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    toLowerCase(lstr);
    rei = RdyEnableIfc[terminal->m_portName];
    miItr = MemoryReqIfc.find(terminal->m_portName);
    mi = NULL;
    if (miItr != MemoryReqIfc.end())
      mi = miItr->second;
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      if (mi) {
	if (first) {
	  file << endl;
	  file << "   Clock uclock <- sceMiGetUClock;" << endl;
	  file << "   Reset ureset <- sceMiGetUReset;" << endl;
	  file << "   let cclock = clk_port.cclock;" << endl;
	  file << "   let creset = clk_port.creset;" << endl;
	  file << endl;
	  if (use_fpga_memory) {
	    input_var = mi->getInterface();
	    toLowerCase(input_var);
	    file << "`ifdef SCEMI_TCP" << endl;
	    // Create fake memory using RegFile for simulation
	    file << "   // RegFile for memory model simulation" << endl;
	    file << "   RegFile#(Bit#(20), Bit#(" << mi->getDataWidth() << ")) " << input_var
		 << " <- mkRegFileFull(clocked_by cclock, reset_by creset);" << endl << endl;
	    file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
	    file << "   /// Submodule Connections" << endl;
	    file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
	    file << "   mkConnection(dut." << input_var << ", " << input_var << ", clocked_by cclock, reset_by creset);" << endl;
	    file << "`else" << endl;
	    first = 0;
	  }
	  if ((terminal->m_portName != mi->getRdy()) && (terminal->m_portName != mi->getEn())) {
	    if (mi->getUsed() == false) {
	      mi->setUsed(true);
	      input_var = string("dut_") + mi->getInterface() + "_req";
	      removeDoubleUnderscore(input_var);
	      file << "   SyncFIFOIfc#(MemoryRequest#(" << mi->getAddressWidth() << ","
		   << mi->getDataWidth() << ")) " << input_var << " <- mkSyncFIFO(1, cclock, creset, uclock);" << endl;
	      input_var = string("dut_") + mi->getInterface() + "_resp";
	      removeDoubleUnderscore(input_var);
	      file << "   SyncFIFOIfc#(MemoryResponse#("
		   << mi->getDataWidth() << ")) " << input_var << " <- mkSyncFIFO(1, uclock, ureset, cclock);" << endl << endl;
	      file << "   mkConnection(dut." << mi->getInterface() << ".request, toPut(dut_"
		   << mi->getInterface() << "_req));" << endl;
	      file << "   mkConnection(toGet(dut_" << mi->getInterface() << "_resp), "
		   << "dut." << mi->getInterface() << ".response);" << endl;
	    }
	  }
	}
      }
    }
  }
  if (first == 0) {
    file << "`endif" << endl;
  }

  file << endl;
  file << "   SceMiResetXactorIfc reset <- mkSceMiResetXactor(clk_port);" << endl;
  file << "   Empty     control <- mkShutdownXactor();" << endl << endl;

  // file << "   let fake_data = False;" << endl;
  // file << "   let family    = KINTEX7;" << endl;
  // file << "`ifdef SCEMI_TCP" << endl;
  // file << "   fake_data = True;" << endl;
  // file << "`endif" << endl;
  // file << "`ifdef SCEMI_PCIE_VIRTEX6" << endl;
  // file << "   family    = VIRTEX6;" << endl;
  // file << "`endif" << endl;
  // file << "`ifdef SCEMI_PCIE_VIRTEX7" << endl;
  // file << "   family    = KINTEX7;" << endl;
  // file << "`endif" << endl;
  // file << "`ifdef SCEMI_PCIE_KINTEX7" << endl;
  // file << "   family    = KINTEX7;" << endl;
  // file << "`endif" << endl;

  // file << "   Empty  simControl <- mkRdBackControl(family, fake_data, conf);" << endl << endl;

  file << "   `READBACKCORE(simControl, conf);" << endl << endl;



  file << "   Empty  tbsimControl <- mkSimulationControl(conf);" << endl << endl;

  if (use_fpga_memory) {
    for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
      mi = miItr->second;
      if (mi == NULL) continue;
      mi->setUsed(false);
    }
    file << "`ifndef SCEMI_TCP" << endl;
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      toLowerCase(lstr);
      rei = RdyEnableIfc[terminal->m_portName];
      miItr = MemoryReqIfc.find(terminal->m_portName);
      mi = NULL;
      if (miItr != MemoryReqIfc.end())
	mi = miItr->second;
      if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
	if (mi) {
	  if (mi->getUsed() == false) {
	    mi->setUsed(true);
	    input_var = string("dut_") + mi->getInterface() + "_req";
	    removeDoubleUnderscore(input_var);
	    file << "   interface request = toGet(" << input_var << ");" << endl;
	    input_var = string("dut_") + mi->getInterface() + "_resp";
	    removeDoubleUnderscore(input_var);
	    file << "   interface response = toPut(" << input_var << ");" << endl;
	  }
	}
      }
    }
    file << "`endif" << endl << endl;
  }
  file << "endmodule" << endl << endl;

  file << "endpackage" << endl;

  file.close();

  return 1;
}

int HdlUtils::generateLockstepSceMiLayerFile(VeriModule *module, const char *new_module_name,
					     const char *scemi_layer_filename)
{
  std::fstream file;
  string errstring;

  //printf ("generateSceMiLayerFile\n");
  file.open(scemi_layer_filename, ios::out);
  if (file.fail()) {
    errstring += "Error HdlUtils::generateSceMiLayerFile(): file ";
    errstring += scemi_layer_filename;
    errstring += " not found\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  // Generate all the data type
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;
  int in_count, out_count;
  in_count = out_count = 0;
  
  createModuleTerminalList(module, mtlist);

  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    if (clk_rst_map[terminal->m_portName] == "") {
      if (terminal->m_dir != d_output)
	in_count += terminal->m_width;
      else
	out_count += terminal->m_width;
    }
  }

  file << "// Copyright Bluespec Inc." << endl;
  file << "// By: edithdl generate scemi utility" << endl << endl;

  file << "package SceMiLayer;" << endl << endl;
  file << "import Clocks::*;" << endl;
  file << "import Connectable::*;" << endl;
  file << "import DefaultValue::*;" << endl;
  file << "import GetPut::*;" << endl;
  file << "import Readback::*;" << endl;
  file << "import SceMi::*;" << endl;
  file << "import SceMiDefines::*;" << endl;
  file << "import " << new_module_name << "::*;" << endl << endl;

  file << "module [SceMiModule] mkSceMiLayer(Empty);" << endl << endl;

  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "   /// Clocks & Resets" << endl;
  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "   Clock                           uclk                <- sceMiGetUClock;" << endl;
  file << "   Reset                           urst                <- sceMiGetUReset;" << endl << endl;

  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "   /// Dut Clock" << endl;
  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "   SceMiClockConfiguration         clk_cfg              = defaultValue;" << endl;
  file << "   clk_cfg.clockNum        = 0;" << endl;
  file << "   clk_cfg.resetCycles     = 4;" << endl;
  file << "   clk_cfg.ratioNumerator  = 2;" << endl;
  file << "   SceMiClockPortIfc               clk_port            <- mkSceMiClockPort( clk_cfg );" << endl << endl;
  file << "   let cclock = clk_port.cclock;" << endl;
  file << "   let creset = clk_port.creset;" << endl;

  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "   /// Design Elements" << endl;
  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "   " << new_module_name << "Ifc dut <- buildDut(mk" 
       << new_module_name << ", clk_port);" << endl << endl;

  file << "   Empty xshutdown <- mkShutdownXactor;" << endl << endl;

  file << "   let fake_data = False;" << endl;
  file << "   let family    = KINTEX7;" << endl;
  file << "`ifdef SCEMI_TCP" << endl;
  file << "   fake_data = True;" << endl;
  file << "`endif" << endl;
  file << "`ifdef SCEMI_PCIE_VIRTEX6" << endl;
  file << "   family    = VIRTEX6;" << endl;
  file << "`endif" << endl;
  file << "`ifdef SCEMI_PCIE_VIRTEX7" << endl;
  file << "   family    = KINTEX7;" << endl;
  file << "`endif" << endl;
  file << "`ifdef SCEMI_PCIE_KINTEX7" << endl;
  file << "   family    = KINTEX7;" << endl;
  file << "`endif" << endl;

  file << "   Empty  simControl <- mkRdBackControl(family, fake_data, clk_cfg);" << endl << endl;
  file << "   Empty  tbsimControl <- mkSimulationControl(clk_cfg);" << endl << endl;

  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "   /// LockStep Xactor code" << endl;
  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << endl;
  file << "   Clock iclock <- invertCurrentClock(clocked_by cclock, reset_by creset);   " << endl;
  file << "   Reset ireset <- mkAsyncResetFromCR(0, iclock);" << endl;
  file << endl;
  file << "   SceMiPipe#(Bit#(" << in_count+1 << "), Bit#(" << out_count
       << ")) lockstep <- mkLockStepPipeXactor(clk_cfg.clockNum, 1024, clk_port, iclock, ireset);" << endl;
  file << "" << endl;
  file << "   Bit#(" << out_count << ") outputs = {";

  int first = 1;
  string lstr;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    if (clk_rst_map[terminal->m_portName] == "") {
      if (terminal->m_dir == d_output) {
	if (!first)
	  file << ", ";
	else
	  first = 0;
	lstr = terminal->m_portName;
	toLowerCase(lstr);
	file << "pack(dut." << lstr << ")";
      }
    }
  }
  file << "};" << endl;
  file << endl;

  file << "   rule every_out;" << endl;
  file << "      lockstep.outputs.put(outputs);" << endl;
  file << "   endrule" << endl;
  file << "   " << endl;
  file << "   rule every_in;" << endl;
  file << "      let value <- lockstep.inputs.get;" << endl;

  int from, to;
  to = in_count;
  file << "      dut.rst_n(unpack(value[" << to << "]));" << endl;
  from = --to;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    if (clk_rst_map[terminal->m_portName] == "") {
      if (terminal->m_dir != d_output) {
	from -= terminal->m_width-1;
	lstr = terminal->m_portName;
	toLowerCase(lstr);
	file << "      dut." << lstr << "(unpack(value["
	     << to;
	if (terminal->m_width > 1)
	  file << ":" << from;
	file << "]));" << endl;
	from--;
	to = from;
      }
    }
  }

  file << "   endrule" << endl;
  file << endl;
  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "   ///" << endl;
  file << "   ////////////////////////////////////////////////////////////////////////////////" << endl;
  file << endl;
  file << "endmodule" << endl << endl;
  
  file << "endpackage" << endl;

  file.close();

  return 1;
}

const char *HdlUtils::getBitType(ModuleTerminal *terminal, string &returnval)
{
  std::ostringstream strm;
  strm.str("");
  returnval = "Bit#(";
  strm << terminal->m_width;
  returnval += strm.str();
  returnval += ")";
  return returnval.c_str();
}

const char *HdlUtils::getPortNameVar(ModuleTerminal *terminal, const char *new_module_name,
				     string &returnval)
{
  returnval = new_module_name;
  returnval += "_";
  returnval += terminal->m_portName;
  return returnval.c_str();
}

int HdlUtils::isRegularModuleTerminal(ModuleTerminal *terminal)
{
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  std::map<std::string,MemoryInterface*>::iterator miItr;

  //cout << terminal->m_portName << " isRdyEnable?" << endl;
  reiItr = RdyEnableIfc.find(terminal->m_portName);
  if (reiItr != RdyEnableIfc.end() && reiItr->second != NULL)
    return 0;
  //cout << terminal->m_portName << " isMemory?" << endl;
  miItr = MemoryReqPortIfc.find(terminal->m_portName);
  if (miItr != MemoryReqPortIfc.end() && miItr->second != NULL)
    return 0;
  miItr = MemoryRespPortIfc.find(terminal->m_portName);
  if (miItr != MemoryRespPortIfc.end() && miItr->second != NULL)
    return 0;

  return 1;
}

void HdlUtils::saveFile(const char *filename)
{
  string savefilename = string(filename) + ".original";

  //cerr << "Saving " << savefilename << endl;

  fstream fin(filename, ios::in);
  fstream fout(savefilename.c_str(), ios::out);
  
  if(fin == NULL) {
    cerr << "Error HdlUtils:saveFile: cannot open file " << filename << endl;
    return;
  }
  
  if(fout == NULL) {
    cerr << "Error HdlUtils:saveFile: cannot open file " << savefilename << endl;
    return;
  }
  
  // read from the first file then write to the second file
  char c;
  while(!fin.eof())
    {
      fin.get(c);
      fout.put(c);
    }
  fin.close();
  fout.close();

  return;
}

int HdlUtils::generateDutVerilog(VeriModule *module, const char *module_name, const char *inst_name,
				 ParameterList &plist, ModuleTerminalList &new_terminals,
				 const char *indir, const char *outdir)
{
  string filename, origfilename;

  origfilename = string(indir) + "/mkBsv" + module_name + "_EDITED_1.v.original";
  filename = string(indir) + "/mkBsv" + module_name + "_EDITED_1.v";

  // Save the original synthesized verilog file
  HdlUtils::saveFile(filename.c_str());
  
  // Now create a simple shell by deleting everything except the header
  // and then add the module instance of the dut
  std::ifstream file;
  std::ofstream outfile;
  char lineBuf[MAX_LINE_SIZE+1];
  char *begin_discard = NULL;

  // Input file
  file.open(origfilename.c_str());
  if (file.fail()) { 
    cerr << "Error HdlUtils:generateDutVerilog: cannot open file " << origfilename << endl;
    return 0;
  }

  // Output file
  filename = string(outdir) + "/mkBsv" + module_name + "_EDITED_1.v";
  outfile.open(filename.c_str(), ios::out | ios::trunc);
  if (outfile.fail()) {
    cerr << "Error HdlUtils:generateDutVerilog: cannot open output file " << filename << endl;
    return 0;
  }

  //cerr << "Writing " << filename << endl;

  // Read in each line.  Right now assuming each line consists of either:
  while (!file.eof()) {

    file.getline(lineBuf, MAX_LINE_SIZE);

    if (begin_discard == NULL) {

      begin_discard = strstr(lineBuf, "// signals for");
      if (begin_discard == NULL) {
	outfile << lineBuf << endl;
	//cerr << "Writing: " << lineBuf << endl;
      }
    } else {

      file.close();
      break;
    }
  }

  // Write out and connect the dut instance
  outfile << "// synopsys translate_on" << endl << endl;
  outfile << "  " << module_name << " " << inst_name << " (";

  ModuleTerminalIterator mterm_it;
  int first = 1;
  for(mterm_it = new_terminals.begin(); mterm_it != new_terminals.end(); mterm_it++) {

    string portname = (*mterm_it).m_portName;
    string netname = (*mterm_it).m_netName;

    if (first == 0) {
      outfile << ", ";
    }
    outfile << "." << portname << "(" << netname << ")";
    first = 0;
  }

  outfile << ");" << endl << endl;
  outfile << "endmodule" << endl;
  outfile.close();

  return 1;
}

int HdlUtils::generateDutFile(VeriModule *module, const char *new_module_name,
			      const char *filename)
{
  std::fstream file;
  string errstring;
  string bits;
  string lower_new_mod_name = new_module_name; 
  string lstr;
  int foundREI = 0;
  std::map<string, int> rei_terminal_map;

  if (isupper(new_module_name[0]))
    lower_new_mod_name[0] = tolower(new_module_name[0]);
    
  file.open(filename, ios::out);
  if (file.fail()) {
    errstring += "Error HdlUtils::generateDutFile(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  printf("Writing %s\n", filename);
  file << "// Copyright Bluespec Inc." << endl;
  file << "// By: edithdl generate scemi utility" << endl << endl;

  file << "package " << new_module_name << ";" << endl << endl;

  file << "import FIFO::*;" << endl;
  file << "import FIFOF::*;" << endl;
  file << "import SpecialFIFOs::*;" << endl;
  file << "import GetPut::*;" << endl;
  file << "import Connectable::*;" << endl;
  file << "import ClientServer::*;" << endl;
  file << "import SceMi::*;" << endl;
  file << "import SceMiCore::*;" << endl;

  if (use_fpga_memory) {
    file << "import Memory::*;" << endl;
    file << "import SceMiMemoryXactor::*;" << endl;
  }

  // Generate all the data type
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;
  
  createModuleTerminalList(module, mtlist);

  // Input data type
  string input_type = new_module_name;
  int in_count = 0;
  int out_count = 0;
  RdyEnableInterface *rei;
  MemoryInterface *mi;
  string interface;

  // Ready enable ports
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  std::map<std::string,MemoryInterface*>::iterator miItr;
  std::list<std::string>::iterator stItr;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    file << "\ntypedef struct {" << endl;
    rei->setUsed(true);
    rei_terminal_map[rei->getRdy()] = 1;
    rei_terminal_map[rei->getEn()] = 1;
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (rei == RdyEnableIfc[lstr]) {
	if (terminal && (terminal->m_dir == d_input) && (terminal->m_portName != rei->getEn())) {
	  //file << "   Bit#(1) " << rei->getRdy() << ";" << endl;
	  //file << "   Bit#(1) " << rei->getEn() << ";" << endl;
	  toLowerCase(lstr);
	  file << "   Bit#(" << terminal->m_width << ") " << lstr << ";" << endl;
	  foundREI = 1;
	  rei_terminal_map[lstr] = 1;
	}
      }
    }
    if (foundREI) {
      file << "} " << new_module_name << "_" << rei->getInterface() << " deriving (Eq,Bits);" << endl;
      foundREI = 0;
    }
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (rei == RdyEnableIfc[lstr]) {
	if (terminal && (terminal->m_dir == d_output) && (terminal->m_portName != rei->getRdy())) {
	  //file << "   Bit#(1) " << rei->getRdy() << ";" << endl;
	  //file << "   Bit#(1) " << rei->getEn() << ";" << endl;
	  toLowerCase(lstr);
	  file << "   Bit#(" << terminal->m_width << ") " << lstr << ";" << endl;
	  foundREI = 1;
	  rei_terminal_map[lstr] = 1;
	}
      }
    }
    if (foundREI) {
      file << "} " << new_module_name << "_" << rei->getInterface() << " deriving (Eq,Bits);" << endl;
      file << endl;
      file << "instance Literal#(" << new_module_name << "_" << rei->getInterface() << ")" << endl;
      file << "   provisos(Literal#(" << new_module_name << "_" << rei->getInterface() << "));" << endl;
      file << "endinstance" << endl;
      foundREI = 0;
    }
  }
  file << endl;


  int first = 1;

  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    //printf("Terminal %s %d\n", terminal->m_portName.c_str(), terminal->m_width);
    if (clk_rst_map[terminal->m_portName] == "") {
      //printf("Non clk-rst Terminal %s %d\n", terminal->m_portName.c_str(), terminal->m_width);
      if (terminal->m_dir != d_output)
	in_count++;
      else
	out_count++;
    }
  }
  first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    //toLowerCase(lstr);
    //printf("Terminal %s %d\n", terminal->m_portName.c_str(), terminal->m_width);
    if (!isRegularModuleTerminal(terminal)) continue;
    input_type = string(new_module_name) + "_" + lstr;
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      file << "typedef Bit#(" << terminal->m_width << ") " << input_type << ";" << endl;
      first = 0;
    }

  }

  // Output data type
  string output_type = new_module_name;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!isRegularModuleTerminal(terminal)) continue;
    output_type = string(new_module_name) + "_" + lstr;
    //toLowerCase(lstr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      file << "typedef Bit#(" << terminal->m_width << ") " << output_type << ";" << endl;
      first = 0;
    }
  }
  if (first == 0)
    file << endl;

  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (first) {
      file << "interface " << new_module_name << "RawIfc#(";
      first = 0;
    }
    else
      file << ", ";
    if (rei->isPut() || rei->isPipePut()) {
      file << "type put_" << rei->getInterface();
    }
    else {
      file << "type get_" << rei->getInterface();
    }
  }

  string dut_ifc = new_module_name;
  dut_ifc += "Ifc";

  // IFC for the Dut
  //first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    //toLowerCase(lstr);
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "") &&
	isRegularModuleTerminal(terminal)) { 
      if (first) {
	file << "interface " << new_module_name << "RawIfc#(";
	first = 0;
      } else {
	file << ", ";
      }
      file << "type put_" << lstr;
    }
  }

  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    //toLowerCase(lstr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "") &&
	isRegularModuleTerminal(terminal)) { 
      if (first) {
	first = 0;
      } else {
	file << ", ";
      }
      file << "type get_" << lstr;
    }
  }
  if (first != 1)
    file << ");" << endl;
  
  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  string ifc_lowercase;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    ifc_lowercase = rei->getInterface();
    toLowerCase(ifc_lowercase);
    if (rei->isPut() || rei->isPipePut()) {
      file << "   interface Put#(" << "put_" << rei->getInterface()
	   << ") " << ifc_lowercase << ";" << endl;
      first = 0;
    }
    else {
      file << "   interface Get#(" << "get_" << rei->getInterface()
	   << ") " << ifc_lowercase << ";" <<  endl;
      first = 0;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    input_type = "put_" + lstr;
    toLowerCase(lstr);
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "") &&
	isRegularModuleTerminal(terminal)) { 
      file << "   interface Put#(" << input_type << ") " << lstr << ";" << endl;
      first = 0;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    output_type = "get_" + lstr;
    toLowerCase(lstr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "") &&
	isRegularModuleTerminal(terminal)) { 
      file << "   interface Get#(" << output_type << ") " << lstr << ";" << endl;
      first = 0;
    }
  }

  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL) continue;
    mi->setUsed(false);
  }
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL || mi->getUsed() == true) continue;
    mi->setUsed(true);
    file << "   interface MemoryClient#(" << mi->getAddressWidth() << ", "
	 << mi->getDataWidth() << ") " << mi->getInterface() << ";" << endl;
  }
  if (first == 0) {
    file << "endinterface" << endl;
    file << endl;
    file << "typedef " << new_module_name << "RawIfc#(";
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (first) {
      first = 0;
    } else {
      file << ", ";
    }
    file << new_module_name << "_" << rei->getInterface();
  }
  //if (first == 0)
  //  file << ") " << new_module_name << "Ifc;" << endl;

  //first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    input_type = string(new_module_name) + "_" + lstr;
    //toLowerCase(lstr);
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "") && 
	isRegularModuleTerminal(terminal)) { 
      if (first) {
	first = 0;
      } else {
	file << ", ";
      }
      file << input_type;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    output_type = string(new_module_name) + "_" + lstr;
    //toLowerCase(lstr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "") && 
	isRegularModuleTerminal(terminal)) { 
      if (first) {
	first = 0;
      } else {
	file << ", ";
      }
      file << output_type;
    }
  }
  if (first == 0)
    file << ") " << new_module_name << "Ifc;" << endl;
  file << endl;

  // Dut
  file << "(* synthesize *)" << endl;
  file << "module [Module] mk" << new_module_name << "(" << dut_ifc << " ifc);" << endl;
  file << endl;

  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isPut() || rei->isPipePut()) {
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  first = 0;
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  file << "   FIFO#(Bit#(" << terminal->m_width << ")) a";
	  file << new_module_name << "_" << rei->getInterface() << "_"
	       << lstr << " <- mkBypassFIFO;" << endl;
	}
      }
    } else {
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  first = 0;
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  file << "   FIFO#(Bit#(" << terminal->m_width << ")) a";
	  file << new_module_name << "_" << rei->getInterface() << "_"
	       << lstr << " <- mkBypassFIFO;" << endl;
	}
      }
    }
  }

  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL) continue;
    mi->setUsed(false);
  }
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL || mi->getUsed() == true) continue;
    mi->setUsed(true);
    file << "   FIFO#(MemoryRequest#(" << mi->getAddressWidth() << ", " << mi->getDataWidth()
	 << ")) a" << new_module_name << "_" << mi->getInterface() << "_req <- mkBypassFIFO;" << endl; 
    file << "   FIFO#(MemoryResponse#(" << mi->getDataWidth()
	 << ")) a" << new_module_name << "_" << mi->getInterface() << "_resp <- mkBypassFIFO;" << endl; 
  }
  file << endl;

  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    ifc_lowercase = rei->getInterface();
    toLowerCase(ifc_lowercase);
    if (rei->isPut() || rei->isPipePut()) {
      file << "   interface Put " << ifc_lowercase << ";" << endl;
      file << "      method Action put(tpl);" << endl;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  first = 0;
	  file << "         a" << new_module_name << "_" << rei->getInterface()
	       << "_" << lstr << ".enq(tpl." << lstr << ");" << endl;
	}
      }
    }
    else {
      file << "   interface Get " << ifc_lowercase << ";" << endl;
      file << "      method ActionValue#(" << new_module_name << "_" << rei->getInterface()
	   << ") get();" << endl;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  file << "         a" << new_module_name << "_" << rei->getInterface() << "_"
	       << lstr << ".deq;" << endl;
	}
      }
      file << "         let o = " << new_module_name << "_" << rei->getInterface() << " {" << endl;
      first = 1;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  if (first == 0) {
	    file << ",";
	    file << endl;
	  }
	  file << "           " << lstr << ": a" << new_module_name << "_" << rei->getInterface()
	       << "_" << lstr << ".first";
	  first = 0;
	}
      }
      file << endl;
      file << "         };" << endl;
      file << "         return o;" << endl;
    }
    file << "      endmethod" << endl;
    file << "   endinterface: " << ifc_lowercase << endl;
    file << endl;
  }

  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL) continue;
    mi->setUsed(false);
  }
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL || mi->getUsed() == true) continue;
    mi->setUsed(true);
    file << "   interface MemoryClient " << mi->getInterface() << ";" << endl;
    file << "     interface request = toGet(a" << new_module_name << "_" << mi->getInterface() << "_req" << ");" << endl;
    file << "     interface response = toPut(a" << new_module_name << "_" << mi->getInterface() << "_resp" << ");" << endl;
    file << "   endinterface" << endl;
  }

  file << "endmodule: mk" << new_module_name << endl << endl;
  file << "endpackage" << endl;

  file.close();

  return 1;
}

int HdlUtils::generateLockstepDutFile(VeriModule *module, const char *new_module_name,
				      const char *filename)
{
  std::fstream file;
  string errstring;
  string bits;
  string lower_new_mod_name = new_module_name; 
  string lstr;
  int foundREI = 0;
  std::map<string, int> rei_terminal_map;

  if (isupper(new_module_name[0]))
    lower_new_mod_name[0] = tolower(new_module_name[0]);
    
  file.open(filename, ios::out);
  if (file.fail()) {
    errstring += "Error HdlUtils::generateLockstepDutFile(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  printf("Writing %s\n", filename);
  file << "// Copyright Bluespec Inc." << endl;
  file << "// By: edithdl generate scemi utility" << endl << endl;

  file << "package " << new_module_name << ";" << endl << endl;

  file << "import FIFO::*;" << endl;
  file << "import FIFOF::*;" << endl;
  file << "import SpecialFIFOs::*;" << endl;
  file << "import GetPut::*;" << endl;
  file << "import Connectable::*;" << endl;
  file << "import ClientServer::*;" << endl;
  file << "import SceMi::*;" << endl;
  file << "import SceMiCore::*;" << endl;

  if (use_fpga_memory) {
    file << "import Memory::*;" << endl;
    file << "import SceMiMemoryXactor::*;" << endl;
  }

  // Generate all the data type
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;
  
  createModuleTerminalList(module, mtlist);

  // Input data type
  string input_type = new_module_name;
  int in_count = 0;
  int out_count = 0;
  RdyEnableInterface *rei;
  MemoryInterface *mi;
  string interface;

  // Ready enable ports
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  std::map<std::string,MemoryInterface*>::iterator miItr;
  std::list<std::string>::iterator stItr;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    file << "\ntypedef struct {" << endl;
    rei->setUsed(true);
    rei_terminal_map[rei->getRdy()] = 1;
    rei_terminal_map[rei->getEn()] = 1;
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (rei == RdyEnableIfc[lstr]) {
	if (terminal && (terminal->m_dir == d_input) && (terminal->m_portName != rei->getEn())) {
	  //file << "   Bit#(1) " << rei->getRdy() << ";" << endl;
	  //file << "   Bit#(1) " << rei->getEn() << ";" << endl;
	  toLowerCase(lstr);
	  file << "   Bit#(" << terminal->m_width << ") " << lstr << ";" << endl;
	  foundREI = 1;
	  rei_terminal_map[lstr] = 1;
	}
      }
    }
    if (foundREI) {
      file << "} " << new_module_name << "_" << rei->getInterface() << " deriving (Eq,Bits);" << endl;
      foundREI = 0;
    }
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (rei == RdyEnableIfc[lstr]) {
	if (terminal && (terminal->m_dir == d_output) && (terminal->m_portName != rei->getRdy())) {
	  //file << "   Bit#(1) " << rei->getRdy() << ";" << endl;
	  //file << "   Bit#(1) " << rei->getEn() << ";" << endl;
	  toLowerCase(lstr);
	  file << "   Bit#(" << terminal->m_width << ") " << lstr << ";" << endl;
	  foundREI = 1;
	  rei_terminal_map[lstr] = 1;
	}
      }
    }
    if (foundREI) {
      file << "} " << new_module_name << "_" << rei->getInterface() << " deriving (Eq,Bits);" << endl;
      file << endl;
      file << "instance Literal#(" << new_module_name << "_" << rei->getInterface() << ")" << endl;
      file << "   provisos(Literal#(" << new_module_name << "_" << rei->getInterface() << "));" << endl;
      file << "endinstance" << endl;
      foundREI = 0;
    }
  }
  file << endl;


  int first = 1;

  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    //printf("Terminal %s %d\n", terminal->m_portName.c_str(), terminal->m_width);
    if (clk_rst_map[terminal->m_portName] == "") {
      //printf("Non clk-rst Terminal %s %d\n", terminal->m_portName.c_str(), terminal->m_width);
      if (terminal->m_dir != d_output)
	in_count++;
      else
	out_count++;
    }
  }
  first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    //toLowerCase(lstr);
    //printf("Terminal %s %d\n", terminal->m_portName.c_str(), terminal->m_width);
    if (!isRegularModuleTerminal(terminal)) continue;
    input_type = string(new_module_name) + "_" + lstr;
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      file << "typedef Bit#(" << terminal->m_width << ") " << input_type << ";" << endl;
      first = 0;
    }

  }

  // Output data type
  string output_type = new_module_name;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!isRegularModuleTerminal(terminal)) continue;
    output_type = string(new_module_name) + "_" + lstr;
    //toLowerCase(lstr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "")) { 
      file << "typedef Bit#(" << terminal->m_width << ") " << output_type << ";" << endl;
      first = 0;
    }
  }
  if (first == 0)
    file << endl;

  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (first) {
      file << "interface " << new_module_name << "RawIfc#(";
      first = 0;
    }
    else
      file << ", ";
    if (rei->isPut() || rei->isPipePut()) {
      file << "type put_" << rei->getInterface();
    }
    else {
      file << "type get_" << rei->getInterface();
    }
  }

  string dut_ifc = new_module_name;
  dut_ifc += "Ifc";

  // IFC for the Dut
  //first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    //toLowerCase(lstr);
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "") &&
	isRegularModuleTerminal(terminal)) { 
      if (first) {
	file << "(* always_ready, always_enabled *)" << endl;
	file << "interface " << new_module_name << "RawIfc#(";
	first = 0;
      } else {
	file << ", ";
      }
      file << "type put_" << lstr;
    }
  }

  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    //toLowerCase(lstr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "") &&
	isRegularModuleTerminal(terminal)) { 
      if (first) {
	first = 0;
      } else {
	file << ", ";
      }
      file << "type get_" << lstr;
    }
  }
  if (first != 1)
    file << ");" << endl;
  
  first = 1;
  file << "   method Action rst_n(Bool value);" << endl;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  string ifc_lowercase;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    ifc_lowercase = rei->getInterface();
    toLowerCase(ifc_lowercase);
    if (rei->isPut() || rei->isPipePut()) {
      file << "   interface Put#(" << "put_" << rei->getInterface()
	   << ") " << ifc_lowercase << ";" << endl;
      first = 0;
    }
    else {
      file << "   interface Get#(" << "get_" << rei->getInterface()
	   << ") " << ifc_lowercase << ";" <<  endl;
      first = 0;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    input_type = "put_" + lstr;
    toLowerCase(lstr);
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "") &&
	isRegularModuleTerminal(terminal)) { 
      file << "   method Action " << lstr << "(" << input_type << " value);" << endl;
      first = 0;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    output_type = "get_" + lstr;
    toLowerCase(lstr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "") &&
	isRegularModuleTerminal(terminal)) { 
      file << "   method " << output_type << " " << lstr << ";" << endl;
      first = 0;
    }
  }

  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL) continue;
    mi->setUsed(false);
  }
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL || mi->getUsed() == true) continue;
    mi->setUsed(true);
    file << "   interface MemoryClient#(" << mi->getAddressWidth() << ", "
	 << mi->getDataWidth() << ") " << mi->getInterface() << ";" << endl;
  }
  if (first == 0) {
    file << "endinterface" << endl;
    file << endl;
    file << "typedef " << new_module_name << "RawIfc#(";
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (first) {
      first = 0;
    } else {
      file << ", ";
    }
    file << new_module_name << "_" << rei->getInterface();
  }
  //if (first == 0)
  //  file << ") " << new_module_name << "Ifc;" << endl;

  //first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    input_type = string(new_module_name) + "_" + lstr;
    //toLowerCase(lstr);
    if ((terminal->m_dir != d_output) && (clk_rst_map[terminal->m_portName] == "") && 
	isRegularModuleTerminal(terminal)) { 
      if (first) {
	first = 0;
      } else {
	file << ", ";
      }
      file << input_type;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (rei_terminal_map[lstr]) continue;
    output_type = string(new_module_name) + "_" + lstr;
    //toLowerCase(lstr);
    if ((terminal->m_dir == d_output) && (clk_rst_map[terminal->m_portName] == "") && 
	isRegularModuleTerminal(terminal)) { 
      if (first) {
	first = 0;
      } else {
	file << ", ";
      }
      file << output_type;
    }
  }
  if (first == 0)
    file << ") " << new_module_name << "Ifc;" << endl;
  file << endl;

  // Dut
  file << "(* synthesize *)" << endl;
  file << "module [Module] mk" << new_module_name << "(" << dut_ifc << " ifc);" << endl;
  file << endl;

  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isPut() || rei->isPipePut()) {
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  first = 0;
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  file << "   FIFO#(Bit#(" << terminal->m_width << ")) a";
	  file << new_module_name << "_" << rei->getInterface() << "_"
	       << lstr << " <- mkBypassFIFO;" << endl;
	}
      }
    } else {
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  first = 0;
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  file << "   FIFO#(Bit#(" << terminal->m_width << ")) a";
	  file << new_module_name << "_" << rei->getInterface() << "_"
	       << lstr << " <- mkBypassFIFO;" << endl;
	}
      }
    }
  }

  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL) continue;
    mi->setUsed(false);
  }
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL || mi->getUsed() == true) continue;
    mi->setUsed(true);
    file << "   FIFO#(MemoryRequest#(" << mi->getAddressWidth() << ", " << mi->getDataWidth()
	 << ")) a" << new_module_name << "_" << mi->getInterface() << "_req <- mkBypassFIFO;" << endl; 
    file << "   FIFO#(MemoryResponse#(" << mi->getDataWidth()
	 << ")) a" << new_module_name << "_" << mi->getInterface() << "_resp <- mkBypassFIFO;" << endl; 
  }
  file << endl;

  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    ifc_lowercase = rei->getInterface();
    toLowerCase(ifc_lowercase);
    if (rei->isPut() || rei->isPipePut()) {
      file << "   interface Put " << ifc_lowercase << ";" << endl;
      file << "      method Action put(tpl);" << endl;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  first = 0;
	  file << "         a" << new_module_name << "_" << rei->getInterface()
	       << "_" << lstr << ".enq(tpl." << lstr << ");" << endl;
	}
      }
    }
    else {
      file << "   interface Get " << ifc_lowercase << ";" << endl;
      file << "      method ActionValue#(" << new_module_name << "_" << rei->getInterface()
	   << ") get();" << endl;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  file << "         a" << new_module_name << "_" << rei->getInterface() << "_"
	       << lstr << ".deq;" << endl;
	}
      }
      file << "         let o = " << new_module_name << "_" << rei->getInterface() << " {" << endl;
      first = 1;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = locateModuleTerminal(mtlist, (*stItr));
	if (terminal) {
	  lstr = terminal->m_portName;
	  toLowerCase(lstr);
	  if (first == 0) {
	    file << ",";
	    file << endl;
	  }
	  file << "           " << lstr << ": a" << new_module_name << "_" << rei->getInterface()
	       << "_" << lstr << ".first";
	  first = 0;
	}
      }
      file << endl;
      file << "         };" << endl;
      file << "         return o;" << endl;
    }
    file << "      endmethod" << endl;
    file << "   endinterface: " << ifc_lowercase << endl;
    file << endl;
  }

  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL) continue;
    mi->setUsed(false);
  }
  for (miItr = MemoryReqIfc.begin(); miItr != MemoryReqIfc.end(); miItr++) {
    mi = miItr->second;
    if (mi == NULL || mi->getUsed() == true) continue;
    mi->setUsed(true);
    file << "   interface MemoryClient " << mi->getInterface() << ";" << endl;
    file << "     interface request = toGet(a" << new_module_name << "_" << mi->getInterface() << "_req" << ");" << endl;
    file << "     interface response = toPut(a" << new_module_name << "_" << mi->getInterface() << "_resp" << ");" << endl;
    file << "   endinterface" << endl;
  }

  file << "endmodule: mk" << new_module_name << endl << endl;
  file << "endpackage" << endl;

  file.close();

  return 1;
}

int HdlUtils::generatePinFile(const char *filename, const char *modulename)
{
  const char *delimiter = " \n\t,:[]'";
  char lineBuf[MAX_LINE_SIZE+1];
  char *token;

  std::ifstream file;
  file.open(filename, ifstream::in);
  if (!file.fail()) {
    // Read in each line.  Right now assuming each line consists of either:
    while (!file.eof()){
      file.getline(lineBuf, MAX_LINE_SIZE);
      token = strtok(lineBuf, delimiter);
      
      // Check comment
      if (token == NULL) continue;
      if (strncmp(token, "//", 2) == 0) continue;
      
      if (!strcmp(token, "port")) {
	cerr << "WARNING HdlUtils::generatePinFile(): found an existing pin file: ";
	cerr << filename;
	cerr <<  ", a new pin file will not be generated." << endl;
	return 0;
      }
    }
    file.close();
  }


  // Find the module
  VeriModule *module = HdlUtils::findModuleByName(modulename);
  if (module == NULL) {
    cerr << "ERROR HdlUtils::generatePinFile(): module " << modulename
	 << " not found." << endl;
    return 0;
  }

  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;

  HdlUtils::createModuleTerminalList(module, mtlist);
  std::fstream afile;

  afile.open(filename, fstream::out | fstream::app);
  if (afile.fail()) {
    cerr << "ERROR HdlUtils::generatePinFile(): unable to open pin file for write ";
    cerr << filename;
    cerr <<  "." << endl;
    return 0;
  }

  //afile << endl;
  afile << "// port statement" << endl;
  afile << "// input/output name LC/TC clk_name" << endl;
  afile << "// LC - loosely coupled  TC - tightly coupled" << endl;
  afile << "// clk_name is the name of clock domain that the port signal operates in" << endl;
  afile << "port" << endl;
  // Inputs
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    //printf("terminal %s %s\n", terminal->m_portName.c_str(), (*map)[terminal->m_portName].c_str());
    if ((terminal->m_dir != d_output) /*&& ((*map)[terminal->m_portName] == "")*/) {
      afile << " input " << terminal->m_portName << " " << terminal->m_width << " CLK" << endl;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((terminal->m_dir == d_output) /*&& ((*map)[terminal->m_portName] == "")*/) {
      afile << " output " << terminal->m_portName << " " << terminal->m_width << " CLK" << endl;
    }
  }
  afile << "endport" << endl;

  afile.close();

  // Now add probe of the outputs
  //afile.open("replay_edits.script", fstream::out);
  //if (afile.fail()) {
  //  cerr << "ERROR CktEdits::addDefaultPortSpec(): unable to open replay_edits.script file ";
  //  cerr << filename;
  //  cerr <<  "." << endl;
  //  return;
  //}

  //afile << "# replay script for adding circuit edits" << endl;
  //afile << endl;
  //for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
  //  terminal = &(*mtItr);
  //  string pname = terminal->m_portName;
  //  toLowerCase(pname);
  //  if ((terminal->m_dir == d_output) && ((*map)[terminal->m_portName] == "")) {
  //    afile << "addprobe " << terminal->m_portName << " " << vpath
  //	    << " " << pname << "_get CLK " << "1'b1 " << "Bit#("
  //	    << terminal->m_width << ")" << endl;
  //  }
  //}
  afile.close();

  return 1;
}

AssignmentIdVisitor::AssignmentIdVisitor(VeriIdDef* rh)
  : _rh (rh)
  , _assigned_ids ()
{ }

IdRefVisitor::IdRefVisitor(const BString & name)
  : _nm (name)
  , _refs ()
  , _ids ()
{ }

ContAssignmentIdVisitor::ContAssignmentIdVisitor(VeriIdDef* rh)
  : _rh (rh)
  , _assigned_ids ()
{ }

ContAssignmentVisitor::ContAssignmentVisitor(VeriIdDef* rh)
  : _rh (rh)
  , _assigned_ids ()
  , _assigns ()
{ }

 
