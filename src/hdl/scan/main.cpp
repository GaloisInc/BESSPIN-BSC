

#include <iostream>         // cout
using std::cout ;
#include <fstream>

// containers used :
// #include "Array.h" // a dynamic array
// #include "Set.h"   // a simple hash table
// #include "Map.h"

// utilities used :
//#include "Strings.h"        // Definition of class to easily create/concatenate char*s.
#include "VeriModule.h"

// Verilog parser, main interface :
#include "veri_file.h"      // Make Verilog reader available

#include <boost/regex.hpp>

#include "VUtils.h"
//#include "Dbg.h"
// #include "process.h"
// #include "ModuleDesc.h"
#include "Globals.h"
#include "options.h"
#include "process.h"


#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif

// -----------------------------------------------------------------------------
// Program main()                                                             
// -----------------------------------------------------------------------------


int main(int argc, char **argv)
{

  /* -------------------------------------------------------------------------- */
  /* Incorporate command line options.                                          */
  /* -------------------------------------------------------------------------- */

  if (decode_options_and_analyze(argc, argv)) {
    return 1;
  }

  VeriModule *module;
  foreachModule(module) {
     if (!module) continue ;
     if(!strcmp(module->GetName(), Globals::GetTopMod())) {
       break;
     }
  }

  //  Globals::InitializeLibrary();

  const char *parent, *name;
  parent = "";
  name   = "main";
  ModuleDesc* result;

  addBaseNames(module);
  module->StaticElaborate(NULL);

  module = renameAllModules(module);
  result = FindAndProcessModule(module, parent, name, 0);
//  result = ProcessModule(module, parent, name, 0, 0);  

  ScanPath* path = result->PathGet();

  //XYZ printf("PATH: %s\n", path->ToString("", "\n", "r: /top").c_str());

  Set* modules = collectModules(result->ModuleGet());
  
  const char* bsctop = Globals::GetBscTopMod();
  if (strlen(bsctop) == 0) {
    bsctop = NULL;
  }
  VeriModule* mod_to_print;
  SetIter si;
  FOREACH_SET_ITEM_BACK(modules, si, &mod_to_print) {

    if (bsctop) {
      const char* name = getModuleNameBase(mod_to_print);
      if (!strcmp(bsctop, name)) {
	//XYZ printf("MATCH %s %s\n", bsctop, name);
	std::string name_new = name;
	name_new += "_EDITED";
	mod_to_print = moduleCopyWithNewName(mod_to_print, name_new.c_str());
      }
    }

    std::string file = Globals::GetPath();
    file += "/";
    file += mod_to_print->GetName();
    file += ".v";

    std::ofstream out_file;
    out_file.open (file.c_str());
    if (out_file.is_open()) {
      Dbg::printf(1, "Printing module %s to file %s\n", mod_to_print->GetName(), file.c_str());
      mod_to_print->PrettyPrint(out_file, 0);
      out_file.close();
    } else {
      fprintf(stderr, "ERROR: Unable to open file '%s'.\n", file.c_str());
    }
  }

  return 0;
}

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */



