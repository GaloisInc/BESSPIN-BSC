
#include "getopt.h"
#include "string.h"

#include "veri_file.h"

#include "Dbg.h"
#include "Globals.h"
#include "VUtils.h"


// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

void print_help (const char* progname)
{
  /* Spaces and tabs are significant in this message; they're chosen so the
     message aligns properly both in a tty and in a Windows message box.
     Please try to preserve them; otherwise the output is very hard to read
     when using vprocess.  */
  fprintf(stdout, "Usage: %s [OPTIONS] VERILOG_FILE VERILOG_FILE ...\n\
       The following OPTIONS are accepted:\n", progname);
}

int decode_options (int argc, char **argv)
{
  Globals::SetExecName(argv[0]);

  int opt_errors_encountered = 0;
  struct option longopts[] =
    {
      { "help",	         0,  0, 'h' },
      { "just-modified", 0,  0, 'j' },
      { "no-elab",       0,  0, 'n' },
      { "regexp",        1,  0, 'r' },
      { "topmod",        1,  0, 'e' },
      { "vdir",	         1,  0, 'v' },
      { "ydir",	         1,  0, 'y' },
      { "outdir",        1,  0, 'o' },
      { "verbose",       0,  0, 'V' },
      { "sim",           0,  0, 's' },
      { 0, 0, 0, 0 }
    };
  
  while (1)
    {
      int opt = getopt_long_only (argc, argv, "Vhjne:o:r:v:y:", longopts, 0);

      if (opt == EOF)
	break;

      //      printf("KKK %s\n", optarg);
      switch (opt)
	{
	case 'e':
	  Globals::SetTopMod(optarg);
          break;
	case 'h':
	  opt_errors_encountered = 1;
	  print_help(Globals::GetExecName());
          break;
	case 'j':
	  Globals::SetPrintOnlyModified();
          break;
	case 'r':
	  Globals::SetRegExpr(optarg);
          break;
	case 'v':
	  Globals::AddVFile(optarg);
          break;
	case 'V':
	  //	  printf("boo\n");
	  Dbg::Incr();
          break;
	case 'y':
	  Globals::AddYDir(optarg);
          break;
	case 'n':
	  Globals::SetNoElab();
          break;
	case 'o':
	  Globals::SetPath(optarg);
          break;
	case 's':
	  Globals::SetIgnore();
          break;
	case '?':
	  opt_errors_encountered = 1;
	  break;
	default:
	  /* unreachable */
	  break;
	}
    }

  if (opt_errors_encountered) {
    fprintf(stderr, "%s: exiting\n", Globals::GetExecName());
    return 1;
  }

//   if (!Gtopmod) {
//     fprintf(stderr, "%s: no top module specified\n", argv[0]);
//     fprintf(stderr, "%s: exiting\n", argv[0]);
//     return 1;
//   }

  char *identifier;

  while (optind < argc) {

    if (!strncmp(argv[optind], "+libext+", 8)) {
      char *expr;
      expr = argv[optind] + 8;
      if (!expr || strlen(expr) == 0) {
	fprintf(stderr, "%s: ill-formed plusarg '%s' exiting\n", argv[0], argv[optind]);
	return 1;
      }
      Globals::AddLibExt(expr);
      optind++;
      continue;
    }

    if (!strncmp(argv[optind], "+define+", 8)) {
      char *expr;
      expr = argv[optind] + 8;
      if (!expr || strlen(expr) == 0) {
	fprintf(stderr, "%s: ill-formed plusarg '%s' exiting\n", argv[0], argv[optind]);
	return 1;
      }
      Globals::AddDefMacro(expr);
      optind++;
      continue;
    }

    if (!strncmp(argv[optind], "+incdir+", 8)) {
      identifier = argv[optind] + 8;
      if (!identifier || strlen(identifier) == 0) {
	fprintf(stderr, "%s: ill-formed plusarg '%s' exiting\n", argv[0], argv[optind]);
	return 1;
      }
      Globals::AddIDir(identifier);
      optind++;
      continue;
    }
    
    //    printf("Inserting %s\n", argv[optind]) ;
    Globals::AddFile(argv[optind]);
    optind++;
  }

  return opt_errors_encountered;
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------


int decode_options_and_analyze(int argc, char **argv)
{

  Dbg::Default(2);

  if (decode_options(argc, argv)) {
    return 1;
  }

  // Create a verilog reader object
  Verific::veri_file veri_reader ;

  if (Globals::GetIgnore()) {
    veri_reader.SetIgnoreTranslateOff(1);
  }

  foreachInStringSet(Globals::GetLibExts(), it) {
    char* expr = (char*) (*it).data();

    //XYZ printf("LIB EXT: %s\n", expr);

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
		  Globals::GetExecName());
		
	  return 1;
	}
	veri_reader.AddLibExt(expr);
	//XYZ printf("ADDING EXT: %s\n", expr + 1);
	expr = next;
      } else {
	//XYZ printf("ADDING EXT: %s\n", expr + 1);
	break;
      }
    }
  }

  foreachInStringSet(Globals::GetDefMacros(), it) {
    char* expr = (char*) (*it).data();
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
		Globals::GetExecName(), expr);
		
	return 1;
      }
      if (veri_reader.DefineMacro(expr, value)) return 1;

      if(!strcmp(expr, "TOP")) {
	Globals::SetBscTopMod(value);
	//XYZ printf("BSC TOP: %s  = %s\n", expr, value);
      }
      //XYZ printf("ADDING DEF: %s = %s\n", expr, value);
    } else {

      if (veri_reader.DefineMacro(expr)) return 1;
      //XYZ printf("ADDING DEF: %s\n", expr);
    }
  }

  foreachInStringSet(Globals::GetYDirs(), it) {
    char* path = (char*) (*it).data();
    veri_reader.AddYDir(path);
  }

  foreachInStringSet(Globals::GetVFiles(), it) {
    char* path = (char*) (*it).data();
    veri_reader.AddVFile(path);
  }

  foreachInStringSet(Globals::GetIDirs(), it) {
    char* path = (char*) (*it).data();
    veri_reader.AddIncludeDir(path);
    //XYZ printf("ADDING INC DIR: %s\n", path);
  }

  foreachInStringSet(Globals::GetFiles(), it) {
    char* path = (char*) (*it).data();
    if (!veri_reader.Analyze(path)) return 1 ;
  }

  VeriModule *module;

  unsigned found = 0;
  unsigned count = 0;
  VeriModule* top;
  foreachModule(module) {
     if (!module) continue ;
     top = module;
     count++;
     if(!strcmp(module->GetName(), Globals::GetTopMod())) {
       found = 1;
       break;
     }
  }

  if (!strcmp(Globals::GetTopMod(), "") && top && !found && count == 1) {
    Globals::SetTopMod(top->GetName());
    found = 1;
  }

  if (!strcmp(Globals::GetTopMod(), "")  && !found && count > 1) {
    fprintf(stderr, "%s: no top module specified\n", argv[0]);
    fprintf(stderr, "%s: exiting\n", argv[0]);
    return 1;
  }

  if (!found) {
    fprintf(stderr, "%s: unable to resolve top module '%s'\n", argv[0], Globals::GetTopMod());
    fprintf(stderr, "%s: exiting\n", argv[0]);
    return 1;
  }

  return 0;
}
