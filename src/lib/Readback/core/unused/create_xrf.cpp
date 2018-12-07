
//#include <cstdlib>
//#include <cstring>
#include <iostream>
//#include <vector>
#include <string>
//#include <sstream>

#include "Design.hpp"
#include "Utils.hpp"
#include "getopt.h"

using namespace std;

void print_help (const char* progname)
{
  /* Spaces and tabs are significant in this message; they're chosen so the
     message aligns properly both in a tty and in a Windows message box.
     Please try to preserve them; otherwise the output is very hard to read
     when using vprocess.  */
  fprintf(stdout, "Usage: %s [OPTIONS] <output_file> \n\
       The following OPTIONS are accepted:\n\
         -h help\n\
         -p <path/prefix>\n\
         -l <ll-file>\n\
         -r <rtl-file>\n\
         -s <syn-file>\n\
         -g <slog-file>\n\
         -i <include-hi>\n\
         -m missing\n", progname);
}

int check_file(const char *filename)
{
  std::fstream file;

  file.open(filename, ios::out);
  if (file.fail()) {
    printf("create_xrf: cannot open file %s.\n", filename);
    return 0;
  }
  file.close();
  return 1;
}

int netlist_export(Design *design, bool include_hidden, const char *file)
{
  string filename = file;
  int ret;

  ret = design->exportDesign(filename, include_hidden);

  return (ret == 0);
}

int main(int argc, char **argv)
{
  opterr = 0;
  optind = 0;

  string llfile, rtlfile, synfile, slogfile, prefix;
  string outfile;
  bool unit_time = false;
  bool include_hidden = false;

  //printf("Num args %d\n", argc);
  struct option longopts[] =
    {
      { "help",	         0,  0, 'h' },
      { "path-prefix",   0,  0, 'p' },
      { "ll-file",       0,  0, 'l' },
      { "rtl-file",      0,  0, 'r' },
      { "syn-file",      0,  0, 's' },
      { "slog-file",     0,  0, 'g' },
      { "unit-time"     ,0,  0, 'u' },
      { "include-hidden",0,  0, 'i' },
      { 0, 0, 0, 0 }
    };
  
  while (1)
    {
      int opt = getopt_long_only (argc, argv, "hp:l:r:s:g:ui", longopts, 0);

      if (opt == EOF) {
	break;
      }

      printf("OPT %c %s\n", opt, optarg);
      switch (opt) {
	
      case 'h':
	print_help("create_xrf");
	return 1;
	break;

      case 'p':

	prefix = optarg;
	break;

      case 'l':
	llfile = optarg;
	if (check_file(optarg) == 0)
	  return 1;
	break;
	
      case 'r':
	rtlfile = optarg;
	if (check_file(optarg) == 0)
	  return 1;
	break;

      case 's':
	synfile = optarg;
	if (check_file(optarg) == 0)
	  return 1;
	break;

      case 'g':
	slogfile = optarg;
	if (check_file(optarg) == 0)
	  return 1;
	break;

      case 'u':

	unit_time = true;
	break;

      case 'i':

	include_hidden = true;
	break;

      }
    }

  if (optind < argc) {
    outfile = argv[optind];
  } else {
    printf("create_xrf: output file is not given.\n");
    return 1;
  }

  // Try getting files from path/prefix if not there before
  if (llfile == "") {

    llfile = prefix + ".ll";
    if (check_file(llfile.c_str()) == 0)
      return 1;
  }

  if (rtlfile == "") {

    rtlfile = prefix + ".rtl";
    if (check_file(rtlfile.c_str()) == 0)
      return 1;
  }

  if (synfile == "") {

    synfile = prefix + ".syn";
    if (check_file(synfile.c_str()) == 0)
      return 1;
  }

  if (slogfile == "") {

    slogfile = prefix + ".slog";
    if (check_file(slogfile.c_str()) == 0)
      return 1;
  }

  Design * design = new Design();
  

  int ret = 0;

  if (ret == 0) {
    cout << "Rtl file: " << rtlfile << endl;
    ret = design->parse_rtl(rtlfile);
  }

  if (ret == 0) {
    ret = design->parse_synth(synfile);
  }

  if (ret == 0) {
    ret = design->parse_ll(llfile);
  }

  if (ret == 0) {
    ret = design->parse_log(slogfile);
  }

  if (ret == 0 && unit_time) {
    ret = design->setUnitTime();
  }

  if (ret == 0)
    ret = netlist_export(design, include_hidden, outfile.c_str());

  return ret;
}

