// Copyright 2009-2010 Bluespec Inc.  All rights reserved

//#include "stdio.h"
#include <unistd.h>
//#include <iostream>
//#include <string>
//#include <cstdlib>
#include <string>
//#include <streambuf>

//#include "flexlm.h"

//#include "Map.h"
#include "veri_file.h"
#include "VeriScope.h"
#include "VeriModule.h"
#include "VeriId.h"
#include "VeriMisc.h"
#include "VeriStatement.h"
#include "VeriExpression.h"
#include "HdlUtils.h"

using namespace std;

int main(int argc, char **argv)
{
  // Decode the options and read in the verilog files 
  string errstring;
  int i;

  //char *arg, *token;
  char **argV = (char **) malloc (argc * sizeof (char *)*3+10) ;
  argV[0] = (char *) "Bluespec::netlist" ;
  for (i = 1 ; i < argc-1; ++i) {
    argV[i] = argv[i];
    //printf("arg %d %s\n", i, argv[i-1]);
  }

  /*
  string bluespecdir;
  if (getenv("BLUESPECDIR"))
    bluespecdir = getenv("BLUESPECDIR");
  string extradir;
  if (bluespecdir != "") {
    extradir = "+libext+.v";
    argV[i] = new char[extradir.size()+1];
    strcpy(argV[i++], extradir.c_str());

    extradir = "-y";
    argV[i] = new char[extradir.size()+1];
    strcpy(argV[i++], extradir.c_str());
    extradir = bluespecdir + "/Verilog";
    argV[i] = new char[extradir.size()+1];
    strcpy(argV[i++], extradir.c_str());

    extradir = "-y";
    argV[i] = new char[extradir.size()+1];
    strcpy(argV[i++], extradir.c_str());
    extradir = bluespecdir + "/Libraries";
    argV[i] = new char[extradir.size()+1];
    strcpy(argV[i++], extradir.c_str());

    extradir = "-y";
    argV[i] = new char[extradir.size()+1];
    strcpy(argV[i++], extradir.c_str());
    extradir = bluespecdir + "/Libraries/BlueNoC";
    argV[i] = new char[extradir.size()+1];
    strcpy(argV[i++], extradir.c_str());
  }
  */
  argV[i++] = argv[argc-1];
  argc = i;

  HdlUtils *hdlUtils = HdlUtils::init();

  int stat = hdlUtils->decode_options(argc-1, argV, errstring);

  if (stat)
    stat &= hdlUtils->analyze(errstring);


  BString found;
  VeriModule *module = HdlUtils::findModuleByName(argV[argc-1]);
  if (module == 0) {
    cerr << "No module found with name '" << argV[argc-1] << endl;
    return 1;
  }

  // Get the list of verilog files used by the module
  std::map<string, int> list;
  HdlUtils::getVerilogFileList(module, list);

  std::map<string, int>::iterator fileIter;
  char currentPath[FILENAME_MAX];
  string ret_filename;
  const char *file_name;

  getcwd(currentPath, FILENAME_MAX);

  for (fileIter = list.begin(); fileIter != list.end(); fileIter++) {

    file_name = (*fileIter).first.c_str();

    // If it is not fully qualified directory then add 'pwd'
    if (file_name[0] != '/') {
      ret_filename = currentPath;
      ret_filename += "/";
    }
    else
      ret_filename = "";

    ret_filename += file_name;
    cout << ret_filename << endl;
  }

  hdlUtils->shutdown();

  delete argV;

  return 0;
}

