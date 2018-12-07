// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED

#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <boost/regex.hpp>
#include <boost/lexical_cast.hpp>
#include <bitset>
#include <dirent.h>
#include <sys/stat.h>
#include <libgen.h>
#include "TestBenchGenerator.h"
#include <cstdlib>

void toLowerCase(std::string &str);

#define MAX_LINE_SIZE 1023

using namespace std;

static int recursive_mkdir(const char *path, mode_t mode)
{
  char *spath = strdup(path);
  char *next_dir = dirname(spath);
  int stat = 0;
  
  if (access(next_dir, F_OK) == 0)
    {
      goto done;
    }
  
  if (strcmp(next_dir, ".") == 0 || strcmp(next_dir, "/") == 0)
    {
      goto done;
    }
  
  stat &= recursive_mkdir(next_dir, mode);
  stat &= mkdir(next_dir, mode);
  
 done:
  if (access(path, F_OK) != 0)
    stat &= mkdir(path, mode);

  free(spath);
  return stat;
}

void TBMessage::convertToBinaryBits()
{
  int words = Bits.size();
  BinaryBits = new char[words];

  for (int i = Width; (i>0); --i ) {
    unsigned int bitidx = i - 1;
    unsigned int wrdidx = bitidx/32;
    bitidx = bitidx & 0x01f;  // bitidx % 32
    if (Bits[bitidx] == '1')
      BinaryBits[wrdidx] |= (0x1 << bitidx);
  }
}

bool TBMessage::operator==(TBMessage& msg) const
{
  if (Type != msg.Type) return false;
  if (Bits != msg.Bits) return false;
  if (Width != msg.Width) return false;
  return true;
}

bool TBMessage::operator!=(TBMessage& msg) const
{
  if (Type != msg.Type) return true;
  if (Bits != msg.Bits) return true;
  if (Width != msg.Width) return true;
  return false;
}

void VCDPort::addSubPort(const char *name, const char *veriname, int w, DirectionE dir, const char *symbol)
{
  VCDPort *p = new VCDPort(name, veriname, w, dir, symbol);
  Ports.push_back(p);
  Width += w;
  p->Master = this;
}

void VCDPort::addSubPort(VCDPort *port)
{
  Ports.push_back(port);
  Width += port->width();
  port->Master = this;
}

VCDPort::~VCDPort()
{
  VCDPortIter i;

  for (i=Ports.begin(); i!=Ports.end(); i++)
    {
      delete *i;
    }
}

void VCDPort::initializeMessage()
{
  if (Init == false && Width > 0) {
    Message = new char[Width+1];
    MsbFirstMessage = new char[Width+1];

    for (unsigned int i=0; i<Width; i++)
      Message[i] = '0';
    Message[Width] = '\0';
    MsbFirstMessage[Width] = '\0';
    Init = true;
  }
}

void VCDPort::setMessage(std::string &input)
{
  //if (input.size() != Width)
  //cerr << "Something wrong in setMessage() in size " << input.size() << " width " << Width << endl;
  int i = 0;

  for (int j=input.size()-1; j>=0; j--) {
    //printf("copying from j %d to i %d\n", j, i);
    Message[i] = input[j];
    i++;
  }
  while (i <= (int)Width-1) {
    Message[i] = '0';
    i++;
  }
  //printf("done set message\n");
}

void VCDPort::setMessageAt(unsigned int i, char c)
{
  Message[i] = c;
}

void VCDPort::setMessageAt(unsigned int from, unsigned int to, std::string &input)
{
  unsigned int j=0;
  unsigned int tmp;

  if (from > to) {
    tmp = from;
    from = to;
    to = tmp;
  }

  for (unsigned int i=from; i<=to; i++)
    Message[i] = input[j++];
}

const char *VCDPort::getMsbFirstMessage()
{
  unsigned j = Width-1;

  if (Message == NULL)
    initializeMessage();

  for (unsigned int i=0; i<=Width-1; i++)
    MsbFirstMessage[i] = Message[j--];
  return MsbFirstMessage;
}

// Constructors
TestBenchGenerator::TestBenchGenerator()
{
  LineNum = 0;
  GenVerification = 0;
  SRFileName = "stimulus.dat";
  SRFileOpen = 0;
  SceMiLatency = 4;
  Tick = PrevTick = 0;
}

TestBenchGenerator::TestBenchGenerator(const char *vcdFile)
{
  setVCDFile(vcdFile);
  LineNum = 0;
  GenVerification = 0;
  SRFileName = "stimulus.dat";
  SRFileOpen = 0;
  SceMiLatency = 4;
  Tick = PrevTick = 0;
}

// Destructor
TestBenchGenerator::~TestBenchGenerator()
{
}

int TestBenchGenerator::readSpecFile(const char* filename)
{
  const char *delimiter = " \n\t,:[]'";
  std::ifstream file;
  char lineBuf[MAX_LINE_SIZE+1];
  char *token;
  char *token2;
  int stat;
  char buf[1024];
  std::string cont_sig, prev_clk_sig;

  LineNum = 0;

  file.open(filename);
  if (file.fail()) {
    ErrMsg = "ERROR TestBenchGenerator::readSpecFile(): unable to open testbench spec file ";
    ErrMsg += filename;
    ErrMsg += ".";
    return 0;
  }

  // Read in each line.  Right now assuming each line consists of either:
  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    LineNum++;
    token = strtok(lineBuf, delimiter);
    if (token && (strncmp(token, "//", 2) != 0)) { // Check comment
      if (!strcmp(token, "top-module")) {
	
	token = strtok(0, delimiter);
	ModuleName = token;
	BSVModuleName += "Bsv" + ModuleName;
	if (VerilogPath == "")
	  VerilogPath = "/mkBridge/scemi_dut_dutIfc";
      }
      else if (!strcmp(token, "input-vcd-path")) {
	
	token = strtok(0, delimiter);
	if (token)
	  VerilogPath = token;
      }
      else if (!strcmp(token, "input-vcd-dut-path")) {
	
	token = strtok(0, delimiter);
	if (token)
	  VCDPath = token;
      }
      else if (!strcmp(token, "scemi-clk-signals")) {
	
	token = strtok(0, delimiter);
	while (token) {
	  token2 = strtok(0, delimiter);
	  if (token2 == NULL || token == NULL) {
	    ErrMsg = "Error TestBenGenerator::readSpecFile(): bad scemi-clk-signals statement in testbench spec file on line ";
	    sprintf(buf, "%d", LineNum);
	    ErrMsg += buf;
	    return 0;
	  }
	  cont_sig = token;
	  cont_sig += string(":") + token2;
	  SceMiControlSignals.push_back(cont_sig);
	  VcdClockToSceMiClockMap[token2] = token;
	  SceMiClockToVcdClockMap[token] = token2;
	  token = strtok(0, delimiter);
	  prev_clk_sig = token2;
	}
      }
      else if (!strcmp(token, "scemi-reset-signals")) {
	
	token = strtok(0, delimiter);
	while (token) {
	  token2 = strtok(0, delimiter);
	  if (token2 == NULL || token == NULL) {
	    ErrMsg = "Error TestBenGenerator::readSpecFile(): bad scemi-reset-signals statement in testbench spec file on line ";
	    sprintf(buf, "%d", LineNum);
	    ErrMsg += buf;
	    return 0;
	  }
	  cont_sig = token;
	  cont_sig += string(":") + token2;
	  SceMiControlSignals.push_back(cont_sig);
	  VcdResetToSceMiResetMap[token2] = token;
	  SceMiResetToVcdResetMap[token] = token2;
	  VcdResetToVCDClockMap[token2] = prev_clk_sig;
	  token = strtok(0, delimiter);
	}
      }
      else if (!strcmp(token, "input-vcd-file")) {
	
	token = strtok(0, delimiter);
	if (token) {
	  VCDFileName = token;
	  //printf("Reading VCD file %s\n", VCDFileName.c_str());
	  stat = setVCDFile(VCDFileName.c_str());
	  if (!stat) {
	    ErrMsg = "Error TestBenGenerator::readSpecFile(): bad vcd file specification in bld file ";
	    ErrMsg += filename;
	    ErrMsg += " on line ";
	    sprintf(buf, "%d", LineNum);
	    ErrMsg += buf;
	    return 0;
	  }
	  cout << "HERE in input-vcd-file" << endl;
	  GenVerification = 1;
	}
      }
      else if (!strcmp(token, "bsv-source-directory")) {
	
	token = strtok(0, delimiter);
	BsvOutputDir = token;
      }
      else if (!strcmp(token, "c++-source-directory")) {
	
	token = strtok(0, delimiter);
	CppOutputDir = token;
      }
      else if (!strcmp(token, "log-directory")) {
	
	token = strtok(0, delimiter);
	LogDir = token;
      }
      else {
	//ErrMsg = "Error TestBenGenerator::readSpecFile(): bad statement in testbench spec file on line ";
	//sprintf(buf, "%d", LineNum);
	//ErrMsg += buf;
	//return 0;
      }
    }
  }
  return 1;
}

bool TestBenchGenerator::getSceMiControlSignals(VeriModule *module, netCollection &signals)
{
  VeriIdDef *net;
  std::string sig;
  std::list<netHandle>::iterator sItr;

  for (sItr = SceMiControlSignals.begin();
       sItr != SceMiControlSignals.end();
       sItr++) {

    sig = *sItr;
    net = HdlUtils::findSignalInModuleByName(module, sig);

    // get signal width
    if (net) {
      signals.push_back(sig);
    } else {
      ErrMsg = "Signal: '" + sig + "' cannot be found in module " + module->Name();
      return false;
    }
  }

  return true;
}

int TestBenchGenerator::generateTestBench(const char *dirname)
{
  std::string foundpath;
  const char *modname;
  const char *new_module_name;
  netCollection signals;
  int stat = 1;

  //printf("GenerateTestBench %s\n", verilogPath());
  VeriModule *module = HdlUtils::findModuleFromPath(NULL, verilogPath(), foundpath);
  if (module == NULL) {
    ErrMsg = "No object1 found with name '" + string(verilogPath()) + "'";
    return 0;
  }

  modname = moduleName();
  new_module_name = bsvModuleName();
  getSceMiControlSignals(module, signals);

  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  //cout << "Map pointer1 " << (void*)map << endl;
  std::list<netHandle>::iterator netItr;
  string s, m;
  for (netItr = SceMiControlSignals.begin(); netItr != SceMiControlSignals.end(); netItr++) {

    s = *netItr;

    m = "";
    size_t pos = s.find(':');
    if (pos != string::npos) {
      m = s.substr(0, pos);
      s = s.substr(pos+1);
      //cout << "MAP from " << s << " to " << m << endl;
      (*map)[s] = m;
    }
    (*map)[s] = m;
  }

  VeriModule *origModule = HdlUtils::findModuleByName(modname);

  // Create TclTb.cpp file
  //printf("GenerateTclTb of %s %p\n", modname, origModule);
  generateTclTbFile(origModule, new_module_name, dirname);

  //printf("GenerateDutXactor %s\n", dirname);
  string cppdir = "build/cpp";
  generateDutXactor(origModule, new_module_name, cppdir.c_str());

  //printf("GenerateDutXactor\n");
  //generateVcdPlayer(origModule, new_module_name, dirname);

  //printf("GenerateTclGui\n");
  generateTclGui(origModule, new_module_name, dirname);

  //generateCapiTemplate(origModule, new_module_name, dirname);

  //cout << "GEN Verification " << GenVerification << endl;
  if (GenVerification)
    stat = generateVerificationFromVCD(origModule, dirname);

  //printf("DONE generateTB\n");
  return stat;
}

int TestBenchGenerator::generateTestBenchTemplate(const char *dirname)
{
  std::string foundpath;
  const char *modname;
  const char *new_module_name;
  netCollection signals;
  int stat = 1;

  //printf("GenerateTestBench Template %s %s\n", moduleName(), dirname);
  VeriModule *module = HdlUtils::findModuleFromPath(NULL, moduleName(), foundpath);
  if (module == NULL) {
    ErrMsg = "No object2 found with name '" + string(verilogPath()) + "'";
    return 0;
  }

  modname = moduleName();
  new_module_name = bsvModuleName();
  getSceMiControlSignals(module, signals);

  string filename = module->Name();
  filename += ".pin";

  if (HdlUtils::init()->readPinFile(filename.c_str()) == 0)
    return 0;

  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  //cout << "Map pointer1 " << (void*)map << endl;
  std::list<netHandle>::iterator netItr;
  string s, m;
  for (netItr = SceMiControlSignals.begin(); netItr != SceMiControlSignals.end(); netItr++) {

    s = *netItr;

    m = "";
    size_t pos = s.find(':');
    if (pos != string::npos) {
      m = s.substr(0, pos);
      s = s.substr(pos+1);
      //cout << "MAP from " << s << " to " << m << endl;
      (*map)[s] = m;
    }
    (*map)[s] = m;
  }

  VeriModule *origModule = HdlUtils::findModuleByName(modname);

  // Create TclTb.cpp file
  //printf("GenerateTclTb of %s %s\n", new_module_name, dirname);
  stat &= generateTclTbFile(origModule, new_module_name, dirname, 1);

  //printf("GenerateTclGui\n");
  stat &= generateTclGui(origModule, new_module_name, dirname, 1);

  stat &= generateCapiTemplate(origModule, new_module_name, dirname);

  //printf("DONE generateTB\n");
  return stat;
}

int TestBenchGenerator::generateSimTbTemplate(VeriModule *module, const char *dirname,
					      int tbtemplate)
{
  std::fstream file;
  string errstring;
  DIR *dp;
  RdyEnableInterface *rei;
  std::list<std::string>::iterator stItr;
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();
  string lstr, pn;
  int first, start, stop;
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;
  
  // Create list of module terminal
  HdlUtils::createModuleTerminalList(module, mtlist);
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();

  string filename = module->Name();
  filename += ".pin";

  if (HdlUtils::init()->readPinFile(filename.c_str()) == 0)
    return 0;

  HdlUtils::init()->setRdyEnableIfcWidth(module);

  if (dirname == NULL || dirname[0] == '\0')
    dirname = "tb";

  string realdir;
  realdir = dirname;
  size_t loc = realdir.find_first_of('/');
  if ((tbtemplate != 1) && (loc != 0)) { // template and not absolute path
    if (loc != string::npos)
      realdir = realdir.substr(0, loc+1) + "cpp";
  }
  filename = realdir;
  if (filename[filename.size()] != '/')
    filename += "/";
  filename += module->Name();
  filename += "_proxy.v";

  if ((dp = opendir(dirname)) == NULL) {
    if(recursive_mkdir(dirname, 0755) != 0) {
      errstring += "Error TestBenchGenerator::generateSimTbVerilog(): cannot open directory ";
      errstring += dirname;
      fprintf(stderr, "%s\n", errstring.c_str());
      return 0;
    }
  }
  closedir(dp);

  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateSimTbVerilog(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "`ifdef BSV_ASSIGNMENT_DELAY" << endl;
  file << "`else" << endl;
  file << "  `define BSV_ASSIGNMENT_DELAY" << endl;
  file << "`endif" << endl << endl;

  file << "`ifdef BSV_POSITIVE_RESET" << endl;
  file << "  `define BSV_RESET_VALUE 1'b1" << endl;
  file << "  `define BSV_RESET_EDGE posedge" << endl;
  file << "`else" << endl;
  file << "  `define BSV_RESET_VALUE 1'b0" << endl;
  file << "  `define BSV_RESET_EDGE negedge" << endl;
  file << "`endif" << endl;

  file << endl;
  file << "module " << module->Name() << "_proxy(";

  first = 1;
  string clkname, rstname;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] != "") {
      pn = terminal->m_portName;
      if ((*map)[terminal->m_portName] == "CLK")
	clkname = pn;
      if ((*map)[terminal->m_portName] == "RST_N")
	rstname = pn;
      if (first) {
	file << pn;
	first = 0;
      }
      else {
	file << "," << endl;
	file << "                 " << pn;
      }
    }
  }

  // Handshake
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }

  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	file << "," << endl;
	if (first) {
	  file << endl;
	  first = 0;
	}
	file << "                 " << *stItr;
      }
      if (string(rei->getRdy()) != "") {
	file << "," << endl;
	file << "                 " << rei->getRdy();
      }
      if (string(rei->getEn()) != "") {
	file << "," << endl;
	file << "                 " << rei->getEn();
      }
    }
  }

  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	file << "," << endl;
	if (first) {
	  file << endl;
	  first = 0;
	}
	file << "                 " << *stItr;
      }
      if (string(rei->getRdy()) != "") {
	file << "," << endl;
	file << "                 " << rei->getRdy();
      }
      if (string(rei->getEn()) != "") {
	file << "," << endl;
	file << "                 " << rei->getEn();
      }
    }
  }

  // Individual ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      pn = terminal->m_portName;
      file << "," << endl;
      if (first) {
	file << endl;
	first = 0;
      }
      file << "                 " << pn;
    }

  }

  file << ");" << endl << endl;
  
  file << "  input " << clkname << ";" << endl;
  file << "  input " << rstname << ";" << endl << endl;

  // Handshake
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      file << "  // action method " << rei->getInterface() << endl;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	start = stop = 0;
	terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
	if (terminal && (terminal->m_dir == d_input)) {
	  stop += terminal->m_width;
	  if (terminal->m_width > 1)
	    file << "  input [" << stop-1 << " : " << start << "] " 
		 << terminal->m_portName << ";" << endl;
	  else
	    file << "  input " 
		 << terminal->m_portName << ";" << endl;
	  start += stop;
	}
      }
      if (string(rei->getRdy()) != "") 
	file << "  output " << rei->getRdy() << ";" << endl;
      if (string(rei->getEn()) != "") 
	file << "  input " << rei->getEn() << ";" << endl;
      file << endl;
    }
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      file << "  // actionvalue method " << rei->getInterface() << endl;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	start = stop = 0;
	terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
	if (terminal && (terminal->m_dir == d_output)) {
	  stop += terminal->m_width;
	  if (terminal->m_width > 1)
	    file << "  output [" << stop-1 << " : " << start << "] " 
		 << terminal->m_portName << ";" << endl;
	  else
	    file << "  output " 
		 << terminal->m_portName << ";" << endl;
	  start += stop;
	}
      }
      if (string(rei->getRdy()) != "") 
	file << "  output " << rei->getRdy() << ";" << endl;
      if (string(rei->getEn()) != "") 
	file << "  input " << rei->getEn() << ";" << endl;
      file << endl;
    }
  }

  // Individual ports
  int in_count = 0;
  int out_count = 0;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    if ((*map)[terminal->m_portName] == "") {
      if (terminal->m_dir != d_output)
	in_count += terminal->m_width;
      else
	out_count += terminal->m_width;
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      pn = terminal->m_portName;
      if (terminal && (terminal->m_dir == d_input)) {
	file << "  input ";
      }
      else {
	file << "  output ";
      }
      if (terminal->m_width > 1)
	file << "[" << terminal->m_width-1 << ":0] ";
      file << pn << ";" << endl; 
    }
  }


  // Handshake
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      file << "  // ports of pipe proxy put submodule for " << rei->getInterface() << endl;
      if (rei->getWidth() > 1)
	file << "  wire [" << rei->getWidth()-1 << " : 0] " << rei->getInterface() << "_data_in$DATA;" << endl;
      else 
	file << "  wire " << rei->getInterface() << "_data_in$DATA;" << endl;
      file << "  wire " << rei->getInterface() << "_data_in$DATA_EN, " << rei->getInterface() 
	   << "_data_in$DATA_RDY;" << endl << endl;
    }
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      file << "  // ports of pipe proxy get submodule for " << rei->getInterface() << endl;
      if (rei->getWidth() > 1)
	file << "  wire [" << rei->getWidth()-1 << " : 0] " << rei->getInterface() << "_data_out$DATA;" << endl;
      else
	file << "  wire " << rei->getInterface() << "_data_out$DATA;" << endl;
      file << "  wire " << rei->getInterface() << "_data_out$DATA_EN, " << rei->getInterface() 
	   << "_data_out$DATA_RDY;" << endl << endl;
    }
  }

  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;

    if (first) {
      file << "  // rule scheduling signals" << endl;
      first = 0;
    }

    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      //file << "  wire CAN_FIRE_" << rei->getInterface() << ";" << endl;
      //file << "  wire WILL_FIRE_" << rei->getInterface() << ";" << endl;
    }
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      //file << "  wire CAN_FIRE_" << rei->getInterface() << ";" << endl;
      //file << "  wire WILL_FIRE_" << rei->getInterface() << ";" << endl;
    }
  }
  file << endl;

  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      file << "  // action method " << rei->getInterface() << endl;
      if (string(rei->getRdy()) != "")
	file << "  assign " << rei->getRdy() << " = " 
	     << rei->getInterface() << "_data_in$DATA_RDY;" << endl;
      //file << "  assign CAN_FIRE_" << rei->getInterface() << " = "
      //   << rei->getInterface() << "_data_in$DATA_RDY;" << endl;
      //file << "  assign WILL_FIRE_" << rei->getInterface() << " = EN_"
      //   << rei->getInterface() << ";" << endl << endl;
    }
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      file << "  // actionvalue method " << rei->getInterface() << endl;
      file << "  assign " << rei->getInterface() << " = "
	   << rei->getInterface() << "_data_out$DATA;" << endl;
      file << "  assign " << rei->getRdy() << " = "
	   << rei->getInterface() << "_data_out$DATA_RDY;" << endl;
      //file << "  assign CAN_FIRE_" << rei->getInterface() << " = "
      //   << rei->getInterface() << "_data_out$DATA_RDY;" << endl;
      //file << "  assign WILL_FIRE_" << rei->getInterface() << " = EN_"
      //   << rei->getInterface() << ";" << endl << endl;
    }
  }
  file << endl;

  // Individual ports
  first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      pn = terminal->m_portName;
      if (terminal && (terminal->m_dir == d_input)) {
	if (first) {
	  file << "  // ports of submodule data" << endl;
	  file << "  wire [" << in_count << ":0] data$INPUTS;" << endl; 
	  file << "  wire [" << out_count-1 << ":0] data$OUTPUTS;" << endl << endl;
	  
	  file << "  // Input assignment" << endl;
	  file << "  assign data$INPUTS = {" << rstname;
	  first = 0;
	}
	file << ", " << pn;
      }
    }
  }
  if (first != 1)
    file << "};" << endl << endl;

  first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      pn = terminal->m_portName;
      if (terminal && (terminal->m_dir != d_input)) {
	if (first) {
	  first = 0;
	  file << "  // Output assignment" << endl;
	  file << "  assign {";
	}
	else
	  file << ", ";
	file << pn;
      }
    }
  }
  if (first != 1)
    file << "} = data$OUTPUTS;" << endl << endl;

  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      file << "  // submodule " << rei->getInterface() << "_data_in" << endl;
      file << "  SceMiInPipeProxyPut #(.WIDTH(" << rei->getWidth() << ")," << endl;
      file << "                        .paramFile(\"\")," << endl;
      file << "                        .transactorName(\"\")," << endl;
      file << "                        .portName(\"scemi_put_" << rei->getInterface()
	   << "_inpipe\")) " << rei->getInterface() << "_data_in(.CLK(CLK)," << endl;
      file << "                                                     .RST_N(RST_N)," << endl;
      file << "                                                     .DATA("
	   << rei->getInterface() << "_data_in$DATA)," << endl;
      file << "                                                     .DATA_EN("
	   << rei->getInterface() << "_data_in$DATA_EN)," << endl;
      file << "                                                     .DATA_RDY("
	   << rei->getInterface() << "_data_in$DATA_RDY));" << endl << endl;
    }
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      file << "  // submodule " << rei->getInterface() << "_data_out" << endl;
      file << "  SceMiOutPipeProxyGet #(.WIDTH(" << rei->getWidth() << ")," << endl;
      file << "                         .paramFile(\"\")," << endl;
      file << "                         .transactorName(\"\")," << endl;
      file << "                         .portName(\"scemi_get_" << rei->getInterface()
	   << "_outpipe\")) " << rei->getInterface() << "_data_out(.CLK(CLK)," << endl;
      file << "                                                      .RST_N(RST_N)," << endl;
      file << "                                                      .DATA_EN("
	   << rei->getInterface() << "_data_out$DATA_EN)," << endl;
      file << "                                                      .DATA("
	   << rei->getInterface() << "_data_out$DATA)," << endl;
      file << "                                                      .DATA_RDY("
	   << rei->getInterface() << "_data_out$DATA_RDY));" << endl << endl;
    }
  }

  // Individual ports
  first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      pn = terminal->m_portName;
      if (terminal && (terminal->m_dir == d_input)) {
	if (first) {
	  first = 0;
	  file << "  // submodule data proxy" << endl;
	  file << "  SceMiPipeProxyRS #(.WIDTH_IN(" << in_count+1 << ")," << endl;
	  file << "                   .WIDTH_OUT(" << out_count << ")," << endl;
	  file << "                   .paramFile(\"\")," << endl;
	  file << "                   .transactorName(\"\")," << endl;
	  file << "                   .portNameIn(\"scemi_lockstep_lock_inpipe\")," << endl;
	  file << "                   .portNameOut(\"scemi_lockstep_lock_outpipe\"))";
	  file << " data(.CLK(" << clkname << ")," << endl;
	  file << "                                                                     ";
	  file << ".RST_N(" << rstname << ")," << endl;
	  file << "                                                                     ";
	  file << ".INPUTS("
	       << "data$INPUTS)," << endl;
	  file << "                                                                     ";
	  file << ".OUTPUTS("
	       << "data$OUTPUTS));" << endl;
	}
      }
      else {
      }
    }
  }

  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      file << "  // submodule " << rei->getInterface() << "_data_in;" << endl;
      file << "  assign " << rei->getInterface() << "_data_in$DATA = {";
      first = 1;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
	if (terminal && (terminal->m_dir == d_input)) {
	  if (first) {
	    file << terminal->m_portName;
	    first = 0;
	  } else {
	    file << ", " << terminal->m_portName;
	  }
	}
      }
      if (first == 0)
	file << "};" << endl;
      if (string(rei->getEn()) != "")
	file << "  assign " << rei->getInterface() << "_data_in$DATA_EN = "
	     << rei->getEn() << ";" << endl;
      file << endl;
    }
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      file << "  // submodule " << rei->getInterface() << "_data_out;" << endl;
      if (string(rei->getEn()) != "")
	file << "  assign " << rei->getInterface() << "_data_out$DATA_EN = "
	     << rei->getEn() << ";" << endl;
      file << endl;
    }
  }


  file << "endmodule  // " << module->Name() << "_proxy" << endl;

  return 1;
}


int TestBenchGenerator::generateTclTbFile(VeriModule *module, const char *new_module_name,
					  const char *dirname, int tbtemplate)
{
  std::fstream file;
  string errstring;
  string lower_new_mod_name = new_module_name; 
  DIR *dp;
  RdyEnableInterface *rei;
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();
  string lstr;
  int first;

  string filename = module->Name();
  filename += ".pin";

  if (HdlUtils::init()->readPinFile(filename.c_str()) == 0)
    return 0;

  HdlUtils::init()->setRdyEnableIfcWidth(module);

  if (isupper(new_module_name[0]))
    lower_new_mod_name[0] = tolower(new_module_name[0]);
    
  if (dirname == NULL || dirname[0] == '\0')
    dirname = "tb";

  string realdir;
  realdir = dirname;
  size_t loc = realdir.find_first_of('/');
  if ((tbtemplate != 1) && (loc != 0)) { // template and not absolute path
    if (loc != string::npos)
      realdir = realdir.substr(0, loc+1) + "cpp";
  }
  filename = realdir;
  if (filename[filename.size()] != '/')
    filename += "/";
  filename += "TclTb.cpp";

  if ((dp = opendir(dirname)) == NULL) {
    if(recursive_mkdir(dirname, 0755) != 0) {
      errstring += "Error TestBenchGenerator::generateTclTbFile(): cannot open directory ";
      errstring += dirname;
      fprintf(stderr, "%s\n", errstring.c_str());
      return 0;
    }
  }
  closedir(dp);

  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateTclTbFile(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "// Copyright Bluespec Inc. 2010-2011" << endl;
  file << "// By: GenTestbench tool" << endl << endl;

  file << "#include <iostream>" << endl;
  file << "#include <stdexcept>" << endl;
  file << "#include <string>" << endl;
  file << "#include <cstdlib>" << endl;
  file << "#include <cstring>" << endl << endl;
  
  file << "#include <pthread.h>" << endl << endl;
  
  file << "// Bluespec's version -- $BLUESPECDIR/tcllib/include" << endl;
  file << "#include \"tcl.h\"" << endl << endl;
  
  file << "#include \"" << new_module_name << "Xactor.h\"" << endl;
  file << "#include \"SceMiHeaders.h\"" << endl << endl;
  
  file << "// Bluespec common code" << endl;
  file << "#include \"bsdebug_common.h\"" << endl;
  file << "#include \"designtcl.h\"" << endl << endl;
  file << "// TBXactor" << endl;
  file << "#include \"TBXactor.h\"" << endl << endl;
  
  file << "using namespace std;" << endl << endl;

  file << "// the package name and namespace for this extension" << endl;

  file << "#define PKG_NAME    \"BSDebug\"" << endl;
  file << "#define NS          \"bsdebug\"" << endl;
  file << "#define PKG_VERSION \"1.0\"" << endl << endl;

  file << "extern std::ofstream *rdback_log_file;" << endl;
  if (tbtemplate) {
    file << "extern int test_usertb();" << endl;
    file << "extern int reset_usertb();" << endl;
    file << "extern void destroy_usertb();" << endl;
  }
  file << "static unsigned int cmdlog = 0;" << endl << endl;

  file << "// static extension global data" << endl;
  file << "class SceMiGlobalData {" << endl;
  file << "public:" << endl;
  file << "  bool             m_initialized;" << endl;
  file << "  " << new_module_name << "Xactor *m_dutXactor;" << endl;
  file << "  TBXactor        *m_tbXactor;" << endl;
  file << "  SceMiServiceThread        * m_serviceThread;" << endl;
  file << "  RdBack::SimulationControl * m_simControl;" << endl;
  file << "  SimulationControl         * m_tbsimControl;" << endl;
  file << "  ProbesXactor              * m_probeControl;" << endl;
  file << "  Design                    * m_design;" << endl;
  file << "  VCDWriter                 * m_vcdWriter;" << endl;
  file << endl;
  file << "  // Simple initializer invoked when the extension is loaded" << endl;
  file << "  SceMiGlobalData()" << endl;
  file << "    : m_dutXactor(0)" << endl;
  file << "    , m_tbXactor(0)" << endl;
  file << "    , m_serviceThread(0)" << endl;
  file << "    , m_simControl(0)" << endl;
  file << "    , m_tbsimControl(0)" << endl;
  file << "    , m_design(0)" << endl;
  file << "    , m_vcdWriter(0)" << endl;
  file << "  {}" << endl;
  file << endl;
  file << "  ~SceMiGlobalData ()" << endl;
  file << "  {" << endl;
  file << "    if (m_initialized) {" << endl;
  file << "      destroy();" << endl;
  file << "    }" << endl;
  file << "  }" << endl;
  file << endl;
  file << "  // Initialization -- call from bsdebug::scemi init <param>" << endl;
  file << "  void init (const char *paramfile)" << endl;
  file << "  {" << endl;
  file << "    // Instantiate TestBench Xactor" << endl;
  file << "    m_tbXactor = TBXactor::getOrCreate();" << endl;
  file << endl;
  file << "    // Start all the services" << endl;
  file << "    m_tbXactor->startAllServices(paramfile);" << endl;
  file << "    m_simControl = m_tbXactor->getSimulationControl();" << endl;
  file << "    m_tbsimControl = m_tbXactor->getTbSimulationControl();" << endl;
  file << "    m_probeControl = m_tbXactor->getProbeControl();" << endl;
  file << "    m_design = m_tbXactor->getDesign();" << endl << endl;
  file << "    // Create the transactor for the dut" << endl;
  file << "    m_dutXactor = " << new_module_name 
       << "Xactor::init(m_tbXactor->getSceMi());" << endl;
  file << "    m_initialized = true;" << endl << endl;
  file << "  }" << endl;
  file << endl;
  file << "  // Destruction -- called from bsdebug::scemi delete" << endl;
  file << "  void destroy () {" << endl;
  file << "" << endl;
  file << "    m_initialized = false;" << endl << endl;
  if (tbtemplate) {
    file << "    destroy_usertb();" << endl << endl;
  }
  file << "    // Stop the simulation side" << endl;
  file << "    if (m_dutXactor) m_dutXactor->shutdown();" << endl << endl;
  file << "    m_dutXactor->destroy();" << endl;
  file << "    m_tbXactor->destroy();" << endl;
  file << "    fflush(stdout);" << endl;
  file << "  }" << endl;
  file << "} SceMiGlobal;" << endl;

  file << endl << endl;
  file << "// forward declarations of C functions which are called by tcl" << endl;
  file << "extern \"C\" {" << endl;
  file << endl;
  file << "  // Package intialization  and cleanup" << endl;
  file << "  extern int Bsdebug_Init (Tcl_Interp * interp);" << endl;
  file << "  extern int Bsdebug_Unload (Tcl_Interp * interp,  int flags);" << endl;
  file << "  extern void Bsdebug_ExitHandler (ClientData clientData);" << endl;
  file << endl;
  file << "  static int SceMi_Cmd(ClientData clientData," << endl;
  file << "                       Tcl_Interp *interp," << endl;
  file << "                       int objc," << endl;
  file << "                       Tcl_Obj *const objv[]);" << endl;
  file << "  static void SceMi_Cmd_Delete(ClientData clientData);" << endl;
  file << endl;
  file << endl;
  file << "  static int Dut_Cmd(ClientData clientData," << endl;
  file << "                     Tcl_Interp *interp," << endl;
  file << "                     int objc," << endl;
  file << "                     Tcl_Obj *const objv[]);" << endl;
  file << "  static void Dut_Cmd_Delete(ClientData clientData);" << endl;
  file << endl;
  file << "} // extern \"C\"" << endl;
  file << endl;
  file << endl;
  file << endl;
  file << "// Function called if/when the dynamic library is unloaded" << endl;
  file << "// Function name must match package library name" << endl;
  file << "int Bsdebug_Unload (Tcl_Interp * interp,  int flags)" << endl;
  file << "{" << endl;
  file << "  if (flags & TCL_UNLOAD_DETACH_FROM_PROCESS) {" << endl;
  file << "    SceMiGlobalData *pglobal = & SceMiGlobal;" << endl;
  file << "    pglobal->destroy();" << endl;
  file << "    Tcl_DeleteExitHandler ( Bsdebug_ExitHandler, &SceMiGlobal);" << endl;
  file << "  }" << endl;
  file << "  return TCL_OK;" << endl;
  file << "}" << endl;
  file << endl;
  file << "// Exit handler called during exit." << endl;
  file << "void Bsdebug_ExitHandler (ClientData clientData)" << endl;
  file << "{" << endl;
  file << "  SceMiGlobalData *pglobal = (SceMiGlobalData *) clientData;" << endl;
  file << "  pglobal->destroy();" << endl;
  file << "}" << endl;
  file << endl;
  file << "// Package initialization function -- called during package load/require" << endl;
  file << "// function name must match package library name" << endl;
  file << "int Bsdebug_Init(Tcl_Interp *interp)" << endl;
  file << "{" << endl;
  file << "  Tcl_Namespace* nsptr = NULL;" << endl;
  file << endl;
  file << "  try {" << endl;
  file << "    // register the exit handler" << endl;
  file << "    Tcl_CreateExitHandler( Bsdebug_ExitHandler, &SceMiGlobal);" << endl;
  file << endl;
  file << "    // Dynmaic binding of this extension to tcl" << endl;
  file << "    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {" << endl;
  file << "      return TCL_ERROR;" << endl;
  file << "    }" << endl;
  file << endl;
  file << "    // Create a namespace NS" << endl;
  file << "    nsptr = (Tcl_Namespace*) Tcl_CreateNamespace(interp, NS, NULL, NULL);" << endl;
  file << "    if (nsptr == NULL) {" << endl;
  file << "      return TCL_ERROR;" << endl;
  file << "    }" << endl;
  file << endl;
  file << "    // Provide the tcl package" << endl;
  file << "    if (Tcl_PkgProvide(interp, PKG_NAME, PKG_VERSION) != TCL_OK) {" << endl;
  file << "      return TCL_ERROR;" << endl;
  file << "    }" << endl;
  file << endl;
  file << "    // Register commands to this tcl extension" << endl;
  file << "    // A top-level tcl bsdebug::scemi command -- application specific boilerplate" << endl;
  file << "    Tcl_CreateObjCommand(interp," << endl;
  file << "			 NS \"::scemi\"," << endl;
  file << "			 (Tcl_ObjCmdProc *) SceMi_Cmd," << endl;
  file << "			 (ClientData) &(SceMiGlobal)," << endl;
  file << "			 (Tcl_CmdDeleteProc *) SceMi_Cmd_Delete);" << endl;
  file << "    Tcl_Export(interp, nsptr, \"scemi\", 0);" << endl;
  file << endl;
  file << "    // A top-level tcl dut command -- application specific" << endl;
  file << "    Tcl_CreateObjCommand(interp," << endl;
  file << "			 NS \"::dut\"," << endl;
  file << "			 (Tcl_ObjCmdProc *) Dut_Cmd," << endl;
  file << "			 (ClientData) &(SceMiGlobal.m_dutXactor)," << endl;
  file << "			 (Tcl_CmdDeleteProc *) Dut_Cmd_Delete);" << endl;
  file << "    Tcl_Export(interp, nsptr, \"dut\", 0);" << endl;
  file << endl;
  file << "    // Bluespec emulation control command" << endl;
  file << "    Tcl_CreateObjCommand(interp," << endl;
  file << "                         NS \"::emu\"," << endl;
  file << "                         (Tcl_ObjCmdProc *) Emu_Cmd," << endl;
  file << "                         (ClientData) &(SceMiGlobal.m_tbsimControl)," << endl;
  file << "                         (Tcl_CmdDeleteProc *) Emu_Cmd_Delete);" << endl;
  file << "    Tcl_Export(interp, nsptr, \"emu\", 0);" << endl;
  file << endl;
  file << "    // Bluespec emulation control command with readback" << endl;
  file << "    Tcl_CreateObjCommand(interp," << endl;
  file << "                         NS \"::rdbk\"," << endl;
  file << "                         (Tcl_ObjCmdProc *) RdBk_Cmd," << endl;
  file << "                         (ClientData) &(SceMiGlobal.m_simControl)," << endl;
  file << "                         (Tcl_CmdDeleteProc *) RdBk_Cmd_Delete);" << endl;
  file << "    Tcl_Export(interp, nsptr, \"rdbk\", 0);" << endl;
  file << endl;
  file << "    // Bluespec probe capture command" << endl;
  file << "    Tcl_CreateObjCommand(interp," << endl;
  file << "                         NS \"::probe\"," << endl;
  file << "                         (Tcl_ObjCmdProc *) Capture_Cmd," << endl;
  file << "                         (ClientData) &(SceMiGlobal.m_probeControl)," << endl;
  file << "                         (Tcl_CmdDeleteProc *) Capture_Cmd_Delete);" << endl;
  file << "    Tcl_Export(interp, nsptr, \"probe\", 0);" << endl;
  file << endl;
  file << "    // Bluespec probe netlist command" << endl;
  file << "    Tcl_CreateObjCommand(interp," << endl;
  file << "                         NS \"::netlist\"," << endl;
  file << "                         (Tcl_ObjCmdProc *) llbits_netlist_cmd," << endl;
  file << "                         (ClientData) &(SceMiGlobal.m_design)," << endl;
  file << "                         (Tcl_CmdDeleteProc *) llbits_netlist_cmd_delete);" << endl;
  file << "    Tcl_Export(interp, nsptr, \"netlist\", 0);" << endl;
  file << endl;
  file << "    // Other command can go here" << endl;
  file << endl;
  file << "  } catch (const exception & error) {" << endl;
  file << "    Tcl_AppendResult(interp, error.what()" << endl;
  file << "                     ,\"\\nCould not initialize bsdebug tcl package\"" << endl;
  file << "                     ,(char *) NULL);" << endl;
  file << "    return TCL_ERROR;" << endl;
  file << "  }" << endl;

  file << "  return TCL_OK;" << endl;
  file << "}" << endl;

  file << endl << endl;
  file << "// implementation of the scemi command ensemble" << endl;
  file << "// at the tcl level, the command will be" << endl;
  file << "// bsdebug::scemi init <params file>" << endl;
  file << "// bsdebug::scemi delete" << endl;
  file << "static int SceMi_Cmd(ClientData clientData,    	//  &(GlobalXactor)," << endl;
  file << "                     Tcl_Interp *interp,      	// Current interpreter" << endl;
  file << "                     int objc,               	// Number of arguments" << endl;
  file << "                     Tcl_Obj *const objv[]   	// Argument strings" << endl;
  file << "         )" << endl;
  file << "{" << endl;
  file << "  // Command table" << endl;
  file << "  enum ScemiCmds { scemi_init, scemi_delete };" << endl;
  file << "  static const cmd_struct cmds_str[] = {" << endl;
  file << "    {\"init\",		scemi_init,		\"<params file>\"}" << endl;
  file << "    ,{\"delete\",		scemi_delete,		\"\"}" << endl;
  file << "    ,{0}                        // MUST BE LAST" << endl;
  file << "  };" << endl;
  file << endl;
  file << "  // Cast client data to proper type" << endl;
  file << "  SceMiGlobalData *pglobal = (SceMiGlobalData *) clientData;" << endl;
  file << endl;
  file << "  // Extract sub command" << endl;
  file << "  ScemiCmds command;" << endl;
  file << "  int index;" << endl;
  file << "  if (objc == 1) goto wrongArgs;" << endl;
  file << "  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], cmds_str, sizeof(cmd_struct)," << endl;
  file << "                                           \"command\", 0, &index ) ) {" << endl;
  file << "    return TCL_ERROR;" << endl;
  file << "  }" << endl;
  file << endl;
  file << endl;
  file << "  command = (enum ScemiCmds) cmds_str[index].enumcode;" << endl;
  file << "  switch (command) {" << endl;
  file << "    case scemi_init:" << endl;
  file << "    {" << endl;
  file << "	if (objc != 3) goto wrongArgs;" << endl;
  file << "	char *paramfile = Tcl_GetString(objv[2]);" << endl;
  file << "	try {" << endl;
  file << "          pglobal->init(paramfile);" << endl;
  file << "	} catch (const exception & error) {" << endl;
  file << "          Tcl_AppendResult(interp, error.what()" << endl;
  file << "                           ,\"\\nCould not initialize emulation\"" << endl;
  file << "                           ,(char *) NULL);" << endl;
  file << "	  return TCL_ERROR;" << endl;
  file << "	}" << endl;
  file << "      break;" << endl;
  file << "    }" << endl;
  file << "    case scemi_delete:" << endl;
  file << "        pglobal->destroy();" << endl;
  file << "        break;" << endl;
  file << "  }" << endl;
  file << "  return TCL_OK;" << endl;
  file << endl;
  file << "wrongArgs:" << endl;
  file << "  dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));" << endl;
  file << "  return TCL_ERROR;" << endl;
  file << "}" << endl;
  file << endl;
  file << "static void SceMi_Cmd_Delete(ClientData clientData)" << endl;
  file << "{" << endl;
  file << "}" << endl;
  file << endl;
  file << endl;
  file << endl;
  file << "// implementation of the Dut command ensemble" << endl;
  file << "// dut request <int>" << endl;
  file << "// dut response" << endl;
  file << "static int Dut_Cmd(ClientData clientData,    	// &(GlobalXactor.m_dutXactor)" << endl;
  file << "                   Tcl_Interp *interp,     	// Current interpreter" << endl;
  file << "                   int objc,               	// Number of arguments" << endl;
  file << "                   Tcl_Obj *const objv[]   	// Argument strings" << endl;
  file << "         )" << endl;
  file << "{" << endl;
  // Generate all the data type
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  
  ModuleTerminal *terminal;
  
  //printf("here %s\n", module->Name());
  HdlUtils::createModuleTerminalList(module, mtlist);
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  //cout << "Map pointer2 " << (void*)map << endl;
  std::list<netHandle>::iterator netItr;
  string s, m;
  for (netItr = SceMiControlSignals.begin(); netItr != SceMiControlSignals.end(); netItr++) {

    s = *netItr;

    m = "";
    size_t pos = s.find(':');
    if (pos != string::npos) {
      m = s.substr(0, pos);
      s = s.substr(pos+1);
      (*map)[s] = m;
    }
    (*map)[s] = m;
  }

  // Input/Output
  int input_counts = 0;
  int output_counts = 0;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    rei = RdyEnableIfc[lstr];
    if (rei) {
      if ((lstr == string(rei->getRdy())) ||
	  (lstr == rei->getEn()))
	continue;
      if ((*map)[terminal->m_portName] == "") {
	file << "  BitT<" << terminal->m_width << "> "
	     << lstr << "_bits;" << endl; 
      }
    } else {
      if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
      if ((*map)[terminal->m_portName] == "") {
	file << "  BitT<" << terminal->m_width << "> "
	     << lstr << "_bits;" << endl; 
	if (terminal->m_dir == d_input)
	  input_counts++;
	else
	  output_counts++;
      }
    }
  }
  
  file << endl;
  file << "  // Command table" << endl;
  int numrei = 0;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
    numrei++;
  }
  first = 1;
  if (numrei > 0 || input_counts > 0 || output_counts > 0) {
    file << "  enum DutCmds {";
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (first == 0)
      file << ", "; 
    if (rei->isPut() || rei->isPipePut()) {
      file << "Dut_Request_" << rei->getInterface();
    } else {
      file << "Dut_Response_" << rei->getInterface();
    }
    first = 0;
  }
  if (input_counts > 0) {
    if (first == 0)
      file << ", "; 
    first = 0;
    file << "Dut_Request";
  }
  if (output_counts > 0) {
    if (first == 0)
      file << ", "; 
    first = 0;
    file << "Dut_Response";
  }
  if (first == 0)
    file << ", ";
  file << "Dut_Test, Dut_Reset, Dut_CmdLog};" << endl;
  file << "  static const cmd_struct cmds_str[] = {" << endl;

  first = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
    numrei++;
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (first == 0)
      file << "    ,{\""; 
    else
      file << "    {\"";
    if (rei->isPut() || rei->isPipePut()) {
      file << "request_" << rei->getInterface() << "\", Dut_Request_"
	   << rei->getInterface() << ", " << "\"int";
      for (int i=1; i<rei->numData(); i++) {
	file << ", int";
      }
      file << "\"}" << endl;
    } else {
      file << "response_" << rei->getInterface() << "\", Dut_Response_"
	   << rei->getInterface() << ", \"\"}" << endl;
    }
    first = 0;
  }
  if (input_counts > 0) {
    if (first == 0)
      file << "    ,"; 
    else
      file << "    ";
    first = 0;
    file << "{\"request\", Dut_Request, \"int";
    for (int i=1; i<input_counts; i++) {
	file << ", int";
    }
    file << "\"}" << endl;
  }
  if (output_counts > 0) {
    if (first == 0)
      file << "    ,"; 
    else
      file << "    ";
    first = 0;
    file << "{\"response\", Dut_Response, \"\"}" << endl;
  }
  if (tbtemplate) {
    if (first == 0)
      file << "    ,"; 
    else
      file << "    ";
    file << "{\"test\", Dut_Test, \"\"}" << endl;
    file << "    ,{\"reset\", Dut_Reset, \"\"}" << endl;
    first = 0;
  }
  if (first == 0)
    file << "    ,"; 
  file << "{\"cmdlog\",	Dut_CmdLog, \"val\"}" << endl;

  file << "    ,{0}                        // MUST BE LAST" << endl;
  file << "  };" << endl;
  file << endl;
  file << "  // Check that client data has been set" << endl;
  file << "  " << new_module_name << "Xactor *dutx = *(" << new_module_name << "Xactor **) clientData;" << endl;
  file << "  if (dutx == 0) {" << endl;
  file << "    Tcl_SetResult (interp, (char *) \"Cannot use dut command before emulation initialization\", TCL_STATIC );" << endl;
  file << "    return TCL_ERROR;" << endl;
  file << "  }" << endl;
  file << endl;
  file << "  // Extract sub command" << endl;
  file << "  DutCmds command;" << endl;
  file << "  int index;" << endl;
  file << "  if (objc == 1) goto wrongArgs;" << endl;
  file << "  if (TCL_OK != Tcl_GetIndexFromObjStruct (interp, objv[1], cmds_str, sizeof(cmd_struct)," << endl;
  file << "                                           \"command\", 0, &index ) ) {" << endl;
  file << "    return TCL_ERROR;" << endl;
  file << "  }" << endl;
  file << "  command = (enum DutCmds) cmds_str[index].enumcode;" << endl;
  file << endl;

  int reiobjs = 2;
  std::list<std::string>::iterator stItr;
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
      reiobjs += rei->numData();
    }
  }
  int mtobjs = 2;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
      mtobjs++;
    }
  }
      
  // Input/Output
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
	terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
	if (terminal && (terminal->m_dir == d_input)) {
	  lstr = *stItr;
	  file << "  long " << lstr << ";" << endl;
	}
      }
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "")
      file << "  long " << terminal->m_portName << ";" << endl;
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  file << "  unsigned int off;" << endl;
  file << "  bool sent;" << endl;
  file << endl;
  file << "  switch (command) {" << endl;

  int i;
  string params;
  string params2;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    i = 2;
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isPut() || rei->isPipePut()) {

      file << "    case Dut_Request_" << rei->getInterface() << ":" << endl;
      file << "      {" << endl;
      file << "        if (objc != " << rei->numData()+i << ") goto wrongArgs;" << endl << endl;

      first = 1;
      params2 = "";

      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
        terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
        lstr = *stItr;
	
	file << "        if (Tcl_GetLongFromObj(interp, objv[" << i++ << "], &"
	     << lstr << ") != TCL_OK) {" << endl;
	file << "          return TCL_ERROR;" << endl;
	file << "        }" << endl;
	file << "        " << lstr << "_bits = " << lstr << ";" << endl << endl;
	if (first) {
	  params2 = " << ";
	  first = 0;
	} else {
	  params2 += " << \" \" << ";
	}
	params2 += lstr;
      }

      file << endl;
      file << "        sent = dutx->" << "put_" << rei->getInterface() << "(";
      composeParams(rei->getInterface(), mtlist, params);
      file << params << ");" << endl << endl;
      
      file << "        Tcl_Obj *r = Tcl_NewBooleanObj( sent );" << endl;
      file << "        Tcl_SetObjResult(interp, r );" << endl << endl;
      file << "        // Log this function call" << endl;
      file << "        if (cmdlog)" << endl;
      file << "          {" << endl;
      file << "            (*rdback_log_file) << \"bsdebug::dut request_" << rei->getInterface()
	   << " \""
	   << params2 << " << std::endl;" << endl;
      file << "          }" << endl;
      file << "        break;" << endl;
      file << "      }" << endl;
    }
    else {

      file << "    case Dut_Response_" << rei->getInterface() << ":" << endl;
      file << "      {" << endl;
      file << "        Tcl_Obj *r, *res;" << endl;
      file << "        if (objc != 2" << ") goto wrongArgs;" << endl;
      file << "        bool gotit;" << endl;
      file << endl;
      params = "";
      composeParams(rei->getInterface(), mtlist, params);
      file << "        gotit = dutx->" << "get_" << rei->getInterface() << "(";
      file << params << ");" << endl << endl;
      
      file << "        if (gotit) {" << endl;
      file << "          // Convert result to a object and return" << endl;

      first = 1;	
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
        terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
        lstr = *stItr;
        if (terminal && (terminal->m_dir == d_output)) {

	  if (first) {
	    file << "          long " << lstr << ";" << endl;
	    file << "          r = Tcl_NewIntObj(1);" << endl;
	    file << "          Tcl_SetObjResult(interp, r );" << endl << endl;
	    file << "          " << lstr << " = " << lstr << "_bits;" << endl;
	    file << "          res = Tcl_NewLongObj(" << lstr << ");" << endl;
	    file << "          Tcl_ListObjAppendElement(interp, r, res);" << endl << endl;
	    first = 0;
	  } else {
	    file << "          long " << lstr << ";" << endl;
	    file << "          " << lstr << " = " << lstr << "_bits;" << endl;
	    file << "          res = Tcl_NewLongObj(" << lstr << ");" << endl;
	    file << "          Tcl_ListObjAppendElement(interp, r, res);" << endl << endl;
	  }
        }
      }
      file << "        } else {" << endl;
      file << "          r = Tcl_NewIntObj(0);" << endl;
      file << "          Tcl_SetObjResult(interp, r );" << endl;
      file << "        }" << endl << endl;
      file << "        // Log this function call" << endl;
      file << "        if (cmdlog)" << endl;
      file << "          {" << endl;
      file << "            (*rdback_log_file) << \"waitForResponse \\\"bsdebug::dut response_" << rei->getInterface() << "\\\" 100\" << std::endl;" << endl;
      file << "          }" << endl;
      file << "        break;" << endl;
      file << "      }" << endl;
    }
  }


  if (input_counts > 0) {
    file << "    case Dut_Request:  // request x y wrt data" << endl;
    file << "      {" << endl;
    file << "        if (objc != " << mtobjs << ") goto wrongArgs;" << endl << endl;
    i = 2;
    params = "";
    params2 = "";
    first = 1;
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
      if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
	file << "        if (Tcl_GetLongFromObj(interp, objv[" << i << "], &"
	     << terminal->m_portName << ") != TCL_OK) {" << endl;
	file << "          return TCL_ERROR;" << endl;
	file << "        }" << endl;
	file << "        " << terminal->m_portName << "_bits = " << terminal->m_portName 
	     << ";" << endl;
	if (first) {
	  params2 = " << ";
	  first = 0;
	} else {
	  params2 += " << \\\" \\\" << ";
	}
	params2 += terminal->m_portName;
	file << endl;
	i++;
      }
    }
    first = 1;
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
      if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
	if (first) {
	  file << "        sent = dutx->" << "send_" << terminal->m_portName << "(";
	  file << terminal->m_portName << "_bits);" << endl;
	  first = 0;
	} else {
	  file << "        sent &= dutx->" << "send_" << terminal->m_portName << "(";
	  file << terminal->m_portName << "_bits);" << endl;
	  first = 0;
	}
      }
    }

    file << "        Tcl_Obj *r = Tcl_NewBooleanObj( sent );" << endl;
    file << "        Tcl_SetObjResult(interp, r );" << endl;
    file << "        // Log this function call" << endl;
    file << "        if (cmdlog)" << endl;
    file << "          {" << endl;
    file << "            (*rdback_log_file) << \"bsdebug::dut request "
	 << params2 << "\" << std::endl;" << endl;
    file << "          }" << endl;
    file << "        break;" << endl;
    file << "      }" << endl;
  }

  if (output_counts > 0) {
    file << "    case Dut_Response:" << endl;
    file << "      {" << endl;
    file << "        Tcl_Obj *r, *res;" << endl;
    first = 1;
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
      if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
	if (first) {
	  file << "        long " << lstr;
	  first = 0;
	} else {
	  file << ", " << lstr;
	}
      }
    }
    if (first == 0) {
      file << ";" << endl;
    }
    file << "        if (objc != 2) goto wrongArgs;" << endl;
    file << "        bool gotit;" << endl;

    first = 1;
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
      if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
	if (first) {
	  file << "        gotit = dutx->" << "receive_" << terminal->m_portName << "(";
	  first = 0;
	} else {
	  file << "        gotit |= dutx->" << "receive_" << terminal->m_portName << "(";
	}
	file << terminal->m_portName << "_bits);" << endl;
      }
    }
    
    file << "        if (gotit) {" << endl;
    file << "          // Convert result to a object and return" << endl;
    for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
      
      rei = reiItr->second;
      if (rei == NULL) continue;
      rei->setUsed(false);
    }
    first = 1;	
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      terminal = &(*mtItr);
      lstr = terminal->m_portName;
      if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
      if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
	if (first) {
	  file << "          r = Tcl_NewIntObj(1);" << endl;
	  file << "          Tcl_SetObjResult(interp, r );" << endl << endl;
	  file << "          " << lstr << " = " << lstr << "_bits;" << endl;
	  file << "          res = Tcl_NewLongObj(" << lstr << ");" << endl;
	  file << "          Tcl_ListObjAppendElement(interp, r, res);" << endl;
	  first = 0;
	} else {
	  file << "          " << lstr << " = " << lstr << "_bits;" << endl;
	  file << "          res = Tcl_NewLongObj(" << lstr << ");" << endl;
	  file << "          Tcl_ListObjAppendElement(interp, r, res);" << endl;
	}
      }
    }
    file << "        } else {" << endl;
    file << "          r = Tcl_NewIntObj(0);" << endl;
    file << "          Tcl_SetObjResult(interp, r );" << endl;
    file << "        }" << endl << endl;

    file << "        // Log this function call" << endl;
    file << "        if (cmdlog)" << endl;
    file << "          {" << endl;
    file << "            (*rdback_log_file) << \"waitForResponse \\\"bsdebug::dut response\\\" 100\" << std::endl;" << endl;
    file << "          }" << endl;
    file << "        break;" << endl;
    file << "      }" << endl;
  }

  if (tbtemplate) {
    file << "   case Dut_Test:" << endl;
    file << "     {" << endl;
    file << "       //if (objc != 2) goto wrongArgs;" << endl << endl;;
    file << "       // **Example how you receive a parameter from the caller below**" << endl;
    file << "       //std::string filename;" << endl;
    file << "       //filename = Tcl_GetString(objv[2]);" << endl << endl;
    file << "       // Redirect cout to our stringstream buffer or any other ostream" << endl;
    file << "       setvbuf(stdout, NULL, _IONBF, 0);" << endl;
    file << "       std::stringstream buffer;" << endl;
    file << "       std::streambuf *sbuf = std::cout.rdbuf();" << endl;
    file << "       test_usertb(); // ** This is where the call to customed function is made.**" << endl;
    file << "       // When done redirect cout to its old self" << endl;
    file << "       std::cout.rdbuf(sbuf);" << endl << endl;
    file << "       Tcl_Obj *r = Tcl_NewStringObj(buffer.str().c_str(), buffer.str().size());" << endl;
    file << "       Tcl_SetObjResult(interp, r);" << endl;
    file << "       break;" << endl;
    file << "     }" << endl;
    file << "   case Dut_Reset:" << endl;
    file << "     {" << endl;
    file << "       //if (objc != 2) goto wrongArgs;" << endl << endl;;
    file << "       // **Example how you receive a parameter from the caller below**" << endl;
    file << "       //std::string filename;" << endl;
    file << "       //filename = Tcl_GetString(objv[2]);" << endl << endl;
    file << "       // Redirect cout to our stringstream buffer or any other ostream" << endl;
    file << "       setvbuf(stdout, NULL, _IONBF, 0);" << endl;
    file << "       std::stringstream buffer;" << endl;
    file << "       std::streambuf *sbuf = std::cout.rdbuf();" << endl;
    file << "       reset_usertb(); // ** This is where the call to customed function is made.**" << endl;
    file << "       // When done redirect cout to its old self" << endl;
    file << "       std::cout.rdbuf(sbuf);" << endl << endl;
    file << "       Tcl_Obj *r = Tcl_NewStringObj(buffer.str().c_str(), buffer.str().size());" << endl;
    file << "       Tcl_SetObjResult(interp, r);" << endl;
    file << "       break;" << endl;
    file << "     }" << endl;
  }
  file << "    case Dut_CmdLog:" << endl;
  file << "      {" << endl;
  file << "        std::string val;" << endl;
  file << "        if (objc >= 3)" << endl;
  file << "	     {" << endl;
  file << "	       val = Tcl_GetString(objv[2]);" << endl;
  file << "	     }" << endl;
  file << endl;
  file << "	   if (val == \"on\")" << endl;
  file << "	     cmdlog = 1;" << endl;
  file << "      }" << endl;
  file << "  }" << endl;
  file << "  return TCL_OK;" << endl;
  file << endl;
  file << "wrongArgs:" << endl;
  file << "  dumpArguments (interp, cmds_str, Tcl_GetString(objv[0]));" << endl;
  file << "  return TCL_ERROR;" << endl;
  file << "}" << endl;

  file << "static void Dut_Cmd_Delete(ClientData clientData)" << endl;
  file << "{" << endl;
  file << "}" << endl;

  file.close();

  return 1;
}

void TestBenchGenerator::composeParams(const char *interface, ModuleTerminalList &mtlist,
				       std::string &params)
{
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();
  std::list<std::string>::iterator stItr;
  std::string lstr;
  ModuleTerminal *terminal;
  RdyEnableInterface *rei;
  int first = 1;

  params = "";
  rei = RdyEnableIfc[interface];
  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
    if (terminal) {
      lstr = *stItr;
      if (first)
	first = 0;
      else {
	params += ", ";
      }
      params += lstr + "_bits";
    }
  }
}

int TestBenchGenerator::generateCapiTemplate(VeriModule *module, const char *new_module_name,
					     const char *dirname)
{
  std::fstream file;
  string errstring;
  string lower_new_mod_name = new_module_name; 
  DIR *dp;
  string lstr;
  ModuleTerminalIterator mtItr;

  if (isupper(new_module_name[0]))
    lower_new_mod_name[0] = tolower(new_module_name[0]);
    
  if (dirname == NULL || dirname[0] == '\0')
    dirname = "tb";

  string realdir, filename;
  realdir = dirname;
  //size_t loc = realdir.find_first_of('/');
  filename = realdir;
  if (filename[filename.size()] != '/')
    filename += "/";
  filename += "usertb.cpp";

  // Do usertb.h
  if ((dp = opendir(dirname)) == NULL) {
    if(recursive_mkdir(dirname, 0755) != 0) {
      errstring += "Error HdlUtils::generateCapiTemplate(): cannot open directory ";
      errstring += dirname;
      fprintf(stderr, "%s\n", errstring.c_str());
      return 0;
    }
  }
  closedir(dp);

  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateCapiTemplate(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "// Copyright Bluespec Inc. 2010-2011" << endl;
  file << "// By: GenTestbench tool" << endl << endl;

  file << "#include <iostream>" << endl;
  file << "#include \"capi.h\"" << endl;
  file << "#include \"usertb.h\"" << endl << endl;
  file << "////////////////////////////////////////////////////////////////////////////////" << endl;
  file << "//" << endl;
  file << "//  This is a basic template for a C++ user testbench" << endl;
  file << "//" << endl;
  file << "//  Please refer to the capi.h file in $project_dir/build/cpp for the C API." << endl;
  file << "//  The capi.h file documents all of the C API calls for Semu, including" << endl;
  file << "//  the interface-customized calls to communicate to your DUT." << endl;
  file << "//" << endl;
  file << "////////////////////////////////////////////////////////////////////////////////" << endl << endl;
  file << "int do_test() {" << endl << endl;

  file << "  //////////////////////////////////////////////////////////////////////////////" << endl;
  file << "  //////////////////////////////////////////////////////////////////////////////" << endl;
  file << "  //" << endl;
  file << "  //  If you do NOT plan to actively control the DUT clock from within your C" << endl;
  file << "  //  testbench, then you should UNcomment the following lines of code.  When" << endl;
  file << "  //  this code is active, then the DUT clock will start running as soon as" << endl;
  file << "  //  the user clicks 'Run' in the Emulation Control Panel." << endl;
  file << "  //" << endl;
  file << "  //  If, instead, you plan to control the clock (e.g. single-step the clock)" << endl;
  file << "  //  from within your C testbench, then leave it 'as is' (that is, commented" << endl;
  file << "  //  out)." << endl;
  file << "  //" << endl;
  file << "  //////////////////////////////////////////////////////////////////////////////" << endl;
  file << "  //////////////////////////////////////////////////////////////////////////////" << endl << endl;

  file << "/*" << endl;
  file << "  if (!semu_start_controlled_clock()) {" << endl;
  file << "    cerr << \"Something wrong, cannot start controlled clock\" << endl;" << endl;
  file << "    return 0;" << endl;
  file << "  }" << endl;
  file << "*/" << endl << endl;

  file << "  //////////////////////////////////////////////////////////////////////////////" << endl;
  file << "  //////////////////////////////////////////////////////////////////////////////" << endl << endl << endl;
  file << "  //  ************    Here's where you insert testbench code   *************" << endl << endl << endl;

  file << "  //////////////////////////////////////////////////////////////////////////////" << endl;
  file << "  //////////////////////////////////////////////////////////////////////////////" << endl << endl;
  file << "  return 1;" << endl;
  file << "}" << endl << endl;

  file.close();

  return 1;
}

int TestBenchGenerator::generateSendHeaders(std::fstream &file, const char *type,
					    const char *portname, const char *prefix)
{
  file << "  bool " << prefix << "send_" << portname << "(";
  file << type << " &" << portname;
  file << "_data);" << endl;
  file << "  bool " << prefix << "sendB_" << portname << "(";
  file << type << " &" << portname;
  file << "_data);" << endl;
  file << "  bool " << prefix << "sendBAck_" << portname << "(";
  file << type << " &" << portname;
  file << "_data);" << endl;
  if (HdlUtils::isPortPipe(portname)) {

    file << "  bool " << prefix << "vector_send_" << portname << "(std::vector<";
    file << type << " > &" << portname;
    file << "_data);" << endl;

    file << "  bool " << prefix << "vector_sendB_" << portname << "(std::vector<";
    file << type << " > &" << portname;
    file << "_data);" << endl;

    file << "  bool " << prefix << "vector_sendAck_" << portname << "(std::vector<";
    file << type << " > &" << portname;
    file << "_data);" << endl;
  }
  file << "  bool " << prefix << "set_emulation_type_" << portname
       << "(BitT<1> &t);" << endl;
  file << endl;

  return 1;
}

int TestBenchGenerator::generateSendHeaders2(std::fstream &file, const char *type,
					     const char *portname, const char *prefix)
{
  file << "  bool " << prefix << "send_" << portname << "(";
  file << type << " &" << portname;
  file << "_data);" << endl;
  file << "  bool " << prefix << "sendB_" << portname << "(";
  file << type << " &" << portname;
  file << "_data);" << endl;
  file << "  bool " << prefix << "sendBAck_" << portname << "(";
  file << type << " &" << portname;
  file << "_data);" << endl;
  if (HdlUtils::isPortPipe(portname)) {

    file << "  bool " << prefix << "vector_send_" << portname << "(std::vector<";
    file << type << " > &" << portname;
    file << "_data);" << endl;

    file << "  bool " << prefix << "vector_sendB_" << portname << "(std::vector<";
    file << type << " > &" << portname;
    file << "_data);" << endl;

    file << "  bool " << prefix << "vector_sendAck_" << portname << "(std::vector<";
    file << type << " > &" << portname;
    file << "_data);" << endl;
  }
  file << "  bool " << prefix << "set_emulation_type_" << portname
       << "(EmulationPortType t);" << endl;
  file << endl;

  return 1;
}

int TestBenchGenerator::generateReiSendHeaders(std::fstream &file, RdyEnableInterface *rei,
					       ModuleTerminalList &mtlist, const char *prefix)
{
  std::list<std::string>::iterator stItr;
  string lstr, params, pipeparams;
  ModuleTerminal *terminal;
  int first = 1;
  ostringstream convert;

  params = pipeparams = "";
  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
    if (terminal && (terminal->m_dir == d_input)) {
      lstr = *stItr;
      if (first == 1)
	first = 0;
      else {
	params += ", ";
	pipeparams += ", ";
      }

      convert.str("");
      convert << terminal->m_width;
      params += string("BitT<") + convert.str() + "> &" + lstr;
      pipeparams += string("std::vector<BitT<") + convert.str() + "> > &" + lstr;
    }
  }
  file << "  bool " << prefix << "put_" << rei->getInterface() << "(";
  file << params;
  file << ");" << endl;
  file << "  bool " << prefix << "putB_" << rei->getInterface() << "(";
  file << params;
  file << ");" << endl;

  if (rei->isPipePut()) {

    file << "  bool " << prefix << "vector_put_" << rei->getInterface() << "(";
    file << pipeparams;
    file << ");" << endl;

    file << "  bool " << prefix << "vector_putB_" << rei->getInterface() << "(";
    file << pipeparams;
    file << ");" << endl;

    file << "  bool " << prefix << "vector_putAck_" << rei->getInterface() << "(";
    file << pipeparams;
    file << ");" << endl;
  }
  file << endl;

  return 1;
}

int TestBenchGenerator::generateGetHeaders(std::fstream &file, const char *type,
					   const char *portname, const char *prefix)
{
  file << "  bool " << prefix << "receive_" << portname << "(";
  file << type << " &" <<  portname 
       << "_data";
  file << ");" << endl;
  file << "  bool " << prefix << "receiveB_" << portname << "(";
  file << type << " &" <<  portname 
       << "_data";
  file << ");" << endl;
  if (false) {
    file << "  bool " << prefix << "receive_" << portname << "(";
    file << type << " &" <<  portname 
	 << "_data, SceMiU64 &timestamp";
    file << ");" << endl;
    file << "  bool " << prefix << "receiveB_" << portname << "(";
    file << type << " &" <<  portname 
	 << "_data, SceMiU64 &timestamp";
    file << ");" << endl;
  }
  if (HdlUtils::isPortPipe(portname)) {

    file << "  unsigned " << prefix << "vector_receive_" << portname << "(std::vector<";
    file << type << " > &" <<  portname 
	 << "_data, unsigned minReturned=0, unsigned maxReturned=0";
    file << ");" << endl;
  }

  file << "  bool " << prefix << "set_emulation_type_" << portname
       << "(BitT<1> &t);" << endl;
  file << endl;

  return 1;
}

int TestBenchGenerator::generateGetHeaders2(std::fstream &file, const char *type,
					    const char *portname, int pipe, const char *prefix)
{
  file << "  bool " << prefix << "receive_" << portname << "(";
  file << type << " &" <<  portname 
       << "_data";
  file << ");" << endl;
  file << "  bool " << prefix << "receiveB_" << portname << "(";
  file << type << " &" <<  portname 
       << "_data";
  file << ");" << endl;

  if (!pipe && false) {
    file << "  bool " << prefix << "receive_time_" << portname << "(";
    file << type << " &" <<  portname 
	 << "_data, SceMiU64 &timestamp";
    file << ");" << endl;
    file << "  bool " << prefix << "receiveB_time_" << portname << "(";
    file << type << " &" <<  portname 
	 << "_data, SceMiU64 &timestamp";
    file << ");" << endl;
  }

  if (pipe) {

    file << "  unsigned " << prefix << "vector_receive_" << portname << "(std::vector<";
    file << type << " > &" <<  portname 
	 << "_data, unsigned minReturned=0, unsigned maxReturned=0";
    file << ");" << endl;
  }

  file << "  bool " << prefix << "set_emulation_type_" << portname
       << "(EmulationPortType t);" << endl;
  file << endl;

  return 1;
}

int TestBenchGenerator::generateReiGetHeaders(std::fstream &file, RdyEnableInterface *rei,
					      ModuleTerminalList &mtlist, int pipe,
					      const char *prefix)
{
  std::list<std::string>::iterator stItr;
  string lstr, params, pipeparams;
  ModuleTerminal *terminal;
  int first = 1;
  ostringstream convert;

  pipeparams = params = "";
  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
    if (terminal && (terminal->m_dir == d_output)) {
      lstr = *stItr;
      if (first == 1)
	first = 0;
      else {
	params += ", ";
	pipeparams += ", ";
      }
      convert.str("");
      convert << terminal->m_width;
      params += string("BitT<") + convert.str() + "> &" + lstr;
      pipeparams += string("std::vector<BitT<") + convert.str() + "> > &" + lstr;
    }
  }
  file << "  bool " << prefix << "get_" << rei->getInterface() << "(";
  file << params;
  file << ");" << endl;
  file << "  bool " << prefix << "getB_" << rei->getInterface() << "(";
  file << params;
  file << ");" << endl;
  file << endl;

  if (!pipe && false) {
    file << "  bool " << prefix << "get_time_" << rei->getInterface() << "(";
    file << params;
    file << ", SceMiU64 &timestamp);" << endl;
    file << "  bool " << prefix << "getB_time_" << rei->getInterface() << "(";
    file << params;
    file << ", SceMiU64 &timestamp);" << endl;
    file << endl;

  } else {

    file << "  unsigned " << prefix << "vector_get_" << rei->getInterface() << "(";
    file << pipeparams;
    file << ", unsigned minReturned=0, unsigned maxReturned=0);" << endl;
    file << endl;
  }

  return 1;
}

int TestBenchGenerator::generateDutXactorContructor(std::fstream &file, ModuleTerminalList &mtlist)
{
  RdyEnableInterface *rei;
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  string lstr;
  std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  int first = 0;
  
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isPipePut()) {
      if (first) {
	file << "  : m_" << rei->getInterface() << " (\"\", \"scemi_put_"
	     << rei->getInterface() << "_inpipe\", XactorAdapter::InPipe)" << endl;
	first = 0;
      } else {
	file << "  , m_" << rei->getInterface() << " (\"\", \"scemi_put_"
	     << rei->getInterface() << "_inpipe\", XactorAdapter::InPipe)" << endl;
      }
    } else if (rei->isPut()) {
      if (first) {
	file << "  : m_" << rei->getInterface() << " (\"\", \"scemi_put_"
	     << rei->getInterface() << "_inport\", XactorAdapter::InPort)" << endl;
	first = 0;
      } else {
	file << "  , m_" << rei->getInterface() << " (\"\", \"scemi_put_"
	     << rei->getInterface() << "_inport\", XactorAdapter::InPort)" << endl;
      }
    }
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isPipeGet()) {
      if (first) {
	file << "  : m_" << rei->getInterface() << " (\"\", \"scemi_get_"
	     << rei->getInterface() << "_outpipe\", XactorAdapter::OutPipe)" << endl;
	first = 0;
      } else {
	file << "  , m_" << rei->getInterface() << " (\"\", \"scemi_get_"
	     << rei->getInterface() << "_outpipe\", XactorAdapter::OutPipe)" << endl;
      }
    } else if (rei->isGet()) {
      if (first) {
	file << "  : m_" << rei->getInterface() << " (\"\", \"scemi_get_"
	     << rei->getInterface() << "_outport\", XactorAdapter::OutPort)" << endl;
	first = 0;
      } else {
	file << "  , m_" << rei->getInterface() << " (\"\", \"scemi_get_"
	     << rei->getInterface() << "_outport\", XactorAdapter::OutPort)" << endl;
      }
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
      if (HdlUtils::isPortPipe(terminal->m_portName.c_str())) {
	if (first) {
	  file << "  : m_" << terminal->m_portName << " (\"\", \"scemi_put_"
	       << terminal->m_portName << "_inpipe\", XactorAdapter::InPipe)" << endl;
	  file << "  : m_" << terminal->m_portName << " (\"\", \"scemi_put_"
	       << terminal->m_portName << "_ctrl_in\", XactorAdapter::InPort)" << endl;
	  first = 0;
	} else {
	  file << "  , m_" << terminal->m_portName << " (\"\", \"scemi_put_"
	       << terminal->m_portName << "_inpipe\", XactorAdapter::InPipe)" << endl;
	  file << "  , m_" << terminal->m_portName << "_ctrl (\"\", \"scemi_put_"
	       << terminal->m_portName << "_ctrl_in\", XactorAdapter::InPort)" << endl;
	}
      } else {
	if (first) {
	  file << "  : m_" << terminal->m_portName << " (\"\", \"scemi_put_"
	       << terminal->m_portName << "_inport\", XactorAdapter::InPort)" << endl;
	  file << "  : m_" << terminal->m_portName << " (\"\", \"scemi_put_"
	       << terminal->m_portName << "_ctrl_in\", XactorAdapter::InPort)" << endl;
	  first = 0;
	} else {
	  file << "  , m_" << terminal->m_portName << " (\"\", \"scemi_put_"
	       << terminal->m_portName << "_inport\", XactorAdapter::InPort)" << endl;
	  file << "  , m_" << terminal->m_portName << "_ctrl (\"\", \"scemi_put_"
	       << terminal->m_portName << "_ctrl_in\", XactorAdapter::InPort)" << endl;
	}
      }
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
      if (HdlUtils::isPortPipe(terminal->m_portName.c_str())) {
	if (first) {
	  file << "  : m_" << terminal->m_portName << " (\"\", \"scemi_get_"
	       << terminal->m_portName << "_outpipe\", XactorAdapter::OutPipe)" << endl;
	  file << "  : m_" << terminal->m_portName << " (\"\", \"scemi_get_"
	       << terminal->m_portName << "_ctrl_in\", XactorAdapter::InPort)" << endl;
	  first = 0;
	} else {
	  file << "  , m_" << terminal->m_portName << " (\"\", \"scemi_get_"
	       << terminal->m_portName << "_outpipe\", XactorAdapter::OutPipe)" << endl;
	  file << "  , m_" << terminal->m_portName << "_ctrl (\"\", \"scemi_get_"
	       << terminal->m_portName << "_ctrl_in\", XactorAdapter::InPort)" << endl;
	}
      } else {
	if (first) {
	  file << "  : m_" << terminal->m_portName << " (\"\", \"scemi_get_"
	       << terminal->m_portName << "_outport\", XactorAdapter::OutPort)" << endl;
	  file << "  : m_" << terminal->m_portName << " (\"\", \"scemi_get_"
	       << terminal->m_portName << "_ctrl_in\", XactorAdapter::InPort)" << endl;
	  first = 0;
	} else {
	  file << "  , m_" << terminal->m_portName << " (\"\", \"scemi_get_"
	       << terminal->m_portName << "_outport\", XactorAdapter::OutPort)" << endl;
	  file << "  , m_" << terminal->m_portName << "_ctrl (\"\", \"scemi_get_"
	       << terminal->m_portName << "_ctrl_in\", XactorAdapter::InPort)" << endl;
	}
      }
    }
  }
  file << "{" << endl;
  file << "}" << endl;
  file << endl;

  return 1;
}

int TestBenchGenerator::generateInlineSend(std::fstream &file, const char *new_module_name,
					   const char *type, const char *portname,
					   const char *prefix)
{
  file << "inline bool " << prefix << "send_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->send_" << portname << "("
       << portname << "_data);" << endl;
  file << "}" << endl;

  file << "inline bool " << prefix << "sendB_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->sendB_" << portname << "("
       << portname << "_data);" << endl;
  file << "}" << endl;

  file << "inline bool " << prefix << "sendBAck_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->sendBAck_" << portname << "("
       << portname << "_data);" << endl;
  file << "}" << endl;

  if (HdlUtils::isPortPipe(portname)) {

    file << "inline bool " << prefix << "vector_send_" << portname << "(std::vector<"
       << type
	 << " > &" << portname << "_data";
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->vector_send_" << portname << "("
	 << portname << "_data);" << endl;
    file << "}" << endl;

    file << "inline bool " << prefix << "vector_sendB_" << portname << "(std::vector<"
       << type
	 << " > &" << portname << "_data";
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->vector_sendB_" << portname << "("
	 << portname << "_data);" << endl;
    file << "}" << endl;

    file << "inline bool " << prefix << "vector_sendAck_" << portname << "(std::vector<"
       << type
	 << " > &" << portname << "_data";
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->vector_sendAck_" << portname << "("
	 << portname << "_data);" << endl;
    file << "}" << endl;
  }

  file << "inline bool " << prefix << "set_emulation_type_" << portname
       << "(EmulationPortType t";
  file << ")" << endl;
  file << "{" << endl;
  file << "  BitT<1> data = t;" << endl;
  file << "  return " << new_module_name << "Xactor::get()->set_emulation_type_" << portname
       << "(data);" << endl;
  file << "}" << endl;
  
  file << endl;

  return 1;
}

int TestBenchGenerator::generateInlineReceive(std::fstream &file, const char *new_module_name,
					      const char *type, const char *portname,
					      int pipe, const char *prefix)
{
  file << "inline bool " << prefix << "receive_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->receive_" << portname << "("
       << portname << "_data);" << endl;
  file << "}" << endl;

  file << "inline bool " << prefix << "receiveB_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->receiveB_" << portname << "("
       << portname << "_data);" << endl;
  file << "}" << endl;

  if (!pipe && false) {
    file << "inline bool " << prefix << "receive_time_" << portname << "("
	 << type
	 << " &" << portname << "_data, SceMiU64 &timestamp";
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->receive_" << portname << "("
	 << portname << "_data, timestamp);" << endl;
    file << "}" << endl;
    
    file << "inline bool " << prefix << "receiveB_time_" << portname << "("
	 << type
	 << " &" << portname << "_data, SceMiU64 &timestamp";
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->receiveB_" << portname << "("
	 << portname << "_data, timestamp);" << endl;
    file << "}" << endl;
  }

  if (pipe) {

    file << "inline unsigned " << prefix << "vector_receive_" << portname << "(std::vector<"
	 << type << " >"
	 << " &" << portname << "_data, unsigned minReturned, unsigned maxReturned";
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->vector_receive_" << portname << "("
	 << portname << "_data, minReturned, maxReturned);" << endl;
    file << "}" << endl;
  }

  file << "inline bool " << prefix << "set_emulation_type_" << portname
       << "(EmulationPortType t";
  file << ")" << endl;
  file << "{" << endl;
  file << "  BitT<1> data = t;" << endl;
  file << "  return " << new_module_name << "Xactor::get()->set_emulation_type_" << portname
       << "(data);" << endl;
  file << "}" << endl;
  file << endl;

  return 1;
}

int TestBenchGenerator::generateSendMethods(std::fstream &file, const char *new_module_name,
					    const char *type, const char *portname,
					    const char *prefix)
{
  file << "bool " << new_module_name << "Xactor::" << prefix
       << "send_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  bool sent = m_" << portname << ".sendNB(" 
       << portname << "_data);" << endl;
  file << "  return sent;" << endl;
  file << "}" << endl;
  file << endl;

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "sendB_" << portname << "(" 
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  m_" << portname << ".send(" 
       << portname << "_data);" << endl;
  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "sendBAck_" << portname << "(" 
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  m_" << portname << ".sendAcknowledge(" 
       << portname << "_data);" << endl;
  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;

  if (HdlUtils::isPortPipe(portname)) {

    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "vector_send_" << portname << "(std::vector<"
	 << type
	 << " > &" << portname << "_data";
    file << ")" << endl;
    file << "{" << endl;
    file << "  return m_" << portname << ".sendNB(" 
	 << portname << "_data);" << endl;
    file << "}" << endl;
    file << endl;

    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "vector_sendB_" << portname << "(std::vector<"
	 << type
	 << " > &" << portname << "_data";
    file << ")" << endl;
    file << "{" << endl;
    file << "  m_" << portname << ".send(" 
	 << portname << "_data);" << endl;
    file << "  return true;" << endl;
    file << "}" << endl;
    file << endl;

    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "vector_sendAck_" << portname << "(std::vector<"
	 << type
	 << " > &" << portname << "_data";
    file << ")" << endl;
    file << "{" << endl;
    file << "  m_" << portname << ".sendAcknowledge(" 
	 << portname << "_data);" << endl;
    file << "  return true;" << endl;
    file << "}" << endl;
    file << endl;
  }

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "set_emulation_type_" << portname
       << "(BitT<1> &t)" << endl;
  file << "{" << endl;
  file << "  m_" << portname << "_ctrl.sendAcknowledge(" 
       << "t);" << endl;
  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;

  return 1;
}

int TestBenchGenerator::generateReiSendMethods(std::fstream &file, RdyEnableInterface *rei,
					       const char *new_module_name,
					       ModuleTerminalList &mtlist,
					       const char *prefix)
{
  std::list<std::string>::iterator stItr;
  string lstr, params, pipeparams, firstvector;
  ModuleTerminal *terminal;
  int first = 1;
  ostringstream convert;
  string lower;

  params = pipeparams = "";
  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
    if (terminal && (terminal->m_dir == d_input)) {
      lstr = *stItr;
      if (first == 1) {
	first = 0;
	firstvector = lstr;
      } else {
	params += ", ";
	pipeparams += ", ";
      }
      convert.str("");
      convert << terminal->m_width;
      params += "BitT<" + convert.str() + "> &" + lstr;
      pipeparams += "std::vector<BitT<" + convert.str() + "> > &" + lstr;
    }
  }

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "put_" << rei->getInterface() << "("
       << params;
  file << ")" << endl;
  file << "{" << endl;
  file << "  " << new_module_name << "_" << rei->getInterface()
       << " data;" << endl << endl;

  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    if (terminal && (terminal->m_dir == d_input)) {
      lstr = *stItr;
      lower = lstr;
      toLowerCase(lower);
      file << "  data.m_" << lower << " = " << lstr << ";" << endl;
    }
  }

  file << endl;
  file << "  bool sent = m_" << rei->getInterface();
  file << ".sendNB(";
  file << "data);" << endl;
  file << "  return sent;" << endl;
  file << "}" << endl;
  file << endl;

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "putB_" << rei->getInterface() << "(" 
       << params;
  file << ")" << endl;
  file << "{" << endl;
  file << "  " << new_module_name << "_" << rei->getInterface()
       << " data;" << endl << endl;

  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    if (terminal && (terminal->m_dir == d_input)) {
      lstr = *stItr;
      lower = lstr;
      toLowerCase(lower);
      file << "  data.m_" << lower << " = " << lstr << ";" << endl;
    }
  }

  file << endl;
  file << "  m_" << rei->getInterface() << ".send(" 
       << "data);" << endl;
  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;

  if (rei->isPipePut()) {

    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "vector_put_" << rei->getInterface() << "("
	 << pipeparams;
    file << ")" << endl;
    file << "{" << endl;
    file << "  std::vector<" << new_module_name << "_" << rei->getInterface()
	 << "> data;" << endl << endl;
    
    file << "  data.resize(" << firstvector << ".size());" << endl;
    file << "  for (int i=0; i<" << firstvector << ".size(); i++) {" << endl;
    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      if (terminal && (terminal->m_dir == d_input)) {
	lstr = *stItr;
	lower = lstr;
	toLowerCase(lower);
	file << "    data[i].m_" << lower << " = " << lstr << "[i];" << endl;
      }
    }
    file << "  };" << endl;
    
    file << endl;
    file << "  return m_" << rei->getInterface();
    file << ".sendNB(";
    file << "data);" << endl;
    file << "}" << endl;
    file << endl;

    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "vector_putB_" << rei->getInterface() << "("
	 << pipeparams;
    file << ")" << endl;
    file << "{" << endl;
    file << "  std::vector<" << new_module_name << "_" << rei->getInterface()
	 << "> data;" << endl << endl;
    
    file << "  data.resize(" << firstvector << ".size());" << endl;
    file << "  for (int i=0; i<" << firstvector << ".size(); i++) {" << endl;
    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      if (terminal && (terminal->m_dir == d_input)) {
	lstr = *stItr;
	lower = lstr;
	toLowerCase(lower);
	file << "    data[i].m_" << lower << " = " << lstr << "[i];" << endl;
      }
    }
    file << "  };" << endl;
    
    file << endl;
    file << "  m_" << rei->getInterface();
    file << ".send(";
    file << "data);" << endl;
    file << "  return true;" << endl;
    file << "}" << endl;
    file << endl;

    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "vector_putAck_" << rei->getInterface() << "("
	 << pipeparams;
    file << ")" << endl;
    file << "{" << endl;
    file << "  std::vector<" << new_module_name << "_" << rei->getInterface()
	 << "> data;" << endl << endl;
    
    file << "  data.resize(" << firstvector << ".size());" << endl;
    file << "  for (int i=0; i<" << firstvector << ".size(); i++) {" << endl;
    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      if (terminal && (terminal->m_dir == d_input)) {
	lstr = *stItr;
	lower = lstr;
	toLowerCase(lower);
	file << "    data[i].m_" << lower << " = " << lstr << "[i];" << endl;
      }
    }
    file << "  };" << endl;
    
    file << endl;
    file << "  m_" << rei->getInterface();
    file << ".sendAcknowledge(";
    file << "data);" << endl;
    file << "  return true;" << endl;
    file << "}" << endl;
    file << endl;

  }

  return 1;
}

int TestBenchGenerator::generateInlineReiPut(std::fstream &file, RdyEnableInterface *rei,
					     const char *new_module_name,
					     ModuleTerminalList &mtlist,
					     const char *prefix)
{
  std::list<std::string>::iterator stItr;
  string lstr, params, params2, pipeparams;
  ModuleTerminal *terminal;
  int first = 1;
  ostringstream convert;
  string lower;

  params = "";
  params2 = "";
  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
    if (terminal && (terminal->m_dir == d_input)) {
      lstr = *stItr;
      if (first == 1)
	first = 0;
      else {
	params += ", ";
	params2 += ", ";
	pipeparams += ", ";
      }

      convert.str("");
      convert << terminal->m_width;
      params += "BitT<" + convert.str() + "> &" + lstr;
      params2 += lstr;
      pipeparams += "std::vector<BitT<" + convert.str() + "> > &" + lstr;
    }
  }

  file << "inline bool " << prefix
       << "put_" << rei->getInterface() << "("
       << params;
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->put_" << rei->getInterface() << "("
       << params2 << ");" << endl;
  file << "}" << endl;

  file << "inline bool " << prefix
       << "putB_" << rei->getInterface() << "("
       << params;
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->putB_" << rei->getInterface() << "("
       << params2 << ");" << endl;
  file << "}" << endl;

  if (rei->isPipePut()) {

    file << "inline bool " << prefix
	 << "vector_put_" << rei->getInterface() << "("
	 << pipeparams;
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->vector_put_" << rei->getInterface() << "("
	 << params2 << ");" << endl;
    file << "}" << endl;

    file << "inline bool " << prefix
	 << "vector_putB_" << rei->getInterface() << "("
	 << pipeparams;
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->vector_putB_" << rei->getInterface() << "("
	 << params2 << ");" << endl;
    file << "}" << endl;

    file << "inline bool " << prefix
	 << "vector_putAck_" << rei->getInterface() << "("
	 << pipeparams;
    file << ")" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->vector_putAck_" << rei->getInterface() << "("
	 << params2 << ");" << endl;
    file << "}" << endl;
  }

  return 1;
}

int TestBenchGenerator::generateGetMethods(std::fstream &file, const char *new_module_name,
					   const char *type, const char *portname,
					   const char *prefix)
{
  file << "bool " << new_module_name << "Xactor::" << prefix
       << "receive_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  //file << "  S" << new_module_name << "_" << portname << " data;" << endl;
  file << "  " << new_module_name << "_" << portname << " data;" << endl;
  file << "  bool gotone = m_" << portname
       << ".receiveNB(data);" << endl;
  file << "  if (gotone) {" << endl;
  file << "    " << portname << "_data = data;" << endl;
  file << "  }" << endl;
  file << "  return gotone;" << endl;
  file << "}" << endl;
  file << endl;

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "receiveB_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  //file << "  S" << new_module_name << "_" << portname << " data;" << endl;
  file << "  " << new_module_name << "_" << portname << " data;" << endl;
  file << "  m_" << portname << ".receive(data);" << endl;
  file << "  " << portname << "_data = data;" << endl;
  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;

  if (false) {
    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "receive_" << portname << "("
	 << type
	 << " &" << portname << "_data, SceMiU64 &timestamp";
    file << ")" << endl;
    file << "{" << endl;
    //file << "  S" << new_module_name << "_" << portname << " data;" << endl;
    file << "  " << new_module_name << "_" << portname << " data;" << endl;
    file << "  bool gotone = m_" << portname
	 << ".receiveNB(data);" << endl;
    file << "  if (gotone) {" << endl;
    file << "    " << portname << "_data = data;" << endl;
    file << "    timestamp = data.getTimeStamp();" << endl;
    file << "  }" << endl;
    file << "  return gotone;" << endl;
    file << "}" << endl;
    file << endl;
    
    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "receiveB_" << portname << "("
	 << type
	 << " &" << portname << "_data, SceMiU64 &timestamp";
    file << ")" << endl;
    file << "{" << endl;
    //file << "  S" << new_module_name << "_" << portname << " data;" << endl;
    file << "  " << new_module_name << "_" << portname << " data;" << endl;
    file << "  m_" << portname << ".receive(data);" << endl;
    file << "  " << portname << "_data = data;" << endl;
    file << "    timestamp = data.getTimeStamp();" << endl;
    file << "  return true;" << endl;
    file << "}" << endl;
    file << endl;
  }

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "set_emulation_type_" << portname
       << "(BitT<1> &t)" << endl;
  file << "{" << endl;
  file << "  m_" << portname << "_ctrl.sendAcknowledge(" 
       << "t);" << endl;
  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;

  return 1;
}

int TestBenchGenerator::generatePipeGetMethods(std::fstream &file, const char *new_module_name,
					       const char *type, const char *portname,
					       const char *prefix)
{
  file << "bool " << new_module_name << "Xactor::" << prefix
       << "receive_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  bool gotone = m_" << portname
       << ".receiveNB(" << portname << "_data);" << endl;
  file << "  return gotone;" << endl;
  file << "}" << endl;
  file << endl;

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "receiveB_" << portname << "("
       << type
       << " &" << portname << "_data";
  file << ")" << endl;
  file << "{" << endl;
  file << "  m_" << portname << ".receive(" << portname << "_data);" << endl;
  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;

  file << "unsigned " << new_module_name << "Xactor::" << prefix
       << "vector_receive_" << portname << "(std::vector<"
       << type
       << " > &" << portname << "_data, unsigned minReturned, unsigned maxReturned";
  file << ")" << endl;
  file << "{" << endl;
  //file << "  S" << new_module_name << "_" << portname << " data;" << endl;
  //file << "  " << portname << "_data.clear();" << endl;
  file << "  unsigned nitems = m_" << portname
       << ".receive(" << portname << "_data, minReturned, maxReturned);" << endl;
  file << "  return nitems;" << endl;
  file << 
"}" << endl;
  file << endl;

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "set_emulation_type_" << portname
       << "(BitT<1> &t)" << endl;
  file << "{" << endl;
  file << "  m_" << portname << "_ctrl.sendAcknowledge(" 
       << "t);" << endl;
  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;
  return 1;
}

int TestBenchGenerator::generateInlineReiGet(std::fstream &file, RdyEnableInterface *rei,
					     const char *new_module_name,
					     ModuleTerminalList &mtlist,
					     const char *prefix)
{
  std::list<std::string>::iterator stItr;
  string lstr, params, params2, pipeparams;
  ModuleTerminal *terminal;
  int first = 1;
  ostringstream convert;
  string lower;

  params = "";
  params2 = "";
  pipeparams = "";
  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
    if (terminal && (terminal->m_dir == d_output)) {
      lstr = *stItr;
      if (first == 1)
	first = 0;
      else {
	params += ", ";
	params2 += ", ";
	pipeparams += ", ";
      }

      convert.str("");
      convert << terminal->m_width;
      params += "BitT<" + convert.str() + "> &" + lstr;
      params2 += lstr;
      pipeparams += "std::vector<BitT<" + convert.str() + "> > &" + lstr;
    }
  }

  file << "inline bool " << prefix
       << "get_" << rei->getInterface() << "("
       << params;
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->get_" << rei->getInterface() << "("
       << params2 << ");" << endl;
  file << "}" << endl;

  file << "inline bool " << prefix
       << "getB_" << rei->getInterface() << "("
       << params;
  file << ")" << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->getB_" << rei->getInterface() << "("
       << params2 << ");" << endl;
  file << "}" << endl;

  if (!rei->isPipeGet() && false) {
    file << "inline bool " << prefix
	 << "get_time_" << rei->getInterface() << "("
	 << params;
    file << ", SceMiU64 &timestamp)" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->get_time_" << rei->getInterface() << "("
	 << params2 << ", timestamp);" << endl;
    file << "}" << endl;
    
    file << "inline bool " << prefix
	 << "getB_time_" << rei->getInterface() << "("
	 << params;
    file << ", SceMiU64 &timestamp)" << endl;
    file << "{" << endl;
    file << "  return " << new_module_name << "Xactor::get()->getB_time_" << rei->getInterface() << "("
	 << params2 << ", timestamp);" << endl;
    file << "}" << endl;
  }

  if (rei->isPipeGet()) {
    
    file << "inline unsigned " << prefix
	 << "vector_get_" << rei->getInterface() << "("
	 << pipeparams;
    file << ", unsigned minReturned, unsigned maxReturned)" << endl;
    file << "{" << endl;
    file << "  unsigned nitems;" << endl << endl;
    file << "  nitems = " << new_module_name << "Xactor::get()->vector_get_" << rei->getInterface() << "("
	 << params2 << ", minReturned, maxReturned);" << endl;
    file << "  return nitems;" << endl;
    file << "}" << endl;
  }

  return 1;
}

int TestBenchGenerator::generateReiGetMethods(std::fstream &file, RdyEnableInterface *rei,
					      const char *new_module_name,
					      ModuleTerminalList &mtlist,
					      const char *prefix)

{
  std::list<std::string>::iterator stItr;
  string lstr, params, pipeparams, lower, resizeparams;
  ModuleTerminal *terminal;
  int first = 1;
  ostringstream convert;

  resizeparams = pipeparams = params = "";
  for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
    terminal = HdlUtils::locateModuleTerminal(mtlist, (*stItr));
    if (terminal && (terminal->m_dir == d_output)) {
      lstr = *stItr;
      if (first == 1)
	first = 0;
      else {
	params += ", ";
	pipeparams += ", ";
      }
      convert.str("");
      convert << terminal->m_width;
      params += "BitT<" + convert.str() + "> &" + lstr;
      pipeparams += "std::vector<BitT<" + convert.str() + "> > &" + lstr;
      resizeparams += "  " + lstr + ".resize(nitems);\n";
    }
  }

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "get_" << rei->getInterface() << "("
       << params;
  file << ")" << endl;
  file << "{" << endl;
  if (rei->isGet()) {
    //file << "  S" << new_module_name << "_" << rei->getInterface() << " data;" << endl << endl;
    file << "  " << new_module_name << "_" << rei->getInterface() << " data;" << endl << endl;
    file << "  bool gotone = m_" << rei->getInterface() << ".receiveNB(" 
	 << "data);" << endl;
    file << "  if (gotone) {" << endl;
    
    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      if (terminal && (terminal->m_dir == d_output)) {
	lstr = *stItr;
	lower = lstr;
	toLowerCase(lower);
	file << "    " << lstr << " = " << "data.m_" << lower << ";" << endl;
      }
    }
    file << "  }" << endl;
    
  } else {
    file << "  " << new_module_name << "_" << rei->getInterface() << " data;" << endl << endl;
    file << "  bool gotone = m_" << rei->getInterface() << ".receiveNB(data);" << endl; 
    int i = 0;
    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      if (terminal && (terminal->m_dir == d_output)) {
	lstr = *stItr;
	lower = lstr;
	toLowerCase(lower);
	file << "  " << lstr << " = " << "data.m_" << lower << ";" << endl;
	i++;
      }
    }
  }

  file << "  return gotone;" << endl;
  file << "}" << endl;
  file << endl;

  file << "bool " << new_module_name << "Xactor::" << prefix
       << "getB_" << rei->getInterface() << "(" 
       << params;
  file << ")" << endl;
  file << "{" << endl;
  if (rei->isGet()) {
    //file << "  S" << new_module_name << "_" << rei->getInterface()
    //	 << " data;" << endl << endl;
    file << "  " << new_module_name << "_" << rei->getInterface()
	 << " data;" << endl << endl;
    file << "  m_" << rei->getInterface() << ".receive(data);" << endl; 

    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      if (terminal && (terminal->m_dir == d_output)) {
	lstr = *stItr;
	lower = lstr;
	toLowerCase(lower);
	file << "  " << lstr << " = " << "data.m_" << lower << ";" << endl;
      }
    }
  } else {
    file << "  " << new_module_name << "_" << rei->getInterface()
	 << " data;" << endl << endl;
    file << "  m_" << rei->getInterface() << ".receive(data);" << endl;
    int i = 0;
    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      if (terminal && (terminal->m_dir == d_output)) {
	lstr = *stItr;
	lower = lstr;
	toLowerCase(lower);
	file << "  " << lstr << " = " << "data.m_" << lower << ";" << endl;
	i++;
      }
    }
  }

  file << "  return true;" << endl;
  file << "}" << endl;
  file << endl;

  if (rei->isPipeGet()) {

    file << "unsigned " << new_module_name << "Xactor::" << prefix
	 << "vector_get_" << rei->getInterface() << "(" 
	 << pipeparams;
    file << ", unsigned minReturned, unsigned maxReturned)" << endl;
    file << "{" << endl;
    file << "  unsigned nitems;" << endl;
    file << "  std::vector<" << new_module_name << "_" << rei->getInterface()
	 << "> data;" << endl << endl;
    file << "  nitems = m_" << rei->getInterface() 
	 << ".receive(data, minReturned, maxReturned);" << endl;
    file << "  if (nitems == 0)" << endl;
    file << "    return nitems;" << endl << endl;
    file << resizeparams << endl;
    file << "  for (int i=0; i<nitems; i++) {" << endl;
    int i = 0;
    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      if (terminal && (terminal->m_dir == d_output)) {
	lstr = *stItr;
	lower = lstr;
	toLowerCase(lower);
	file << "    " << lstr << "[i] = " << "data[i].m_" << lower << ";" << endl;
	i++;
      }
    }
    file << "  }" << endl;
    file << "  return nitems;" << endl;
    file << "}" << endl;
    file << endl;
  }

  if (!rei->isPipeGet() && false) {
    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "get_time_" << rei->getInterface() << "("
	 << params;
    file << ", SceMiU64 &timestamp)" << endl;
    file << "{" << endl;
    if (rei->isGet()) {
      //file << "  S" << new_module_name << "_" << rei->getInterface() << " data;" << endl << endl;
      file << "  " << new_module_name << "_" << rei->getInterface() << " data;" << endl << endl;
      file << "  bool gotone = m_" << rei->getInterface() << ".receiveNB(" 
	   << "data);" << endl;
      
      file << "  if (gotone) {" << endl;
      file << "    timestamp = data.getTimeStamp();" << endl;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	if (terminal && (terminal->m_dir == d_output)) {
	  lstr = *stItr;
	  lower = lstr;
	  toLowerCase(lower);
	  file << "    " << lstr << " = " << "data.m_" << lower << ";" << endl;
	}
      }
      file << "  }" << endl;
    } else {
      file << "  " << new_module_name << "_" << rei->getInterface() << " data;" << endl << endl;
      file << "  bool gotone = m_" << rei->getInterface() << ".receiveNB(" 
	   << "data);" << endl;
      
      file << "  if (gotone) {" << endl;
      int i = 0;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	if (terminal && (terminal->m_dir == d_output)) {
	  lstr = *stItr;
	  lower = lstr;
	  toLowerCase(lower);
	  file << "    " << lstr << " = " << "data.m_" << lower << ";" << endl;
	  i++;
	}
      }
      file << "  }" << endl;
    }
    
    file << "  return gotone;" << endl;
    file << "}" << endl;
    file << endl;
    
    file << "bool " << new_module_name << "Xactor::" << prefix
	 << "getB_time_" << rei->getInterface() << "(" 
	 << params;
    file << ", SceMiU64 &timestamp)" << endl;
    file << "{" << endl;
    if (rei->isGet()) {
      //file << "  S" << new_module_name << "_" << rei->getInterface()
      //	 << " data;" << endl << endl;
      file << "  " << new_module_name << "_" << rei->getInterface()
	   << " data;" << endl << endl;
      file << "  m_" << rei->getInterface() << ".receive(data);" << endl; 
      file << "  timestamp = data.getTimeStamp();" << endl;
      
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	if (terminal && (terminal->m_dir == d_output)) {
	  lstr = *stItr;
	  lower = lstr;
	  toLowerCase(lower);
	  file << "  " << lstr << " = " << "data.m_" << lower << ";" << endl;
	}
      }
      
    } else { 
      file << "  " << new_module_name << "_" << rei->getInterface()
	   << " data;" << endl << endl;
      file << "  m_" << rei->getInterface() << ".receive(data);" << endl; 
      file << "  //timestamp = data.getTimeStamp();" << endl;
      
      int i = 0;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	if (terminal && (terminal->m_dir == d_output)) {
	  lstr = *stItr;
	  lower = lstr;
	  toLowerCase(lower);
	  file << "    " << lstr << " = " << "data.m_" << lower << ";" << endl;
	  i++;
	}
      }
    }
    file << "  return true;" << endl;
    file << "}" << endl;
  }

  file << endl;

  return 1;
}

int TestBenchGenerator::generateDutXactor(VeriModule *module,
					  const char *new_module_name,
					  const char *dirname)
{
  std::fstream file;
  string errstring;
  string lower_new_mod_name = new_module_name; 
  string lstr;
  DIR *dp;

  string filename = module->Name();
  filename += ".pin";

  //HdlUtils::init()->readPortSpec(filename.c_str());

  if (isupper(new_module_name[0]))
    lower_new_mod_name[0] = tolower(new_module_name[0]);
    
  string realdir;
  realdir = dirname;
  //size_t loc = realdir.find_first_of('/');
  //if (loc != string::npos)
  //  realdir = realdir.substr(0, loc+1) + "cpp";
  filename = realdir;

  if ((dp = opendir(filename.c_str())) == NULL) {
    if(recursive_mkdir(filename.c_str(), 0755) != 0) {
      errstring += "Error TestBenchGenerator::generateDutXactor(): cannot open directory ";
      errstring += filename;
      fprintf(stderr, "%s\n", errstring.c_str());
      return 0;
    }
  }

  if (filename[filename.size()] != '/')
    filename += "/";
  filename += new_module_name;
  filename += "Xactor.h";

  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateDutXactorFile(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  // Generate all the data type
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  
  ModuleTerminal *terminal;
  
  HdlUtils::createModuleTerminalList(module, mtlist);
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  //cout << "Map pointer3 " << (void*)map << endl;

  std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();
  //HdlUtils::init()->setRdyEnableIfcWidth(module);

  int in_count = 0;
  int out_count = 0;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    if ((*map)[terminal->m_portName] == "") {
      if (terminal->m_dir != d_output)
	in_count++;
      else
	out_count++;
    }
  }

  file << "// Copyright Bluespec Inc. 2012-2013" << endl;
  file << "// By: GenTestbench tool" << endl << endl;
  file << "#pragma once" << endl;
  file << endl;
  file << "// Include Bluespec's SceMi C++ api" << endl;
  file << "#include \"DutXactor.h\"" << endl;
  file << "#include \"SceMiHeaders.h\"" << endl;
  file << endl;

  RdyEnableInterface *rei;
  string interface;
  string typestring;

  // Ready enable ports
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  std::list<std::string>::iterator stItr;

  // ******************
  // typedef StampedT<>
  // ******************
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    file << "typedef StampedT<" << new_module_name << "_"<< rei->getInterface()
      //<< "> S" << new_module_name << "_" << rei->getInterface()
      //<< ";" << endl;
	 << "> S" << new_module_name << "_" << rei->getInterface()
	 << ";" << endl;
  }
  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
      file << "typedef StampedT<" << new_module_name << "_"<< terminal->m_portName
	//<< "> S" << new_module_name << "_" << terminal->m_portName
	//<< ";" << endl;
	   << "> S" << new_module_name << "_" << terminal->m_portName
	   << ";" << endl;
    }
  }
  file << endl;
  file << "// Define a class for the top-level transactor" << endl;
  file << "class " << new_module_name << "Xactor : public DutXactor {" << endl << endl;
  file << " protected:" << endl << endl;
  file << "  static " << new_module_name << "Xactor *m_xactor;" << endl << endl;
  file << "  // Data members include transactors contained in the model" << endl;
  file << "  // Data Xactors" << endl;


  // **************
  // InportProxyT<>
  // **************
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isPut()) {
      file << "  InProxyT < " << new_module_name << "_" << rei->getInterface() << " > m_"
	   << rei->getInterface() << ";" << endl;
    } else if (rei->isPipePut()) {
      file << "  InProxyT < " << new_module_name << "_" << rei->getInterface() << " > m_"
	   << rei->getInterface() << ";" << endl;
    } else if (rei->isPipeGet()) {
      file << "  OutProxyT < " << new_module_name << "_" << rei->getInterface() << " > m_"
	   << rei->getInterface() << ";" << endl;
    } else {
      //file << "  OutProxyT < S" << new_module_name << "_" << rei->getInterface() << " > m_"
      file << "  OutProxyT < " << new_module_name << "_" << rei->getInterface() << " > m_"
	   << rei->getInterface() << ";" << endl;
    }
  }
  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
      if (HdlUtils::isPortPipe(terminal->m_portName.c_str())) {
	file << "  InProxyT < " << new_module_name << "_" << terminal->m_portName << " > m_"
	     << terminal->m_portName << ";" << endl;
	file << "  InProxyT < BitT<1> > m_"
	     << terminal->m_portName << "_ctrl" << ";" << endl;
      } else {
	file << "  InProxyT < " << new_module_name << "_" << terminal->m_portName << " > m_"
	     << terminal->m_portName << ";" << endl;
	file << "  InProxyT < BitT<1> > m_"
	   << terminal->m_portName << "_ctrl" << ";" << endl;
      }
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
      if (HdlUtils::isPortPipe(terminal->m_portName.c_str())) {
	file << "  OutProxyT < " << new_module_name << "_" << terminal->m_portName << " > m_"
	     << terminal->m_portName << ";" << endl;
	file << "  InProxyT < BitT<1> > m_"
	     << terminal->m_portName << "_ctrl" << ";" << endl;
      } else {
	//file << "  OutProxyT < S" << new_module_name << "_" << terminal->m_portName << " > m_"
	file << "  OutProxyT < " << new_module_name << "_" << terminal->m_portName << " > m_"
	     << terminal->m_portName << ";" << endl;
	file << "  InProxyT < BitT<1> > m_"
	     << terminal->m_portName << "_ctrl" << ";" << endl;
      }
    }
  }
  file << endl;
  file << "  // Constructor" << endl;
  file << "  " << new_module_name << "Xactor (SceMi *scemi);" << endl;
  file << endl;
  file << "  // Destructor" << endl;
  file << "  ~" << new_module_name << "Xactor();" << endl << endl;
  file << " public:" << endl;
  file << endl;
  file << "  // Initialize transactor" << endl;
  file << "  static " << new_module_name << "Xactor *init(SceMi *scemi);" << endl;
  file << "  static " << new_module_name << "Xactor *get() { return m_xactor; }" << endl << endl;
  file << "  // Destroy transactor" << endl;
  file << "  void destroy();" << endl << endl;
  file << "  // Public interface ....." << endl;

  // *******************
  // bool sendB and send
  // *******************
  file << "  // Put/Send" << endl;
  ostringstream convert;
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
      convert.str("");
      convert << rei->getWidth();
      typestring = string(new_module_name) + "_" + rei->getInterface();;
      generateReiSendHeaders(file, rei, mtlist);
    }
  }
  // Regular ports

  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
      convert.str("");
      convert << terminal->m_width;
      typestring = string("BitT<") + convert.str() + ">";
      generateSendHeaders(file, typestring.c_str(), terminal->m_portName.c_str());
    }
  }
  //file << endl;

  // *****************
  // bool getB and get
  // *****************
  file << "  // Get/Receive" << endl;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isGet() || rei->isPipeGet()) {
      convert.str("");
      convert << rei->getWidth();
      typestring = string(new_module_name) + "_" + rei->getInterface();
      generateReiGetHeaders(file, rei, mtlist, rei->isPipeGet());
    }
  }
  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
      convert.str("");
      convert << terminal->m_width;
      typestring = string("BitT<") + convert.str() + ">";
      generateGetHeaders(file, typestring.c_str(), terminal->m_portName.c_str());
    }
  }

  file << "};" << endl;

  file.close();


  filename = realdir;
  if (filename[filename.size()] != '/')
    filename += "/";
  filename += new_module_name;
  filename += "Xactor.cpp";

  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateDutXactorFile(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "// Copyright Bluespec Inc. 2012-2013" << endl;
  file << "// By: GenTestbench tool" << endl << endl;
  file << "#include <iostream>" << endl;
  file << "#include \"" << new_module_name << "Xactor.h\"" << endl;
  file << endl;
  file << "using namespace std;" << endl;
  file << endl;
  file << new_module_name << "Xactor *" << new_module_name 
       << "Xactor::m_xactor = NULL;" << endl << endl;
  file << new_module_name << "Xactor *" << new_module_name 
       << "Xactor::init(SceMi *scemi)" << endl;
  file << "{" << endl;
  file << "  if (m_xactor != NULL)" << endl;
  file << "    return m_xactor;" << endl << endl;
  file << "  m_xactor = new " << new_module_name << "Xactor(scemi);" << endl << endl;
  file << "  return m_xactor;" << endl;
  file << "}" << endl << endl;
  file << "void " << new_module_name << "Xactor::destroy()" << endl;
  file << "{" << endl;
  file << "  delete m_xactor;" << endl;
  file << "  m_xactor = NULL;" << endl;
  file << "}" << endl << endl;
  file << new_module_name << "Xactor::" << new_module_name << "Xactor(SceMi *scemi)" << endl;
  file << "  : DutXactor(scemi)" << endl;

  //**************************
  // Contructor initialization
  //**************************
  generateDutXactorContructor(file, mtlist);

  // Destructor
  file << new_module_name << "Xactor::~" << new_module_name << "Xactor()" << endl;
  file << "{" << endl;
  file << "}" << endl;
  file << endl;


  //******
  // send
  //******

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
      convert.str("");
      typestring = string(new_module_name) + "_" + rei->getInterface();
      generateReiSendMethods(file, rei, new_module_name, mtlist);
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    rei = RdyEnableIfc[lstr];
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
      convert.str("");
      convert << terminal->m_width;
      typestring = string("BitT<") + convert.str() + ">";
      generateSendMethods(file, new_module_name, typestring.c_str(), terminal->m_portName.c_str());
    }
  }


  //*****
  // get
  //*****
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isGet() || rei->isPipeGet()) {
      convert.str("");
      convert << rei->getWidth();
      typestring = string(new_module_name) + "_" + rei->getInterface();
      generateReiGetMethods(file, rei, new_module_name, mtlist);
    }
  }
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
      convert.str("");
      convert << terminal->m_width;
      typestring = string("BitT<") + convert.str() + ">";
      if (HdlUtils::isPortPipe(terminal->m_portName.c_str()))
	generatePipeGetMethods(file, new_module_name, typestring.c_str(), terminal->m_portName.c_str());
      else
	generateGetMethods(file, new_module_name, typestring.c_str(), terminal->m_portName.c_str());
    }
  }
  /*
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    rei = RdyEnableIfc[lstr];
    if (rei) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
      typestring = string("S") + new_module_name + "_" + terminal->m_portName;
      generateGetMethods(file, new_module_name, typestring.c_str(), terminal->m_portName.c_str());
    }
  }
  */

  file.close();
  
  // capi.h
  if (realdir == "")
    filename = "capi.h";
  filename = realdir + "/capi.h";

  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateDutXactorFile(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "// Copyright Bluespec Inc. 2012-2013" << endl;
  file << "// By: GenTestbench tool" << endl << endl;
  file << "#pragma once" << endl;
  file << endl;
  file << "#include \"semu_capi.h\"" << endl;
  file << "#include \"" << new_module_name << "Xactor.h\"" << endl << endl;

  file << "using namespace std;" << endl;

  file << "//" << endl; 
  file << "// This is Bluespec C API for communicating with the DUT" << endl;
  file << "//" << endl << endl;

  file << "#ifdef __cplusplus" << endl;
  file << "extern \"C\" {" << endl;
  file << "#endif" << endl << endl;

  file << "// Below is capi for interfacing with scemi controlled clocks" << endl;
  file << "// All functions return 1 - success, 0 - failed." << endl << endl;
  file << "/*" << endl;
  file << "// This is how you advance controlled clock 1 cycle" << endl;
  file << "if (semu_advance_controlled_clock(1)) do_something();" << endl << endl;

  file << "// Same as above, except the call is blocked until the number of cycles are done" << endl;
  file << "if (semu_advance_controlled_clockB(cycles)) do_something();" << endl << endl;

  file << "// This is how you start free-running controlled clock" << endl;
  file << "if (semu_start_controlled_clock()) do_something;" << endl << endl;

  file << "// This is how you stop controlled clock" << endl;
  file << "if (semu_stop_controlled_clock()) do_something();" << endl << endl;

  file << "// This is how you get current cycle of the controlled clock" << endl;
  file << "SceMiU64 clock_cycles;" << endl;
  file << "if (semu_get_current_controlled_clock_cycle(clock_cycles)) do_something();" << endl << endl;

  file << "// This is how you assert controlled reset for 8 cycles" << endl;
  file << "int number_of_cycles = 8;" << endl;
  file << "if (semu_assert_reset(number_of_cycles)) do_something();" << endl << endl;
  file << "*/" << endl << endl;

  file << "  // Reset" << endl;
  file << "  bool semu_assert_reset(unsigned short number_of_cycles, const char *reset_name=NULL);"
       << endl << endl;

  // *******************
  // bool sendB and send
  // *******************
  file << "  // Below is capi for interfacing with the DUT" << endl;
  file << "  // The calls with 'B' denotes blocking calls" << endl << endl;;
  file << "  // send/put (non-blocking) and sendB/putB (blocking) to the DUT" << endl;
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
      convert.str("");
      convert << rei->getWidth();
      typestring = string(new_module_name) + "_" + rei->getInterface();;
      generateReiSendHeaders(file, rei, mtlist, "semu_");
    }
  }
  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
      convert.str("");
      convert << terminal->m_width;
      typestring = string("BitT<") + convert.str() + ">";
      generateSendHeaders2(file, typestring.c_str(), terminal->m_portName.c_str(),
			   "semu_");
    }
  }
  file << endl;

  // *****************
  // bool getB and get
  // *****************
  file << "  // receive/get (non-blocking) and receiveB/getB (blocking) from the DUT" << endl;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isGet() || rei->isPipeGet()) {
      convert.str("");
      convert << rei->getWidth();
      typestring = string(new_module_name) + "_" + rei->getInterface();
      generateReiGetHeaders(file, rei, mtlist, rei->isPipeGet(), "semu_");
    }
  }
  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
      convert.str("");
      convert << terminal->m_width;
      typestring = string("BitT<") + convert.str() + ">";
      if (HdlUtils::isPortPipe(terminal->m_portName.c_str()))
	generateGetHeaders2(file, typestring.c_str(), terminal->m_portName.c_str(), 1,
			  "semu_");
      else
	generateGetHeaders2(file, typestring.c_str(), terminal->m_portName.c_str(), 0,
			  "semu_");
    }
  }

  file << "#ifdef __cplusplus" << endl;
  file << "};" << endl;
  file << "#endif" << endl << endl;

  // Inline implementations
  file << "// send/put" << endl;
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
      convert.str("");
      convert << rei->getWidth();
      typestring = string(new_module_name) + "_" + rei->getInterface();;
      generateInlineReiPut(file, rei, new_module_name, mtlist, "semu_");
    }
  }
  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    rei = RdyEnableIfc[lstr];
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir != d_output)) {
      convert.str("");
      convert << terminal->m_width;
      typestring = string("BitT<") + convert.str() + ">";
      generateInlineSend(file, new_module_name, typestring.c_str(), terminal->m_portName.c_str(),
			 "semu_");
    }
  }

  // *****************
  // bool getB and get
  // *****************
  file << "// receive/get" << endl;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    if (rei->isGet() || rei->isPipeGet()) {
      convert.str("");
      convert << rei->getWidth();
      typestring = string(new_module_name) + "_" + rei->getInterface();
      generateInlineReiGet(file, rei, new_module_name, mtlist, "semu_");
    }
  }
  // Regular ports
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if (((*map)[terminal->m_portName] == "") && (terminal->m_dir == d_output)) {
      convert.str("");
      convert << terminal->m_width;
      typestring = string("BitT<") + convert.str() + ">";
      if (HdlUtils::isPortPipe(terminal->m_portName.c_str()))
	generateInlineReceive(file, new_module_name, typestring.c_str(),
			      terminal->m_portName.c_str(), 1, "semu_");
      else
	generateInlineReceive(file, new_module_name, typestring.c_str(),
			      terminal->m_portName.c_str(), 0, "semu_");
    }
  }

  file << "inline bool semu_assert_reset(unsigned short number_of_cycles, const char *reset_name)"
       << endl;
  file << "{" << endl;
  file << "  return " << new_module_name << "Xactor::get()->assertReset(number_of_cycles);" << endl;
  file << "}" << endl;
  file.close();
  
  return 1;
}

int TestBenchGenerator::generateTclGui(VeriModule *module, const char *new_module_name,
				       const char *dirname, int tbtemplate)
{
  std::fstream file;
  string errstring;
  string lower_new_mod_name = new_module_name; 
  //char buf[100];
  string pn, params;
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  //RdyEnableInterface *rei;
  string lstr;
  //std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();
  std::list<std::string>::iterator stItr;

  if (isupper(new_module_name[0]))
    lower_new_mod_name[0] = tolower(new_module_name[0]);
    
  string filename, realdir;

  realdir = dirname;
  size_t loc = realdir.find_last_of('/');
  if ((tbtemplate != 1) && (loc != 0)) { // template and not absolute path
    if (loc != string::npos)
      realdir = realdir.substr(0, loc+1) + "scripts";
  }

  // Write dbg-gui if normal run (not tbtemplate)
  /* NO LONGER generate dbg-gui in scripts directory
  if (tbtemplate == 0) {
    if (realdir == "")
      filename = "dbg-gui";
    else
      filename = realdir + "/dbg-gui";

    std::string command = "cp ";
    command += std::getenv("BLUESPECDIR");
    command += "/tcllib/simtb/dbg-gui ";
    command += filename;
    system(command.c_str());

    command = "chmod 775 ";
    command += filename;
    system(command.c_str());

  }
  */

  // Write debug-gui if normal run (not tbtemplate)
  /* TURN OFF debug-gui generation, there is permanent one in the svn tree
  if (tbtemplate == 0) {
    if (realdir == "")
      filename = "debug-gui";
    else
      filename = realdir + "/debug-gui";
    
    printf("Writing %s\n", filename.c_str());
    
    file.open(filename.c_str(), ios::out);
    if (file.fail()) {
      errstring += "Error HdlUtils::generateTclGui(): file ";
      errstring += filename;
      errstring += " cannot be open\n";
      fprintf(stderr, "%s\n", errstring.c_str());
      return 0;
    }
    
    file << "#!/bin/sh" << endl;
    file << "# \\" << endl;
    file << "exec $BLUESPECDIR/bin/bluewish \"$0\" \"$@\"" << endl;
    file << endl;
    file << "lappend auto_path ." << endl;
    file << "lappend auto_path $env(BLUESPECDIR)/tcllib/scemi" << endl;
    file << "lappend auto_path $env(BLUESPECDIR)/tcllib/workstation" << endl;
    file << "lappend auto_path $env(BLUESPECDIR)/tcllib/simtb" << endl;
    file << endl;
    file << "package require Bluetcl" << endl;
    file << "package require Waves" << endl;
    file << "package require BSDebug" << endl;
    file << "package require Iwidgets 4.0" << endl;
    file << "package require emu_control" << endl;
    file << endl;
    file << "# standard Bluespec colours and fonts" << endl;
    file << "fonts::set_colours" << endl;
    file << "fonts::initialize" << endl;
    file << endl;
    file << "######################### OPTIONS #################" << endl;
    file << "# Where to find .bo files" << endl;
    file << "#Bluetcl::flags set -p ../gen_dut:+" << endl;
    file << endl;
    file << "# Select wave viewer  Gtk or Novas" << endl;
    file << "Waves::set_options viewer GtkWave" << endl;
    file << "# Waves::set_options viewer {Novas}" << endl;
    file << "Waves::set_options {GtkWave,Command} gtkwave {GtkWave,Options} -W" << endl;
    file << "Waves::set_options {Novas,Command} {nWave}" << endl;
    file << "Waves::set_options {Novas,Options} {-nologo}" << endl;
    file << endl;
    file << endl;
    file << "######################### Starting Emulation #################" << endl;
    file << endl;
    file << "## load and setup the software side of the scemi" << endl;
    file << "set paramFile mkBridge.params" << endl;
    file << "if { $::argc >= 1 } {" << endl;
    file << "    set paramFile [lindex $::argv 0]" << endl;
    file << "    puts \"Using $paramFile\"" << endl;
    file << "}" << endl;
    file << "if { ! [file readable $paramFile ]  } {" << endl;
    file << "    puts stderr \"Error: Could not open $paramFile\"" << endl;
    file << "    exit 1" << endl;
    file << "}" << endl;
    file << "if { [catch \"bsdebug::scemi init $paramFile\"  err ]} {" << endl;
    file << "    puts \"Could not start scemi\"" << endl;
    file << "    puts $err" << endl;
    file << "    exit 1" << endl;
    file << "}" << endl;
    file << endl;
    file << "set mode [lindex $::argv 1]" << endl;
    file << "set tbgui [lindex $::argv 2]\n\n";
    file << "## load the bit allocation file\n";
    file << "set rtlFile [lindex $::argv 3]\n";
    file << "set llFile [lindex $::argv 4]\n";
    file << "set edfFile [lindex $::argv 5]\n";
    file << "set logFile [lindex $::argv 6]\n";
    file << "set top [lindex $::argv 7]" << endl;
    file << "set rdbackLog [lindex $::argv 8]\n\n";
    file << "if {$mode == \"\"} {" << endl;
    file << "    if {$rtlFile == \"\"} {" << endl;
    file << "        set mode \"sim\"" << endl;
    file << "    } else {" << endl;
    file << "        set mode \"emu\"" << endl;
    file << "    }" << endl;
    file << "}" << endl << endl;
    file << "if {$mode == \"emu\"} {" << endl;
    file << "    bsdebug::netlist load $rtlFile $llFile $edfFile $logFile\n";
    file << "}" << endl << endl;
    file << "######################### Starting the GUI #################" << endl;
    file << endl;
    file << "# source the local files" << endl;
    //file << "source " << realdir << "/gui_top.tcl" << endl;
    file << "if {$tbgui == \"man\"} {" << endl;
    file << "    source " << realdir << "/gui_dut.tcl" << endl;
    file << "} else {" << endl;
    file << "    source $tbgui/gui_dut.tcl" << endl;
    file << "}" << endl;
    file << endl;
    file << "bsdebug::rdbk set verbose off" << endl;
    file << endl;
    file << "# log readback activities if the 5th parameter is non-empty" << endl;
    file << "if { $rdbackLog == \"log\" } {" << endl;
    file << "    bsdebug::netlist cmdlog on" << endl;
    file << "    bsdebug::rdbk set cmdlog on" << endl;
    file << "    bsdebug::dut cmdlog on" << endl;
    file << "}" << endl << endl;

    file << "# Run emulation window" << endl;
    file << "emu_control .control $top $mode $tbgui" << endl;

    file.close();
    string command = "chmod 775 ";
    command += filename;
    system(command.c_str());
  }
  */

  generateGuiDutTclFile(file, module, realdir, tbtemplate);

  if (tbtemplate) return 1;

  //string srcdir = "src";
  //generateGuiDutTclFile(file, module, srcdir, true);

  //
  // Start of gui_tb.tcl generation
  //
  if (realdir == "")
    filename = "gui_tb.tcl";
  else
    filename = realdir + "/gui_tb.tcl";
  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateTclGui(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "package require Tk" << endl;
  file << "namespace import bsdebug::dut" << endl;
  file << endl;
  file << "namespace eval GuiDut {" << endl;
  file << "    variable updatetime 500" << endl << endl;

  file << "    #If you are going to build a DUT UI put it in this proc" << endl;
  file << "    #Build the window frame in top" << endl;
  file << "    proc mkDutControl { frame } {" << endl;
  file << "        variable top" << endl << endl;
  file << "        set top $frame" << endl;
  file << "        # set button_frame [ttk::frame $top.button_frame]" << endl;
  file << "        return $top" << endl;
  file << "    }" << endl;
  file << "    proc getLoop {} {" << endl;
  file << "        if { [catch getLoopInternal err] } {" << endl;
  file << "            puts stderr \"Status loop failed,  interface will not respond\"" << endl;
  file << "            puts stderr $err" << endl;
  file << "        }" << endl;
  file << "    }" << endl;
  file << "    proc getLoop {} {" << endl;
  file << "        if { [catch getLoopInternal err] } {" << endl;
  file << "            puts stderr \"Status loop failed,  interface will not respond\"" << endl;
  file << "            puts stderr $err" << endl;
  file << "        }" << endl;
  file << "    }" << endl << endl;
  file << "    # Loop watching status" << endl;
  file << "    proc getLoopInternal {} {" << endl;
  file << "        variable updatetime" << endl;
  file << endl;
  file << "        set res [dut response]" << endl;
  file << "        while { $res != \"\" } {" << endl;
  file << "	       puts \"$res\"" << endl;
  file << "            set res [dut response]" << endl;
  file << "	}" << endl;
  file << "	after $updatetime GuiDut::getLoop" << endl;
  file << "     update" << endl;
  file << "    }" << endl;
  file << endl;
  file << "# ======" << endl;
  file << endl;
  file << "    proc chk_num {s} {" << endl;
  file << "        #string is integer $s" << endl;
  file << "        return true" << endl;
  file << "    }" << endl;
  file << endl;
  file << "}" << endl;

  file.close();


  //
  // Start of gui_top.tcl generation
  //
  if (realdir == "")
    filename = "gui_top.tcl";
  else
    filename = realdir + "/gui_top.tcl";

  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateTclGui(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "lappend auto_path ." << endl;
  file << "package require BSDebug" << endl;
  file << "#package require ReadBackGui" << endl;
  file << "#package require ProbeGui" << endl;
  file << endl;
  file << "fonts::set_colours" << endl;
  file << "fonts::initialize" << endl;
  file << endl;
  file << "# Display the window" << endl;
  file << endl;
  file << "proc buildEmulationWin { {win .emu} top} {" << endl;
  file << "    if { $win != \".\" } {" << endl;
  file << "        toplevel $win" << endl;
  file << "    }" << endl;
  file << "    wm title $win \"Bluespec Emulation\"" << endl;
  //file << "    wm geometry . 751x600" << endl;
  file << "    set paned [ttk::panedwindow $win.paned -orient vertical]" << endl;
  file << "    pack $paned -side top -expand yes -fill both -pady 5 -padx 2" << endl;
  file << endl;
  file << "    set c [ReadBackGui::mkEmulationControlPanels $win true $top]" << endl;
  file << "    $paned add $c -weight 0" << endl;
  file << endl;
  //file << "    set w [ReadBackGui::mkWavePanel $win]" << endl;
  //file << "    $paned add $w -weight 1" << endl;
  file << endl;
  file << "    set d [GuiDut::mkDutControl $win]" << endl;
  file << "    $paned add $d -weight 1" << endl;
  file << endl;
  file << "#    set p [ProbeGui::mkProbePanel $win \"scemi_test.vcd\"]" << endl;
  file << "#    $paned add $p -weight 1" << endl;
  file << endl;
  file << "    # start the status loop here" << endl;
  file << "    after 500 ReadBackGui::statusLoop" << endl;
  file << "#    after 500 ProbeGui::statusLoop" << endl;
  file << "    #after 500 GuiDut::getLoop" << endl;
  file << "}" << endl;

  file.close();

  filename = "pkgIndex.tcl";
  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateTclGui(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << " # pkgIndex.tcl -- tells Tcl how to load my package." << endl;
  file << " package ifneeded \"BSDebug\" 1.0	\\" << endl;
  file << "    [list load [file join $dir libbsdebug.so]]" << endl;

  file.close();

  return 1;
}

bool TestBenchGenerator::fileExist(const char *filename)
{
  std::fstream file;

  // Check if file already exist
  file.open(filename, ios::in);
  if (!file.fail()) {
    file.close();
    return false;
  }

  file.close();
  return true;
}

bool TestBenchGenerator::openVCDFile()
{
  if (VCDFileName.size() == 0) {
    cerr << "Error: VCD file name was not previously set." << endl;
    return false;
  }

  // cerr << "Initializing VCD file" << endl;

  VCD_file.open(vcdFileName());
  if (VCD_file.fail()) {
    std::cerr << "ERROR TestBenchGenerator::openVCDFile(): unable to open vcd file "
	      << vcdFileName()
              << "." << std::endl;
    return false;
  }

  return true;
}

#define fileprefix if (tbtemplate) file << "        #"; else file << "        "

int TestBenchGenerator::generateGuiDutTclFile(std::fstream &file, VeriModule *module, 
					      string &realdir, int tbtemplate)
{
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  RdyEnableInterface *rei;
  string lstr;
  std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();  //
  string filename;
  string errstring;
  char buf[100];
  std::list<std::string>::iterator stItr;
  string pn, params;
  DIR *dp;
  int first;

  if ((dp = opendir(realdir.c_str())) == NULL) {
    if(recursive_mkdir(realdir.c_str(), 0755) != 0) {
      errstring += "Error TestBenchGenerator::generateGuiDutTclFile(): cannot open directory ";
      errstring += realdir;
      fprintf(stderr, "%s\n", errstring.c_str());
      return 0;
    }
  }

  //
  // Start of gui_dut.tcl generation
  //
  if (realdir == "")
    filename = "gui_dut.tcl";
  filename = realdir + "/gui_dut.tcl";

  // Check if file already exist
  if (tbtemplate) {
    file.open(filename.c_str(), ios::in);
    if (!file.fail()) {
      file.close();
      //return 0;
    }
    file.close();
  }

  printf("Writing %s\n", filename.c_str());
  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateGuiDutTclFile(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "package require Tk" << endl;
  file << "namespace import bsdebug::dut" << endl;
  file << endl;
  file << "namespace eval GuiDut {" << endl;
  file << "    variable dut" << endl;

  // Generate all the data type
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;

  ModuleTerminal *terminal;
  
  HdlUtils::createModuleTerminalList(module, mtlist);
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();

  int start_rei_port_index = 1;
  int start_rei_outport_index;
  int start_reg_port_index;
  int start_reg_outport_index;

  int i = 1;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {

    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    rei->setUsed(true);
    for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
      file << "    variable e" << i << " 0" << endl;
      i++;
    }
  }

  start_reg_port_index = i;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {

    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    rei = RdyEnableIfc[lstr];
    if (rei && ((lstr == string(rei->getRdy())) ||
		(lstr == rei->getEn())))
      continue;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      file << "    variable e" << i << " 0" << endl;
      i++;
    }
  }
  file << "    variable updatetime 500" << endl;
  file << endl;
  if (tbtemplate)
    file << "    # Below is a sample code (commented out) for the generated manually input testbench" << endl;
  file << "    proc mkDutControl { frame } {" << endl;
  file << "        variable top" << endl;
  fileprefix;
  file << "variable putbut" << endl;
  fileprefix;
  file << "variable getbut" << endl;
  file << endl;
  file << "        set top $frame" << endl;
  file << endl;

  file << "        ## Button Frame" << endl;
  fileprefix;
  file << "set button_frame" << " [ttk::frame $top.button_frame" 
       << "]" << endl;
  fileprefix;
  file << "set inputs_label [ttk::labelframe $top.button_frame.label]" << endl;
  fileprefix;
  file << "set inputs_text [ttk::label $top.blabel -justify center -text \"Inputs/Outputs\"]" << endl;
  fileprefix;
  file << "pack $inputs_text" << endl;
  fileprefix;
  file << "grid $inputs_label -row 1 -column 0" << endl;

  int row, col;
  i = start_rei_port_index;
  row = 2;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
      
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    col = 1;
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	if ((col % 7) == 0) {
	  row++;
	  col = 1;
	}
	lstr = *stItr;
	pn = lstr;
	toLowerCase(pn);
	snprintf(buf, 99, "%d", i++);
	fileprefix;
	file << "set " << pn << "_text [ttk::label $button_frame" 
	     << "." << pn << "_text -text " << pn << " -justify left -width 0 -anchor e]"
	     << endl;
	fileprefix;
	file << "set e" << buf << " [ttk::entry $button_frame" << "." 
	     << pn << "_entry -textvariable GuiDut::e" << buf 
	     << " -width 8 -validate key -validatecommand \"GuiDut::chk_num %P\"]" << endl;
	fileprefix;
	file << "grid $" << pn << "_text -pady 5 -padx 5 -row " << row << " -column " << col++ << endl;
	fileprefix;
	file << "grid $e" << buf << " -pady 5 -padx 5 -row " << row << " -column " << col++ << endl << endl;
      }
      fileprefix;
      file << "set putbut" << buf << " [ttk::button $button_frame.put" << buf << " -text \"Put\" -command \"GuiDut::do_put_" << rei->getInterface() << "\"]" << endl;
      fileprefix;
      file << "grid $putbut" << buf << " -pady 5 -padx 5 -row " << row << " -column " << col++ << endl << endl;
    }
    row++;
  }
    
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    col = 1;
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	if ((col % 7) == 0) {
	  row++;
	  col = 1;
	}
	lstr = *stItr;
	pn = lstr;
	toLowerCase(pn);
	snprintf(buf, 99, "%d", i++);
	fileprefix;
	file << "set " << pn << "_text [ttk::label $button_frame" 
	     << "." << pn << "_text -text " << pn << " -justify left -width 0 -anchor e]"
	     << endl;
	fileprefix;
	file << "set e" << buf << " [ttk::entry $button_frame" << "." 
	     << pn << "_entry -textvariable GuiDut::e" << buf 
	     << " -width 8 -validate key -validatecommand \"GuiDut::chk_num %P\" -state disabled]" << endl;
	fileprefix;
	file << "grid $" << pn << "_text -pady 5 -padx 5 -row " << row << " -column " << col++ << endl;
	fileprefix;
	file << "grid $e" << buf << " -pady 5 -padx 5 -row " << row << " -column " << col++ << endl << endl;
      }
      fileprefix;
      file << "set getbut" << buf << " [ttk::button $button_frame.get" << buf << " -text \"Get\" -command \"GuiDut::do_get_" << rei->getInterface() << "\"]" << endl;
      fileprefix;
      file << "grid $getbut" << buf << " -pady 5 -padx 5 -row " << row << " -column " << col << endl << endl;
    }
    row++;
  }

  first = 1;
  col = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    if ((col % 7) == 0) {
      row++;
      col = 1;
    }
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      if (terminal->m_dir != d_output) {
	pn = terminal->m_portName;
	first = 0;
	toLowerCase(pn);
	snprintf(buf, 99, "%d", i++);
	fileprefix;
	file << "set " << pn << "_text [ttk::label $button_frame" 
	     << "." << pn << "_text -text " << pn << " -justify left -width 0 -anchor e]"
	     << endl;
	fileprefix;
	file << "set e" << buf << " [ttk::entry $button_frame" << "." 
	     << pn << "_entry -textvariable GuiDut::e" << buf 
	     << " -width 8 -validate key -validatecommand \"GuiDut::chk_num %P\"]" << endl;
	fileprefix;
	file << "grid $" << pn << "_text -pady 5 -padx 5 -row " << row << " -column " << col++ << endl;
	fileprefix;
	file << "grid $e" << buf << " -pady 5 -padx 5 -row " << row << " -column " << col++ << endl << endl;
      }
    }
  }
  if (first == 0) {
    fileprefix;
    file << "set putbut [ttk::button $button_frame.put -text \"Send\" -command \"GuiDut::do_send\"]" << endl;
    fileprefix;
    file << "grid $putbut -pady 5 -padx 5 -row " << row << " -column " << col << endl << endl;
  }
    
  col = 1;
  first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    if ((col % 7) == 0) {
      row++;
      col = 1;
    }
    terminal = &(*mtItr);
    lstr = terminal->m_portName;
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      if (terminal->m_dir == d_output) {
	if (first) {
	  row++;
	  first = 0;
	}
	pn = terminal->m_portName;
	toLowerCase(pn);
	snprintf(buf, 99, "%d", i++);
	fileprefix;
	file << "set " << pn << "_text [ttk::label $button_frame" 
	     << "." << pn << "_text -text " << pn << " -justify left -width 0 -anchor e]"
	     << endl;
	fileprefix;
	file << "set e" << buf << " [ttk::entry $button_frame" << "." 
	     << pn << "_entry -textvariable GuiDut::e" << buf 
	     << " -width 8 -validate key -validatecommand \"GuiDut::chk_num %P\" -state disabled]" << endl;
	fileprefix;
	file << "grid $" << pn << "_text -pady 5 -padx 5 -row " << row << " -column " << col++ << endl;
	fileprefix;
	file << "grid $e" << buf << " -pady 5 -padx 5 -row " << row << " -column " << col++ << endl << endl;
      }
    }
  }
    
  if (first == 0) {
    fileprefix;
    file << "set getbut [ttk::button $button_frame.get -text \"Receive \" -command \"GuiDut::do_get\"]" << endl;
    fileprefix;
    file << "grid $getbut -pady 5 -padx 5 -row " << row << " -column " << col << endl;
  }
    
  file << endl;
  fileprefix;
  file << "pack $button_frame -anchor n -side top" << endl;
  file << endl;
  file << "        return $top" << endl;
  file << "    }" << endl;
  file << endl;
 
  // Rdy Enable sends
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    
    rei = reiItr->second;
    if (rei == NULL) continue;
    rei->setUsed(false);
  }
  i = start_rei_port_index;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isPut() || rei->isPipePut()) {
      rei->setUsed(true);
      params = "";
      file << "    proc do_put_" << rei->getInterface() << " {} {" << endl;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	file << "        variable e" << i << ";" << endl;
	params += " $e" + string(itoa(i));
	i++;
      }
      file << "        set msg \"Blocked\"" << endl;
      file << endl;
      file << "        set sent [dut request_" << rei->getInterface() << params;
      file << "]" << endl;
      file << "        if {$sent} {" << endl;
      file << "          set msg \"sending" << ": " << params << "\"" << endl;
      file << "        } else {" << endl;
      file << "          set msg \"sending failed.\"" << endl;
      file << "        }" << endl;
      file << "        puts \"$msg\"" << endl;
      file << "    }" << endl << endl;
    }
  }
  int j;
  for (reiItr = RdyEnableIfc.begin(); reiItr != RdyEnableIfc.end(); reiItr++) {
    
    j = 1;
    rei = reiItr->second;
    if (rei == NULL || rei->getUsed() == true) continue;
    if (rei->isGet() || rei->isPipeGet()) {
      rei->setUsed(true);
      params = "";
      file << "    proc do_get_" << rei->getInterface() << " {} {" << endl;
      file << "        set res [dut response_" << rei->getInterface();
      file << "]" << endl;
      file << "        set found [lindex $res 0]" << endl;
      file << "        if {$found != 0} {" << endl;
      start_rei_outport_index = i;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	lstr = *stItr;
	file << "          set GuiDut::e" << itoa(i++) << " [lindex $res " << j++ << "]" << endl;
      }

      i = start_rei_outport_index;
      j = 1;
      for(stItr = rei->dataBegin(); stItr != rei->dataEnd(); stItr++) {
	lstr = *stItr;
	if (j == 1) {
	  file << "          set hex [format 0x%x $GuiDut::e" << itoa(i) << "]" << endl;
	  file << "          set msg \"Response: " << lstr << " = $GuiDut::e" << itoa(i++)
	       << " (hex: $hex)" << endl;
	} else {
	  file << " " << lstr << " = $GuiDut::e" << itoa(i++);
	}
	j++;
      }
      file << "\"" << endl;
      file << "        } else {" << endl;
      file << "          set msg \"No response received.\"" << endl;
      file << "        }" << endl;
      file << "        puts \"$msg\"" << endl;
      file << "    }" << endl << endl;
    }
  }

  // Individual port sends
  i = start_reg_port_index;
  first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      
    terminal = &(*mtItr);
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "")
      if (terminal->m_dir != d_output) {
	if (first) {
	  params = "";
	  file << "    proc do_send {} {" << endl;
	  file << "        variable e" << i << ";" << endl;
	  params += " $e" + string(itoa(i));
	  i++;
	  first = 0;
	} else {
	  file << "        variable e" << i << ";" << endl;
	  params += " $e" + string(itoa(i));
	  i++;
	}
      }
  }
  if (first == 0) {
    file << "        set msg \"Blocked\"" << endl;
    file << endl;
    file << "        set sent [dut request" << params;
    file << "]" << endl;
    file << "        if {$sent} {" << endl;
    file << "          set msg \"sending" << ": " << params << "\"" << endl;
    file << "        } else {" << endl;
    file << "          set msg \"sending failed.\"" << endl;
    file << "        }" << endl;
    file << "        puts \"$msg\"" << endl;
    file << "    }" << endl << endl;
  }
  j = 1;
  start_reg_outport_index = i;
  first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      if (terminal->m_dir == d_output) {
	if (first) {
	  file << "    proc do_get {} {" << endl;
	  file << endl;
	  file << "        set res [dut response]" << endl;
	  file << "        set found [lindex $res 0]" << endl;
	  file << "        if { $found != 0 } {" << endl;
	  file << "          set GuiDut::e" << itoa(i++) << " [lindex $res " << j++ << "]" << endl;
	  first = 0;
	} else {
	  file << "          set GuiDut::e" << itoa(i++) << " [lindex $res " << j++ << "]" << endl;
	}
      }
    }
  }
  j = 1;
  i = start_reg_outport_index;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      
    terminal = &(*mtItr);
    if (!HdlUtils::init()->isRegularModuleTerminal(terminal)) continue;
    if ((*map)[terminal->m_portName] == "") {
      if (terminal->m_dir == d_output) {
	lstr = terminal->m_portName;
	if (j == 1) {
	  file << "          set msg \"Response: " << lstr << " = $GuiDut::e" << itoa(i++);
	} else {
	  file << " " << lstr << " = $GuiDut::e" << itoa(i++);
	}
	j++;
      }
    }
  }
  if (first == 0) {
    file << "\"" << endl;
    file << "        } else {" << endl;
    file << "          set msg \"No response received.\"" << endl;
    file << "        }" << endl;
    file << "        puts \"$msg\"" << endl;
    file << "    }" << endl << endl;
  }

  file << "    proc getLoop {} {" << endl;
  file << "        if { [catch getLoopInternal err] } {" << endl;
  file << "            puts stderr \"Status loop failed,  interface will not respond\"" << endl;
  file << "            puts stderr $err" << endl;
  file << "        }" << endl;
  file << "    }" << endl << endl;
  file << "    # Loop watching status" << endl;
  file << "    proc getLoopInternal {} {" << endl;
  file << "        variable updatetime" << endl;
  file << endl;
  file << "        set res [dut response]" << endl;
  file << "        while { $res != \"\" } {" << endl;
  file << "            puts \"$res\"" << endl;
  file << "            set res [dut response]" << endl;
  file << "	}" << endl;
  file << "	after $updatetime GuiDut::getLoop" << endl;
  file << "        update" << endl;
  file << "    }" << endl;
  file << endl;
  file << "# ======" << endl;
  file << endl;
  file << "    proc chk_num {s} {" << endl;
  file << "        #string is integer $s" << endl;
  file << "        return true;" << endl;
  file << "    }" << endl;
  file << endl;
  file << "    proc do_test {} {" << endl;
  if (tbtemplate) {
    file << "        set msg \"Blocked\"" << endl;
    file << "        set sent [dut test]" << endl;
    file << "        if {$sent != \"\"} {" << endl;
    file << "            set msg \"start testing...\"" << endl;
    file << "            puts \"$msg\"" << endl;
    file << "            puts \"$sent\"" << endl;
    file << "        }" << endl;
  }
  file << "    }" << endl;
  file << "    proc do_reset {} {" << endl;
  if (tbtemplate) {
    file << "        set msg \"Blocked\"" << endl;
    file << "        set sent [dut reset]" << endl;
    file << "        if {$sent != \"\"} {" << endl;
    file << "            set msg \"reset testbench...\"" << endl;
    file << "            puts \"$msg\"" << endl;
    file << "            puts \"$sent\"" << endl;
    file << "        }" << endl;
  }
  file << "    }" << endl;
  file << endl;
  file << "}" << endl;
  
  file.close();
  
  return 1;
}

void TestBenchGenerator::closeVCDFile()
{
  VCD_file.close();
}

bool TestBenchGenerator::setVCDFile(const char* name)
{
  VCDFileName = name;

  //printf("Openning file %s\n", name);

  VCD_file.open(vcdFileName());
  if (VCD_file.fail()) {
    std::cerr << "ERROR TestBenchGenerator::setVCDFile(): unable to open vcd file "
	      << vcdFileName()
              << "." << std::endl;
    VCDFileName.resize(0);
    return false;
  }

  LineNum = 0;
  return true;
}

bool TestBenchGenerator::openSRFile()
{
  if (SRFileName.size() == 0) {
    cerr << "Error: Stimulus Response file name was not previously set." << endl;
    return false;
  }

  SR_file.open(stimulusResponseFileName(), ios::out);
  if (SR_file.fail()) {
    std::cerr << "ERROR TestBenchGenerator::openSRFile(): unable to open sr file "
	      << SRFileName
              << "." << std::endl;
    return false;
  }

  SRFileOpen = 1;

  return true;
}

void TestBenchGenerator::closeSRFile()
{
  SR_file.close();
}

bool TestBenchGenerator::setStimulusResponseFile(const char* name)
{
  SRFileName = name;
  return true;
}

int TestBenchGenerator::generateVerificationFromVCD(VeriModule *module, const char *dirname)
{
  cout << "Read vcd file" << endl;
  if (readVCDFile(module) == 0)
    return 0;

  //generateVerificationScript(module, "verify-dut-batch");
  cout << "gen stimulus" << endl;
  generateStimulusResponsesDataFile("stimulus.dat");

  cout << "Write vcd dirname " << dirname << endl;
  writeVcdPlayerHFile(dirname);
  writeVcdPlayerCFile(dirname);

  return 1;
}

int TestBenchGenerator::generateVerificationScript(VeriModule *module, const char *scriptname)
{
  std::fstream file;
  string errstring;
  std::map<std::string,std::string>::iterator portmapItr;
  std::string portname, vcdname;
  int first;

  printf("Writing %s\n", scriptname);
  printf("Writescriptname\n");

  file.open(scriptname, ios::out);
  if (file.fail()) {
    errstring += "Error TestBenchGenerator::generateVerificationScript(): file ";
    errstring += scriptname;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }

  file << "#!/bin/sh" << endl;
  file << "# \\" << endl;
  file << "exec $BLUESPECDIR/bin/bluetcl \"$0\" \"$@\"" << endl << endl;

  file << "lappend auto_path ." << endl;
  file << "lappend auto_path $env(BLUESPECDIR)/tcllib/scemi" << endl << endl;

  file << "package require Bluetcl" << endl;
  file << "package require BSDebug" << endl << endl;

  file << "######################### Starting Emulation #################" << endl << endl;

  file << "## load and setup the software side of the scemi" << endl;
  file << "set paramFile mkBridge.params" << endl;
  file << "if { $::argc >= 1 } {" << endl;
  file << "    set paramFile [lindex $::argv 0]" << endl;
  file << "    puts \"Using $paramFile\"" << endl;
  file << "}" << endl;
  file << "if { ! [file readable $paramFile ]  } {" << endl;
  file << "    puts stderr \"Error: Could not open $paramFile\"" << endl;
  file << "     exit 1" << endl;
  file << "}" << endl;
  file << "if { [catch \"bsdebug::scemi init $paramFile\"  err ]} {" << endl;
  file << "  puts \"Could not start scemi\"" << endl;
  file << "  puts $err" << endl;
  file << "  exit 1" << endl;
  file << "}" << endl << endl;

  file << "bsdebug::emu set verbose off" << endl << endl;

  file << "# sends a run command to the scemi hw, and then" << endl;
  file << "# waits until the finish message is received from the hw." << endl;
  file << "# During the wait, update is called to allow other tcl events to be processed" << endl;
  file << "# returns the current scemi time" << endl;
  file << "proc runNClocks { n } {" << endl;
  file << "    set timeout 200" << endl;
  file << "    bsdebug::emu run $n" << endl;
  file << "    set stat \"\"" << endl;
  file << "    while { $stat == \"\" } {" << endl;
  file << "        set stat [bsdebug::emu checkresponse $timeout]" << endl;
  file << "        update" << endl;
  file << "    }" << endl;
  file << "    set currentTime [lindex $stat 4]" << endl;
  file << "    return $currentTime" << endl;
  file << "}" << endl << endl;

  file << "# Runs \"cmd\" iteratively until a response is returned," << endl;
  file << "# user can set a maximun number of calls and wait time between calls" << endl;
  file << "proc waitForResponse { cmd maxcalls { waittime_ms 100 } } {" << endl;
  file << "    set r [eval $cmd]" << endl;
  file << "    set count $maxcalls" << endl;
  file << "    while { $r == \"\" && $count > 0 } {" << endl;
  file << "        after $waittime_ms" << endl;
  file << "        incr count -1" << endl;
  file << "        set r [eval $cmd]" << endl;
  file << "    }" << endl;
  file << "    if { $r == \"\" } {" << endl;
  file << "        puts \"$cmd did not return a response after [expr ($maxcalls * $waittime_ms)/1000.0] seconds\"" << endl;
  file << "    }" << endl;
  //file << "    puts $r" << endl;
  file << "    return $r" << endl;
  file << "}" << endl << endl;


  file << "proc sendRequest {";
  first = 1;
  for (portmapItr = InPortToVCDMap.begin(); portmapItr != InPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    if (first)
      first = 0;
    else
      file << " ";
    file << portname;
  }
  file << "} {" << endl;
  //file << "  puts \"\nRequest a:$a b:$b cin:$cin\"" << endl;
  file << "  waitForResponse \"bsdebug::dut request";
  for (portmapItr = InPortToVCDMap.begin(); portmapItr != InPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    file << " $" << portname;
  }
  file << "\" 100" << endl;
  file << "}" << endl << endl;

  file << "proc getResponse {} {" << endl;
  //file << "    puts "Get Sum"" << endl;
  file << "    waitForResponse \"bsdebug::dut response\" 100" << endl;
  file << "}" << endl << endl;

  file << "proc checkResponse { count";
  for (portmapItr = InPortToVCDMap.begin(); portmapItr != InPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    file << " " << portname;
  }
  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    file << " " << portname;
  }
  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = "exp_" + portmapItr->first;
    file << " " << portname;
  }
  file << " } {" << endl;
  file << "    puts \"\"" << endl;;
  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    file << "    scan $" << portname << " %x " << portname << "_d" << endl;
  }
  file << "    puts \"Verification #$count input:";
  for (portmapItr = InPortToVCDMap.begin(); portmapItr != InPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    file << " " << portname << "($" << portname << ")";
  }
  file << " output:";
  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    file << " " << portname << "($" << portname << "_d)";
  }
  file << "\"" << endl;
  file << "    if {";
  first = 1;
  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    if (first)
      first = 0;
    else
      file << " ||";
    file << " $" << portname << "_d != $exp_" << portname;
  }
  file << " } {" << endl;
  file << "	 puts \" => ERROR: expected output:";
  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    file << " " << portname << "($exp_" << portname << ")";
  }
  file << "\"" << endl;
  file << "    } else {" << endl;
  file << "	 puts \" => Passed.\"" << endl;
  file << "    }" << endl;
  file << "}" << endl << endl;

  generateTclStimulusResponses(file);
  
  file.close();

  char buf[1024];
  sprintf(buf, "chmod 775 %s", scriptname);
  system(buf);

  //printf("DONE generateVerification\n");
  return 1;
}

bool TestBenchGenerator::generateStimulusResponsesDataFile(const char *filename)
{
  std::fstream file;
  StimulusRespIterator srItr;

  cout << "Here 1" << endl;
  file.open(filename, ios::out);
  if (file.fail()) {
    std::cerr << "ERROR TestBenchGenerator::generateStimulusResponsesDataFile(): unable to open file "
	      << filename
              << "." << std::endl;
    return false;
  }

  cout << "Here 2" << endl;
  for (srItr = SortedStimulusResponses.begin();
       srItr != SortedStimulusResponses.end();
       srItr++)
    {
      cout << "Yo " << *srItr << endl;
      if ((*srItr)->isInput())
	writeStimulus(file, *srItr);
      else if ((*srItr)->isOutput())
	writeResponse(file, *srItr);
      else if ((*srItr)->isReset())
	writeReset(file, *srItr);
    }

  file.close();

  return true;
}

void TestBenchGenerator::generateTclStimulusResponses(std::fstream &file)
{
  StimulusRespIterator srItr;
  std::string stimulus, response, port;
  long tick;

  int numiarg = 0;
  int numarg = 0;
  std::map<std::string, long> nametoidxmap;
  std::vector<std::string> namevector;
  std::map<std::string,std::string>::iterator portmapItr;
  std::string vcdname, portname;
  for (portmapItr = InPortToVCDMap.begin(); portmapItr != InPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    nametoidxmap[portname] = numarg++;
    numiarg++;
  }
  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    nametoidxmap[portname] = numarg++;
  }

  int i = 0;
  tick = 0;
  namevector.resize(numarg);
  for (srItr = SortedStimulusResponses.begin();
       srItr != SortedStimulusResponses.end();
       srItr++)
    {
      if ((*srItr)->tick() > tick) {
	if (tick != 0) {
	  generateTclTest(file, i, namevector, numiarg, numarg);
	}
	tick = (*srItr)->tick();
      }

      port = (*srItr)->portName();
      namevector[nametoidxmap[port]] = (*srItr)->bits();
    }
  
  generateTclTest(file, i, namevector, numiarg, numarg);
  file << "puts \"\"" << endl;
}

void TestBenchGenerator::generateTclTest(std::fstream &file, int &count,
					 std::vector<std::string> &namevector,
					 int numiarg, int numarg)
{
  std::map<std::string,std::string>::iterator portmapItr;
  std::string vcdname, portname;
  VCDPort *p;

  file << endl;
  count++;
  file << "# Test " << count << endl;
  file << "runNClocks 10" << endl;
  file << "sendRequest";
  
  for (int k=0; k<numiarg; k++) {
    //printf("name %d %s\n", k, namevector[k].c_str());
    //file << " " << bitset<200>(namevector[k]).to_ulong();
    file << " " << namevector[k];
  }
  
  file << endl;
  file << "runNClocks 10" << endl;
  file << "set resp [getResponse]" << endl;
  file << "regsub -all {_} $resp \"\" resp2" << endl;
  
  for (portmapItr = OutPortToVCDMap.begin();
       portmapItr != OutPortToVCDMap.end();
       portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    p = VeriNameToVCDPortMap[portname];
    file << "regexp {";
    file << portname;
    if (p && (p->width() >= 2)) 
      file << " = [0-9]+\\'h([0-9a-f]+)} $resp2 match ";
    else
      file << " = ([0-9]+)} $resp2 match ";
    file << portname << endl;
  }
  
  file << "checkResponse " << count;
  int k;
  for (k=0; k<numiarg; k++) {
    //file << " " << bitset<200>(namevector[k]).to_ulong();
    file << " " << namevector[k];
  }
  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    vcdname = portmapItr->second;
    if (vcdname == "") continue;
    portname = portmapItr->first;
    file << " $" << portname;
  }
  for (; k<numarg; k++) {
    //file << " " << bitset<200>(namevector[k]).to_ulong();
    file << " " << namevector[k];
  }

  file << endl;
}

bool TestBenchGenerator::readVCDFile(VeriModule *module)
{
  bool stat = true;

  //const char *delimiter = " \n\t,:";
  char lineBuf[MAX_LINE_SIZE+1];
  boost::cmatch matches;
  size_t pos;
  LineNum = 0;
  VCDPort *masterPort, *subPort, *aPort;
  int size;
  TBMessage *msg;
  unsigned long ticknum;

  //char *token;

  // Set up regex

  // - enddefinition
  std::string scope = "\\$enddefinitions\\s+\\$end"; 
  boost::regex enddef_exp(scope);
  // - scope
  scope = "\\$scope\\s+(module|begin|function)\\s+(\\S+)\\s+\\$end"; 
  boost::regex scope_exp(scope);
  // - upscope
  scope = "\\$upscope\\s+\\$end"; 
  boost::regex upscope_exp(scope);
  // - var
  scope = "\\$var\\s+(wire|reg)\\s+(\\d+)\\s+(\\S+)\\s+(\\w+)\\s+\\$end"; 
  boost::regex var_exp(scope);
  scope = "\\$var\\s+(wire|reg)\\s+(\\d+)\\s+(\\S+)\\s+(\\w+)\\s+\\[(\\d+)\\]\\s+\\$end"; 
  boost::regex varsub_exp(scope);
  scope = "\\$var\\s+(wire|reg)\\s+(\\d+)\\s+(\\S+)\\s+(\\w+)\\s+\\[(\\d+):(\\d+)\\]\\s+\\$end"; 
  boost::regex varmsub_exp(scope);
  // - clock tick
  scope = "^#(\\d+)";
  boost::regex clock_exp(scope);
  // - bit data
  scope = "^(\\d)(\\S+)";
  boost::regex bitdata_exp(scope);
  // - binary data
  scope = "^b([0-1]+)\\s+(\\S+)";
  boost::regex bindata_exp(scope);


  std::string current;
  std::string sofar;
  std::string m1, m2, m3, m4, m5, m6, tick, veriname, clkname, resetname;
  DirectionE dir;
  int w;
  InPortsDirty = OutPortsDirty = 0;

  printf("Writing %s\n", SRFileName.c_str());

  // Read in each line.  Right now assuming each line consists of either:
  while (!VCD_file.eof()){

    VCD_file.getline(lineBuf, MAX_LINE_SIZE);
    LineNum++;
    printf("Line %d: %s\n", LineNum, lineBuf);
    
    // $enddefinitions
    if (boost::regex_match(lineBuf, enddef_exp)) {

      printf("enddefinitions1 match\n");
      stat = checkVCDPortDefinition(module);
      if (stat == false)
	return stat;
    }
    // $scope
    else if (boost::regex_match(lineBuf, matches, scope_exp)) {
      
      current = matches[2];
      if (sofar == "")
	sofar = current;
      else
	sofar = sofar + "." + current;
      printf("Scope2 sofar %s\n", sofar.c_str());
    }
    // $upscope
    else if (boost::regex_match(lineBuf, upscope_exp)) {
      pos = sofar.find_last_of('.');
      if (pos != string::npos)
	sofar = sofar.substr(0, pos);
      else
	sofar = "";
      printf("upscope3 sofar %s\n", sofar.c_str());
    }
    // $var
    else if (boost::regex_match(lineBuf, matches, var_exp)) {
      printf("Sofar1 %s vcdpath %s\n", sofar.c_str(), vcdPath());
      
      if (!hierarchyMatch(sofar.c_str(), vcdPath()))
	continue;

      printf("Line1 %d: %s\n", LineNum, lineBuf);
      m1 = matches[1];
      m2 = matches[2];
      m3 = matches[3];
      m4 = matches[4];

      veriname = VCDToInPortMap[m4];
      if (veriname == "") {
	veriname = VCDToOutPortMap[m4];
	dir = d_output;
      }
      else
	dir = d_input;

      // Now check for clock/reset
      if (veriname == "") {

	clkname = VcdClockToSceMiClockMap[m4];

	if (clkname != "") {

	  SymbolToVCDClockMap[m3] = m4;
	  VcdClockToClockTick[clkname] = 0;  // initialize clock tick
	  VcdClockToClockState[clkname] = false;  // initialize clock state
	}

	resetname = VcdResetToSceMiResetMap[m4];

	if (resetname != "") {

	  SymbolToVCDResetMap[m3] = m4;
	  VcdResetToResetState[m4] = true;  // initialize clock state
	}
	continue;
      }
      
      if (NameToVCDPortMap[m4] == NULL) {
	size = atoi(m2.c_str());
	masterPort = new VCDPort(m4.c_str(), veriname.c_str(), size, dir, m3.c_str());
	NameToVCDPortMap[m4] = masterPort;
	VeriNameToVCDPortMap[veriname] = masterPort;
	SymbolToVCDPortMap[m3] = masterPort;
	if (dir == d_input)
	  InPorts.push_back(masterPort);
	else
	  OutPorts.push_back(masterPort);
	
	printf("var4 line %d match %s %s %s %s %d veriname %s\n", LineNum, m1.c_str(), m2.c_str(), m3.c_str(), m4.c_str(),
	       size, veriname.c_str());

	
      }
    }
    else if (boost::regex_match(lineBuf, matches, varsub_exp)) {
      printf("Sofar2 %s vcdpath %s\n", sofar.c_str(), vcdPath());
      
      if (!hierarchyMatch(sofar.c_str(), vcdPath()))
	continue;

      printf("Line2 %d: %s\n", LineNum, lineBuf);
      m1 = matches[1];
      m2 = matches[2];
      m3 = matches[3];
      m4 = matches[4];
      m5 = matches[5];

      masterPort = NameToVCDPortMap[m4];
      veriname = VCDToInPortMap[m4];
      if (veriname == "") {
	veriname = VCDToOutPortMap[m4];
	dir = d_output;
      }
      else
	dir = d_input;
      
      if (veriname == "") continue;
      if (masterPort == NULL) {

	masterPort = new VCDPort(m4.c_str(), veriname.c_str(), 0, dir, "");
	NameToVCDPortMap[m4] = masterPort;
	VeriNameToVCDPortMap[veriname] = masterPort;
	SymbolToVCDPortMap[m3] = masterPort;
	if (dir == d_input)
	  InPorts.push_back(masterPort);
	else
	  OutPorts.push_back(masterPort);
      }


      size = atoi(m2.c_str());      
      m4 += "[" + m5 + "]";
      if (NameToVCDPortMap[m4] == NULL) {
	w = atoi(m5.c_str());
	subPort = new VCDPort(m4.c_str(), "", 1, dir, m3.c_str());
	subPort->setFrom(w);
	subPort->setTo(w);
	NameToVCDPortMap[m4] = subPort;
	SymbolToVCDPortMap[m3] = subPort;
	masterPort->addSubPort(subPort);
	printf("Set2 Master of subport %s to %s width %d\n", m4.c_str(), masterPort->portName(), masterPort->width());
      }
      printf("var4sub line %d match %s %s %s %s %s %d veriname %s\n", LineNum, m1.c_str(), m2.c_str(), m3.c_str(), m4.c_str(), m5.c_str(), size, veriname.c_str());
    }
    else if (boost::regex_match(lineBuf, matches, varmsub_exp)) {
      printf("Sofar3 %s vcdpath %s\n", sofar.c_str(), vcdPath());
      
      if (!hierarchyMatch(sofar.c_str(), vcdPath()))
	continue;

      printf("Line3 %d: %s\n", LineNum, lineBuf);
      m1 = matches[1];
      m2 = matches[2];
      m3 = matches[3];
      m4 = matches[4];
      m5 = matches[5];
      m6 = matches[6];

      masterPort = NameToVCDPortMap[m4];
      veriname = VCDToInPortMap[m4];
      if (veriname == "") {
	veriname = VCDToOutPortMap[m4];
	dir = d_output;
      }
      else
	dir = d_input;
      
      if (veriname == "") continue;

      if (masterPort == NULL) {
	masterPort = new VCDPort(m4.c_str(), veriname.c_str(), 0, dir, "");
	NameToVCDPortMap[m4] = masterPort; 
	VeriNameToVCDPortMap[veriname] = masterPort;
	SymbolToVCDPortMap[m3] = masterPort;
	if (dir == d_input)
	  InPorts.push_back(masterPort);
	else
	  OutPorts.push_back(masterPort);
      }
      
      size = atoi(m2.c_str());      
      m4 += "[" + m5 + "]";
      if (NameToVCDPortMap[m4] == NULL) {
	int i1 = atoi(m5.c_str());
	int i2 = atoi(m6.c_str());
	w = abs(i2 - i1) + 1;
	subPort = new VCDPort(m4.c_str(), "", w, dir, m3.c_str());
	subPort->setFrom(i1);
	subPort->setTo(i2);
	NameToVCDPortMap[m4] = subPort;
	SymbolToVCDPortMap[m4] = subPort;
	masterPort->addSubPort(subPort);
	printf("Set1 Master of subport %s to %s width %d\n", m4.c_str(), masterPort->portName(), masterPort->width());
      }
      printf("var4msub line %d match %s %s %s %s %s %s %d veriname %s\n", LineNum, m1.c_str(), m2.c_str(), m3.c_str(), m4.c_str(), m5.c_str(), m6.c_str(), size, veriname.c_str());
    }
    // clock tick
    else if (boost::regex_match(lineBuf, matches, clock_exp)) {
      //tick = matches[1];
      //PrevTick = Tick;
      //Tick = atol(tick.c_str());
      
      printf("tick5 match %s\n", m1.c_str());
      
      // Process stimulus/response
      processStimulusResponse();
    }
    // 1 bit data
    else if (boost::regex_match(lineBuf, matches, bitdata_exp)) {
      m1 = matches[1];
      m2 = matches[2];
      printf("bitdata6 match %s %s\n", m1.c_str(), m2.c_str());

      aPort = SymbolToVCDPortMap[m2];
      if (aPort) {
	masterPort = aPort->master();
	if (masterPort == NULL)
	  masterPort = aPort;
	printf("Found aPort %s master %s\n", aPort->portName(), masterPort->portName());
	std::string vcdportname = masterPort->portName();
	std::string portname = masterPort->veriPortName();
	printf("VeriPortName %s\n", portname.c_str());
	if (portname == "") continue;

	printf("before compose1 %s\n", m1.c_str());
	composeMessage(masterPort, aPort, m1);
	printf("after compose1 %s\n", m1.c_str());
	if (aPort->portType()==d_input)
	  InPortsDirty = 1;
	else
	  OutPortsDirty = 1;
      }
      else { // Now do the clock/reset signal
	clkname = SymbolToVCDClockMap[m2];
	if (clkname != "") {
	  if ((VcdClockToClockState[clkname] == false) &&
	      (m1 == "1"))
	    VcdClockToClockTick[clkname]++;
	  if (m1 == "1")
	    VcdClockToClockState[clkname] = true;
	  else
	    VcdClockToClockState[clkname] = false;
	}
	resetname = SymbolToVCDResetMap[m2];
	if (resetname != "") {
	  if (m1 == "1" && VcdResetToResetState[resetname] == false) {
	    clkname = VcdResetToVCDClockMap[resetname];
	    ticknum = VcdClockToClockTick[clkname];
	    //ticknum = atol(tick.c_str());
	    msg = new TBMessage(TBReset, ticknum, resetname, "1", 1);
	    SortedStimulusResponses.push_back(msg);
	    VcdResetToResetState[resetname] = true;
	  }
	  else if (m1 == "0" && VcdResetToResetState[resetname] == true) {
	    clkname = VcdResetToVCDClockMap[resetname];
	    ticknum = VcdClockToClockTick[clkname];
	    //ticknum = atol(tick.c_str());
	    msg = new TBMessage(TBReset, ticknum, resetname, "0", 1);
	    SortedStimulusResponses.push_back(msg);
	    VcdResetToResetState[resetname] = false;
	  }
	  printf("Line %d: %s\n", LineNum, lineBuf);
	  cout << "Found a clock tick " << clkname << " " << m1 << " count " << VcdClockToClockTick[clkname] << endl;
	}
      }
    }
    // n bits data
    else if (boost::regex_match(lineBuf, matches, bindata_exp)) {
      m1 = matches[1];
      m2 = matches[2];
      printf("bindata7 match %s %s\n", m1.c_str(), m2.c_str());
      aPort = SymbolToVCDPortMap[m2];
      if (aPort) {
	masterPort = aPort->master();
	if (masterPort == NULL)
	  masterPort = aPort;
	printf("Line %d: %s\n", LineNum, lineBuf);
	printf("Found aPort %s master %s\n", aPort->portName(), masterPort->portName());
	std::string vcdportname = masterPort->portName();
	std::string portname = masterPort->veriPortName();
	//printf("VeriPortName %s\n", portname.c_str());
	if (portname == "") continue;

	printf("before compose %s\n", m1.c_str());
	composeMessage(masterPort, aPort, m1);
	printf("after compose %s\n", m1.c_str());
	if (aPort->portType()==d_input)
	  InPortsDirty = 1;
	else
	  OutPortsDirty = 1;
      }
    }
    else {

      // nothing
    }
  }

  return stat;
}

void TestBenchGenerator::composeMessage(VCDPort *master, VCDPort *subport,
					std::string &input)
{
  master->initializeMessage();

  if (master == subport && subport->master()==NULL) {
    //printf("Master same as subport %s\n", master->portName());
    master->setMessage(input);
  }
  else {
    //printf("Master NOT same as subport %s\n", master->portName());
    master->setMessageAt(subport->from(), subport->to(), input);
  }
}

bool TestBenchGenerator::hierarchyMatch(const char *s1, const char *s2)
{
  string h1 = s1;
  string h2 = s2;

  if (h1[0] == '/')
    h1 = h1.substr(1);

  if (h2[0] == '/')
    h2 = h2.substr(1);

  std::replace(h1.begin(), h1.end(), '/', '.');
  std::replace(h2.begin(), h2.end(), '/', '.');

  //printf("h1 %s h2 %s\n", h1.c_str(), h2.c_str());

  if (h1 == h2)
    return true;

  //printf("NOT match\n");

  return false;
}

bool TestBenchGenerator::checkVCDPortDefinition(VeriModule *module)
{
  std::map<std::string,std::string>::iterator portmapItr;
  std::string portname;

  ErrMsg = "\n";
  bool stat = true;

  for (portmapItr = InPortToVCDMap.begin(); portmapItr != InPortToVCDMap.end(); portmapItr++) {
    
    portname = portmapItr->second;

    if (NameToVCDPortMap[portname] == NULL) {
      ErrMsg += "ERROR TestBenchGenerator::checkVCDPortDefinition: unable to find input signal ";
      ErrMsg += portmapItr->first;
      ErrMsg += " with given portmap name ";
      ErrMsg += portname;
      ErrMsg += " in the vcd file ";
      ErrMsg += VCDFileName;
      ErrMsg += ".  Please make sure all the input signals for the dut are specified in the vcd file.\n";
      stat = false;
    }
  }

  for (portmapItr = OutPortToVCDMap.begin(); portmapItr != OutPortToVCDMap.end(); portmapItr++) {
    
    portname = portmapItr->second;
    if (NameToVCDPortMap[portname] == NULL) {
      ErrMsg += "ERROR TestBenchGenerator::checkVCDPortDefinition: unable to find output signal";
      ErrMsg += portmapItr->first;
      ErrMsg += " with given portmap name ";
      ErrMsg += portname;
      ErrMsg += " in the vcd file ";
      ErrMsg += VCDFileName;
      ErrMsg += ".\n";
      stat = false;
    }
  }

  ModuleTerminal *terminal;
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  std::string vcdportname;
  char buf[1024];
  
  HdlUtils::createModuleTerminalList(module, mtlist);

  // Input/Output
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    vcdportname = InPortToVCDMap[terminal->m_portName];
    //printf("Hash of port %s is %s\n", terminal->m_portName.c_str(), vcdportname.c_str());
    if (vcdportname == "")
      vcdportname = OutPortToVCDMap[terminal->m_portName];
    if (vcdportname == "") continue;
    VCDPort *p = NameToVCDPortMap[vcdportname];
    //printf("P %s %p\n", vcdportname.c_str(), p);
    if (p && p->master()) p = p->master();
    if (p!=NULL && p->width() != terminal->m_width) {
      ErrMsg += "ERROR TestBenchGenerator::checkVCDPortDefinition: the bits size of the signal ";
      ErrMsg += vcdportname;
      ErrMsg += "[";
      sprintf(buf, "%d", p->width());
      ErrMsg += buf;
      ErrMsg += "] in the vcd file is not equal to the size of port ";
      ErrMsg += terminal->m_portName;
      ErrMsg += "[";
      sprintf(buf, "%d", terminal->m_width);
      ErrMsg += buf;
      ErrMsg += "] in the verilog definition.\n";
      stat = false;
    }
  }

  return stat;
}

void TestBenchGenerator::processStimulusResponse()
{
  TBMessage *msg;
  VCDPortIter vcdPortItr;
  VCDPort *port;
  string clkname, resetname;
  long tick;
  
  if (InPortsDirty) {
    //printf("here1\n");
    for (vcdPortItr = InPorts.begin();
	 vcdPortItr != InPorts.end();
	 vcdPortItr++) {

      port = *vcdPortItr;
      clkname = SceMiClockToVcdClockMap[InPortToSceMiClockMap[port->portName()]];
      tick = VcdClockToClockTick[clkname];
      msg = new TBMessage(TBInput, tick, port->portName(), port->getMsbFirstMessage(),
			  port->width());
      //cout << "Input message " << port->portName() << " clk name " << clkname << " tick " << tick << endl; 
      SortedStimulusResponses.push_back(msg);
    }
  }

  if (OutPortsDirty) {
    //printf("here2\n");
    for (vcdPortItr = OutPorts.begin();
	 vcdPortItr != OutPorts.end();
	 vcdPortItr++) {

      port = *vcdPortItr;
      clkname = SceMiClockToVcdClockMap[OutPortToSceMiClockMap[port->portName()]];
      tick = VcdClockToClockTick[clkname];
      //cerr << "Before Process out message " << port->getMsbFirstMessage() << endl;
      msg = new TBMessage(TBOutput, tick, port->portName(), port->getMsbFirstMessage(),
			  port->width());
      SortedStimulusResponses.push_back(msg);
    }
  }

  InPortsDirty = OutPortsDirty = 0;
}

void TestBenchGenerator::writeStimulus(std::fstream &file, TBMessage *msg)
{
  file << "Input @ " << msg->tick() << ": " << msg->portName() << "(" << msg->bits() << ")" << std::endl;
  //printf("WRITE STIMULUS %s %s\n", portname.c_str(), stimulus.c_str());
}

void TestBenchGenerator::writeResponse(std::fstream &file, TBMessage *msg)
{
  file << "Output @ " << msg->tick() << ": " << msg->portName() << "(" << msg->bits() << ")" << std::endl;
  //printf("WRITE RESPONSE %s %s\n", portname.c_str(), response.c_str());
}

void TestBenchGenerator::writeReset(std::fstream &file, TBMessage *msg)
{
  file << "Reset @ " << msg->tick() << ": " << msg->portName() << "(" << msg->bits() << ")" << std::endl;
  //printf("WRITE RESPONSE %s %s\n", portname.c_str(), response.c_str());
}

void TestBenchGenerator::writeMessage(std::fstream &file, TBMessage *msg)
{
  if (msg->isInput()) {
    //printf("WRITE INPUT %s %s\n", msg->portName().c_str(), msg->bits().c_str());
    file << "Input @ " << msg->tick() << ": " << msg->portName() << "(" << msg->bits()
	 << ")" << std::endl;
    //SR_file << "I: " << msg->portName() << "(" << bitset<200>(msg->bits()).to_ulong()
    //	    << ")" << std::endl;
  }
  else {
    //printf("WRITE RESPONSE %s %s\n", msg->portName().c_str(), msg->bits().c_str());
    file << "Output @ " << msg->tick() << ": " << msg->portName() << "(" << msg->bits()
	 << ")" << std::endl;
    //SR_file << "O: " << msg->portName() << "(" << bitset<200>(msg->bits()).to_ulong()
    //	    << ")" << std::endl;
  }
}

/*
void TestBenchGenerator::writeCapiHFile(VeriModule *module, const char *new_module_name,
					const char *dirname)
{
  std::fstream file;
  string errstring;
  string lower_new_mod_name = new_module_name; 
  std::map<std::string,RdyEnableInterface*>::iterator reiItr;
  RdyEnableInterface *rei;
  string lstr;
  std::map<std::string,RdyEnableInterface*> &RdyEnableIfc = HdlUtils::init()->getRdyEnableIfc();

  if (isupper(new_module_name[0]))
    lower_new_mod_name[0] = tolower(new_module_name[0]);
    
  string filename, realdir;
  realdir = dirname;
  size_t loc = realdir.find_last_of('/');
  if (loc != string::npos)
    realdir = realdir.substr(0, loc+1) + "cpp";
  if (realdir == "")
    filename = "capi.h";
  else
    filename = realdir + "/capi.h";

  printf("Writing %s\n", filename.c_str());

  file.open(filename.c_str(), ios::out);
  if (file.fail()) {
    errstring += "Error HdlUtils::writeCapiHFile(): file ";
    errstring += filename;
    errstring += " cannot be open\n";
    fprintf(stderr, "%s\n", errstring.c_str());
    return 0;
  }


  file << "// Copyright Bluespec Inc. 2012-2013" << endl;
  file << "// By: GenTestbench tool" << endl << endl;
  file << "#pragma once" << endl << endl;
  file << "#include \"capi.h\"" << endl << endl;

  file << "//" << endl;
  file << "// This is Bluespec C API for communicating with the DUT" << endl;
  file << "//" << endl;
  file << "" << endl;
  file << "#ifdef __cplusplus" << endl;
  file << "extern "C" {" << endl;
  file << "#endif" << endl;
  file << "" << endl;
  file << "  // Return true if successful, false if failed" << endl;
  file << "" << endl;
  file << "  // For initialization of testbench including connection to the Dut" << endl;
  file << "  bool semubsv_capi_init(const char *scemi_params_filename);" << endl;
  file << "" << endl;
  file << "  // Simple put" << endl;
  file << "  bool bsv_capi_put(const char *portname, std::string &data);" << endl;
  file << "  bool bsv_capi_put_blocking(const char *portname, std::string &data);" << endl;
  file << "" << endl;
  file << "  // Simple get" << endl;
  file << "  bool bsv_capi_get(const char *portname, std::string &data, SceMiU64 &timestamp);" << endl;
  file << "  bool bsv_capi_get_blocking(const char *portname, std::string &data, SceMiU64 &timestamp);" << endl;
  file << "" << endl;
  file << "  // Advance clock" << endl;
  file << "  bool bsv_capi_advance_clock(unsigned int number_of_edges);" << endl;
  file << "" << endl;
  file << "#ifdef __cplusplus" << endl;
  file << "};" << endl;
  file << "#endif" << endl;
  file << "" << endl;

  file.close();
}
*/

void TestBenchGenerator::writeVcdPlayerHFile(const char *dirname)
{
  string realdir;
  realdir = dirname;
  size_t loc = realdir.find_first_of('/');
  if (loc != string::npos)
    realdir = realdir.substr(0, loc+1) + "cpp/";
  string filename = realdir;

  const char *new_module_name = bsvModuleName();
  filename += "VcdPlayer.h";

  if (CppOutputDir != "")
    filename = CppOutputDir + "/" + filename;

  CAPI_file.open(filename.c_str(), ios::out);
  if (CAPI_file.fail()) {
    std::cerr << "ERROR TestBenchGenerator::writeVcdPlayerHFile(): unable to open file "
	      << "VcdPlayer.h"
              << "." << std::endl;
    return;
  }

  printf("Writing %s\n", filename.c_str());

  CAPI_file << "// Copyright Bluespec Inc. 2012-2013" << endl;
  CAPI_file << "// By: GenTestbench tool" << endl << endl;
  CAPI_file << "#pragma once" << endl << endl;
  CAPI_file << "#include <iostream>" << endl;
  CAPI_file << "#include <stdexcept>" << endl;
  CAPI_file << "#include <string>" << endl;
  CAPI_file << "#include <cstdlib>" << endl;
  CAPI_file << "#include <cstring>" << endl << endl;
  CAPI_file << "#include \"tcl.h\"" << endl;
  CAPI_file << "#include \"" << new_module_name << "Xactor.h\"" << endl;
  CAPI_file << "#include \"VcdUtils.h\"" << endl;
  CAPI_file << endl;
  CAPI_file << "// Bluespec common code" << endl;
  CAPI_file << "#include \"bsdebug_common.h\"" << endl;
  CAPI_file << endl;
  CAPI_file << "using namespace std;" << endl;
  CAPI_file << endl;
  CAPI_file << "//" << endl;
  CAPI_file << "// This is Bluespec C API for communicating with the DUT" << endl;
  CAPI_file << "//" << endl;
  CAPI_file << endl;
  CAPI_file << "struct BsCapiMessageData {" << endl;
  CAPI_file << "  char *m_data;" << endl;
  CAPI_file << "  unsigned int m_data_size;" << endl;
  CAPI_file << "  SceMiU64 m_timestamp;" << endl;
  CAPI_file << "" << endl;
  CAPI_file << "  BsCapiMessageData() { m_data = NULL; m_data_size = 0; m_timestamp = 0; }" << endl;
  CAPI_file << "  ~BsCapiMessageData() { delete m_data; }" << endl;
  CAPI_file << "};" << endl;
  CAPI_file << endl;
  CAPI_file << "// Forward declare VcdPlayer class" << endl;
  CAPI_file << "class VcdPlayer;" << endl << endl;
  CAPI_file << "// static extension global data" << endl;
  CAPI_file << "class SceMiGlobalData {" << endl;
  CAPI_file << "public:" << endl;
  CAPI_file << "  bool                  m_initialized;" << endl;
  CAPI_file << "  SceMi                 * m_scemi;" << endl;
  CAPI_file << "  " << new_module_name << "Xactor       * m_dutX;" << endl;
  CAPI_file << "  SceMiServiceThread    * m_serviceThread;" << endl;
  CAPI_file << "  SimulationControl     * m_simControl;" << endl;
  CAPI_file << "  ProbesXactor          * m_probeControl;" << endl;
  CAPI_file << "  SceMiU64                m_dut_timestamp;" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Simple initializer invoked when the extension is loaded" << endl;
  CAPI_file << "  SceMiGlobalData ()" << endl;
  CAPI_file << "    : m_initialized(false)" << endl;
  CAPI_file << "    , m_scemi(0)" << endl;
  CAPI_file << "    , m_dutX(0)" << endl;
  CAPI_file << "    , m_serviceThread(0)" << endl;
  CAPI_file << "    , m_simControl(0)" << endl;
  CAPI_file << "    , m_probeControl(0)" << endl;
  CAPI_file << "    , m_dut_timestamp(0)" << endl;
  CAPI_file << "  {}" << endl;
  CAPI_file << endl;
  CAPI_file << "  ~SceMiGlobalData ()" << endl;
  CAPI_file << "  {" << endl;
  CAPI_file << "    if (m_initialized) {" << endl;
  CAPI_file << "      destroy();" << endl;
  CAPI_file << "    }" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Initialization -- call from bsdebug::scemi init <param>" << endl;
  CAPI_file << "  static SceMiGlobalData *init (const char *paramfile);" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Destruction -- called from bsdebug::scemi delete" << endl;
  CAPI_file << "  void destroy();" << endl;
  CAPI_file << endl;
  CAPI_file << "};" << endl;
  CAPI_file << endl;
  CAPI_file << "class VcdPlayerServiceThread : public Thread" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << " private:" << endl;
  CAPI_file << "  bool       m_looprunning;" << endl;
  CAPI_file << "  bool       m_stopping;" << endl;
  CAPI_file << "  VcdPlayer  *m_player;" << endl;
  CAPI_file << "  " << endl;
  CAPI_file << "  static unsigned m_thread_count;" << endl;
  CAPI_file << "  " << endl;
  CAPI_file << " public:" << endl;
  CAPI_file << "  // Constructor registers the scemi pointer and starts the SceMi" << endl;
  CAPI_file << "  // service loop in a separate thread" << endl;
  CAPI_file << "  VcdPlayerServiceThread(VcdPlayer *player)" << endl;
  CAPI_file << "    : Thread ()" << endl;
  CAPI_file << "    , m_player(player)" << endl;
  CAPI_file << "  {" << endl;
  CAPI_file << "    if (m_thread_count > 0) {" << endl;
  CAPI_file << "      std::cerr << \"VcdPlayerServiceThread new(): instantiating more than one thread is not allowed\"" << endl;
  CAPI_file << "		<< std::endl;" << endl;
  CAPI_file << "      exit(1);" << endl;
  CAPI_file << "    }" << endl;
  CAPI_file << "    m_thread_count++;" << endl;
  CAPI_file << "    start();" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << " private:" << endl;
  CAPI_file << "  VcdPlayerServiceThread( const VcdPlayerServiceThread &);" << endl;
  CAPI_file << endl;
  CAPI_file << " public:" << endl;
  CAPI_file << "  VcdPlayerServiceThread() {}" << endl;
  CAPI_file << "  ~VcdPlayerServiceThread() {" << endl;
  CAPI_file << "    if (m_running) {" << endl;
  CAPI_file << "      stop();" << endl;
  CAPI_file << "      join();" << endl;
  CAPI_file << "    }" << endl;
  CAPI_file << "    m_running = false;" << endl;
  CAPI_file << "  };" << endl;
  CAPI_file << endl;
  CAPI_file << " protected:" << endl;
  CAPI_file << "  virtual void  main();" << endl;
  CAPI_file << "};" << endl << endl;
  CAPI_file << "// static extension global data" << endl;
  CAPI_file << "class VcdPlayer {" << endl;
  CAPI_file << endl;
  CAPI_file << "  string DataFileName;" << endl;
  CAPI_file << "  ifstream File;" << endl;
  CAPI_file << "  unsigned LineNum;" << endl;
  CAPI_file << endl;
  CAPI_file << "  char LineBuf[1023];" << endl;
  CAPI_file << "  char *Token;" << endl;
  CAPI_file << "  char Buf[1024];" << endl;
  CAPI_file << "  const char *Delimiter;" << endl;
  CAPI_file << endl;
  CAPI_file << "  SceMiGlobalData *SGlobal;" << endl;
  CAPI_file << endl;
  CAPI_file << "  VcdPlayerServiceThread *ServiceThread;" << endl;
  CAPI_file << "  VcdUtils *Utils;" << endl;
  CAPI_file << "  std::fstream Res_file;" << endl << endl;
  CAPI_file << "  TBMessageSet TBMessages;" << endl << endl;
  CAPI_file << "  VcdPlayer(); // protected from being called" << endl;
  CAPI_file << endl;
  CAPI_file << "public:" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Constructor" << endl;
  CAPI_file << "  VcdPlayer(const char *filename);" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Destructor" << endl;
  CAPI_file << "  ~VcdPlayer();" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Play the next input in the vcd file" << endl;
  CAPI_file << "  bool playNextInput();" << endl;
  CAPI_file << endl;
  CAPI_file << "  // True if it is at the end of vcd file" << endl;
  CAPI_file << "  bool finish() { return (File.eof()); }" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Play to the end without stopping." << endl;
  CAPI_file << "  bool play();" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Assert/Deassert reset" << endl;
  CAPI_file << "  void assertReset(bool state);" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Advance Dut clock by the given delta" << endl;
  CAPI_file << "  void advanceClock(unsigned int delta);" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Return the current Dut clock" << endl;
  CAPI_file << "  SceMiU64 getTimeStamp();" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Verify output" << endl;
  CAPI_file << "  bool verifyOutputVcd(const char *vcdfile);" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Service loop for reading outputs" << endl;
  CAPI_file << "  int ServiceLoop();" << endl;
  CAPI_file << "};" << endl;
  CAPI_file << endl;

  CAPI_file.close();
}

void TestBenchGenerator::writeVcdPlayerCFile(const char *dirname)
{
  string realdir;
  realdir = dirname;
  size_t loc = realdir.find_first_of('/');
  if (loc != string::npos)
    realdir = realdir.substr(0, loc+1) + "cpp/";
  string filename = realdir;

  const char *new_module_name = bsvModuleName();
  filename += "VcdPlayer.cpp";

  if (CppOutputDir != "")
    filename = CppOutputDir + "/" + filename;

  
  CAPI_file.open(filename.c_str(), ios::out);
  if (CAPI_file.fail()) {
    std::cerr << "ERROR TestBenchGenerator::writeVcdPlayerCFile(): unable to open file "
	      << "VcdPlayer.cpp"
              << "." << std::endl;
    return;
  }

  ModuleTerminal *terminal;
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  std::string foundpath;
  //VeriModule *module = HdlUtils::findModuleFromPath(NULL, verilogPath(), foundpath);
  VeriModule *module = HdlUtils::findModuleByName(moduleName());
  //cout << "Verilog path: " << verilogPath() << " module " << module->Name() << endl;
  if (module == NULL) {
    ErrMsg = "No object3 found with name '" + string(moduleName()) + "'";
    return;  }
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  //cout << "Map pointer5 " << (void*)map << endl;

  //cout << "Get map of clk: " << (*map)["CLK"] << endl;
  
  HdlUtils::createModuleTerminalList(module, mtlist);

  printf("Writing %s\n", filename.c_str());

  CAPI_file << "// Copyright Bluespec Inc. 2010-2011" << endl;
  CAPI_file << "// By: GenTestbench tool" << endl;
  CAPI_file << endl;
  CAPI_file << "#include <sched.h>" << endl;
  CAPI_file << "#include <stdio.h>" << endl;
  CAPI_file << "#include <stdlib.h>" << endl;
  CAPI_file << "#include <fstream>" << endl << endl;
  CAPI_file << "#include <boost/regex.hpp>" << endl;
  CAPI_file << "#include <boost/lexical_cast.hpp>" << endl;
  CAPI_file << endl;
  CAPI_file << "#include \"VcdPlayer.h\"" << endl;
  CAPI_file << "#include \"capi.h\"" << endl << endl;
  CAPI_file << "unsigned VcdPlayerServiceThread::m_thread_count = 0;" << endl;
  CAPI_file << endl;
  CAPI_file << "SceMiGlobalData *the_scemi_global_data = NULL;" << endl;
  CAPI_file << endl;
  CAPI_file << "#define MAX_LINE_SIZE 1023" << endl;
  CAPI_file << endl;
  CAPI_file << "void VcdPlayerServiceThread::main() {" << endl;
  CAPI_file << "  while( ! m_stop ) {" << endl;
  CAPI_file << "    if (0 == m_player->ServiceLoop()) {" << endl;
  CAPI_file << "      sched_yield();" << endl;
  CAPI_file << "    }" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << "  m_running = false;" << endl;
  CAPI_file << "  std::cerr << \"VcdPlayer Service thread finished!\" << std::endl;" << endl;
  CAPI_file << "}" << endl << endl;
  CAPI_file << "// Initialization -- call from bsdebug::scemi init <param>" << endl;
  CAPI_file << "SceMiGlobalData *SceMiGlobalData::init (const char *paramfile) {" << endl;
  CAPI_file << endl;
  CAPI_file << "  if (the_scemi_global_data && the_scemi_global_data->m_initialized) throw std::runtime_error (\"scemi is already initialized\");" << endl << endl;
  CAPI_file << "  the_scemi_global_data = new SceMiGlobalData();" << endl << endl;
  CAPI_file << "  // Initialize SceMi" << endl;
  CAPI_file << "  int sceMiVersion = SceMi::Version( SCEMI_VERSION_STRING );" << endl;
  CAPI_file << "  SceMiParameters params( paramfile );" << endl;
  CAPI_file << "  the_scemi_global_data->m_scemi = SceMi::Init( sceMiVersion, & params );" << endl;
  CAPI_file << "  if (! the_scemi_global_data->m_scemi) throw std::runtime_error (\"Could not initialize SceMi\");" << endl;
  CAPI_file << endl;
  CAPI_file << "  // initialize ProbesXactor          * probeControl;" << endl;
  CAPI_file << "  //the_scemi_global_data->m_probeControl = ProbesXactor::init(\"\", \"scemi_dut_prb_control\", NULL, the_scemi_global_data->m_scemi);" << endl;
  CAPI_file << endl;
  CAPI_file << "  /* initiate the SCE-MI transactors and threads */" << endl;
  CAPI_file << "  // Create the transactor" << endl;
  CAPI_file << "  the_scemi_global_data->m_dutX = " << new_module_name << "Xactor::init(the_scemi_global_data->m_scemi);" << endl;
  CAPI_file << endl;
  CAPI_file << "  the_scemi_global_data->m_simControl  = new ::SimulationControl (\"\", \"scemi_simControl\", the_scemi_global_data->m_scemi);" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Start a SceMiService thread;" << endl;
  CAPI_file << "  the_scemi_global_data->m_serviceThread = new SceMiServiceThread(the_scemi_global_data->m_scemi);" << endl;
  CAPI_file << endl;
  CAPI_file << "  the_scemi_global_data->m_initialized = true;" << endl << endl;
  CAPI_file << "  // Set dut timestamp" << endl;
  CAPI_file << "  semu_get_current_controlled_clock_cycle(the_scemi_global_data->m_dut_timestamp);" << endl << endl;
  CAPI_file << "  return the_scemi_global_data;" << endl;
  CAPI_file << "}" << endl;
  CAPI_file << endl;
  CAPI_file << "// Destruction -- called from bsdebug::scemi delete" << endl;
  CAPI_file << "void SceMiGlobalData::destroy()" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  m_initialized = false;" << endl;
  CAPI_file << "  // Stop the simulation side" << endl;
  CAPI_file << "  if (m_dutX) m_dutX->shutdown();" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Stop and join with the service thread, then shut down scemi --" << endl;
  CAPI_file << "  if (m_serviceThread) {" << endl;
  CAPI_file << "    m_serviceThread->stop();" << endl;
  CAPI_file << "    m_serviceThread->join();" << endl;
  CAPI_file << "    delete m_serviceThread;  m_serviceThread = 0;" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Delete the simulation control" << endl;
  CAPI_file << "  delete m_simControl; m_simControl = 0;" << endl;
  CAPI_file << endl;
  CAPI_file << "  //Delete the Dut" << endl;
  CAPI_file << "  m_dutX->destroy();" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Shutdown the probes transactor" << endl;
  CAPI_file << "  ProbesXactor::shutdown();" << endl;
  CAPI_file << endl;
  CAPI_file << "  // Shutdown SceMi" << endl;
  CAPI_file << "  if (m_scemi) {" << endl;
  CAPI_file << "    SceMi::Shutdown(m_scemi);" << endl;
  CAPI_file << "    m_scemi = 0;" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << "}" << endl;
  CAPI_file << endl;
  CAPI_file << "// Constructor" << endl;
  CAPI_file << "VcdPlayer::VcdPlayer(const char *filename)" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  DataFileName = filename;" << endl;
  CAPI_file << "  File.open(filename);" << endl;
  CAPI_file << "  if (File.fail()) {" << endl;
  CAPI_file << "    cerr << \"ERROR VcdPlayer::VcdPlayer(): unable to open data file \";" << endl;
  CAPI_file << "    cerr << filename;" << endl;
  CAPI_file << "    cerr << \".\" << endl;" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << "  LineNum = 0;" << endl;
  CAPI_file << endl;
  CAPI_file << "  Delimiter = \" \\n\\t,:[]'()\";" << endl;
  CAPI_file << "  SGlobal = SceMiGlobalData::init(\"mkBridge.params\");" << endl;
  CAPI_file << "  //SGlobal->m_probeControl->enableAll();" << endl;
  CAPI_file << endl;
  CAPI_file << "  semu_get_current_controlled_clock_cycle(the_scemi_global_data->m_dut_timestamp);" << endl << endl;
  CAPI_file << "  Utils = new VcdUtils();" << endl;
  CAPI_file << "" << endl;
  CAPI_file << "  Res_file.open(\"responses.dat\", ios::out);" << endl;
  CAPI_file << "  if (Res_file.fail()) {" << endl;
  CAPI_file << "    std::cerr << \"ERROR VcdPlayer::VcdPlayer(): unable to open file \"" << endl;
  CAPI_file << "	      << \"responses.dat\"" << endl;
  CAPI_file << "              << \".\" << std::endl;" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << "" << endl;
  CAPI_file << "  ServiceThread = new VcdPlayerServiceThread(this);" << endl;
  CAPI_file << "}" << endl;
  CAPI_file << endl;
  CAPI_file << "// Destructor" << endl;
  CAPI_file << "VcdPlayer::~VcdPlayer()" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  // Stop and join with the service thread" << endl;
  CAPI_file << "  if (ServiceThread) {" << endl;
  CAPI_file << "    ServiceThread->stop();" << endl;
  CAPI_file << "    ServiceThread->join();" << endl;
  CAPI_file << "    delete ServiceThread;  ServiceThread = 0;" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << "" << endl;
  CAPI_file << "  // Delete VcdUtils" << endl;
  CAPI_file << "  delete Utils;" << endl;
  CAPI_file << "" << endl;
  CAPI_file << "  // Close responses.dat file" << endl;
  CAPI_file << "  Res_file.close();" << endl;
  CAPI_file << "" << endl;
  CAPI_file << "  // Delete globals and scemi" << endl;
  CAPI_file << "  delete SGlobal;" << endl;
  CAPI_file << "}" << endl << endl;
  CAPI_file << "bool VcdPlayer::playNextInput()" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  string tick;" << endl;
  CAPI_file << "  string portname;" << endl;
  CAPI_file << "  string data;" << endl;
  CAPI_file << "  unsigned ticknum;" << endl;
  CAPI_file << "  int delta;" << endl;
  CAPI_file << endl;
  CAPI_file << "  if (finish()) return false;" << endl;
  CAPI_file << endl;
  CAPI_file << "  File.getline(LineBuf, MAX_LINE_SIZE);" << endl;
  CAPI_file << "  LineNum++;" << endl;
  CAPI_file << "  Token = strtok(LineBuf, Delimiter);" << endl;
  CAPI_file << "  if (Token && (strncmp(Token, \"//\", 2) != 0)) { // Check comment" << endl;
  CAPI_file << "    if (!strcmp(Token, \"Input\")) {" << endl;
  CAPI_file << "      Token = strtok(0, Delimiter);" << endl;
  CAPI_file << "      if (*Token == '@') {" << endl;
  CAPI_file << "        Token = strtok(0, Delimiter);" << endl;
  CAPI_file << "        if (Token == NULL) return false;" << endl;
  CAPI_file << endl;
  CAPI_file << "	// Move the dut clock by to the found tick" << endl;
  CAPI_file << "        tick = Token;" << endl;
  CAPI_file << "        ticknum = atoi(tick.c_str());" << endl;
  CAPI_file << "        delta = ticknum - SGlobal->m_dut_timestamp - 1;" << endl;
  CAPI_file << "        if (delta < 0 && ticknum != 0) {" << endl;
  CAPI_file << "          cerr << \"Something wrong at line \" << LineNum" << endl;
  CAPI_file << "            << \" of input file \" << DataFileName" << endl;
  CAPI_file << "            << \". The time for the input \" << ticknum" << endl;
  CAPI_file << "            << \" is before the current simulation time\" << SGlobal->m_dut_timestamp" << endl;
  CAPI_file << "            << \".\" << endl;" << endl;
  CAPI_file << "          return false;" << endl;
  CAPI_file << "        }" << endl;
  CAPI_file << "        else if (delta > 0) {" << endl;
  CAPI_file << "          advanceClock(delta);" << endl;
  CAPI_file << "        }" << endl;
  CAPI_file << "        Token = strtok(0, Delimiter);" << endl;
  CAPI_file << "        portname = Token;" << endl;
  CAPI_file << "        Token = strtok(0, Delimiter);" << endl;
  CAPI_file << "        if (Token == NULL) return false;" << endl;
  CAPI_file << "        data = Token;" << endl << endl;
  CAPI_file << "        // Got an input and send the request" << endl;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((*map)[terminal->m_portName] == "")
      if (terminal->m_dir == d_input) {
	CAPI_file << "        BitT<" << terminal->m_width << "> data_bit_" << terminal->m_portName
		  << ";" << endl;
      }
  }
  int first = 1;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((*map)[terminal->m_portName] == "")
      if (terminal->m_dir == d_input) {
	if (first)
	  CAPI_file << "        if ";
	else
	  CAPI_file << "        else if ";
	CAPI_file << "(portname == \"" << terminal->m_portName << "\") {" << endl;
	CAPI_file << "            data_bit_" << terminal->m_portName << " = atoi(data.c_str());" << endl;
	CAPI_file << "            SGlobal->m_dutX->sendB_" << terminal->m_portName << "(data_bit_"
		  << terminal->m_portName << ");" << endl;
	CAPI_file << "          }" << endl;
      }
  }
  CAPI_file << "        else return false;" << endl;
  CAPI_file << "      }" << endl;
  CAPI_file << "    }" << endl;
  CAPI_file << "    else if (!strcmp(Token, \"Reset\")) {" << endl;
  CAPI_file << "      Token = strtok(0, Delimiter);" << endl;
  CAPI_file << "      if (*Token == '@') {" << endl;
  CAPI_file << "        Token = strtok(0, Delimiter);" << endl;
  CAPI_file << "        if (Token == NULL) return false;" << endl;
  CAPI_file << endl;
  CAPI_file << "	// Move the dut clock by to the found tick" << endl;
  CAPI_file << "        tick = Token;" << endl;
  CAPI_file << "	ticknum = atoi(tick.c_str());" << endl;
  CAPI_file << "	delta = ticknum - SGlobal->m_dut_timestamp - 1;" << endl;
  CAPI_file << "	if (delta < 0 && ticknum != 0) {" << endl;
  CAPI_file << "	  cerr << \"Something wrong at line \" << LineNum" << endl;
  CAPI_file << "	       << \" of input file \" << DataFileName" << endl;
  CAPI_file << "	       << \". The time for the input \" << ticknum" << endl;
  CAPI_file << "	       << \" is before the current simulation time\" << SGlobal->m_dut_timestamp" << endl;
  CAPI_file << "	       << \".\" << endl;" << endl;
  CAPI_file << "	  return false;" << endl;
  CAPI_file << "	}" << endl;
  CAPI_file << "	else if (delta > 0) {" << endl;
  CAPI_file << "	  advanceClock(delta);" << endl;
  CAPI_file << "	}" << endl;
  CAPI_file << "        Token = strtok(0, Delimiter);" << endl;
  CAPI_file << "        portname = Token;" << endl;
  CAPI_file << "        Token = strtok(0, Delimiter);" << endl;
  CAPI_file << "        if (Token == NULL) return false;" << endl;
  CAPI_file << "        data = Token;" << endl << endl;
  CAPI_file << "        // Got an input and send the request" << endl;
  CAPI_file << "        SGlobal->m_dutX->assertReset(data[0] == '1');" << endl;
  CAPI_file << "      }" << endl;
  CAPI_file << "    }" << endl;
  CAPI_file << "    else if (!strcmp(Token, \"Output\")) {" << endl;
  CAPI_file << "      return true;" << endl;
  CAPI_file << "    }" << endl;
  CAPI_file << "    else return false;" << endl;
  CAPI_file << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << endl;
  CAPI_file << "  return true;" << endl;
  CAPI_file << "}" << endl;
  CAPI_file << endl;
  CAPI_file << "bool VcdPlayer::play()" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  bool stat = true;" << endl;
  CAPI_file << endl;
  CAPI_file << "  while (!finish()) {" << endl;
  CAPI_file << " " << endl;
  CAPI_file << "    stat &= playNextInput();" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << endl;
  CAPI_file << "  advanceClock(100);" << endl << endl;
  CAPI_file << "  return stat;" << endl;
  CAPI_file << "}" << endl;
  CAPI_file << endl;
  CAPI_file << "void VcdPlayer::advanceClock(unsigned int delta)" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  semu_advance_controlled_clockB(delta);" << endl;
  CAPI_file << "}" << endl;
  CAPI_file << endl;
  CAPI_file << "void VcdPlayer::assertReset(bool state)" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  SGlobal->m_dutX->assertReset(state);" << endl;
  CAPI_file << "}" << endl << endl;
  CAPI_file << "bool VcdPlayer::verifyOutputVcd(const char *vcdfile)" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  int stat = Utils->readSpecFile(\"testbench.spec\");" << endl;
  CAPI_file << "  if (stat == 0)" << endl;
  CAPI_file << "    return false;" << endl;
  CAPI_file << "" << endl;
  CAPI_file << "  StimulusRespList stimList, respList;" << endl;
  CAPI_file << "  Utils->createMessageList(\"stimulus.dat\", stimList);" << endl;
  CAPI_file << "  Utils->createMessageList(\"responses.dat\", respList);" << endl;
  CAPI_file << "  Utils->compareOutputs(stimList, respList);" << endl;
  CAPI_file << "}" << endl << endl;
  CAPI_file << "// Service loop for reading outputs from the dut" << endl;
  CAPI_file << "int VcdPlayer::ServiceLoop()" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  int gotone, stat, write_prev_tick;" << endl;
  CAPI_file << "  std::stringstream stream;" << endl;
  CAPI_file << "  std::string data;" << endl;
  CAPI_file << "  TBMessageIter msgItr;" << endl;
  CAPI_file << "  TBMessage *msg;" << endl;
  CAPI_file << "  SceMiU64 prevtstamp, tstamp;" << endl;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((*map)[terminal->m_portName] == "")
      if (terminal->m_dir == d_output) {
	//CAPI_file << "  S" << new_module_name << "_" << terminal->m_portName << " "
	CAPI_file << "  " << new_module_name << "_" << terminal->m_portName << " "
		  << terminal->m_portName << "_resp;" << endl;
      }
  }
  CAPI_file << endl;
  CAPI_file << "  msgItr = TBMessages.begin();" << endl;
  CAPI_file << "  if (msgItr != TBMessages.end())" << endl;
  CAPI_file << "    prevtstamp = (*msgItr)->tick();" << endl;
  CAPI_file << "  else prevtstamp = numeric_limits<long>::max();" << endl;
  CAPI_file << "  stat = 0;" << endl;
  CAPI_file << "  write_prev_tick = 0;" << endl << endl;
  for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
    terminal = &(*mtItr);
    if ((*map)[terminal->m_portName] == "")
      if (terminal->m_dir == d_output) {
	CAPI_file << "  gotone = SGlobal->m_dutX->receiveB_" << terminal->m_portName
		  << "(" << terminal->m_portName << "_resp);" << endl;
	CAPI_file << "  if (gotone) {" << endl;
	CAPI_file << "    semu_get_current_controlled_clock_cycle(tstamp);" << endl;
	CAPI_file << "    if (tstamp > prevtstamp)" << endl;
	CAPI_file << "      write_prev_tick = 1;" << endl;
	CAPI_file << "    stream.str(\"\");" << endl;
	CAPI_file << "    stream.clear();" << endl;
	CAPI_file << "    data = \"\";" << endl;
	CAPI_file << "    stream << " << terminal->m_portName << "_resp;" << endl;
	CAPI_file << "    stream >> data;" << endl;
	CAPI_file << "    msg = new TBMessage(false, tstamp, \"" << terminal->m_portName 
		  << "\", data, 0);" << endl;
	CAPI_file << "    TBMessages.insert(msg);" << endl;

	CAPI_file << "    stat += 1;" << endl;
	CAPI_file << "  }" << endl;
	CAPI_file << "" << endl;
      }
  }
  CAPI_file << "  if (write_prev_tick) {" << endl;
  
  CAPI_file << "    for (msgItr = TBMessages.begin(); msgItr != TBMessages.end(); msgItr++) {" << endl;
  CAPI_file << "      msg = *msgItr;" << endl;

  CAPI_file << "      if (msg->tick() > prevtstamp) break;" << endl;
  CAPI_file << "      Res_file << \"Output @ \";" << endl;
  CAPI_file << "      Res_file << std::dec << msg->tick();" << endl; 
  CAPI_file << "      Res_file << \": \" << msg->portName() << \"(\" << msg->bits() << \")\""
	    << " << std::endl;" << endl;
  CAPI_file << "      delete msg;" << endl;
  CAPI_file << "    }" << endl;
  CAPI_file << "    TBMessages.erase(TBMessages.begin(), msgItr);" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << "  return stat;" << endl;
  CAPI_file << "}" << endl;
  CAPI_file << endl;

  CAPI_file.close();

  filename = CppOutputDir;
  if ((filename[filename.size()] != '/') && (filename != ""))
    filename += "/";
  filename += "playvcd.cpp";

  printf("Writing %s\n", filename.c_str());

  CAPI_file.open(filename.c_str(), ios::out);
  if (CAPI_file.fail()) {
    std::cerr << "ERROR TestBenchGenerator::writeVcdPlayerCFile(): unable to open file "
	      << filename
              << "." << std::endl;
    return;
  }

  CAPI_file << "// Copyright Bluespec Inc. 2010-2011" << endl;
  CAPI_file << "// By: GenTestbench tool" << endl;
  CAPI_file << "" << endl;
  CAPI_file << "#include \"VcdPlayer.h\"" << endl;
  CAPI_file << endl;
  CAPI_file << "// Main" << endl;
  CAPI_file << "int main (int argc , char *argv[])" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  VcdPlayer player(\"stimulus.dat\");" << endl;
  CAPI_file << endl;
  CAPI_file << "  player.play();" << endl;
  CAPI_file << endl;
  CAPI_file << "  player.verifyOutputVcd(\"scemi_test.vcd\");" << endl;
  CAPI_file << "}" << endl;

  CAPI_file.close();
}

void TestBenchGenerator::writeDummyInputsDueToLatency(VeriModule *module)
{
  ModuleTerminalList mtlist;
  ModuleTerminalIterator mtItr;
  ModuleTerminal *terminal;
  
  HdlUtils::createModuleTerminalList(module, mtlist);
  std::map<string, string> *map = &HdlUtils::getControlSignalsMap();
  TBMessage *fake_input;
  //TBMessage *fake_output;
  
  CAPI_file << endl;
  CAPI_file << "  // More inputs to account for the latency" << endl;
  for (int i=1; i<SceMiLatency; i++) {
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      
      terminal = &(*mtItr);
      if ((*map)[terminal->m_portName] == "")
	if (terminal->m_dir == d_input) {
	  fake_input = new TBMessage(TBInput, i, terminal->m_portName, "0", terminal->m_width);
	  writeCAPI(fake_input);
	}
    }
    /*
    for (mtItr = mtlist.begin(); mtItr != mtlist.end(); mtItr++) {
      
      terminal = &(*mtItr);
      if ((*map)[terminal->m_portName] == "")
	if (terminal->m_dir == d_output) {
	  fake_output = new TBMessage(TBOutput, i, terminal->m_portName, "0", terminal->m_width);
	  writeCAPI(fake_output);
	}
    }
    */
    if (SceMiLatencyCount > 0)
      SceMiLatencyCount--;
    //printf("latencycount = %d\n", SceMiLatencyCount);
  }
}

void TestBenchGenerator::writePlayVcdFile()
{
  string filename = "playvcd.cpp";

  if (CppOutputDir != "")
    filename = CppOutputDir + "/" + filename;

  CAPI_file.open(filename.c_str(), ios::out);
  if (CAPI_file.fail()) {
    std::cerr << "ERROR TestBenchGenerator::writePlayVcdFile(): unable to open file "
	      << "playvcd.cpp"
              << "." << std::endl;
    return;
  }

  CAPI_file << "// Copyright Bluespec Inc. 2010-2011" << endl;
  CAPI_file << "// By: GenTestbench tool" << endl;
  CAPI_file << endl;
  CAPI_file << "#include \"VcdPlayer.h\"" << endl;
  CAPI_file << endl;
  CAPI_file << "// Main" << endl;
  CAPI_file << "int main (int argc , char *argv[])" << endl;
  CAPI_file << "{" << endl;
  CAPI_file << "  VcdPlayer player(\"stimulus.dat\");" << endl;
  CAPI_file << endl;
  CAPI_file << "  while (!player.finish()) {" << endl;
  CAPI_file << endl;
  CAPI_file << "    player.playNextInput();" << endl;
  CAPI_file << "  }" << endl;
  CAPI_file << "}" << endl;
  CAPI_file << endl;

  CAPI_file.close();
}

void TestBenchGenerator::writeCAPI(TBMessage *msg)
{
  TBMessage *thismsg;

  if (msg->isInput()) {
    
    CAPI_file << "  pglobal->m_dutX->" << msg->portName() << "_sendB(\""
	      << msg->bits() << "\");" << endl;
  }
  else {
    if (SceMiLatencyCount == 0 && DelayedOutMsgs.size() == 0) {
      CAPI_file << "  gotit = pglobal->m_dutX->" << msg->portName() << "_get("
		<< msg->portName() << "_bits);" << endl;
      CAPI_file << "  if (gotit) cout << \"@\" << \"" << msg->tick() << "\" << \" response: " 
		<< msg->portName() << " = \" << " << msg->portName() << "_bits << endl;" << endl;
    }
    else if (SceMiLatencyCount == 0) {
      DelayedOutMsgs.push_back(msg);
      thismsg = DelayedOutMsgs.front();
      DelayedOutMsgs.pop_front();
      CAPI_file << "  gotit = pglobal->m_dutX->" << thismsg->portName() << "_get("
		<< thismsg->portName() << "_bits);" << endl;
      CAPI_file << "  if (gotit) cout << \"@\" << \"" << thismsg->tick() << "\" << \" response: " 
		<< thismsg->portName() << " = \" << " << thismsg->portName() << "_bits << endl;" << endl;
      delete thismsg;
    }
    else {
      DelayedOutMsgs.push_back(msg);
      CAPI_file << "  gotit = pglobal->m_dutX->" << msg->portName() << "_get("
		<< msg->portName() << "_bits);" << endl;
      CAPI_file << "  if (gotit) cout << \"@\" << \"" << msg->tick() << "\" << \" response: " 
		<< msg->portName() << " = \" << " << msg->portName() << "_bits << endl;" << endl;
      //printf("PUSHING tick %ld\n", msg->tick());
    }
  }
}

void TestBenchGenerator::getSceMiClockName(std::string &name)
{
  std::map<std::string,std::string>::iterator itr;

  itr = VcdClockToSceMiClockMap.begin();
  if (itr != VcdClockToSceMiClockMap.end())
    name = itr->second;
}

void TestBenchGenerator::getDutClockName(std::string &name)
{
  std::map<std::string,std::string>::iterator itr;

  itr = VcdClockToSceMiClockMap.begin();
  if (itr != VcdClockToSceMiClockMap.end())
    name = itr->first;
}

void TestBenchGenerator::getSceMiResetName(std::string &name)
{
  std::map<std::string,std::string>::iterator itr;

  itr = VcdResetToSceMiResetMap.begin();
  if (itr != VcdResetToSceMiResetMap.end())
    name = itr->second;
}

void TestBenchGenerator::getDutResetName(std::string &name)
{
  std::map<std::string,std::string>::iterator itr;

  itr = VcdResetToSceMiResetMap.begin();
  if (itr != VcdResetToSceMiResetMap.end())
    name = itr->first;
}
