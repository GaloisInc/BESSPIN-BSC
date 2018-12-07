// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <map>
#include <list>
#include <iostream>
#include <fstream>
#include <vector>
#include "string.h"
#include "Types.h"
#include "VeriModule.h"
#include "HdlUtils.h"

#ifdef VERIFIC_NAMESPACE
using namespace Verific ; // start using Verific namespace
#endif

/* TestBenchGenerator
 *
 * Generate c++ testbench code for simple test and
 * verification of a dut. 
 *
 */

enum TBMessageType { TBInput, TBOutput, TBClock, TBReset };

struct TBMessage {
  TBMessageType Type;
  bool Checked;
  long Tick;
  std::string PortName;
  std::string Bits;
  char *BinaryBits;
  unsigned int Width;

  // Constructor
  TBMessage(TBMessageType i, long t, std::string &pn, std::string &b, unsigned int width)
  : Type(i), Tick(t), PortName(pn), Bits(b), Width(width) { BinaryBits = NULL; }
  TBMessage(TBMessageType i, long t, std::string &pn, const char *s, unsigned int width)
  : Type(i), Tick(t), PortName(pn), Bits(s), Width(width) { BinaryBits = NULL; }
  TBMessage(TBMessageType i, long t, const char *pn, const char *s, unsigned int width)
  : Type(i), Tick(t), PortName(pn), Bits(s), Width(width) { BinaryBits = NULL; }

  bool operator==(TBMessage &msg) const;
  bool operator!=(TBMessage &msg) const;

  bool isInput() { return Type == TBInput; }
  bool isOutput() { return Type == TBOutput; }
  bool isClock() { return Type == TBClock; }
  bool isReset() { return Type == TBReset; }
  long tick() { return Tick; }
  std::string &portName() { return PortName; }
  std::string &bits() { return Bits; }
  void setBits(std::string &b) { Bits = b; }
  void convertToBinaryBits();
  const char *binaryBits() { return BinaryBits; }

  bool checked() { return Checked; }
  void setChecked(bool c) { Checked = c; }

  void setPortName(const char *n) { PortName = n; }

  /// overload the put-to operator for this class
  friend std::ostream & operator<< (std::ostream &os, TBMessage &msg) {
    if (msg.isInput()) {
      os << "Input @ " << msg.tick() << ": " << msg.portName() << "(" << msg.bits()
	 << ")]" << std::endl;
    }
    else if (msg.isOutput()) {
      os << "[Output @ " << msg.tick() << ": " << msg.portName() << "(" << msg.bits()
	 << ")" << std::endl;
    }
    else if (msg.isOutput()) {
      os << "[Clock: " << msg.portName() << "(" << msg.bits()
	 << ")" << std::endl;
    }
    else if (msg.isReset()) {
      os << "[Reset: " << msg.portName() << "(" << msg.bits()
	 << ")" << std::endl;
    }
    return os;
  }
};

class VCDPort;
typedef std::list<VCDPort*> VCDPortList;
typedef std::list<VCDPort*>::iterator VCDPortIter;

class VCDPort {

 protected:

  std::string PortName;
  std::string VeriPortName;
  VCDPortList Ports;
  unsigned int Width;
  int From;
  int To;
  DirectionE Dir;
  std::string VCDSymbol;
  VCDPort *Master;
  char *Message;
  char *MsbFirstMessage;
  bool Init;

 public:
  
 VCDPort(const char *name, const char *veriname, unsigned int w, DirectionE dir, const char *symbol)
   : PortName(name), VeriPortName(veriname), Width(w), Dir(dir), VCDSymbol(symbol)
  {
    From = To = -1; Master = NULL; Message = NULL; Init = false;
  }

  ~VCDPort();

  VCDPort *master() { return Master; }
  void setMaster(VCDPort *p) { Master = p; }

  const char *portName() { return PortName.c_str(); }
  void setPortName(const char *n) { PortName = n; }

  const char *veriPortName() { return VeriPortName.c_str(); }
  void setVeriPortName(const char *n) { VeriPortName = n; }

  unsigned int width() { return Width; }
  void setWidth(unsigned int w) { Width = w; }

  int from() { return From; }
  void setFrom(int f) { From = f; }

  int to() { return To; }
  void setTo(int t) { To = t; }

  const char *vcdSymbol() { return VCDSymbol.c_str(); }
  void setVCDSymbol(const char *s) { VCDSymbol = s; }

  void addSubPort(const char *name, const char *veriname, int w, DirectionE dir, const char *symbol);
  void addSubPort(VCDPort *p);
  void beginSubPort(VCDPortIter &iter) { iter = Ports.begin(); }
  int endSubPort(VCDPortIter &iter) { return iter == Ports.end(); }
  DirectionE portType() { return Dir; }

  void initializeMessage();
  void setMessage(std::string &input);
  void setMessageAt(unsigned int i, char c);
  void setMessageAt(unsigned int from, unsigned int to, std::string &input);
  const char *getMessage() { return Message; }
  const char *getMsbFirstMessage();
};

typedef std::list<TBMessage*> StimulusRespList;
typedef std::list<TBMessage*>::iterator StimulusRespIterator;


// C++ class for creating and verifying vcd inputs/outpus 
class TestBenchGenerator {
 protected:

  std::string ErrMsg;

  // Spec file related
  std::string SpecFileName;
  int LineNum;
  std::string ModuleName;
  std::string BSVModuleName;
  std::string VerilogPath;
  std::string VCDPath;
  netCollection SceMiControlSignals;
  std::string BsvOutputDir;
  std::string CppOutputDir;
  std::string LogDir;

  std::fstream SR_file;
  std::string SRFileName;
  int SRFileOpen;

  std::fstream CAPI_file;

  int SceMiLatency;
  int SceMiLatencyCount;

  StimulusRespList DelayedOutMsgs;

  // VCD related
  std::string VCDFileName;
  std::ifstream VCD_file;
  std::map<std::string,std::string> SymbolToVCDClockMap;
  std::map<std::string,std::string> VcdClockToSceMiClockMap;
  std::map<std::string,std::string> SymbolToVCDResetMap;
  std::map<std::string,std::string> VcdResetToSceMiResetMap;
  std::map<std::string,std::string> VcdResetToVCDClockMap;
  std::map<std::string,std::string> InPortToSceMiClockMap;
  std::map<std::string,std::string> OutPortToSceMiClockMap;
  std::map<std::string,std::string> InPortToVCDMap;
  std::map<std::string,std::string> VCDToInPortMap;
  std::map<std::string,std::string> OutPortToVCDMap;
  std::map<std::string,std::string> VCDToOutPortMap;
  std::map<std::string,VCDPort*> NameToVCDPortMap;
  std::map<std::string,VCDPort*> VeriNameToVCDPortMap;
  std::map<std::string,VCDPort*> SymbolToVCDPortMap;
  std::map<std::string,long> VcdClockToClockTick;
  std::map<std::string,bool> VcdClockToClockState;
  std::map<std::string,bool> VcdResetToResetState;
  std::map<std::string,std::string> SceMiClockToVcdClockMap;
  std::map<std::string,std::string> SceMiResetToVcdResetMap;
	  
  VCDPortList InPorts;
  VCDPortList OutPorts;
  int InPortsDirty;
  int OutPortsDirty;
  long Tick;
  long PrevTick;

  StimulusRespList SortedStimulusResponses;

  int GenVerification;

public:

  // Constructors
  TestBenchGenerator();
  TestBenchGenerator(const char *vcdFile);

  // Destructor
  ~TestBenchGenerator();

  // File name
  const char *vcdFileName() { return VCDFileName.c_str(); }
  const char *moduleName() { return ModuleName.c_str(); }
  const char *bsvModuleName() { return BSVModuleName.c_str(); }
  const char *verilogPath() { return VerilogPath.c_str(); }
  const char *vcdPath() { return VCDPath.c_str(); }
  const char *bsvOutputDir() { return BsvOutputDir.c_str(); }
  void setBsvOutputDir(const char *o) { BsvOutputDir = o; }
  const char *cppOutputDir() { return CppOutputDir.c_str(); }
  const char *logDir() { return LogDir.c_str(); }
  const char *stimulusResponseFileName() { return SRFileName.c_str(); }

  VCDPortIter beginOutPorts() { return OutPorts.begin(); }
  VCDPortIter endOutPorts() { return OutPorts.end(); }

  std::map<std::string,std::string>::iterator beginOutPortNames() { return VCDToOutPortMap.begin(); }
  std::map<std::string,std::string>::iterator endOutPortNames() { return VCDToOutPortMap.end(); }

  // The names of all the control signals
  bool getSceMiControlSignals(VeriModule *module, netCollection &signals);
  void getSceMiControlSignals(netCollection &signals) { signals = SceMiControlSignals; }
  void setSceMiControlSignals(netCollection &signals) { SceMiControlSignals = signals; }
  void getSceMiClockName(std::string &name);
  void getSceMiResetName(std::string &name);
  void getDutClockName(std::string &name);
  void getDutResetName(std::string &name);

  // Read vcd portmap file
  int readSpecFile(const char *filename);

  // Generate code functions
  int generateTestBench(const char *outputdir);
  int generateTestBenchTemplate(const char *outputdir);
  int generateSimTbTemplate(VeriModule *module, const char *dirname,
			    int tbtemplate);

  // Check vcd file for all the input and output signals
  bool checkVCDPortDefinition(VeriModule *module);

  void generateTclStimulusResponses(std::fstream &file);
  bool generateStimulusResponsesDataFile(const char *filename);

  void generateTclTest(std::fstream &file, int &count,
		       std::vector<std::string> &namevector,
		       int numiarg, int numarg);
  int generateGuiDutTclFile(std::fstream &file, VeriModule *module,
			    std::string &realdir, int tbtemplate=0);

  int generateCapiTemplate(VeriModule *module, const char *new_module_name,
			   const char *dirname);

  // Start a VCD file
  bool openVCDFile();

  // Start a VCD file
  void closeVCDFile();

  // Start a SR file
  bool openSRFile();

  // Start a VCD file
  void closeSRFile();

  // Set the name of output VCD file
  bool setVCDFile(const char* name);

  // Set the name of output VCD file
  bool setStimulusResponseFile(const char* name);

  // Read vcd portmap file
  bool readPortMapFile(const char *filename);

  // Read vcd file
  bool readVCDFile(VeriModule *module);

  bool hierarchyMatch(const char *h1, const char *h2);

  bool fileExist(const char *file);

  int generateTclTbFile(VeriModule *module, const char *new_module_name,
			const char *dirname, int tbtemplate=0);
  void composeParams(const char *interface, ModuleTerminalList &mtlist,
		     std::string &params);
  int generateReiSendHeaders(std::fstream &file, RdyEnableInterface *rei,
			     ModuleTerminalList &mtlist, const char *prefix="");
  int generateSendHeaders(std::fstream &file, const char *type, const char *portname,
			  const char *prefix="");
  int generateSendHeaders2(std::fstream &file, const char *type, const char *portname,
			   const char *prefix="");
  int generateReiGetHeaders(std::fstream &file, RdyEnableInterface *rei,
			    ModuleTerminalList &mtlist, int pipe, const char *prefix="");
  int generateGetHeaders(std::fstream &file, const char *type, const char *portname,
			 const char *prefix="");
  int generateGetHeaders2(std::fstream &file, const char *type, const char *portname, int pipe,
			 const char *prefix="");
  int generateDutXactorContructor(std::fstream &file, ModuleTerminalList &mtlist);
  int generateReiSendMethods(std::fstream &file, RdyEnableInterface *rei,
			     const char *new_module_name, ModuleTerminalList &mtlist,
			     const char *prefix="");
  int generateSendMethods(std::fstream &file, const char *new_module_name,
			  const char *type, const char *portname, const char *prefix="");
  int generateReiGetMethods(std::fstream &file, RdyEnableInterface *rei,
			    const char *new_module_name, ModuleTerminalList &mtlist,
			    const char *prefix="");
  int generateGetMethods(std::fstream &file, const char *new_module_name, 
			 const char *type, const char *portname, const char *prefix="");
  int generatePipeGetMethods(std::fstream &file, const char *new_module_name, 
			     const char *type, const char *portname, const char *prefix="");
  int generateInlineSend(std::fstream &file, const char *new_module_name,
			 const char *type, const char *portname,
			 const char *prefix);
  
  int generateInlineReceive(std::fstream &file, const char *new_module_name,
			    const char *type, const char *portname, int pipe,
			    const char *prefix);
  
  int generateInlineReiPut(std::fstream &file, RdyEnableInterface *rei,
			   const char *new_module_name,
			   ModuleTerminalList &mtlist,
			   const char *prefix);

  int generateInlineReiGet(std::fstream &file, RdyEnableInterface *rei,
			   const char *new_module_name,
			   ModuleTerminalList &mtlist,
			   const char *prefix);

  int generateDutXactor(VeriModule *module, const char *new_module_name, const char *dirname);

  int generateVcdPlayer(VeriModule *module, const char *new_module_name, const char *dirname);

  int generateTclGui(VeriModule *module, const char *new_module_name,
		     const char *dirname, int tbtemplate=0);

  int generateVerificationScript(VeriModule *module, const char *scriptname);

  int generateVerificationFromVCD(VeriModule *module, const char *dirname);

  void getErrorMessage(std::string &msg) { msg = ErrMsg; }

  void processStimulusResponse();

  void writeStimulus(std::fstream &file, TBMessage *msg);

  void writeResponse(std::fstream &file, TBMessage *msg);

  void writeReset(std::fstream &file, TBMessage *msg);

  void writeMessage(std::fstream &file, TBMessage *msg);

  void composeMessage(VCDPort *master, VCDPort *subport, std::string &input);

  void writeVcdPlayerHFile(const char *dirname);

  void writeVcdPlayerCFile(const char *dirname);

  void writePlayVcdFile();

  void writeCAPI(TBMessage *msg);

  void writeDummyInputsDueToLatency(VeriModule *module);

};

