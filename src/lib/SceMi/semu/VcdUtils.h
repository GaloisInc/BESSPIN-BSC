// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <map>
#include <set>
#include <list>
#include <iostream>
#include <fstream>
#include <vector>
#include "string.h"

/* VcdPlayer
 *
 * Generate c++ testbench code for simple test and
 * verification of a dut. 
 *
 */

// Port directions
enum DirectionE {
  d_input, d_output, d_inout
};

typedef std::string BString;
typedef BString  netHandle;
typedef std::list<netHandle> netCollection;

struct TBMessage {
  bool Input;
  bool Checked;
  long Tick;
  std::string PortName;
  std::string Bits;
  char *BinaryBits;
  unsigned int Width;

  // Constructor
  TBMessage(bool i, long t, std::string &pn, std::string &b, unsigned int width)
  : Input(i), Tick(t), PortName(pn), Bits(b), Width(width) { BinaryBits = NULL; Checked = false; }
  TBMessage(bool i, long t, std::string &pn, const char *s, unsigned int width)
  : Input(i), Tick(t), PortName(pn), Bits(s), Width(width) { BinaryBits = NULL; Checked = false; }
  TBMessage(bool i, long t, const char *pn, const char *s, unsigned int width)
  : Input(i), Tick(t), PortName(pn), Bits(s), Width(width) { BinaryBits = NULL; Checked = false; }
  TBMessage(bool i, long t, const char *pn, std::string &b, unsigned int width)
  : Input(i), Tick(t), PortName(pn), Bits(b), Width(width) { BinaryBits = NULL; Checked = false; }

  bool operator==(TBMessage &msg) const;
  bool operator!=(TBMessage &msg) const;
  bool operator < (const TBMessage &msg) const;

  bool compareBits(const std::string &b1, const std::string &b2) const;
  void strip_underscore(const std::string &in, std::string &out) const;
  void strip_leading_zeros(const std::string &in, std::string &out) const;
  void convert_binary_to_hex(const std::string &ins, std::string &out) const;

  bool isInput() { return Input; }
  bool isOutput() { return !Input; }
  long tick() const { return Tick; }
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
	 << ")]";
    }
    else {
      os << "[Output @ " << msg.tick() << ": " << msg.portName() << "(" << msg.bits()
	 << ")]";
    }
    return os;
  }
};

// Functor class for comparing TBMessage
class CompareTBMessage {
 public:
  bool operator() (const TBMessage *l, const TBMessage *r) {
    return (l->tick() < r->tick());
  }
};

class VCDPort;
typedef std::list<VCDPort*> VCDPortList;
typedef std::list<VCDPort*>::iterator VCDPortIter;
typedef std::multiset<TBMessage *, CompareTBMessage> TBMessageSet;
typedef TBMessageSet::iterator TBMessageIter;


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
    From = To = -1; Master = NULL; Message = NULL; MsbFirstMessage = NULL; Init = false;
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
class VcdUtils {
 protected:

  std::string ErrMsg;

  // Spec file related
  std::string VCDFileName;
  std::string ModuleName;
  std::string BSVModuleName;
  std::string VerilogPath;
  std::string VCDPath;
  std::string OutputDir;

  netCollection SceMiControlSignals;

  int SceMiLatency;
  int SceMiLatencyCount;

  StimulusRespList DelayedOutMsgs;

  // VCD related

  std::map<std::string,std::string> InPortToVCDMap;
  std::map<std::string,std::string> VCDToInPortMap;
  std::map<std::string,std::string> OutPortToVCDMap;
  std::map<std::string,std::string> VCDToOutPortMap;
  std::map<std::string,VCDPort*> NameToVCDPortMap;
  std::map<std::string,VCDPort*> VeriNameToVCDPortMap;
  std::map<std::string,VCDPort*> SymbolToVCDPortMap;

  std::map<std::string,TBMessage*> LastResponse;

  VCDPortList InPorts;
  VCDPortList OutPorts;
  int InPortsDirty;
  int OutPortsDirty;
  long Tick;
  long PrevTick;
  int LineNum;

  StimulusRespList SortedStimulusResponses;

  int parsePortSpec(std::ifstream &file, char *lineBuf);

public:

  // Constructors
  VcdUtils();

  // Destructor
  ~VcdUtils();

  // File name
  const char *vcdFileName() { return VCDFileName.c_str(); }
  const char *moduleName() { return ModuleName.c_str(); }
  const char *bsvModuleName() { return BSVModuleName.c_str(); }
  const char *verilogPath() { return VerilogPath.c_str(); }
  const char *vcdPath() { return VCDPath.c_str(); }
  const char *outputDir() { return OutputDir.c_str(); }

  VCDPortIter beginOutPorts() { return OutPorts.begin(); }
  VCDPortIter endOutPorts() { return OutPorts.end(); }

  std::map<std::string,std::string>::iterator beginOutPortNames() { return VCDToOutPortMap.begin(); }
  std::map<std::string,std::string>::iterator endOutPortNames() { return VCDToOutPortMap.end(); }

  // The names of all the control signals
  void getSceMiControlSignals(netCollection &signals) { signals = SceMiControlSignals; }

  // Read vcd portmap file
  int readSpecFile(const char *filename);

  bool readVCDFile(const char *vcdfile);

  // Read port spec file
  bool readPortSpecFile(const char *filename);

  bool hierarchyMatch(const char *h1, const char *h2);

  void getErrorMessage(std::string &msg) { msg = ErrMsg; }

  void processStimulusResponse();

  bool createMessageList(const char *datafile, StimulusRespList &list, int output_only=1);

  void compareOutputs(StimulusRespList &inlist, StimulusRespList &outlist);

  void compareOutputAtTime(StimulusRespIterator &outItr, StimulusRespIterator &inItr,
			   StimulusRespList &inlist);

  void writeStimulus(std::fstream &file, TBMessage *msg);

  void writeResponse(std::fstream &file, TBMessage *msg);

  void writeMessage(std::fstream &file, TBMessage *msg);

  void composeMessage(VCDPort *master, VCDPort *subport, std::string &input);

  bool generateStimulusResponsesDataFile(const char *filename);
};

