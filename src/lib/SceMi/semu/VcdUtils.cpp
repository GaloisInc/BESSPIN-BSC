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
#include <VcdUtils.h>

void toLowerCase(std::string &str);

#define MAX_LINE_SIZE 1023

using namespace std;

void TBMessage::convertToBinaryBits()
{
  int words = Bits.size();
  BinaryBits = new char[words];

  for (int i = Width; (i>0) ; --i ) {
    unsigned int bitidx = i - 1;
    unsigned int wrdidx = bitidx/32;
    bitidx = bitidx & 0x01f;  // bitidx % 32
    if (Bits[bitidx] == '1')
      BinaryBits[wrdidx] |= (0x1 << bitidx);
  }
}

bool TBMessage::operator==(TBMessage& msg) const
{
  if (Input != msg.Input) return false;
  if (compareBits(Bits, msg.Bits)) return false;
  if (Width != msg.Width) return false;
  return true;
}

bool TBMessage::operator!=(TBMessage& msg) const
{
  if (Input != msg.Input) return true;
  if (!compareBits(Bits, msg.Bits)) return true;
  if (Width != msg.Width) return true;
  return false;
}

bool TBMessage::operator<(const TBMessage& msg) const
{
  return (this->tick() < msg.tick());
}

bool TBMessage::compareBits(const std::string &b1, const std::string &b2) const
{
  long bval1, bval2;
  size_t loc;
  std::stringstream stream;
  std::string hval1, hval2, temp;

  if (b1 == b2) return true;

  //cout << "compareBits " << b1 << " " << b2 << endl;

  loc = b1.find('h');
  if (loc != string::npos) {
    strip_underscore(b1.substr(loc+1), temp);
    //cout << "after strip1 " << temp << endl;
    strip_leading_zeros(temp, hval1);
    //cout << "after strip2 " << hval1 << endl;
  }
  else {
    strip_leading_zeros(b1, temp);
    //cout << "after strip3 " << temp << endl;
    convert_binary_to_hex(temp, hval1);
    //cout << "after convert " << hval1 << endl;
  }

  loc = b2.find('h');
  if (loc != string::npos) {
    strip_underscore(b2.substr(loc+1), temp);
    //cout << "after strip4 " << temp << endl;
    strip_leading_zeros(temp, hval2);
    //cout << "after strip5 " << hval2 << endl;
  }
  else {
    strip_leading_zeros(b2, temp);
    //cout << "after strip6 " << temp << endl;
    convert_binary_to_hex(temp, hval2);
    //cout << "after convert " << hval2 << endl;
  }

  //cout << "b1 " << b1 << " b2 " << b2 << endl;
  //cout << "hval1 " << hval1 << " hval2 " << hval2 << endl;
  return (hval1 == hval2);
}

void TBMessage::strip_underscore(const std::string &in, std::string &out) const
{
  size_t pos = 0;

  out.resize(in.size());
  for (size_t i=0; i<in.size(); i++) {
    if (in[i] != '_')
      out[pos++] = in[i];
  }
  out.resize(pos);
}

void TBMessage::strip_leading_zeros(const std::string &in, std::string &out) const
{
  size_t pos = 0;
  size_t i;
  
  out.resize(in.size());
  for (i=0; i<in.size(); i++) {
    if (in[i] != '0')
      break;
  }
  while (i<in.size()) {
    out[pos++] = in[i];
    i++;
  }
  if (pos == 0)
    out[pos++] = '0';
  out.resize(pos);
}

void TBMessage::convert_binary_to_hex(const std::string &ins, std::string &out) const
{
  size_t pos = 0;
  long i;
  std::string in;
  std::stringstream stream;

  out = "";

  for (i=ins.size()-1; i>=3; i-=4) {

    //cout << "here1 substring " << i << endl;
    in = ins.substr(i-3,4);
    bitset<4> set(in);	
    stream << std::hex << set.to_ulong() << endl;
    stream >> in;
    out = in + out;
  }
  if (i >= 0) {
    //cout << "here2 substring " << i << endl;
    in = ins.substr(0, i+1);
    bitset<4> set(in);	
    stream << std::hex << set.to_ulong() << endl;
    stream >> in;
    out = in + out;
  }
  else if (out == "")
    if (ins[0] == '1')
      out = "1";
    else
      out = "0";

  //cout << "Convert binary to hex from " << ins << " to " << out << endl;
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
  unsigned int i = 0;

  for (int j=input.size()-1; j>=0; j--) {
    //printf("copying from j %d to i %d\n", j, i);
    Message[i] = input[j];
    i++;
  }
  while (i <= Width-1) {
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
  if (MsbFirstMessage == NULL) {
    return "0";
  }
  //printf("getMsbFirstMessage portname %s master %p width %d j=%d message %s\n", portName(), master(), Width, j, Message);
  for (unsigned int i=0; i<=Width-1; i++)
    MsbFirstMessage[i] = Message[j--];
  //printf("done getMsbFirstMessage\n", j);
  return MsbFirstMessage;
}

// Constructors
VcdUtils::VcdUtils()
{
  SceMiLatency = 4;
  SceMiLatencyCount = 0;
  InPortsDirty = OutPortsDirty = 0;
  Tick = PrevTick = 0;
}

// Destructor
VcdUtils::~VcdUtils()
{
}

int VcdUtils::readSpecFile(const char* filename)
{
  const char *delimiter = " \n\t,:[]'";
  std::ifstream file;
  char lineBuf[MAX_LINE_SIZE+1];
  char *token;
  char *token2;
  int stat;
  char buf[1024];
  std::string cont_sig;

  LineNum = 0;

  file.open(filename);
  if (file.fail()) {
    ErrMsg = "ERROR VcdUtils::readSpecFile(): unable to open testbench spec file ";
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
      if (!strcmp(token, "module")) {
	
	token = strtok(0, delimiter);
	ModuleName = token;
      }
      else if (!strcmp(token, "bsvmodule")) {
	
	token = strtok(0, delimiter);
	BSVModuleName = token;
      }
      else if (!strcmp(token, "verilog-path")) {
	
	token = strtok(0, delimiter);
	VerilogPath = token;
      }
      else if (!strcmp(token, "vcd-path")) {
	
	token = strtok(0, delimiter);
	VCDPath = token;
      }
      else if (!strcmp(token, "scemi-clk-signals")) {
	
	token = strtok(0, delimiter);
	while (token) {
	  token2 = strtok(0, delimiter);
	  cont_sig = token;
	  cont_sig += string(":") + token2;
	  SceMiControlSignals.push_back(cont_sig);
	  token = strtok(0, delimiter);
	}
      }
      else if (!strcmp(token, "scemi-reset-signals")) {
	
	token = strtok(0, delimiter);
	while (token) {
	  token2 = strtok(0, delimiter);
	  cont_sig = token;
	  cont_sig += string(":") + token2;
	  SceMiControlSignals.push_back(cont_sig);
	  token = strtok(0, delimiter);
	}
      }
      else if (!strcmp(token, "vcd-file")) {
	
	token = strtok(0, delimiter);
	VCDFileName = token;
	//printf("Reading VCD file %s\n", VCDFileName.c_str());
	//stat = setVCDFile(VCDFileName.c_str());
	//if (!stat) {
	//  ErrMsg = "Error TestBenGenerator::readSpecFile(): bad vcd file in testbench spec file on line ";
	//  sprintf(buf, "%d", LineNum);
	//  ErrMsg += buf;
	//  return 0;
	//}
      }
      else if (!strcmp(token, "outputdir")) {
	
	token = strtok(0, delimiter);
	OutputDir = token;
      }
      else if (!strcmp(token, "port")) {

	stat = parsePortSpec(file, lineBuf);
	if (!stat)
	  return 0;
      }
      else if (!strcmp(token, "scemi-latency")) {
	
	token = strtok(0, delimiter);
	SceMiLatency = atoi(token);
      }
      else {
	ErrMsg = "Error TestBenGenerator::readSpecFile(): bad statement in testbench spec file on line ";
	sprintf(buf, "%d", LineNum);
	ErrMsg += buf;
	return 0;
      }
    }
  }
  return 1;
}

// do fabric
int VcdUtils::parsePortSpec(std::ifstream &file, char *lineBuf)
{
  const char *delimiter = " \n\t:";
  char *token;
  BString portname, vcdname;
  char buf[1024];

  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    LineNum++;
    token = strtok(lineBuf, delimiter);

    // Check comment
    if (token == NULL) continue;
    if (strncmp(token, "//", 2) == 0) continue;
    
    if (!strcmp(token, "input")) {
      continue;
    }
    else if (!strcmp(token, "output")) {
      portname = strtok(0, delimiter);
      if (portname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error VcdUtils::parsePortMap(): bad output port in port map file on line "
	     << buf << endl;
	return 0;
      }
      vcdname = portname;
      if (vcdname == "") {
	sprintf(buf, "%d", LineNum);
	cerr << "Error VcdUtils::parsePortMap(): bad output vcd signal name in port map file on line "
	     << buf << endl;
	return 0;
      }

      //printf("HASH outporttovcdmap %s %s\n", portname.c_str(), vcdname.c_str());
      OutPortToVCDMap[portname] = vcdname;
      VCDToOutPortMap[vcdname] = portname;
    }
    else if (!strcmp(token, "endport")) {
      return 1;
    } else {
      sprintf(buf, "%d", LineNum);
      cerr << "Error VcdUtils::parsePortMap(): bad statement in port map file on line "
	   << buf << endl;
      return 0;
    }
  }

  return 1;
}

bool VcdUtils::createMessageList(const char *datafile, StimulusRespList &list,
				 int output_only)
{
  std::fstream file;
  const char *delimiter = " \n\t,:[]()";
  char lineBuf[MAX_LINE_SIZE+1];
  char *token;
  TBMessage *msg;
  VCDPort *port;
  
  file.open(datafile, ios::in);
  if (file.fail()) {
    std::cerr << "ERROR VcdUtils::createMessageList(): unable to open file "
	      << datafile
              << "." << std::endl;
    return false;
  }

  string tick;
  string portname;
  string data;

  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    token = strtok(lineBuf, delimiter);
    if (token && (strncmp(token, "//", 2) != 0)) { // Check comment
      if (!strcmp(token, "Input")) {
	if (output_only) continue;
	token = strtok(0, delimiter);
	if (*token == '@') {
	  token = strtok(0, delimiter);
	  if (token == NULL) return false;
	  tick = token;
	  token = strtok(0, delimiter);
	  portname = token;
	  token = strtok(0, delimiter);
	  if (token == NULL) return false;
	  data = token;
	}
	else return false;

	port = NameToVCDPortMap[portname];
	msg = new TBMessage(true, atol(tick.c_str()), portname, data, port->width()); 
	list.push_back(msg);
      }
      else if (!strcmp(token, "Output")) {
	token = strtok(0, delimiter);
	if (*token == '@') {
	  token = strtok(0, delimiter);
	  if (token == NULL) return false;
	  tick = token;
	  token = strtok(0, delimiter);
	  portname = token;
	  token = strtok(0, delimiter);
	  if (token == NULL) return false;
	  data = token;
	}
	else return false;

	port = NameToVCDPortMap[portname];

	if (port == NULL) {
	  msg = new TBMessage(false, atol(tick.c_str()), portname, data, 32); 
	} else {
	  msg = new TBMessage(false, atol(tick.c_str()), portname, data, port->width()); 
	}
	//printf("After tbmessage\n");
	list.push_back(msg);
      }
      else if (!strcmp(token, "Reset")) {
	continue;
      }
      else return false;
      
      // Got an input and send the request
    }
  }
  return true;
}

void VcdUtils::compareOutputs(StimulusRespList &inlist, StimulusRespList &outlist)
{
  StimulusRespIterator isrItr, osrItr;

  //printf("inlist size %d outlist size %d\n", inlist.size(), outlist.size());

  isrItr = inlist.begin();
  osrItr = outlist.begin();

  while (osrItr != outlist.end()) {

    compareOutputAtTime(osrItr, isrItr, inlist);
    LastResponse[(*osrItr)->portName()] = *osrItr;
    osrItr++;
  } 

  cerr << endl;
}

void VcdUtils::compareOutputAtTime(StimulusRespIterator &outItr,
				   StimulusRespIterator &inItr,
				   StimulusRespList &inlist)
{
  StimulusRespIterator inItr2;
  TBMessage *lastMsg;

  //cout << "\n> Next Response yo Output: " << **outItr << endl;
  while ((inItr != inlist.end()) && ((*inItr)->tick() < (*outItr)->tick())) {
     
    if ((*inItr)->checked() == false) {
      lastMsg = LastResponse[(*inItr)->portName()];
      if ((lastMsg == NULL) || !lastMsg->compareBits(lastMsg->bits(), (*inItr)->bits())) {
	cerr << "  **Error: Found extra output from simulation VCD that cannot be accounted for:" << endl;
	cerr << "           Simulated: " << **inItr << endl;
      }
    }
    inItr++;
  }
  
  inItr2 = inItr;

  //cout << "Here " << (*outItr)->tick() << " " << (*inItr2)->tick() << endl;
  while ((inItr2 != inlist.end()) && ((*outItr)->tick() >= (*inItr2)->tick())) {
    //cout << " Checking against original VCD: " << *(*inItr2) << endl;
    if ((*inItr2)->portName() == (*outItr)->portName())
      break;
    
    inItr2++;
  }

  //cout << "Here2 " << *(*outItr) << " " << *(*inItr2) << " " << *(*inItr) << endl;
  if (inItr2 == inlist.end() || ((*outItr)->tick() < (*inItr2)->tick())) {
    cerr << "  **Error: Matching input cannot be found for " << *(*outItr) << endl;
    //cout << "Here3 " << (*outItr)->tick() << " " << (*inItr2)->tick() << " " << (*inItr)->tick() << endl;
    return;
  }
  else if ((*inItr2)->portName() == (*outItr)->portName()) {
    if (**inItr2 != **outItr) {
      cerr << "  **Error: Found output response that is not the same:" << endl;
      cerr << "           Original: " << **inItr2 << endl;
      cerr << "           Simulated: " << **outItr << endl;
    }
    else {
      cerr << "  " << *(*outItr) << " -> GOOD" << endl;
      (*inItr2)->setChecked(true);
    }
  }
  else if ((*outItr)->tick() < (*inItr2)->tick()) {
    cerr << "  **Error: Matching input cannot be found for " << *(*outItr) << endl;
  }
}

bool VcdUtils::generateStimulusResponsesDataFile(const char *filename)
{
  std::fstream file;
  StimulusRespIterator srItr;

  file.open(filename, ios::out);
  if (file.fail()) {
    std::cerr << "ERROR VcdUtils::generateStimulusResponsesDataFile(): unable to open file "
	      << filename
              << "." << std::endl;
    return false;
  }

  for (srItr = SortedStimulusResponses.begin();
       srItr != SortedStimulusResponses.end();
       srItr++)
    {
      if ((*srItr)->isInput())
	writeStimulus(file, *srItr);
      else
	writeResponse(file, *srItr);
    }

  file.close();

  return true;
}

bool VcdUtils::readVCDFile(const char *vcdfile)
{
  bool stat = true;
  std::ifstream VCD_file;

  printf("\nRead vcdfile %s\n", vcdfile);

  //const char *delimiter = " \n\t,:";
  char lineBuf[MAX_LINE_SIZE+1];
  boost::cmatch matches;
  size_t pos;
  LineNum = 0;
  VCDPort *masterPort, *subPort, *aPort;
  int size;

  LineNum = 0;

  VCD_file.open(vcdfile);
  if (VCD_file.fail()) {
    ErrMsg = "ERROR VcdUtils::readVCDFile(): unable to open vcd file ";
    ErrMsg += vcdfile;
    ErrMsg += ".";
    return 0;
  }

  VCD_file.close();
  VCD_file.open(vcdfile);
  if (VCD_file.fail()) {
    ErrMsg = "ERROR VcdUtils::readVCDFile(): unable to open vcd file ";
    ErrMsg += vcdfile;
    ErrMsg += ".";
    return 0;
  }

  // Set up regex

  // - enddefinition
  std::string scope = "\\$enddefinitions\\s+\\$end"; 
  boost::regex enddef_exp(scope);
  // - scope
  scope = "\\$scope\\s+module\\s+(\\S+)\\s+\\$end"; 
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
  std::string m1, m2, m3, m4, m5, m6, tick, veriname;
  DirectionE dir;
  int w;
  InPortsDirty = OutPortsDirty = 0;

  // Read in each line.  Right now assuming each line consists of either:
  while (!VCD_file.eof()){

    VCD_file.getline(lineBuf, MAX_LINE_SIZE);
    LineNum++;
    
    // $enddefinitions
    if (boost::regex_match(lineBuf, enddef_exp)) {

    }
    // $scope
    else if (boost::regex_match(lineBuf, matches, scope_exp)) {
      
      current = matches[1];
      if (sofar == "")
	sofar = current;
      else
	sofar = sofar + "." + current;
      //printf("Scope2 sofar %s\n", sofar.c_str());
    }
    // $upscope
    else if (boost::regex_match(lineBuf, upscope_exp)) {
      pos = sofar.find_last_of('.');
      if (pos != string::npos)
	sofar = sofar.substr(0, pos);
      else
	sofar = "";
      //printf("upscope3 sofar %s\n", sofar.c_str());
    }
    // $var
    else if (boost::regex_match(lineBuf, matches, var_exp)) {
      //printf("Sofar1 %s vcdpath %s\n", sofar.c_str(), verilogPath());
      
      if (!hierarchyMatch(sofar.c_str(), verilogPath()))
	continue;

      //printf("Line1 %d: %s\n", LineNum, lineBuf);
      m1 = matches[1];
      m2 = matches[2];
      m3 = matches[3];
      m4 = matches[4];

      //veriname = VCDToInPortMap[m4];
      //if (veriname == "") {
      //veriname = VCDToOutPortMap[m4];
      //dir = d_output;
      //}
      //else
      //dir = d_input;
      //if (veriname == "") continue;
      
      if (NameToVCDPortMap[m4] == NULL) {
	size = atoi(m2.c_str());
	masterPort = new VCDPort(m4.c_str(), veriname.c_str(), size, dir, m3.c_str());
	//printf("NameToVCDPort1 %s %s\n", m4.c_str(), veriname.c_str());
	NameToVCDPortMap[m4] = masterPort;
	VeriNameToVCDPortMap[veriname] = masterPort;
	SymbolToVCDPortMap[m3] = masterPort;
	if (dir == d_input)
	  InPorts.push_back(masterPort);
	else
	  OutPorts.push_back(masterPort);
	
	//printf("var4 line %d match %s %s %s %s %d veriname %s\n", LineNum, m1.c_str(), m2.c_str(), m3.c_str(), m4.c_str(),
	//     size, veriname.c_str());

	
      }
    }
    else if (boost::regex_match(lineBuf, matches, varsub_exp)) {
      //printf("Sofar2 %s vcdpath %s\n", sofar.c_str(), vcdPath());
      
      if (!hierarchyMatch(sofar.c_str(), verilogPath()))
	continue;

      //printf("Line2 %d: %s\n", LineNum, lineBuf);
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
	//printf("NameToVCDPort2 %s %s\n", m4.c_str(), veriname.c_str());
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
	//printf("NameToVCDPort2 %s %s\n", m4.c_str(), "-");
	NameToVCDPortMap[m4] = subPort;
	SymbolToVCDPortMap[m3] = subPort;
	masterPort->addSubPort(subPort);
	//printf("Set2 Master of subport %s to %s width %d\n", m4.c_str(), masterPort->portName(), masterPort->width());
      }
      //printf("var4sub line %d match %s %s %s %s %s %d veriname %s\n", LineNum, m1.c_str(), m2.c_str(), m3.c_str(), m4.c_str(), m5.c_str(), size, veriname.c_str());
    }
    else if (boost::regex_match(lineBuf, matches, varmsub_exp)) {
      //printf("Sofar3 %s vcdpath %s\n", sofar.c_str(), vcdPath());
      
      if (!hierarchyMatch(sofar.c_str(), verilogPath()))
	continue;

      //printf("Line3 %d: %s\n", LineNum, lineBuf);
      m1 = matches[1];
      m2 = matches[2];
      m3 = matches[3];
      m4 = matches[4];
      m5 = matches[5];
      m6 = matches[6];

      masterPort = NameToVCDPortMap[m4];
      //veriname = VCDToInPortMap[m4];
      //if (veriname == "") {
      //veriname = VCDToOutPortMap[m4];
      //dir = d_output;
      //}
      //else
      //dir = d_input;
      
      //if (veriname == "") continue;

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
	//printf("Set1 Master of subport %s to %s width %d\n", m4.c_str(), masterPort->portName(), masterPort->width());
      }
      //printf("var4msub line %d match %s %s %s %s %s %s %d veriname %s\n", LineNum, m1.c_str(), m2.c_str(), m3.c_str(), m4.c_str(), m5.c_str(), m6.c_str(), size, veriname.c_str());
    }
    // clock tick
    else if (boost::regex_match(lineBuf, matches, clock_exp)) {
      tick = matches[1];
      PrevTick = Tick;
      Tick = atol(tick.c_str());
      
      //printf("tick5 match %s tick %ld\n", m1.c_str(), Tick);
      
      // Process stimulus/response
      processStimulusResponse();
    }
    // 1 bit data
    else if (boost::regex_match(lineBuf, matches, bitdata_exp)) {
      m1 = matches[1];
      m2 = matches[2];
      //printf("bitdata6 match %s %s\n", m1.c_str(), m2.c_str());

      aPort = SymbolToVCDPortMap[m2];
      if (aPort) {
	masterPort = aPort->master();
	if (masterPort == NULL)
	  masterPort = aPort;
	std::string vcdportname = masterPort->portName();
	//std::string portname = masterPort->veriPortName();
	//printf("VeriPortName %s\n", portname.c_str());
	//if (portname == "") continue;

	//printf("before compose1\n");
	composeMessage(masterPort, aPort, m1);
	//printf("after compose1\n");
	if (aPort->portType()==d_input)
	  InPortsDirty = 1;
	else
	  OutPortsDirty = 1;
	/*  
	printf("Add Message1 %s\n", masterPort->getMessage());
	msg = new TBMessage(is_input, ticknum, portname, masterPort->getMessage());
	StimulusResponses.push_back(msg);
	StimulusResponsesMap[masterPort->portName()] = msg;
	*/
      }
    }
    // n bits data
    else if (boost::regex_match(lineBuf, matches, bindata_exp)) {
      m1 = matches[1];
      m2 = matches[2];
      //printf("bindata7 match %s %s\n", m1.c_str(), m2.c_str());

      aPort = SymbolToVCDPortMap[m2];
      if (aPort) {
	masterPort = aPort->master();
	if (masterPort == NULL)
	  masterPort = aPort;
	//printf("Found aPort %s master %s\n", aPort->portName(), masterPort->portName());
	std::string vcdportname = masterPort->portName();
	//std::string portname = masterPort->veriPortName();
	//printf("VeriPortName %s\n", portname.c_str());
	//if (portname == "") continue;

	//printf("before compose %s\n", m1.c_str());
	composeMessage(masterPort, aPort, m1);
	//printf("after compose\n");
	if (aPort->portType()==d_input)
	  InPortsDirty = 1;
	else
	  OutPortsDirty = 1;
	/*
	printf("Add Message2 %s\n", masterPort->getMessage());
	
	msg = new TBMessage(is_input, ticknum, portname, masterPort->getMessage());
	StimulusResponses.push_back(msg);
	*/
      }
    }
    else {

      // nothing
    }
  }

  return stat;
}

void VcdUtils::composeMessage(VCDPort *master, VCDPort *subport,
					std::string &input)
{
  master->initializeMessage();

  if (master == subport && subport->master()==NULL) {
    master->setMessage(input);
  }
  else {
    master->setMessageAt(subport->from(), subport->to(), input);
  }
}

bool VcdUtils::hierarchyMatch(const char *s1, const char *s2)
{
  string h1 = s1;
  string h2 = s2;

  if (h1[0] == '/')
    h1 = h1.substr(1);

  if (h2[0] == '/')
    h2 = h2.substr(1);

  std::replace(h1.begin(), h1.end(), '/', '.');
  std::replace(h2.begin(), h2.end(), '/', '.');

  if (h1 == h2)
    return true;

  return false;
}

void VcdUtils::processStimulusResponse()
{
  TBMessage *msg;
  VCDPortIter vcdPortItr;
  VCDPort *port;

  //cout << "In processStimulusResponse\n" << endl;

  if (InPortsDirty) {
    //cout << "Found dirty inport\n" << endl;
    for (vcdPortItr = InPorts.begin();
	 vcdPortItr != InPorts.end();
	 vcdPortItr++) {

      port = *vcdPortItr;
      msg = new TBMessage(true, PrevTick, port->portName(), port->getMsbFirstMessage(),
			  port->width());
      //cout << "Process in message " << *msg << " " << endl;
      //cout << " message " << port->getMessage() << endl;
      //cout << " msbmessage " << port->getMsbFirstMessage() << endl;
      SortedStimulusResponses.push_back(msg);
    }
  }

  if (OutPortsDirty) {
    //cout << "Found dirty outport\n" << endl;
    for (vcdPortItr = OutPorts.begin();
	 vcdPortItr != OutPorts.end();
	 vcdPortItr++) {

      port = *vcdPortItr;
      //cerr << "Before Process out message " << port->getMsbFirstMessage() << endl;
      msg = new TBMessage(false, PrevTick, port->portName(), port->getMsbFirstMessage(),
			  port->width());
      //cerr << "Process out message " << *msg << endl;
      //cerr << " msbmessage1 " << port->getMsbFirstMessage() << endl;
      //if (port->getMessage())
      //cerr << " message1 " << port->getMessage() << endl;
      SortedStimulusResponses.push_back(msg);
    }
  }

  InPortsDirty = OutPortsDirty = 0;
}

void VcdUtils::writeStimulus(std::fstream &file, TBMessage *msg)
{
  file << "Input @ " << msg->tick() << ": " << msg->portName() << "(" << msg->bits() << ")" << std::endl;
  //printf("WRITE STIMULUS %s %s\n", portname.c_str(), stimulus.c_str());
}

void VcdUtils::writeResponse(std::fstream &file, TBMessage *msg)
{
  file << "Output @ " << msg->tick() << ": " << msg->portName() << "(" << msg->bits() << ")" << std::endl;
  //printf("WRITE RESPONSE %s %s\n", portname.c_str(), response.c_str());
}

void VcdUtils::writeMessage(std::fstream &file, TBMessage *msg)
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


