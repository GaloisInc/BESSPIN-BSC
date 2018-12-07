// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED

#include <time.h>
#include <cstdio>
#include <cstring>
#include <list>
#include <set>
#include <map>
#include <string>
#include <math.h>

#include "Target.h"
#include "VCDWriter.h"
#include "ProbeXactorT.h"
#include "CaptureXactorT.h"
#include "SerialProbeXactor.h"

using namespace std;

// VCD generator version
static const unsigned int major_rev = 2;
static const unsigned int minor_rev = 1;
static unsigned int has_power = 0;


/// replace characters invalid for the vcd with underscore
static std::string munge_name(const std::string & str)
{
    std::string out = str;
    size_t pos = 0;

    while (true)
    {
        pos = out.find_first_of(" :", pos);
	if (pos == std::string::npos)
	{
  	    break;
	}
	else
	{
	    out.at(pos) = '_';
	}
    }

    return out;
}

static bool isPrefix(const std::string& a, const std::string& b) {
  if (a.size() < b.size()) {
    return b.substr(0,a.size()) == a;
  }
  return false;
}

// Member for class Change
Change::Change(unsigned int n, const std::string &v, ChangeType t)
  : num(n), value(v), changetype(t)
{
  removeLeadingZeros();
}
Change::Change(unsigned int n, SceMiU32 v)
  : num(n)
{
  stringstream strm;
  strm << v;
  value = strm.str();
  removeLeadingZeros();
  changetype = BinaryChange;
}
void Change::removeLeadingZeros ()
{
  std::string::size_type pos = value.find_first_not_of("0");
  if (pos == std::string::npos) {
    value = "0";
  }
  else {
    value = value.substr(pos);
  }
}
bool Change::operator== (const Change &r)
{
  return value == r.value;
}


// Constructors
VCDWriter::VCDWriter()
    : VCD_file(NULL)
    , VCD_started(false)
    , TimeScale("1 s")  // default to seconds
    , GreatestCaptureInterval(2) // unused
    , GreatestCaptureIntervalPadding(10)
    , NextVcdId(0)
    , MaxScale(0.0)
    , ScopeCurrent("")
    , _lastTime(0)
    , _lastTimeValid(false)
{ Probes_xactor = NULL; }

VCDWriter::VCDWriter(const char *vcdFile)
    : VCD_file(NULL)
    , VCD_started(false)
    , TimeScale("1 s")  // default to seconds
    , GreatestCaptureInterval(2) // unused
    , GreatestCaptureIntervalPadding(10)
    , NextVcdId(0)
    , MaxScale(0.0)
    , ScopeCurrent("")
    , _lastTime(0)
    , _lastTimeValid(false)
{ setVCDFile(vcdFile); Probes_xactor = NULL; }

VCDWriter::VCDWriter(const char *vcdFile, const tTime padding)
    : VCD_file(NULL)
    , VCD_started(false)
    , TimeScale("1 s")  // default to seconds
    , GreatestCaptureInterval(2) // unused
    , GreatestCaptureIntervalPadding(padding)
    , NextVcdId(0)
    , MaxScale(0.0)
    , ScopeCurrent("")
    , _lastTime(0)
    , _lastTimeValid(false)
{ setVCDFile(vcdFile); Probes_xactor = NULL; }

// Destructor
VCDWriter::~VCDWriter()
{
  flushAllChanges();
  endVCDFile();
}

  // File name
const char *VCDWriter::vcdFileName()
{ return VCDFileName.c_str(); }

  // Register ProbeXactor for tracking
void VCDWriter::registerProbe(ProbeXactor *probe)
{ Probes.push_back(probe); }

  // Register CaptureXactor for tracking
void  VCDWriter::registerCapture(CaptureXactor *c)
{ Captures.push_back(c); }

  // Register ProbeXactor for tracking
void  VCDWriter::registerProbesXactor(ProbesXactor *probeXtr)
{ Probes_xactor = probeXtr; }

  // Return number of ProbeXactors being tracked
size_t  VCDWriter::numProbes()
{ return Probes.size(); }
size_t  VCDWriter::numCaptures()
{ return Captures.size(); }

void VCDWriter::startVCDFile()
{
  // Already started, just returns
  if (VCD_started == true)
    return;

  if (VCDFileName.size() == 0) {
    cerr << "Error: VCD file name was not previously set." << endl;
    return;
  }

  // cerr << "Initializing VCD file" << endl;

  if (VCD_file == NULL)
    VCD_file = fopen(VCDFileName.c_str(), "w");

  // if writing a new header, dump the hierarchy and id map
  if (vcdWriteHeader() == true)
    {
      //fputs("$scope module main $end\n", VCD_file);
      vcdWriteDefs();
      //fputs("$upscope $end\n", VCD_file);
      fputs("$enddefinitions $end\n", VCD_file);
      vcdWriteInitials();
      flushVCDFile();
    }

  VCD_started = true;
}

void VCDWriter::reset(const char *backup_vcdfile)
{
  flushAllChanges();
  endVCDFile();
  NextVcdId = 0;
  ScopeCurrent   = "";
  _lastTime      = 0;
  _lastTimeValid = false;
  VCD_started    = false;
  VCD_file       = NULL;

  // Copy
  if (backup_vcdfile) {

    fstream fin(vcdFileName(), ios::in | ios::binary);
    fstream fout(backup_vcdfile, ios::out | ios::binary);
    
    if(!fin.is_open()) {
      return;
    }
    
    if(!fout.is_open()) {
      cerr << "Error: cannot open back up vcd file " << backup_vcdfile << endl;
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
  }

  startVCDFile();
}

void VCDWriter::endVCDFile()
{
  // Never started, just returns
  if (VCD_started == false)
    return;

  VCD_started = false;

  if (VCD_file != NULL)
    fclose(VCD_file);
  else
    return;
}

bool VCDWriter::setVCDFile(const char* name)
{
  if (!strcmp(name, VCDFileName.c_str()))
    return true;

  if (VCD_file != NULL)
    fclose(VCD_file);
  VCDFileName.resize(0);

  if (name == NULL)
  {
    VCD_file = NULL;
    return true;
  }

  VCDFileName = name;
  VCD_file = fopen(name, "w");
  VCD_started = false;

  if (VCD_file == NULL)
  {
    VCDFileName.resize(0);
    perror(name);
    return false;
  }

  return true;
}

bool VCDWriter::vcdWriteHeader()
{
  if (VCD_file == NULL)
    return false;

  FileTarget dest(VCD_file);

  time_t t = time(NULL);
  dest.write_string("$date\n\t%s$end\n", ctime(&t));
  dest.write_string("$version\n");
  dest.write_string("\tBluespec SceMi VCD dumper %d.%d\n", major_rev, minor_rev);
  dest.write_string("$end\n");
  dest.write_string("$timescale\n\t%s\n$end\n", TimeScale.c_str());

  //next_seq_num = kept_seq_num;

  return true;
}


void VCDWriter::unregisterProbe(ProbeXactor *c)
{
  std::vector<ProbeXactor* >::iterator probeItr;

  for (probeItr = Probes.begin();
       probeItr != Probes.end();
       ++probeItr)
    if ((*probeItr) == c)
      {
	Probes.erase(probeItr);
	return;
      }
}

void VCDWriter::unregisterCapture(CaptureXactor *c)
{
  std::vector<CaptureXactor* >::iterator captureItr;

  for (captureItr = Captures.begin();
       captureItr != Captures.end();
       ++captureItr)
    if ((*captureItr) == c)
      {
	Captures.erase(captureItr);
	return;
      }
}

void VCDWriter::computeGreatestCaptureInterval()
{
  std::vector<CaptureXactor* >::iterator captureItr;
  tTime interval;

  GreatestCaptureInterval = 0;

  for (captureItr = Captures.begin();
       captureItr != Captures.end();
       ++captureItr)  {
    interval =  (*captureItr)->sampleInterval();
    GreatestCaptureInterval = max(GreatestCaptureInterval, interval);
  }
  if(Probes_xactor) {
    for (unsigned int j=0; j < Probes_xactor->getNumberOfProbes(); j++) {
      SerialProbeDef * def = Probes_xactor->getProbeDefFromIndex(j);
      if (def->getProbeType() == NewCaptureProbe)
	interval = def->getSamples() * (tTime) pow((float) 2.0, (int) def->getOffset());
      else
	interval =  def->getSamples() ;
      GreatestCaptureInterval = max(GreatestCaptureInterval,interval ) ;
    }
  }
  // cerr << "Greatest capture interval: " << GreatestCaptureInterval << endl;
  GreatestCaptureInterval +=  GreatestCaptureIntervalPadding; // padding
}

void VCDWriter::vcdWriteDefs()
{
  std::vector<ProbeXactor* >::iterator probeItr;
  std::vector<CaptureXactor* >::iterator captureItr;
  std::string probe_name;
  unsigned int width;

  // This is the old probe and capture definition
  // This should be taken out when the time is right!
  unsigned int probeindex = 0;
  for (probeItr = Probes.begin();
       probeItr != Probes.end();
       ++probeItr)
    {
      probe_name = (*probeItr)->name();
      width = (*probeItr)->getBitSize();
      probeindex = vcdWriteDef(probe_name.c_str(), width);
      ProbeMap[probe_name] = probeindex;
    }

  for (captureItr = Captures.begin();
       captureItr != Captures.end();
       ++captureItr)
    {
      probe_name = (*captureItr)->name();
      width = (*captureItr)->getBitSize();
      probeindex = vcdWriteDef(probe_name.c_str(), width);
      ProbeMap[probe_name] = probeindex;
    }

  for (probeItr = Probes.begin();
       probeItr != Probes.end();
       ++probeItr)
    {
      probe_name = (*probeItr)->name();
      BSVType * pobj = (*probeItr)->getProbeObject() ;
      vcdWriteProbeHier ("", probe_name.c_str(), pobj, ProbeMap[probe_name] );
    }

  for (captureItr = Captures.begin();
       captureItr != Captures.end();
       ++captureItr)
    {
      probe_name = (*captureItr)->name();
      BSVType * pobj = (*probeItr)->getProbeObject() ;
      vcdWriteProbeHier ("", probe_name.c_str(), pobj, ProbeMap[probe_name] );
    }

  HierProbeDefNodeIterator nodeItr;
  HierProbeDefNode *node;
  ProbeNameMap writtenDefs;

  // This is the new SerialProbe definition
  if (Probes_xactor) {

    buildVcdHierarchicalDefinition();

    for (nodeItr = HierProbeDefNode::topBegin();
	 nodeItr != HierProbeDefNode::topEnd();
	 nodeItr++) {

      node = (*nodeItr).second;
      vcdWriteHierarchicalDefs(node, writtenDefs, 1);
    }
  }

  // get scope back to the top level
  std::string top("");
  goToScope(top);
  flushVCDFile();
  computeGreatestCaptureInterval();
}

void VCDWriter::vcdWriteHierarchicalDefs(HierProbeDefNode *node,
					 ProbeNameMap &upperWrittenDefs, int top)
{
  HierProbeDefNodeIterator childItr;
  ProbeNameMap writtenDefs;
  ProbeNameMapIterator defItr;
  SerialProbeDef *def;
  std::string probe_name;
  unsigned int width;
  unsigned int probeindex;
  int nbytes;

  if (node->def() == NULL) {
    fputs("$scope module ", VCD_file);
    fputs(node->name(), VCD_file);
    fputs(" $end\n", VCD_file);

    for (childItr = node->childBegin();
	 childItr != node->childEnd();
	 childItr++) {

      vcdWriteHierarchicalDefs((*childItr).second, writtenDefs);
    }

    // Write out the hierarchical BSVType definition for any definition that was written out
    if (writtenDefs.size() > 0) {

      for (defItr = writtenDefs.begin(); defItr != writtenDefs.end(); defItr++) {

	for (unsigned int j=0; j < Probes_xactor->getNumberOfProbes(); j++) {
	  def = Probes_xactor->getProbeDefFromIndex(j);
	  probe_name = def->getLabel();
	  width = Probes_xactor->getBitSize(def->getProbeNum());
	  // If the label of the probe matches any earlier probe definition in the param file
	  if (probe_name == (*defItr).first) {
	    Packet pdata;
	    nbytes = 1+(width*32/8);
	    pdata.num_bits = width;
	    if (width > 0) {
	      pdata.data = (SceMiU32 *) malloc (nbytes);
	      memset(pdata.data, 0, nbytes);
	      BSVType * pobj = Probes_xactor->createData(def->getProbeNum(), &pdata, 0);

	      vcdWriteProbeHier ("", probe_name.c_str(), pobj, ProbeMap[probe_name] );
	      free (pdata.data);

	      if (def->isPowerProbe()) {
		probe_name = probe_name + "_Power";
		probeindex = vcdWriteRealDef(probe_name.c_str(), 32);
		ProbeMap[probe_name] = probeindex;
		ProbeNumVCDMap[def->getPowerProbeNum()] = probeindex;
		IsPowerProbeMap[probeindex] = 1;
		has_power = 1;
	      }
	    }
	  }
	}
      }
    }
    /*
    if (top==1 && has_power==1) {
      probe_name = "TotalPower";
      probeindex = vcdWriteRealDef(probe_name.c_str(), 32);
      ProbeMap[probe_name] = probeindex;
      ProbeNumVCDMap[Probes_xactor->getTotalPowerProbeNum()] = probeindex;
      IsPowerProbeMap[probeindex] = 1;
    }
    */
    fputs("$upscope $end\n", VCD_file);

  } else {

    vcdWriteLeafDef(node, upperWrittenDefs);
  }
}

void VCDWriter::vcdWriteLeafDef(HierProbeDefNode *node,
				ProbeNameMap &upperWrittenDefs)
{
  SerialProbeDef *def;
  std::string probe_name;
  unsigned int width;
  unsigned int probeindex = 0;
  std::string snDefName;
  bool written;
  bool real_mode = false;

  def = node->def();
  if (def == NULL)
    return;

  probe_name = def->getLabel();
  if (def->isPowerProbe()) {
    width = Probes_xactor->getBitSize(def->getProbeNum());
    if (width == 0) {
      width = def->getWidth();
      real_mode = true;
    }
  }
  else {
    width = Probes_xactor->getBitSize(def->getProbeNum());
  }
  if (width > 0) {

    // This is a probe with bundle of subnets
    if (def->numSubNets() > 0) {
      SubNetProbeDef *snDef;
      SubNetProbeDefIterator snDefItr;
      stringstream strm;

      for (snDefItr = def->subNetsBegin(); snDefItr != def->subNetsEnd(); snDefItr++) {

	snDef = *snDefItr;
	written = node->parent()->writtenSignal(snDef->name());
	if (written) {
	  snDefName = probe_name + "_" + snDef->name();
	  probeindex = vcdWriteDef(snDefName.c_str(), snDef->width());
	  upperWrittenDefs[node->def()->getLabel()] = snDefName;
	  snDef->setAlias(snDefName.c_str());
	} else {
	  probeindex = vcdWriteDef(snDef->name(), snDef->width());
	  node->parent()->addWrittenSignal(snDef->name());
	  upperWrittenDefs[snDef->name()] = snDef->name();
	}
	ProbeMap[snDef->name()] = probeindex;
	snDef->setVcdID(probeindex);
      }
    } else { // This is individual probe
      if (real_mode) {
	probeindex = vcdWriteRealDef(probe_name.c_str(), width);
	IsPowerProbeMap[probeindex] = 1;
      }
      else
	probeindex = vcdWriteDef(probe_name.c_str(), width);
      upperWrittenDefs[probe_name.c_str()] = probe_name;
      ProbeMap[probe_name] = probeindex;
      ProbeNumVCDMap[def->getProbeNum()] = probeindex;
    }
  }
}

void VCDWriter::buildVcdHierarchicalDefinition()
{
  SerialProbeDef *def;

  // This is the new SerialProbe definition
  if (Probes_xactor) {
    for (unsigned int j=0; j < Probes_xactor->getNumberOfProbes(); j++) {
      def = Probes_xactor->getProbeDefFromIndex(j);
      buildVcdDefinitionTree(def);
    }
  }
}

void VCDWriter::buildVcdDefinitionTree(SerialProbeDef *def)
{
  std::string module_name;
  std::string hier_name = def->getPath();
  size_t pos = 0;
  size_t slash;
  HierProbeDefNode *node, *child;

  if (hier_name[0] == '/') {
    slash = 0;
    pos++;
    slash = hier_name.find_first_of('/', pos);
  } else {
    hier_name = "/main/" + hier_name;
    pos++;
    slash = hier_name.find_first_of('/', pos);
  }
  module_name = hier_name.substr(pos, slash-pos);

  node = HierProbeDefNode::findTopHierProbeDefNode(module_name.c_str());
  if (node == NULL) {
    node = HierProbeDefNode::createTopHierProbeDefNode(module_name.c_str());
  }

  // Build all hierarchical nodes for each module in the path
  pos = slash+1;
  slash = hier_name.find_first_of('/', pos);
  while (slash != std::string::npos) {
    module_name = hier_name.substr(pos, slash-pos);

    if (module_name == "") {
      pos = hier_name.length();
      slash = hier_name.length();
      break;
    }

    child = node->findChild(module_name.c_str());
    if (child == NULL) {
      child = new HierProbeDefNode(module_name.c_str(), node, NULL);
    }
    node = child;
    pos = slash+1;
    slash = hier_name.find_first_of('/', pos);
  }

  // Build the last module after the last slash if it exists
  if (pos < hier_name.length()) {

    module_name = hier_name.substr(pos);

    if (module_name != "") {

      child = node->findChild(module_name.c_str());
      if (child == NULL) {
	child = new HierProbeDefNode(module_name.c_str(), node, NULL);
      }
      node = child;
    }
  }

  // Build the leaf node that points to the signal(SerialProbeDef)
  child = node->findChild(def->getLabel());
  if (child)
    return;
  else {
    child = new HierProbeDefNode(def->getLabel(), node, def);
 }
}


// Write and generate the probe hierarchy for this member pair
void VCDWriter::vcdWriteProbeHier( string hier, const char * nm, BSVType * t,
                                   const unsigned int parentidx)
{
  if ( nm == 0 || t == 0 ) {
    cerr << "unexpected null pointer in vcdWriteProbeHier" << endl;
    return;
  }

  BSVType::BSVKind kind = t->getKind();
  if (kind == BSVType::BSV_Primitive) return;
  if (kind == BSVType::BSV_Enum) return;

  FileTarget dest(VCD_file);

  string appendStr;
  switch (kind) {
    case BSVType::BSV_Struct:       appendStr = "_struct";   break;
    case BSVType::BSV_TaggedUnion:  appendStr = "_union";    break;
    case BSVType::BSV_Vector:       appendStr = "_vector";   break;
    default:                        appendStr = "_expanded"; break;
  }

  string modName = nm + appendStr;
  dest.write_string("$scope module %s $end\n", modName.c_str());

  VcdNumToStructNum[parentidx] = NextVcdId;
  unsigned int tuwidth = t->getTaggedUnionTagWidth();
  if (tuwidth > 0) {
    vcdWriteDef("the_tag", tuwidth );
  }

  unsigned int memCount = t-> getMemberCount();

  // Dump these members as signals
  unsigned int newparent = NextVcdId;

  for (unsigned int i = 0 ; i < memCount; ++ i) {
    BSVType * mem = t->getMember(i);
    const char * name = t->getMemberName(i);
    unsigned int sz = mem->getBitSize();
    vcdWriteDef(name, sz );
  }

  // Dump the hierarchy
  for (unsigned int i = 0 ; i < memCount; ++ i) {
    BSVType * mem = t->getMember(i);
    const char * name = t->getMemberName(i);
    vcdWriteProbeHier(hier+modName, name, mem, newparent + i );
  }

  dest.write_string("$upscope $end\n");
}

unsigned int VCDWriter::vcdWriteRealDef(const char* name,
					unsigned int width)
{
  unsigned int thisindex = NextVcdId ++;

  VcdSignalWidth.push_back(width);
  VcdSignalExists.push_back(true);

  FileTarget dest(VCD_file);
  dest.write_string("$var real %d ", width);
  vcdWriteID(thisindex);

  string tmpname = name;
  size_t space;
  space = tmpname.find_first_of(' ', 0);
  while (space != std::string::npos) {
    tmpname.replace(space, 1, 1, '_');
    space = tmpname.find_first_of(' ', space+1);
  }

  dest.write_string(" %s $end\n", tmpname.c_str());
  return thisindex;
}

unsigned int VCDWriter::vcdWriteDef(const char* name,
                                    unsigned int width)
{
  unsigned int thisindex = NextVcdId ++;

  VcdSignalWidth.push_back(width);
  VcdSignalExists.push_back(true);

  FileTarget dest(VCD_file);
  dest.write_string("$var reg %d ", width);
  vcdWriteID(thisindex);

  string tmpname = munge_name(name);

  dest.write_string(" %s $end\n", tmpname.c_str());
  return thisindex;
}

unsigned int VCDWriter::vcdWriteDef(const char* name,
                                    unsigned int width,
				    unsigned int index)
{

  NextVcdId = max(NextVcdId, index + 1);

  if (index == VcdSignalExists.size()) {

    VcdSignalWidth.push_back(width);
    VcdSignalExists.push_back(true);

  } else {

    unsigned int sz_new = max((unsigned int)VcdSignalExists.size(), index+1);

    VcdSignalExists.resize(sz_new, false);
    VcdSignalWidth.resize(sz_new, 0);
    VcdSignalWidth[index] = width;
    VcdSignalExists[index] = true;

  }

  FileTarget dest(VCD_file);
  dest.write_string("$var reg %d ", width);
  vcdWriteID(index);

  string tmpname = munge_name(name);

  dest.write_string(" %s $end\n", tmpname.c_str());
  return index;
}

void VCDWriter::vcdWriteID(unsigned int num)
{
  char buf[6];
  char* cptr = buf + 6;
  unsigned int len = 0;
  *(--cptr) = '\0';
  do {
    *(--cptr) = static_cast<char>('!' + (num % 94));
    num = num / 94;
    ++len;
  } while (num > 0);
  FileTarget dest(VCD_file);
  dest.write_data(cptr,sizeof(char),len);
}

void VCDWriter::vcdWriteInitials()
{
  char str[128];
  std::string valstr;
  unsigned int width;
  FileTarget dest(VCD_file);
  sprintf(str, "%6.4f", getMaxScale());
  valstr = str;
  //int power = 0;

  LastChangeVal.resize(VcdSignalExists.size(), Change(0, 0));

  dest.write_string("$dumpvars\n");
  for (unsigned int id = 0; id < NextVcdId ; ++ id) {

    if (VcdSignalExists[id]) {
      if (IsPowerProbeMap[id] == 1) {
	printRealChange(id, valstr.c_str());
	//printZero(id);
	//printf("first change id %d val %s\n", id, valstr.c_str());
	Changes[1].push_back(Change(id, "0.0", RealChange ));
	LastChangeVal[id] = Change(id, valstr.c_str(), RealChange);
	//power = 1;
      }
      else {
	width = VcdSignalWidth[id];

	printZ(width, id);
	LastChangeVal[id] = Change(id, "z");
      }
    }
  }

  dest.write_string("$end\n");
  //if (power)
  //  flushAllChangesUpToTime(1, true);
}

void VCDWriter::goToScope (std::string & instname)
{

  //  printf("GOTO: %s | SCOPE CURRENT %s\n", instname.c_str(), ScopeCurrent.c_str());

  if (instname == ScopeCurrent) {
    //    printf("STAYAT: %s\n", ScopeCurrent.c_str());
    return;
  }

  if (isPrefix(ScopeCurrent, instname)) {
    std::string rest = instname.substr(ScopeCurrent.size());
    //    printf("REST: %s\n", rest.c_str());
    size_t pos = rest.find_first_of("/");
    std::string mod = rest.substr(0, pos+1);
    ScopeCurrent.append(mod);
    //    printf("DOWNTO: %s\n", ScopeCurrent.c_str());
    fprintf(VCD_file, "$scope module %s $end\n", mod.substr(0, mod.size()-1).c_str());
    goToScope(instname);
    return;
  }

  size_t pos = ScopeCurrent.substr(0, ScopeCurrent.size()-1).find_last_of("/");
  if (pos ==  std::string::npos) {
    ScopeCurrent.erase(0);
  }
  ScopeCurrent.erase(pos+1);
  // printf("UPTO: %s\n", ScopeCurrent.c_str());
  fprintf(VCD_file, "$upscope $end\n");
  goToScope(instname);
  return;

}

unsigned int VCDWriter::vcdWriteSimpleHier(std::string & name, 
					   unsigned int width, 
					   unsigned int id)
{
  unsigned int ret;

  // printf("WRITE SIMPLE %s [%d] %d\n", name.c_str(), width, id);

  size_t pos = name.find_last_of("/");
  if (pos == std::string::npos) {
    // this is the leaf
    ret = vcdWriteDef(name.c_str(), width, id);
  }
  else {
    // children exist
    std::string instname = name.substr(0, pos+1);
    std::string child = name.substr(pos+1);
    goToScope(instname);
    ret = vcdWriteDef(child.c_str(), width, id);
  }

  return ret;
}

// This is for supporting the old BSV Probes/Captures
void VCDWriter::addChangeProbe(tTime time, unsigned int proben,  BSVType *val, bool toZ )
{
  // Probe numbers == vcd id

  std::string valstr;
  if (toZ) {
    valstr = "z";
  }
  else {
    std::stringstream str;
    val->getBitString(str);
    valstr = str.str();
    if (VcdSignalWidth[proben] != valstr.size()) {
      fprintf(stderr, "Unexpected probe width %d, %d != %d\n", proben, VcdSignalWidth[proben], (int)valstr.size());
    }
  }

  Changes[time].push_back(Change(proben, valstr));
  addChangeHier (time, proben, val, toZ);

  updateLatestTime(time, false);
}


// This is for supporting the new Verilog HW Probes/Captures
void VCDWriter::addChangeSerial(tTime time, unsigned int probeN, BSVType *val, bool toZ )
{
  unsigned int idx = ProbeNumVCDMap[probeN];
  std::string valstr;
  if (toZ) {
    valstr = "z";
  }
  else {
    std::stringstream str;
    val->getBitString(str);
    valstr = str.str();
  }

  // If this probe is a bundle of a bunch of subnets then the message
  // must be disassembled and written out for each subnet
  SerialProbeDef * def = Probes_xactor->getProbeDefFromProbeNum(probeN);
  if (def && def->numSubNets()) {

    addChangeSubNets(time, def, idx, val, toZ);

  } else {
    // This is an individual signal although it still might
    // be hierarchical (ie. BSV struct/tagged union).

    //fprintf(stderr, "AddChangeSerial time %lld probe %d val %s toZ %d\n", time, probeN, valstr.c_str(), toZ);
    Changes[time].push_back(Change(idx, valstr ));
    addChangeHier (time, idx, val, toZ);
  }
}

// This is for supporting the new Verilog HW Probes/Captures
void VCDWriter::addRealSerial(tTime time, unsigned int probeN, float val, bool toZ )
{
  unsigned int idx = ProbeNumVCDMap[probeN];
  char str[128];
  std::string valstr;
  if (toZ) {
    valstr = "0.0";
  }
  else {
    sprintf(str, "%6.4f", val);
    valstr = str;
  }

  //fprintf(stderr, "AddRealSerial time %lld probe %d val %s toZ %d\n", time, probeN, valstr.c_str(), toZ);
  Changes[time].push_back(Change(idx, valstr, RealChange ));
}

void VCDWriter::addChangeSubNets (tTime time, SerialProbeDef *def,
				  unsigned int vcdId, BSVType *val, bool toZ)
{
  SubNetProbeDef *snDef;
  SubNetProbeDefIterator snDefItr;
  stringstream strm;
  std::string valchar;

  for (snDefItr = def->subNetsBegin(); snDefItr != def->subNetsEnd(); snDefItr++) {

    snDef = *snDefItr;
    if (toZ) {
      valchar = "z";
      Changes[time].push_back(Change(snDef->vcdID(), "z"));
    }
    else {
      val->getBitStringRange(strm, snDef->fromBit(), snDef->toBit());
      valchar = strm.str();
      Changes[time].push_back(Change(snDef->vcdID(), valchar));
    }
    //fprintf(stderr, "AddChangeSubNets time %lld probe %d val %s toZ %d\n", time, snDef->vcdID(), valchar.c_str(), toZ);
    strm.str("");
  }
}

void VCDWriter::addChangeHier (tTime time, unsigned int vcdId, BSVType *val, bool toZ)
{
  BSVType::BSVKind kind = val->getKind();
  if (kind == BSVType::BSV_Primitive) return;
  if (kind == BSVType::BSV_Enum) return;

  unsigned int rootId = VcdNumToStructNum[vcdId];

  unsigned int tuwidth = val->getTaggedUnionTagWidth();
  if (tuwidth > 0) {
    if (toZ) {
      Changes[time].push_back(Change(rootId, "z"));
    }
    else {
      SceMiU32 tval = val->getTaggedUnionTag();
      Changes[time].push_back(Change(rootId, tval));
    }
    ++rootId;
  }

  unsigned int memCount = val->getMemberCount();

  // Dump the hierarchy
  for (unsigned int i = 0 ; i < memCount; ++ i) {
    BSVType * mem = val->getMember(i);

    if (toZ) {
      Changes[time].push_back(Change(rootId+i, "z"));
    }
    else {
      stringstream strm;
      mem->getBitString(strm);
      std::string valchar = strm.str();
      Changes[time].push_back(Change(rootId+i, valchar ));
    }

    addChangeHier (time, rootId + i, mem, toZ);
  }
}


int VCDWriter::vcdOutputAtTime(tTime time)
{
  FileTarget dest(VCD_file);

  if ( (_lastTimeValid) && ( _lastTime >= time)) {
    if (_lastTime != time) {
      cerr << "vcd time dumped out of order " << time << " < " << _lastTime << endl;
    }
    return 0;
  }
  _lastTime = time ;
  dest.write_string("#%llu\n", time);
  _lastTimeValid = true;
  return 1;
}

void VCDWriter::flushAllChanges()
{
  map<tTime,tChangeList>::reverse_iterator rcl = Changes.rbegin();
  if (rcl != Changes.rend()) {
    tTime tend = rcl->first;
    flushAllChangesUpToTime(tend+1, true);
  }
}

void VCDWriter::flushAllChangesUpToTime(tTime attime, bool force)
{
  bool written = false;
  bool writtenAtT;
  map<tTime,tChangeList>::iterator cl = Changes.begin();
  while (cl != Changes.end())
    {
      //printf("Next Flush\n");
      tTime t = cl->first;
      writtenAtT = false;

      if (t <= attime) {
	/* Print all VCD entries for this time */
	tChangeList& ch_list = cl->second;
	for (tChangeList::iterator p = ch_list.begin();
	     p != ch_list.end();
	     ++p)
	  {
	    Change& ch = *p;
	    //fprintf(stderr, "flushAllChangesUpToTime time %lld probe %d val %s\n", attime, ch.num, ch.value.c_str());
	    if (ch == LastChangeVal[ch.num]) continue;
	    //fprintf(stderr, "flushAllChangesUpToTime time %lld probe %d not EQUAL\n", attime, ch.num);
	    if (vcdOutputAtTime(t))
	      writtenAtT = true;
	    if (writtenAtT) {
	      written = true;
	      if (ch.type() == BinaryChange) {
		printChange(VcdSignalWidth[ch.num], ch.num, ch.value.c_str());
	      }
	      else {
		printRealChange(ch.num, ch.value.c_str());
	      }
	      LastChangeVal[ch.num] = ch;
	    }
	  }

	Changes.erase(cl++);

      } else {
	break;
      }
    }
  if (force && !written) {
    vcdOutputAtTime(attime);
  }

  flushVCDFile();
}

// Given the current latest time, flush all changes up until
// the latest time less the GreatestCaptureInterval
// Force will guarantee that the file is changes  even if only a TS
void VCDWriter::updateLatestTime(tTime tend, bool force)
{
  tTime tlatest;

  if (tend > GreatestCaptureInterval) {
    tlatest = tend - GreatestCaptureInterval;
    flushAllChangesUpToTime(tlatest, force);
  }
}

// The VCD format elides leading zeros
void VCDWriter::printBinary(Target* dest, unsigned int width, tUInt64 value)
{
  bool leading_zero = true;
  while (width-- > 0)
  {
    if ((value >> width) & 0x1llu)
    {
      dest->write_char('1');
      leading_zero = false;
    }
    else if (!leading_zero)
    {
      dest->write_char('0');
    }
  }

  if (leading_zero)
  {
    // there were no non-zero bits at all!
    dest->write_char('0');
  }
}

void VCDWriter::printX(unsigned int bits, unsigned int num)
{
  FileTarget dest(VCD_file);
  if (bits == 1)
    dest.write_char('x');
  else
    dest.write_string("bx ");
  vcdWriteID(num);
  dest.write_char('\n');
}

void VCDWriter::printZ(unsigned int bits, unsigned int num)
{
  FileTarget dest(VCD_file);
  if (bits == 1)
    dest.write_char('z');
  else
    dest.write_string("bz ");
  vcdWriteID(num);
  dest.write_char('\n');
}

void VCDWriter::printZero(unsigned int num)
{
  FileTarget dest(VCD_file);
  dest.write_string("r0.0 ");
  vcdWriteID(num);
  dest.write_char('\n');
}

void VCDWriter::printChange(unsigned int bits, unsigned int num, const char *val)
{
  FileTarget dest(VCD_file);
  if (bits == 1) {
    dest.write_char(*val);
  }
  else
  {
    dest.write_char('b');
    //printf("printChange %s\n", val);
    dest.write_string(val);
    dest.write_char(' ');
  }
  vcdWriteID(num);
  dest.write_char('\n');
}

void VCDWriter::printRealChange(unsigned int num, const char *val)
{
  FileTarget dest(VCD_file);
  dest.write_char('r');
  //printf("printRealChange %s\n", val);
  dest.write_string(val);
  dest.write_char(' ');

  vcdWriteID(num);
  dest.write_char('\n');
}

void VCDWriter::flushVCDFile()
{
  if (VCD_file != NULL)
    fflush(VCD_file);
}

unsigned int VCDWriter::getID(ProbeXactor &px)
{
  return ProbeMap[px.name()];
}

unsigned int VCDWriter::getID(CaptureXactor &px)
{
  return ProbeMap[px.name()];
}
