// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED

#include <time.h>
#include <cstdio>
#include <cstring>
#include <list>
#include <set>
#include <map>
#include <string>
#include <math.h>
#include <fstream>
#include <cstdarg>

#include "RdBackVCDWriter.hpp"

using namespace std;
using namespace RdBack;

// VCD generator version
static const unsigned int major_rev = 2;
static const unsigned int minor_rev = 1;

class RdBackFileTarget
{
private:
  FILE* out;
public:
  RdBackFileTarget(FILE* file_ptr): out(file_ptr) { }
  ~RdBackFileTarget() { }
  void write_char(char c) { fputc(c, out); }
  void write_char(char c, unsigned int count) { while (count-- > 0) fputc(c, out); }
  void write_string(const char* fmt ...)
  {
    va_list ap;
    va_start(ap,fmt);
    vfprintf(out,fmt,ap);
    va_end(ap);
  }
  void write_data(const void* data, unsigned int size, unsigned int num)
  {
    if (fwrite(data,size,num,out) != num)
      perror("RdBackFileTarget::write_data");
  }
};

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
    , NextVcdId(0)
    , MaxScale(0.0)
    , ScopeCurrent("")
    , _lastTime(0)
    , _lastTimeValid(false)
{
}

VCDWriter::VCDWriter(const char *vcdFile)
    : VCD_file(NULL)
    , VCD_started(false)
    , TimeScale("1 s")  // default to seconds
    , NextVcdId(0)
    , MaxScale(0.0)
    , ScopeCurrent("")
    , _lastTime(0)
    , _lastTimeValid(false)
{
  setVCDFile(vcdFile);
}

// Destructor
VCDWriter::~VCDWriter()
{
  flushAllChanges();
  endVCDFile();
}

  // File name
const char *VCDWriter::vcdFileName()
{ return VCDFileName.c_str(); }

void VCDWriter::registerReadBackProbe(ReadBackProbe *p)
{
    ReadBackProbes.insert(p);
}

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

  RdBackFileTarget dest(VCD_file);

  time_t t = time(NULL);
  dest.write_string("$date\n\t%s$end\n", ctime(&t));
  dest.write_string("$version\n");
  dest.write_string("\tBluespec Readback VCD dumper %d.%d\n", major_rev, minor_rev);
  dest.write_string("$end\n");
  dest.write_string("$timescale\n\t%s\n$end\n", TimeScale.c_str());

  //next_seq_num = kept_seq_num;

  return true;
}


void VCDWriter::unregisterReadBackProbe(ReadBackProbe *p)
{
    ReadBackProbes.erase(p);
}

void VCDWriter::vcdWriteDefs()
{
  std::string probe_name;
  unsigned int width;
  unsigned int index;

  for (tRdBkSet::iterator it = ReadBackProbes.begin();
       it != ReadBackProbes.end();
       it++)
  {
      probe_name = (*it)->getName();
      width = (*it)->getWidth();
      index = (*it)->getId();
      vcdWriteSimpleHier(probe_name, width, index);
  }

  // get scope back to the top level
  std::string top("");
  goToScope(top);
  flushVCDFile();
}


unsigned int VCDWriter::vcdWriteRealDef(const char* name,
					unsigned int width)
{
  unsigned int thisindex = NextVcdId ++;

  VcdSignalWidth.push_back(width);
  VcdSignalExists.push_back(true);

  RdBackFileTarget dest(VCD_file);
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

  RdBackFileTarget dest(VCD_file);
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

  RdBackFileTarget dest(VCD_file);
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
  RdBackFileTarget dest(VCD_file);
  dest.write_data(cptr,sizeof(char),len);
}

void VCDWriter::vcdWriteInitials()
{
  char str[128];
  std::string valstr;
  unsigned int width;
  RdBackFileTarget dest(VCD_file);
  sprintf(str, "%6.4f", getMaxScale());
  valstr = str;

  LastChangeVal.resize(VcdSignalExists.size(), Change(0, ""));

  dest.write_string("$dumpvars\n");
  for (unsigned int id = 0; id < NextVcdId ; ++ id) {
    if (VcdSignalExists[id]) {
      width = VcdSignalWidth[id];
      printZ(width, id);
      LastChangeVal[id] = Change(id, "z");
    }
  }
  dest.write_string("$end\n");
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

void VCDWriter::addChangeReadBack(tTime time, unsigned int probeN, const std::string & val, bool toZ)
{
  Changes[time].push_back(Change(probeN, toZ ? "z" : val));
}


int VCDWriter::vcdOutputAtTime(tTime time)
{
  RdBackFileTarget dest(VCD_file);

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

// Flush all changes up until the latest time
void VCDWriter::updateLatestTime(tTime tend, bool force)
{
  flushAllChangesUpToTime(tend, force);
}

void VCDWriter::printX(unsigned int bits, unsigned int num)
{
  RdBackFileTarget dest(VCD_file);
  if (bits == 1)
    dest.write_char('x');
  else
    dest.write_string("bx ");
  vcdWriteID(num);
  dest.write_char('\n');
}

void VCDWriter::printZ(unsigned int bits, unsigned int num)
{
  RdBackFileTarget dest(VCD_file);
  if (bits == 1)
    dest.write_char('z');
  else
    dest.write_string("bz ");
  vcdWriteID(num);
  dest.write_char('\n');
}

void VCDWriter::printZero(unsigned int num)
{
  RdBackFileTarget dest(VCD_file);
  dest.write_string("r0.0 ");
  vcdWriteID(num);
  dest.write_char('\n');
}

void VCDWriter::printChange(unsigned int bits, unsigned int num, const char *val)
{
  RdBackFileTarget dest(VCD_file);
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
  RdBackFileTarget dest(VCD_file);
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
