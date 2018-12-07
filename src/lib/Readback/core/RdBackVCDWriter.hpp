// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <list>
#include <vector>

#include "ReadBackProbe.hpp"

namespace RdBack {

/* VCD change buffer mechanism
 *
 * VCD changes are buffered and accumulated until all clocks which
 * can assign a change to that time have occured, at which point
 * changes for that time are written to the VCD file.
 */
typedef unsigned char      tUInt8;
typedef unsigned int       tUInt32;
typedef unsigned long long tUInt64;
typedef tUInt64 tTime;

class CompareReadBackProbeName {
 public:
  bool operator() (ReadBackProbe *l, ReadBackProbe *r) {
    if (l->getBaseName() == r->getBaseName()) {
      return (l->getIndex() < r->getIndex());
    } else {
      return (l->getBaseName() < r->getBaseName());
    }
  }
};

typedef std::set<ReadBackProbe*,  CompareReadBackProbeName> tRdBkSet;

enum ChangeType { BinaryChange, RealChange };

// Represents a change of a value
class Change
{
public:
  Change(unsigned int n, const std::string &v, ChangeType=BinaryChange);
  void removeLeadingZeros ();
  bool operator== (const Change &);
  ChangeType type() { return changetype; }

public:
  tUInt32 num;   // VCD ID number
  std::string value;       // the value
  ChangeType changetype;
};

// Represents the list of changes at a single time
typedef std::list<Change> tChangeList;

// C++ class for dumping VCD from Probe
class VCDWriter {
protected:

  // VCD output filename and unix file pointer
  std::string VCDFileName;
  FILE* VCD_file;

  // Flag that records the file has been opened and header written
  bool VCD_started;

  // A vector of ReadBackProbe that the VCDWriter keep track of
  tRdBkSet ReadBackProbes;

  // A vector of last change value written out to vcd file
  std::vector<Change> LastChangeVal;

  // A list of changes for each moment in time
  std::map<tTime,tChangeList> Changes;  // all pending changes

  // The unit of time period of each output time unit
  std::string TimeScale;

  // Mapping from VCD id to the VCD Id of the first child in the structure.
  std::map<unsigned int,unsigned int> VcdNumToStructNum;
  std::vector<unsigned int> VcdSignalWidth;
  std::vector<bool>         VcdSignalExists;

  // The next vcd Index assigned to be assigned.
  unsigned int NextVcdId;

  // MaxScale for graph of real numbers
  float MaxScale;

  // The current hierarchy scope
  std::string ScopeCurrent;

  // the current time in vcd file #<n>
  tTime   _lastTime;
  bool    _lastTimeValid;

public:

  // Constructors
  VCDWriter();
  VCDWriter(const char *vcdFile);

  // Destructor
  ~VCDWriter();

  // File name
  const char *vcdFileName();

  // Register ReadBackProbe for tracking
  void registerReadBackProbe(ReadBackProbe *p);

  // Unregister ReadBackProbe for tracking
  void unregisterReadBackProbe(ReadBackProbe *p);

  // Set timescale
  void setTimeScale(const char* ts) { TimeScale = ts; }

  // Helper methods for writing various type of data to VCD file
  bool vcdWriteHeader();
  void vcdWriteDefs();
  void vcdWriteInitials();
  unsigned int vcdWriteDef(const char* name, unsigned int width);
  unsigned int vcdWriteDef(const char* name, unsigned int width, unsigned int index);
  unsigned int vcdWriteRealDef(const char* name, unsigned int width);
  void vcdWriteID(unsigned int num);
  void goToScope(std::string & inst_name);
  unsigned int vcdWriteSimpleHier(std::string & name, unsigned int width, unsigned int id);

  // Add change of data
  void addChangeReadBack(tTime time, unsigned int probeN, const std::string & val, bool toZ=false);

  // Start a VCD file
  void startVCDFile();

  // End a VCD file
  void endVCDFile();

  // Reset the writer
  void reset(const char *backup_vcdfile);

  // Set the name of output VCD file
  bool setVCDFile(const char* name);

  // Print new time stamp
  int vcdOutputAtTime(tTime time);

  // Flush vcd file
  void flushVCDFile();

  // Flush changes to vcd file
  void updateLatestTime(tTime time, bool force);

  void setMaxScale(float v) { MaxScale = v; }
  float getMaxScale() { return MaxScale; }

private:
  // Flush changes to vcd file
  void flushAllChanges();
  void flushAllChangesUpToTime(tTime time, bool force);

  // Printing routines
  void printX(unsigned int bits, unsigned int num);
  void printZ(unsigned int bits, unsigned int num);
  void printZero(unsigned int num);
  void printChange(unsigned int bits, unsigned int num, const char *v);
  void printRealChange(unsigned int num, const char *v);
};

};
