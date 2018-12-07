// Copyright (c) 2009, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <string>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <list>

#include "BSVType.h"

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

class ProbeXactor;
class CaptureXactor;
class Target;
class ProbesXactor;
class SerialProbeDef;
class HierProbeDefNode;

enum ChangeType { BinaryChange, RealChange };

// Represents a change of a value
class Change
{
public:
  Change(unsigned int n, const std::string &v, ChangeType=BinaryChange);
  Change(unsigned int n, SceMiU32 v);
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

// A map (hashtable) of vcd definition written at a given
// hierarchy level
typedef std::map<std::string, std::string> ProbeNameMap;
typedef std::map<std::string, std::string>::iterator ProbeNameMapIterator;


// C++ class for dumping VCD from Probe
class VCDWriter {
protected:

  // VCD output filename and unix file pointer
  std::string VCDFileName;
  FILE* VCD_file;

  // Flag that records the file has been opened and header written
  bool VCD_started;

  // A vector of ProbeXactor that the VCDWriter keeps track of
  std::vector<ProbeXactor*> Probes;

  // A vector of CaptureXactor that the VCDWriter keeps track of
  std::vector<CaptureXactor*> Captures;

  // A vector of last change value written out to vcd file
  std::vector<Change> LastChangeVal;

  // Pointer to the new ProbesXactor (Serial)
  ProbesXactor *Probes_xactor;

  // A mapping from ProbeXactor name and the vcd ID
  std::map<std::string,unsigned int> ProbeMap;

  // mapping from probe numbers (in the params file) to the vcd number (used to convert to vcd code)
  std::map<unsigned int,unsigned int> ProbeNumVCDMap;

  // mapping from vcd numbers to whether the probe is power probe
  std::map<unsigned int,unsigned int> IsPowerProbeMap;

  // A list of changes for each moment in time
  std::map<tTime,tChangeList> Changes;  // all pending changes

  // The unit of time period of each output time unit
  std::string TimeScale;

  // The greatest capture interval of all the captures registered to this writer
  tTime GreatestCaptureInterval;

  // Padding for the greatest capture interval
  tTime GreatestCaptureIntervalPadding;

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
  VCDWriter(const char *vcdFile, const tTime padding);

  // Destructor
  ~VCDWriter();

  // File name
  const char *vcdFileName();

  // Register ProbeXactor for tracking
  void registerProbe(ProbeXactor *probe);

  // Unregister ProbeXactor for tracking
  void unregisterProbe(ProbeXactor *probe);

  // Register CaptureXactor for tracking
  void registerCapture(CaptureXactor *c);

  // Unregister CaptureXactor for tracking
  void unregisterCapture(CaptureXactor *c);

  // Register ProbeXactor for tracking
  void registerProbesXactor(ProbesXactor *probeXtr);

  // Register ProbeXactor for tracking
  void unregisterProbesXactor() { Probes_xactor = NULL; }

  // Calculate the greatest capture interval
  void computeGreatestCaptureInterval();

  // Return number of ProbeXactors being tracked
  size_t numProbes();
  size_t numCaptures();

  // Set timescale
  void setTimeScale(const char* ts) { TimeScale = ts; }

  // Helper methods for writing various type of data to VCD file
  bool vcdWriteHeader();
  void vcdWriteDefs();
  void vcdWriteHierarchicalDefs(HierProbeDefNode *node, ProbeNameMap &map, int top=0);
  void vcdWriteLeafDef(HierProbeDefNode *node, ProbeNameMap &map);
  void vcdWriteInitials();
  unsigned int vcdWriteDef(const char* name, unsigned int width);
  unsigned int vcdWriteDef(const char* name, unsigned int width, unsigned int index);
  unsigned int vcdWriteRealDef(const char* name, unsigned int width);
  void vcdWriteID(unsigned int num);
  void buildVcdHierarchicalDefinition();
  void buildVcdDefinitionTree(SerialProbeDef *def);
  void goToScope(std::string & inst_name);
  unsigned int vcdWriteSimpleHier(std::string & name, unsigned int width, unsigned int id);

  // Get vcd ID given a ProbeXactor
  unsigned int getID(ProbeXactor &px);
  unsigned int getID(CaptureXactor &px);

  // Add change of data
  void addChangeProbe(tTime time, unsigned int probeN, BSVType *val, bool toZ=false );
  void addChangeSerial(tTime, unsigned int probeN, BSVType *val, bool toZ=false );
  void addChangeHier (tTime time, unsigned int vcdId, BSVType *val, bool toZ);
  void addChangeSubNets (tTime time, SerialProbeDef *def,
			 unsigned int vcdId, BSVType *val, bool toZ);
  void addRealSerial(tTime time, unsigned int probeN, float val, bool toZ=false);

  // Start a VCD file
  void startVCDFile();

  // Start a VCD file
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
  void printBinary(Target* dest, unsigned int width, tUInt64 value);
  void printChange(unsigned int bits, unsigned int num, const char *v);
  void printRealChange(unsigned int num, const char *v);
  void vcdWriteProbeHier(std::string hier, const char *nm,  BSVType *t, const unsigned int);
};

