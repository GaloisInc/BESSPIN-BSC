// Copyright (c) 2010, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

#include <iostream>

#include "BSVType.h"

typedef unsigned char      tUInt8;
typedef unsigned int       tUInt32;
typedef unsigned long long tUInt64;
typedef tUInt64 tTime;

// C++ class for dumping Data from Scan Chain
class DataWriter {
protected:

  // Data output filename and unix file pointer
  std::string DataFileName;
  FILE* Data_file;

  // Flag that records the file has been opened and header written
  bool Data_started;

public:

  // Constructors
  DataWriter();
  DataWriter(const char *dataFile);

  // Destructor
  ~DataWriter();

  // File name
  const char *dataFileName();

  // Add change of data
  void addData(SceMiU32 data);
  void addStart(tTime time, unsigned int probe_num, unsigned int scan_num, unsigned int probe_prev, unsigned int scan_prev);
  void addFinish();


  // Start a Data file
  void startDataFile();

  // End a Data file
  void endDataFile();

  // Set the name of output Data file
  bool setDataFile(const char* name);

  // Flush data file
  void flushDataFile();


};


