// Copyright (c) 2010, Bluespec, Inc.  ALL RIGHTS RESERVED

#include <cstring>

#include "Target.h"
#include "DataWriter.h"

using namespace std;

// Data generator version
static const unsigned int major_rev = 2;
static const unsigned int minor_rev = 1;

// Constructors
DataWriter::DataWriter()
    : Data_file(NULL)
    , Data_started(false)
{ }

DataWriter::DataWriter(const char *dataFile)
    : Data_file(NULL)
    , Data_started(false)
{ setDataFile(dataFile); }

// Destructor
DataWriter::~DataWriter()
{
  flushDataFile();
  endDataFile();
}

  // File name
const char *DataWriter::dataFileName() 
{ return DataFileName.c_str(); }


void DataWriter::startDataFile()
{
  // Already started, just returns
  if (Data_started == true)
    return;

  if (DataFileName.size() == 0) {
    cerr << "Error: Data file name was not previously set." << endl;
    return;
  }

  Data_file = fopen(DataFileName.c_str(), "w");

  Data_started = true;
}

void DataWriter::endDataFile()
{

  // Already started, just returns
  if (Data_started == false)
    return;

  Data_started = false;

  if (Data_file != NULL) {
    fclose(Data_file);
  } else {
    return;
  }
}

bool DataWriter::setDataFile(const char* name)
{
  if (!strcmp(name, DataFileName.c_str()))
    return true;

  if (Data_file != NULL)
    fclose(Data_file);
  DataFileName.resize(0);

  if (name == NULL)
  {
    Data_file = NULL;
    return true;
  }

  DataFileName = name;
  Data_file = fopen(name, "w");
  Data_started = false;

  if (Data_file == NULL)
  {
    DataFileName.resize(0);
    perror(name);
    return false;
  }

  return true;
}

void DataWriter::addData(SceMiU32 data)
{
  FileTarget dest(Data_file);
  dest.write_string("D: %08x\n", data);
  flushDataFile();
}

void DataWriter::addStart(tTime time, unsigned int probe_num, unsigned int scan_num, unsigned int probe_prev, unsigned int scan_prev)
{
  FileTarget dest(Data_file);
  if (false && probe_num == probe_prev && scan_num == scan_prev) {
    dest.write_string("R: %llu\n", time);
  } else {
    dest.write_string("P: %d %d %llu\n", probe_num, scan_num, time);
  }
  flushDataFile();
}

void DataWriter::addFinish()
{
  FileTarget dest(Data_file);
  dest.write_string("finish:\n");
  flushDataFile();
}

void DataWriter::flushDataFile()
{
  if (Data_file != NULL)
    fflush(Data_file);
}

