// Copyright (c) 2013, Bluespec, Inc.  ALL RIGHTS RESERVED
#include <set>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <bitset>

#include <argp.h>
#include <unistd.h>
#include <stdint.h>

#include "fpga.h"
#include "cable.h"
#include "jtag.h"
#include "interface.h"
#include "log.h"
#include "control.h"

#include "Design.hpp"
#include "IReadBackControl.h"
#include "VCDWriter.h"

using namespace std;

const char *argp_program_version      = "1.0";
const char *argp_program_bug_address  = "<support@bluespec.com>";

static char doc[] = "Application to readback state from various Xilinx FPGAs via JTAG";

/* the options we understand */
static struct argp_option options[] = {
  { "list",               'l', 0,              0, "list supported fpgas and cables" },
  { "cable",              'c', "CABLE_TYPE",   0, "Use specified cable (or \"auto\" to detect)" },
  { "index",              'i', "JTAG_LOC",     0, "Use specified device at location in chain" },
  { "xrf",                'x', "XRF_FILE",     0, "Location of XRF database" },
  { "signal",             's', "SIGNAL_PATH",  0, "Get Signal Value" },
  { "collection",         'z', "SIGNALS_FILE", 0, "Parse Signal Collection File" },
  { "waveform",           'w', "FILENAME",     0, "Name of VCD file to dump." },
  { "verbose",            'v', 0,              0, "Show verbose messages" },
  { "debug",              'd', 0,              OPTION_HIDDEN, "Show super verbose messages" },
  { 0 }
};

/* used by main to communicate with parse_opt. */
struct arguments
{
  bool           list_fpgas_cables;
  string         cable_name;
  uint32_t       device_index;
  string         xrf_path;
  string         vcd_path;
  vector<string> signals;
  string         signal_collection;
  bool           verbose_mode;
  bool           debug_mode;

  arguments()
  {
    list_fpgas_cables = false;
    cable_name        = "";
    device_index      = 0;
    xrf_path          = "";
    vcd_path          = "dump1.vcd";
    signals           = vector<string>();
    signal_collection = "";
    verbose_mode      = false;
    debug_mode        = false;
  }
};

static error_t
parse_opt(int key, char *arg, struct argp_state *state)
{
  struct arguments *p_args = static_cast<arguments*>(state->input);

  switch(key) {
    case 'l':
      p_args->list_fpgas_cables = true;
      break;

    case 'c':
      p_args->cable_name = arg;
      break;

    case 'd':
      p_args->debug_mode = true;
      break;

    case 'i':
      p_args->device_index = atoi(arg);
      break;

    case 'x':
      p_args->xrf_path = arg;
      break;

    case 's':
      p_args->signals.push_back(arg);
      break;

    case 'z':
      p_args->signal_collection = arg;
      break;

    case 'v':
      p_args->verbose_mode = true;
      break;

    case 'w':
      p_args->vcd_path = arg;
      break;

    default:
      return ARGP_ERR_UNKNOWN;
  }
  
  return 0;
}

void
list_fpgas_and_cables()
{
  try {
    set<string> fpgas = interface_registry<fpga>::shared_instance().get_entries();
    set<string> cables = interface_registry<cable>::shared_instance().get_entries();
    set<string>::iterator str;
    cout << "supported fpgas" << endl << "-------" << endl;
    for(str = fpgas.begin(); str != fpgas.end(); str++) {
      fpga *f = interface_registry<fpga>::shared_instance().create_object_of(*str);
      printf("  %-15s  %s\n", str->c_str(), f->description.c_str());
      delete f;
    }
    cout << endl;
    cout << "supported cables  ('*' are present)" << endl << "-------" << endl;
    for(str = cables.begin(); str != cables.end(); str++) {
      cable *c = interface_registry<cable>::shared_instance().create_object_of(*str);
      string present_str = c->is_present() ? string("*") : string(" ");
      printf("%s %-15s  %s\n", present_str.c_str(), str->c_str(), c->description.c_str());
      delete c;
    }
    cout << endl;
  } catch (const string& err) {
    cerr << err << endl;
  }
}

string 
convert_binstr_to_hexstr(const string &s)
{
  int numofchar;
  int32_t index;
  uint8_t byteNibble = 0;
  string outstr, tmp;

  if (s.empty())
    return string("");

  // process each group of 4 bits 
  numofchar = (s.length()+3) / 4;
  index = s.length() - 1;
  for(int i = 0; i < numofchar; i++) {
    if (s[index] == '1') byteNibble |= 1;
    index--;
    if (index >= 0) {
      if (s[index] == '1') byteNibble |= 2;
      index--;
    }
    if (index >= 0) {
      if (s[index] == '1') byteNibble |= 4;
      index--;
    }
    if (index >= 0) {
      if (s[index] == '1') byteNibble |= 8;
      index--;
    }
    
    if (byteNibble < 10) {
      tmp += byteNibble + '0';
      outstr.insert(0, tmp);
    } else {
      tmp += byteNibble - 10 + 'A';
      outstr.insert(0, tmp);
    }
    byteNibble = 0;
    tmp = "";
  }
  return outstr;
}

void
parse_signal_collection(struct arguments &s_args)
{
  string line;
  ifstream collection (s_args.signal_collection.c_str());
  if (collection.is_open()) {
    while(getline(collection,line)) {
      stringstream ss(line);
      string col; string path; string fmt;
      ss >> col >> path >> fmt;
      if (path != "") {
	s_args.signals.push_back(path);
      }
    }
    collection.close();
  } else {
    cerr << "Unable to open " << s_args.signal_collection << endl;
    exit(-1);
  }
}

int
main(int argc, char *argv[])
{
  Log logfile;

  static struct argp argp = { options, parse_opt, 0, doc };

  // defaults 
  struct arguments s_args = arguments();

  // parse commandline
  argp_parse (&argp, argc, argv, 0, 0, &s_args);

  // If just listing the cables, do it and exit.
  if (s_args.list_fpgas_cables) {
    list_fpgas_and_cables();
    return 0;
  }

  // If setting verbose mode, update the logger
  if (s_args.verbose_mode) {
    logfile.SetOutputLevel(MSG_DEBUG);
  }

  // If we are in debug mode, update the logger to dump everything
  if (s_args.debug_mode) {
    logfile.SetOutputLevel(MSG_DEBUG10);
  }

  // If we have received a signal list, process the file
  if (s_args.signal_collection != "") {
    parse_signal_collection(s_args);
  }

  RdBack::JtagSimulationControl *pSimControl = 0;
  Design *pDesign = 0;
  VCDWriter *pVCDWriter = 0;
  unsigned int iterations = 0;
  unsigned int status;
  vector<Signal*> vSignalList;
  vector<string>::iterator iSignalStr;
  vector<Signal*>::iterator iSignal;

  // otherwise we are here to do some work.  
  try {
    // auto-detect cable
    if (s_args.cable_name == "") {
      set<string> cables = interface_registry<cable>::shared_instance().get_entries();
      set<string>::iterator str;

      for(str = cables.begin(); str != cables.end(); str++) {
	cable *c = interface_registry<cable>::shared_instance().create_object_of(*str);
	if (c->is_present()) {
	  cout << "Using " << c->description.c_str() << endl;
	  s_args.cable_name = str->c_str();
	}
	delete c;
      }
      if (s_args.cable_name == "") {
	throw string ("Failed to locate a supported JTAG cable!");
      }
    }

    // Allocate Objects
    pSimControl = new RdBack::JtagSimulationControl(s_args.cable_name, s_args.device_index);
    if (!pSimControl) {
      throw string("Failed to allocate JtagSimulationControl object");
    }

    pDesign = new Design();
    if (!pDesign) {
      throw string("Failed to allocate Design object");
    }

    pVCDWriter = new VCDWriter(s_args.vcd_path.c_str(), 0);
    if (!pVCDWriter) {
      throw string("Failed to allocate VCDWriter object");
    }

    // Connect them up
    pDesign->setSimControl((RdBack::IReadBackSimulationControl*)pSimControl);
    pDesign->setVCDWriter(pVCDWriter);

    // Verify the XRF is specified
    if (s_args.xrf_path.empty()) {
      throw string("Must specify XRF database path!");
    }
    
    // Read in the XRF database
    pDesign->parse_xrf(s_args.xrf_path);
    pDesign->syncConfig();

    // Find all the signals for which we want to capture state
    for(iSignalStr = s_args.signals.begin(); iSignalStr != s_args.signals.end(); iSignalStr++) {
      Signal *pSignal = pDesign->findSignal(RTL, *iSignalStr);
      if (pSignal == NULL) {
	throw string(string("Signal: ") + *iSignalStr + string(" was not found!  Please check your signal path!"));
      }
      pDesign->enableSignal(pSignal);
      vSignalList.push_back(pSignal);
    }

    // Get the data until we have it all, or timed out trying...
    do {
      //usleep(100);
      status = RdBack::JtagSimulationControl::logRdBack(pSimControl);
      iterations++;
    } while((status == 0) && (iterations < 2000));

    // If we timed out, provide error status
    if (iterations == 2000) {
      throw string("timed out waiting for readback data!");
    }

    // Refresh the signal values in the VCD file.
    pDesign->flushVCD();

    // Now that the samples are in, let's display them!
    for(iSignal = vSignalList.begin(); iSignal != vSignalList.end(); iSignal++) {
      cout << (*iSignal)->getFullName() << " " 
	   << (*iSignal)->getAvail()    << "'h" 
	   << convert_binstr_to_hexstr((*iSignal)->getValueStr(false)) << " "
	   << (*iSignal)->getValueStr(false)
	   << endl;
    }

    delete pDesign;
    delete pVCDWriter;
    delete pSimControl;
  } 

  catch(const runtime_error &error) {
    cerr << error.what() << endl;
    if (pDesign) delete pDesign;
    if (pVCDWriter) delete pVCDWriter;
    if (pSimControl) delete pSimControl;
    return -1;
  } 

  catch(const string &msg) {
    cerr << msg << endl;
    if (pDesign) delete pDesign;
    if (pVCDWriter) delete pVCDWriter;
    if (pSimControl) delete pSimControl;
    return -1;
  }

  catch(...) {
    cerr << "Unknown error occurred" << endl;
    if (pDesign) delete pDesign;
    if (pVCDWriter) delete pVCDWriter;
    if (pSimControl) delete pSimControl;
    return -2;
  }

  cout << "Done." << endl;
  return 0;
}
