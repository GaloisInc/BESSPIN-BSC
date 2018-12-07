// Copyright 2010 Bluespec Inc.  All rights reserved
#pragma once

// Common type defintion -- wrapper around std types.

#include <cstdio>
#include <string>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <iostream>
#include "VeriId.h"

typedef std::set<Verific::VeriIdDef*> tIdSet;
typedef std::list<Verific::VeriIdDef*> tIdList;
typedef std::set<Verific::VeriStatement*> tStatementSet;
typedef std::set<Verific::VeriIdRef*> tRefSet;
typedef std::set<Verific::VeriModuleItem*> tModuleItemSet;

// Define string type
typedef std::string BString;
typedef std::list<BString> BStringList;
typedef std::set<BString>  BStringSet;
typedef std::list<BString>::iterator BStringListIterator;

// Some int containers
typedef std::list<int> IntList;
typedef std::set<int>  IntSet;

typedef std::list<unsigned int> UIntList;
typedef std::set<unsigned int>  UIntSet;

// Handles into the netlist model  -- these will change
typedef BString instHandle;
typedef BString  netHandle;
typedef BString  portHandle;

typedef std::list<netHandle> netCollection;

// Utility conversion function
const BString getSignalName(const netHandle h);
const BString getWidthString(const netHandle h);
void buildConcatString (const BStringList &sigs, BString &concat);
void splitInstanceName (const BString &full, BString & up, BString & thisinst);
char * itoa (int i);
void removeAllSpaces(std::string &str);
void qualifySpecialChar(BString &h);


// Port directions
enum DirectionE {
  d_input, d_output, d_inout
};
const char * getStrFromDir ( const DirectionE d );

// Terminal are names ports
struct ModuleTerminal {
  BString m_portName;
  BString m_netName;
  DirectionE m_dir;
  unsigned int m_width;
  int m_sync_clock;
  ModuleTerminal (const BString &portName, const BString &netName, DirectionE dir = d_input,
		  int w = 0, int sync = 0)
  : m_portName(portName), m_netName(netName), m_dir(dir), m_width(w), m_sync_clock(sync) {};
};

typedef std::list<ModuleTerminal> ModuleTerminalList;
typedef std::list<ModuleTerminal>::iterator ModuleTerminalIterator;


// Parameters -- name value pairs
struct Parameter {
  BString m_name;
  BString m_value;
  Parameter (const BString &name, const BString &value)
    : m_name(name), m_value(value) {}
  Parameter (const BString &name, const unsigned int value)
    : m_name(name), m_value() {
    static char buf[12];
    snprintf (buf, 12, "%d", value);
    m_value = buf;
  }
};

typedef std::list<Parameter> ParameterList;
typedef std::list<Parameter>::iterator ParameterListIterator;


// Added ports
struct PushedPort {
  BString m_portName;
  BString m_uniqPortName;
  bool    m_broadcast;          // e.g. Clocks and Reset,  not ack and data
  unsigned int m_width;
  PushedPort (const BString &port, const BString &unique, bool broadcast, unsigned int width)
    : m_portName(port), m_uniqPortName(unique), m_broadcast(broadcast), m_width(width) {}
};

typedef std::list< PushedPort >  AddedPortList;
typedef std::list< PushedPort >::const_iterator  AddedPortListIterator;

// SignalType for probes
enum SignalType { CktModAny=1, CktModFlop=2, CktModReg=4, CktModInput=8, CktModOutput=16 };


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

enum CosimFlavor { Observe, Replace };
enum ScanFlavor  { State, Inputs, InputsBB, Outputs };

// Which set of modifications? To create the modified design or the
// associated cosim design (for sim only).
enum ModSet { ProbeMods, CosimMods }; 


std::string ToString(CosimFlavor flavor);
void FromString(CosimFlavor &flavor, std::string label);

////////////////////////////////////////////////////////////////////////////////
/// Some utility functions for printing things ...
////////////////////////////////////////////////////////////////////////////////

using std::cout;
using std::endl;
using std::list;

template <class T>
std::ostream& operator<< (std::ostream& os, std::list<T> _x)
{

 os << "<list";
 typename std::list<T>::iterator it;
 for( it = _x.begin(); it != _x.end(); ++it)
   {
     T zow = (T) *it;
     os << " ";
     os << zow;
   }
  os << ">";
  return os;
}

template <class T>
std::ostream& operator<< (std::ostream& os, std::set<T> _x)
{

 os << "<set";
 typename std::set<T>::iterator it;
 for( it = _x.begin(); it != _x.end(); ++it)
   {
     T zow = (T) *it;
     os << " ";
     os << zow;
//     os << zow->Name();
   }
  os << ">";
  return os;
}


template < class T >
std::string ToString(const T &arg)
{
	std::ostringstream out;
	out << arg;

	return(out.str());
}



const AddedPortList& getPushedPort(const AddedPortList & ports, const BString & label);

