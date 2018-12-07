/// Copyright (c) 2014-2016, Bluespec Inc.  ALL RIGHTS RESERVED

#include "LuminaPlus.h"

using namespace std;

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

LuminaPlus *LuminaPlus::m_luminaplus = NULL;

LuminaPlus *LuminaPlus::getOrCreate()
{
  if (m_luminaplus == NULL)
    m_luminaplus = new LuminaPlus();

  return m_luminaplus;
}

void LuminaPlus::destroy()
{
  if (m_luminaplus) {
    delete m_luminaplus;
    m_luminaplus = NULL;
  }
}

LuminaPlus::LuminaPlus()
  : m_initialized(false)
  , m_pControl(0)
  , m_pDesign(0)
  , m_pVCDWriter(0)
{
}

LuminaPlus::~LuminaPlus()
{
  if (m_pDesign) {
    delete m_pDesign;
    m_pDesign = 0;
  }

  if (m_pVCDWriter) {
    delete m_pVCDWriter;
    m_pVCDWriter = 0;
  }

  if (m_pControl) {
    delete m_pControl;
    m_pControl = 0;
  }

  fflush(stdout);
  m_initialized = false;
}

void LuminaPlus::init(const unsigned int port)
{
  // Allocate
  m_pControl = new LuminaPlusControl(port);
  m_pDesign     = new Design();
  m_pVCDWriter  = new RdBack::VCDWriter("dump1.vcd");

  // Connect
  RdBackControl *pRdBackControl = m_pControl; // cast to the base class
  m_pDesign->setControl(pRdBackControl);
  m_pDesign->setVCDWriter(m_pVCDWriter);

  // Start the service loop
  m_pControl->startServiceLoop();

  m_initialized = true;
}

string LuminaPlus::query(const string &signal)
{
  stringstream ss;
  if (!m_initialized) {
    throw string("Cannot perform signal query until library is initialized!");
  }

  Signal *pSignal = m_pDesign->findSignal(RTL, signal.c_str());
  if (pSignal == NULL) {
    throw string("Cannot find signal `" + signal + "'");
  }
  ss << pSignal->getAvail() << " " 
     << convert_binstr_to_hexstr(pSignal->getValueStr(false)) << " "
     << pSignal->getValueStr(false);
  cout << pSignal->getFullName() << " "
       << ss.str() << endl;
  return ss.str();
}

