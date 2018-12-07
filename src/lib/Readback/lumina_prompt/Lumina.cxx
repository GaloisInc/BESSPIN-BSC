/// Copyright (c) 2014-2016, Bluespec Inc.  ALL RIGHTS RESERVED

#include <string>
#include <set>

#include "Lumina.h"

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

Lumina *Lumina::m_lumina = NULL;

Lumina *Lumina::getOrCreate()
{
  if (m_lumina == NULL)
    m_lumina = new Lumina();

  return m_lumina;
}

void Lumina::destroy()
{
  if (m_lumina) {
    delete m_lumina;
    m_lumina = NULL;
  }
}

Lumina::Lumina()
  : m_initialized(false)
  , m_pControl(0)
  , m_pDesign(0)
  , m_pVCDWriter(0)
{
}

Lumina::~Lumina()
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

#ifdef JTAG_CONTROL
void Lumina::init(const string &cbl, const uint32_t &device)
#else
void Lumina::init(const unsigned int port)
#endif
{
#ifdef JTAG_CONTROL
  set<string> cables = interface_registry<cable>::shared_instance().get_entries();
  set<string>::iterator str;
  string realcable = "";

  // auto-detect cable
  if (cbl == "") {
    for(str = cables.begin(); str != cables.end(); str++) {
      cable *c = interface_registry<cable>::shared_instance().create_object_of(*str);
      if (c->is_present()) {
	cout << "Using " << c->description.c_str() << endl;
	realcable = str->c_str();
      }
      delete c;
    }
    if (realcable == "") {
      throw string("Failed to locate a supported JTAG cable!");
    }
  } else {
    realcable = cbl;
  }
#endif

  // Allocate
#ifdef JTAG_CONTROL
  m_pControl    = new JtagRdBackControl(realcable, device);
#else
  m_pControl    = new SocketLuminaControl(port);
#endif
  m_pDesign     = new Design();
  m_pVCDWriter  = new RdBack::VCDWriter("dump1.vcd");

  // Connect
  RdBackControl *pRdBackControl = m_pControl; // cast to the base class
  m_pDesign->setControl(pRdBackControl);
  m_pDesign->setVCDWriter(m_pVCDWriter);
    
  m_initialized = true;
}

void Lumina::do_readback ()
{
  int status;

  if (!m_initialized) {
    throw string("Cannot perform readback until library is initialized!");
  }

  status = m_pControl->readState();
  if (false)
    cout << "Read request: " << (status ? "success" : "error") << endl;

  m_pDesign->flushVCD();
}

string Lumina::query(const string &signal)
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

