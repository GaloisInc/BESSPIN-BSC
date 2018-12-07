/// Copyright (c) 2014-2016, Bluespec Inc.  ALL RIGHTS RESERVED

#include <string>

#include "Design.hpp"

#ifdef JTAG_CONTROL
#include "control.h"
#else
#include "SocketLuminaControl.hpp"
#endif

class Lumina
{
  static Lumina                   * m_lumina;

 protected:

  bool                              m_initialized;
#ifdef JTAG_CONTROL
  JtagRdBackControl               * m_pControl;
#else
  SocketLuminaControl             * m_pControl;
#endif
  Design                          * m_pDesign;
  RdBack::VCDWriter               * m_pVCDWriter;

  // Protected cannot be called by user
  Lumina();
  ~Lumina();

 public:

  static Lumina *getOrCreate();
  static void destroy();

  // Operations
#ifdef JTAG_CONTROL
  void init(const std::string &cbl, const uint32_t &device);
#else
  void init(const unsigned int port);
#endif
  void do_readback();
  std::string query(const std::string &signal);

  // Accessors
#ifdef JTAG_CONTROL
  JtagRdBackControl* getJtagRdBackControl() { return m_pControl; }
#else
  SocketLuminaControl* getSocketLuminaControl() { return m_pControl; }
#endif
  Design* getDesign() { return m_pDesign; }

};

