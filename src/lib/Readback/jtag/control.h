// Copyright (c) 2013-2016, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// C++ transactor side for the Bluespec readback control module mkInternalJtag
// which can be found in $BLUESPECDIR/lib/Readback

#include <stdint.h>
#include <iostream>
#include <sstream>

#include "jtag.h"
#include "cable.h"
#include "log.h"
#include "LuminaControl.hpp"

  class JtagRdBackControl : public LuminaControl
  {
  private:
    cable     *m_cable;
    jtag      *m_jtag;
    Log        m_logfile;

  public:
    JtagRdBackControl (const std::string &cablename, const unsigned int &device);
    ~JtagRdBackControl();

  private:
    // Disallow default and copy constructors
    JtagRdBackControl & operator= (const JtagRdBackControl &);
    JtagRdBackControl( const JtagRdBackControl &);

    // Wait until the hardware is ready to receive a command
    void waitForReady();

  public:
    bool readState();

    bool sendRdBackClear();
    bool sendRdBackStore (unsigned int code);
    bool sendRdBackFinish(unsigned int config);
    bool sendRdBackBreakCode(unsigned int code);
  };

