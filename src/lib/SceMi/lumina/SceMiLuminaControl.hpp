// Copyright (c) 2013-2016, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// C++ transactor side for the Bluespec readback control module mkSceMiLumina
// which can be found in $BLUESPECDIR/lib/Readback

#include "LuminaControl.hpp"

// Include the Bluespec SceMi library
#include "scemi.h"
// Include pipes
#include "scemi_pipes.h"

// Include additional Bluespec elements (built on the basic SceMi library)
#include "SceMiServiceThread.h"
#include "InportProxyT.h"
#include "OutpipeXactorT.h"
#include "BitT.h"


class SceMiLuminaControl : public LuminaControl
{
private:
  SceMi                       *m_scemi;
  SceMiServiceThread          *m_serviceThread;

  InportProxyT < BitT<32> >   *m_req_in;
  OutpipeXactorT < BitT<32> > *m_rdback_out;

public:
  SceMiLuminaControl(const char *instname, const char *paramfile);
  ~SceMiLuminaControl();

private:
  // Disallow default and copy constructors
  SceMiLuminaControl & operator= (const SceMiLuminaControl &);
  SceMiLuminaControl( const SceMiLuminaControl &);

  // Wait until the hardware is ready to receive a command
  void waitForReady();

public:
  bool readState();

  bool sendRdBackClear();
  bool sendRdBackStore (unsigned int code);
  bool sendRdBackFinish(unsigned int config);
  bool sendRdBackBreakCode(unsigned int code);
};
