// Copyright (c) 2016, Bluespec, Inc.  ALL RIGHTS RESERVED
#pragma once

// Base class that abstracts the functions that Lumina uses
// for communicating across the link

#include "RdBackControl.hpp"

class LuminaControl : public RdBackControl
{
public:
  LuminaControl() : RdBackControl() {};
  virtual ~LuminaControl() {};

  // Send a Readback request and wait for the data to arrive.
  // Returns whether a complete response was successfully received
  // (otherwise an error or timeout occurred).
  //
  virtual bool readState() = 0;
};

