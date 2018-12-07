#pragma once

#include "fpga.h"
#include "interface.h"
#include "cable.h"

using namespace std;

class xc7vx690t : public fpga
{
public:
  xc7vx690t();
  ~xc7vx690t();

  static fpga* create() { return new xc7vx690t(); }

public:
};

REGISTER(xc7vx690t, fpga)
