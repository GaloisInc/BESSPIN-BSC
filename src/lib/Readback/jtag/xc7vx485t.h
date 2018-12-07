#pragma once

#include "fpga.h"
#include "interface.h"
#include "cable.h"

using namespace std;

class xc7vx485t : public fpga
{
public:
  xc7vx485t();
  ~xc7vx485t();

  static fpga* create() { return new xc7vx485t(); }

public:
};

REGISTER(xc7vx485t, fpga)
