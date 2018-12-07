#pragma once

#include "fpga.h"
#include "interface.h"
#include "cable.h"

using namespace std;

class xc7v2000t : public fpga
{
public:
  xc7v2000t();
  ~xc7v2000t();

  static fpga* create() { return new xc7v2000t(); }

public:
};

REGISTER(xc7v2000t, fpga)
