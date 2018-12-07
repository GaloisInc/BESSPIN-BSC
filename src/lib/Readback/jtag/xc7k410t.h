#pragma once

#include "fpga.h"
#include "interface.h"
#include "cable.h"

using namespace std;

class xc7k410t : public fpga
{
public:
  xc7k410t();
  ~xc7k410t();

  static fpga* create() { return new xc7k410t(); }

public:
};

REGISTER(xc7k410t, fpga)
