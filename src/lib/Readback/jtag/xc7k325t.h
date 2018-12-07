#pragma once

#include "fpga.h"
#include "interface.h"
#include "cable.h"

using namespace std;

class xc7k325t : public fpga
{
public:
  xc7k325t();
  ~xc7k325t();

  static fpga* create() { return new xc7k325t(); }

public:
};

REGISTER(xc7k325t, fpga)
