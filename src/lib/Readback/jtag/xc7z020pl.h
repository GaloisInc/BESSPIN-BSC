#pragma once

#include "fpga.h"
#include "interface.h"
#include "cable.h"

using namespace std;

class xc7z020pl : public fpga
{
public:
  xc7z020pl();
  ~xc7z020pl();

  static fpga* create() { return new xc7z020pl(); }

public:
};

REGISTER(xc7z020pl, fpga)
