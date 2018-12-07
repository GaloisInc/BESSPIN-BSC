#pragma once

#include "fpga.h"
#include "interface.h"
#include "cable.h"

using namespace std;

class xc7z020ps : public fpga
{
public:
  xc7z020ps();
  ~xc7z020ps();

  static fpga* create() { return new xc7z020ps(); }

public:
};

REGISTER(xc7z020ps, fpga)
