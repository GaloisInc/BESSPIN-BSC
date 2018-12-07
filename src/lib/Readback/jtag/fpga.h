#pragma once

#include <string>
#include <map>
#include <stdint.h>

#include "log.h"
#include "cable.h"
#include "jtag.h"
#include "interface.h"

using namespace std;

typedef map<string, uint32_t> instruction;

class jtag;

class fpga
{
protected:
  Log          logfile;
  instruction  ircodes;

public:
  string         name;
  string         description;
  const uint32_t idcode;
  const uint32_t idmask;
  const uint32_t irlength;

public:
  fpga(const char *name, const char *description, const uint32_t &idcode, const uint32_t &idmask, const uint32_t &irlen);
  virtual ~fpga();

  static fpga *find_by_idcode(const uint32_t &idcode);

  void set_instruction(jtag *j, const string &idcode);
};
