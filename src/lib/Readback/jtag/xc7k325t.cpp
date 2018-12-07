#include "xc7k325t.h"

xc7k325t::xc7k325t()
  : fpga("XC7K325T", "Xilinx Kintex-7 XC7K325T (KC705)", 0x03651093, 0x0FFFFFFF, 6)
{
  ircodes["IDCODE"]          = 0x09;
  ircodes["BYPASS"]          = 0x3F;
  ircodes["EXTEST"]          = 0x26;
  ircodes["SAMPLE"]          = 0x01;
  ircodes["PRELOAD"]         = 0x01;
  ircodes["USERCODE"]        = 0x08;
  ircodes["HIGHZ"]           = 0x0A;
  ircodes["EXTEST_PULSE"]    = 0x3C;
  ircodes["EXTEST_TRAIN"]    = 0x3D;
  ircodes["ISC_ENABLE"]      = 0x10;
  ircodes["ISC_PROGRAM"]     = 0x11;
  ircodes["ISC_NOOP"]        = 0x14;
  ircodes["XSC_READ_RSVD"]   = 0x15;
  ircodes["ISC_DISABLE"]     = 0x16;
  ircodes["XSC_PROGRAM_KEY"] = 0x12;
  ircodes["XSC_DNA"]         = 0x17;
  ircodes["CFG_OUT"]         = 0x04;
  ircodes["CFG_IN"]          = 0x05;
  ircodes["JPROGRAM"]        = 0x0B;
  ircodes["JSTART"]          = 0x0C;
  ircodes["JSHUTDOWN"]       = 0x0D;
  ircodes["FUSE_CTS"]        = 0x30;
  ircodes["FUSE_KEY"]        = 0x31;
  ircodes["FUSE_DNA"]        = 0x32;
  ircodes["FUSE_USER"]       = 0x33;
  ircodes["FUSE_CNTL"]       = 0x34;
  ircodes["USER1"]           = 0x02;
  ircodes["USER2"]           = 0x03;
  ircodes["USER3"]           = 0x22;
  ircodes["USER4"]           = 0x23;
  ircodes["XADC_DRP"]        = 0x37;
  ircodes["INTEST_RSVD"]     = 0x07;
}

xc7k325t::~xc7k325t()
{
}
