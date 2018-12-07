#include "xc7v2000t.h"

xc7v2000t::xc7v2000t()
  : fpga("XC7V2000T", "Xilinx Virtex-7 XC7V2000T", 0x036b3093, 0x0FFFFFFF, 24)
{
  ircodes["IDCODE"]    	      = 0x249249;
  ircodes["VERSION"]   	      = 0x965965;
  ircodes["BYPASS"]    	      = 0xFFFFFF;
  ircodes["EXTEST"]    	      = 0x9A69A6;
  ircodes["SAMPLE"]    	      = 0x041041;
  ircodes["PRELOAD"]   	      = 0x041041;
  ircodes["USERCODE"]  	      = 0x224924;
  ircodes["HIGHZ"]     	      = 0x28A28A;
  ircodes["EXTEST_PULSE"]     = 0xF3CF3C;
  ircodes["EXTEST_TRAIN"]     = 0xF7DF7D;
  ircodes["ISC_ENABLE"]       = 0x410410;
  ircodes["ISC_PROGRAM"]      = 0x451451;
  ircodes["XSC_PROGRAM_SLR0"] = 0x464924;
  ircodes["XSC_PROGRAM_SLR1"] = 0x911924;
  ircodes["XSC_PROGRAM_SLR2"] = 0x924464;
  ircodes["XSC_PROGRAM_SLR3"] = 0x924911;
  ircodes["ISC_NOOP"]         = 0x514514;
  ircodes["XSC_READ_RSVD"]    = 0x555555;
  ircodes["ISC_DISABLE"]      = 0x596596;
  ircodes["XSC_PROGRAM_KEY"]  = 0x492492;
  ircodes["XSC_DNA"]          = 0x5E4924;
  ircodes["CFG_OUT_SLR0"]     = 0x124924;
  ircodes["CFG_IN_SLR0"]      = 0x164924;
  ircodes["CFG_OUT_SLR1"]     = 0x904924;
  ircodes["CFG_IN_SLR1"]      = 0x905924;
  ircodes["CFG_OUT_SLR2"]     = 0x924124;
  ircodes["CFG_IN_SLR2"]      = 0x924164;
  ircodes["CFG_OUT_SLR3"]     = 0x924904;
  ircodes["CFG_IN_SLR3"]      = 0x924905;
  ircodes["JPROGRAM"]         = 0x2CB2CB;
  ircodes["JSTART"]           = 0x30C30C;
  ircodes["JSHUTDOWN"]        = 0x34D34D;
  ircodes["FUSE_CTS_SLR0"]    = 0xC24924;
  ircodes["FUSE_KEY_SLR0"]    = 0xC64924;
  ircodes["FUSE_DNA_SLR0"]    = 0xCA4924;
  ircodes["FUSE_USER_SLR0"]   = 0xCE4924;
  ircodes["FUSE_CNTL_SLR0"]   = 0xD24924;
  ircodes["FUSE_CTS_SLR1"]    = 0x930924;
  ircodes["FUSE_KEY_SLR1"]    = 0x931924;
  ircodes["FUSE_DNA_SLR1"]    = 0x932924;
  ircodes["FUSE_USER_SLR1"]   = 0x933924;
  ircodes["FUSE_CNTL_SLR1"]   = 0x934924;
  ircodes["FUSE_CTS_SLR2"]    = 0x924C24;
  ircodes["FUSE_KEY_SLR2"]    = 0x924C64;
  ircodes["FUSE_DNA_SLR2"]    = 0x924CA4;
  ircodes["FUSE_USER_SLR2"]   = 0x924CE4;
  ircodes["FUSE_CNTL_SLR2"]   = 0x924D24;
  ircodes["FUSE_CTS_SLR3"]    = 0x924930;
  ircodes["FUSE_KEY_SLR3"]    = 0x924931;
  ircodes["FUSE_DNA_SLR3"]    = 0x924932;
  ircodes["FUSE_USER_SLR3"]   = 0x924933;
  ircodes["FUSE_CNTL_SLR3"]   = 0x924934;
  ircodes["USER1"]            = 0x0A4924;
  ircodes["USER1"]            = 0x0E4924;
  ircodes["USER1"]            = 0x8A4924;
  ircodes["USER1"]            = 0x8E4924;
  ircodes["XADC_DRP"]         = 0xDE4924;
  ircodes["INTEST_RSVD"]      = 0x1C71C7;
}

xc7v2000t::~xc7v2000t()
{
}
