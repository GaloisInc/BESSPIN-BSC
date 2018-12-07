/* Copyright 2000--2003 Bluespec, Inc.  All rights reserved. */

/* $Id: Printer.h,v 1.9 2003/01/10 21:39:11 elf Exp $ */

#if !defined(PRINTER_GUARD)
#define PRINTER_GUARD
  void Printer_m_print_(objp, varcp);
  void Printer_m_printBit_(objp, varcp);
  void Printer_m_printCycle_(objp);
  void Printer_m_printBitDec_(objp, varcp);
  void Printer_m_exitWith_(objp, varcp);
#endif

objp new_Printer(objp, const struct varinfo *);
