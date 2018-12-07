#pragma once

#include <stdio.h>
#include <stdarg.h>
// #include "Strings.h"

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/

class Dbg
{
 public:

  static void printf(const char *fmt, ...);
  static void printf(unsigned level, const char *fmt, ...);
  static unsigned Mode(unsigned level);
  static unsigned Mode();
  static void Incr();
  static void Default(unsigned level);
  static void SetIndent(unsigned level);

 private:

  static unsigned _level;
  static unsigned _default;
  static unsigned _indent;

} ; // class Dbg;

