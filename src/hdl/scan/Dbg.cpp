
//#include <stdio.h>
//#include <stdarg.h>


#include "Dbg.h"
// #include "Strings.h"
// #include "VeriTreeNode.h"

unsigned Dbg::_level   = 0;
unsigned Dbg::_default = 1;
unsigned Dbg::_indent  = 0;

void PrintLevel(unsigned level)
{

  if (level == 0) return;
  unsigned i;
  for(i=0;i<level;i++)
    {
      printf(" ");
    }
  printf("[%d]", level);
}

void Dbg::printf(char const *fmt, ...)
{
  va_list arg;
  va_start(arg, fmt);
  if (_level >= _default) {
    PrintLevel(_indent);
    vprintf(fmt, arg);
  }
  va_end(arg);
}

void Dbg::printf(unsigned level, const char *fmt, ...)
{
  va_list arg;
  va_start(arg, fmt);
  if (_level >= level) {
    PrintLevel(_indent);
    vprintf(fmt, arg);
  }
  va_end(arg);
}

unsigned Dbg::Mode()
{
  return _level >= _default;
}

unsigned Dbg::Mode(unsigned level)
{
  return _level >= level;
}

void Dbg::Incr()
{
  _level++;
}

void Dbg::Default(unsigned level)
{
  _default = level;
}

void Dbg::SetIndent(unsigned level)
{
  _indent = level;
}


