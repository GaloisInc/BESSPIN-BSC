#include <cstdlib>

#include "don_hack.h"

extern "C" unsigned int rand32 ()
{
  unsigned int res;
  res = (unsigned int)random();
  return res;
}
