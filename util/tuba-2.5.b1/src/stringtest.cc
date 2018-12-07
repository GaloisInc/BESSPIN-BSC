#include "Tstring.h"

Tstring 
proc()
{
  Tstring temp;
  
  temp = "hello";
  temp += ' ';
  temp += "there";
  
  return temp;
}

main()
{
  Tstring s;
  
  s = proc();

  printf("string is '%s'\n", s.data());
  return 0;
}
