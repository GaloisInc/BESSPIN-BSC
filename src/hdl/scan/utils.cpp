
#include "string.h"
#include <string>
#include <cstdio>
#include "utils.h"

char * itoa (int i)
{
  char str_val [32] ;
  snprintf (str_val, sizeof (str_val), "%d", i) ;

  /* Use strdup to duplicate str_val which is currently on the stack. */
  return strdup (str_val) ;
}

const char* removeQuotes (const char* orig)
{
  int length = strlen(orig);
  char* orig_dup = strdup(orig);
  if (orig_dup[length-1] =='"') {
    orig_dup[length-1] = '\0';
  }
  if (orig_dup[0] == '"') {
    orig_dup++;
  }
  return orig_dup;
}

char* removeQuotes (char* orig)
{
  return (char*) removeQuotes ((const char*) orig);
}

