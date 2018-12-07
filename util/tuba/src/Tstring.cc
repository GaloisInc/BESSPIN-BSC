/* Tstring.cc
 *
 * code for Tcl-based dynamic string class
 *
 * 0.1 jun 97
 *
 */

#include <string.h> 
#include "Tstring.h"

Tstring::Tstring()
{
  Tcl_DStringInit(&string_);
}

Tstring::Tstring(const char* str)
{
  Tcl_DStringInit(&string_);
  Tcl_DStringAppend(&string_, (char*)str, -1);
}

Tstring::Tstring(const Tstring& rhs)
{
  Tcl_DStringInit(&string_);
  Tcl_DStringAppend(&string_, (char*)rhs.data(), -1);
}

Tstring::~Tstring()
{
  Tcl_DStringFree(&string_);
}

Tstring&
Tstring::operator=(const Tstring& rhs)
{
  Tcl_DStringFree(&string_);
  Tcl_DStringInit(&string_);
  Tcl_DStringAppend(&string_, (char*)rhs.data(), -1);
  return *this;
}

int
Tstring::operator==(const Tstring& rhs)
{
  return (strcmp(data(), rhs.data()) == 0);
}

int
Tstring::operator!=(const Tstring& rhs)
{
  return !operator==(rhs);
}

Tstring&
Tstring::operator+=(const char* addString)
{
  Tcl_DStringAppend(&string_, (char*)addString, -1);
  return *this;
}

Tstring&
Tstring::operator+=(char addChar)
{
  Tcl_DStringAppend(&string_, &addChar, 1);
  return *this;
}

Tstring::operator const char*() const
{
  return Tcl_DStringValue(&string_);
}

const char*
Tstring::data() const
{
  return Tcl_DStringValue(&string_);
}

int
Tstring::length() const
{
  return Tcl_DStringLength(&string_);
}
