#ifndef __STRING_H__
#define __STRING_H__

/* Tstring.h
 *
 * dynamic string class using TCL_DString
 *
 * 0.1 jun 97
 *
 */
 
#include <tcl.h>


class Tstring {
 public:
  Tstring();
  Tstring(const Tstring& rhs);
  Tstring(const char* str);
  
  virtual ~Tstring();
  
  Tstring& operator=(const Tstring& rhs);
  Tstring& operator+=(const char* addTstring);
  Tstring& operator+=(char addChar);
  int operator==(const Tstring& rhs);
  int operator!=(const Tstring& rhs);
  operator const char*() const;
  virtual const char* data() const;
  virtual int length() const;
  
 protected:
 private:
  Tcl_DString string_;
};

#endif
