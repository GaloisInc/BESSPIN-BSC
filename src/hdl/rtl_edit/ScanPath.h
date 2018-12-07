#pragma once
#include <list>
#include <string>
#include "VeriExpression.h"

class ScanPath;

typedef std::list<ScanPath>                         tScanPathList;

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/

class ScanPath
{

 public :
  // Constructors
  explicit ScanPath();
  explicit ScanPath(char* name);
  explicit ScanPath(char* name, unsigned size, int index = -1);
  explicit ScanPath(char* name, Verific::VeriExpression* width, int index = -1);
  explicit ScanPath(const char* name, ScanPath* mp);
  // Destructor
  ~ScanPath() ;

 private :
  // Prevent compiler from defining the following
  ScanPath& operator=(const ScanPath &) ; // Purposely leave unimplemented

 public:

 void              Prepend(ScanPath *path);
 void              Postpend(ScanPath* path);
 void              Pop();
 unsigned          IsEmpty();
 unsigned          Length();
 unsigned          Length(bool fixed);
 std::string       ToString();
 std::string       ToString(const char* prefix);
 std::string       ToString(const char* reg_start, const char* reg_end, const char* prefix);
 tScanPathList     PathsGet();

 private:

 tScanPathList             _paths;
 std::string               _name;
 unsigned                  _size;
 int                      _index;
 Verific::VeriExpression* _width;
 ScanPath*                _mp;

} ; // class ScanPath;
