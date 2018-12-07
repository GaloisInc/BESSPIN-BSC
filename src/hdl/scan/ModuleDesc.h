
#ifndef _MODULEDESC_H_
#define _MODULEDESC_H_

#include <map>
#include <list>
#include <string>
#include <iostream>

#include "veri_file.h"
#include "VeriExpression.h"

class ScanPath;
class ModuleDesc;

typedef std::map<std::string,  ModuleDesc*>         tStringMap;
typedef std::map<Verific::VeriModule*, ModuleDesc*> tModuleMap;
typedef std::list<ScanPath>                         tScanPathList;
typedef std::list<ModuleDesc>                       tModuleDescList;

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
  explicit ScanPath(const char* name, ModuleDesc* md);
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
 unsigned          Length(unsigned fixed);
 std::string       ToString();
 std::string       ToString(const char* prefix);
 std::string       ToString(const char* reg_start, const char* reg_end, const char* prefix);
 tScanPathList     PathsGet();
 ScanPath*         Flatten();

 private:

 tScanPathList             _paths;
 std::string               _name;
 unsigned                  _size;
 int                      _index;
 Verific::VeriExpression* _width;
 ModuleDesc*              _md;
 ScanPath*                _path_flat;

} ; // class ScanPath;

class ModuleDesc
{

public :
  // Constructors
  explicit ModuleDesc(Verific::VeriModule* module);
  // Destructor
  ~ModuleDesc() ;

private :
  // Prevent compiler from defining the following
  ModuleDesc& operator=(const ModuleDesc &) ; // Purposely leave unimplemented

public :

  Verific::VeriModule* ModuleGet();
  void        PathSet(ScanPath* path);
  ScanPath*   PathGet();
  unsigned    IncludesNone();
  unsigned    IncludesAll();
  void        AllSet(unsigned value);
  void        NoneSet(unsigned value);
  std::string KeyGet();
  void        ModuleSet(Verific::VeriModule* module);

 private:

  Verific::VeriModule* _module;
  unsigned             _includes_all;
  unsigned             _includes_none;
  ScanPath*            _path;
  
} ; // class ModuleDesc;

class ModuleDescMap
{

 public:

  static ModuleDesc* GetAllDesc(Verific::VeriModule* module);
  static ModuleDesc* GetNoneDesc(Verific::VeriModule* module);
  static ModuleDesc* GetDesc(ModuleDesc *md);
  static ModuleDesc* GetDesc(Verific::VeriModule* module);
  static void Insert(ModuleDesc md);
  static void Clear();

 private:
  static tStringMap      _smap;
  static tModuleMap      _pmap;
  static tModuleDescList _mdlist;

} ; // class ModuleDescMap;

class ScanPathSet
{

 public:

  static void Insert(ScanPath path);

 private:
  static tScanPathList _slist;

}; 


#endif // #ifndef _MODULEDESC_H_
