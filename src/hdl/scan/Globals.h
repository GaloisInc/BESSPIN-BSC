
#include "V.h"
#include "utils.h"

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/

typedef Verific::VeriExpression tExpr;

class Globals
{
 public:

  static void AddYDir(char *path);
  static void AddIDir(char *path);
  static void AddVFile(char *path);
  static void AddFile(char *path);
  static void AddDefMacro(char *expr);
  static void SetExecName(char *name);
  static void AddLibExt(char *path);
  static void SetNoElab();
  static void SetIgnore();
  static void SetRegExpr(char *expr);
  static void SetPrintOnlyModified();
  static void SetTopMod(char *name);
  static void SetTopMod(const char *name);
  static void SetBscTopMod(const char *name);
  static void SetBscTopMod(char *name);
  static void SetCosimMod(char *name);
  static void SetCosimMod(const char *name);
  static void SetCosimInst(char *name);
  static void SetCosimInst(const char *name);
  static void SetPath(char *name);
  static void SetPrefix(char *name);
  static void SetSize(unsigned size);
  static void SetWidth(tExpr* width_expr);

  static tStringSet*      GetYDirs();
  static tStringSet*      GetIDirs();
  static tStringSet*      GetVFiles();
  static tStringSet*      GetFiles();
  static tStringSet*      GetDefMacros();
  static tStringSet*      GetLibExts();
  static const char*     GetExecName();
  static unsigned        GetNoElab();
  static unsigned        GetIgnore();
  static const char*     GetRegExpr();
  static unsigned        GetPrintOnlyModified();
  static const char*     GetTopMod();
  static const char*     GetBscTopMod();
  static const char*     GetCosimMod();
  static const char*     GetCosimInst();
  static const char*     GetPath();
  static const char*     GetPrefix();
  static unsigned        GetSize();
  static tExpr*          GetWidth();

//  static int          GetError();

 private:

  static tStringSet  _files;
  static tStringSet  _vfiles;
  static tStringSet  _ydirs;
  static tStringSet  _idirs;
  static tStringSet  _dmacros;
  static tStringSet  _libexts;
  static tStringSet  _libnames;
  static std::string _exec_name;
  static unsigned    _no_elab;
  static unsigned    _ignore;
  static std::string _regexpr;
  static unsigned    _printall;
  static std::string _topmod;
  static std::string _bsctopmod;
  static std::string _cosimmod;
  static std::string _cosiminst;
  static std::string _path;
  static std::string _prefix;
  static unsigned    _size;
  static tExpr*      _width;

} ; // class Globals;

