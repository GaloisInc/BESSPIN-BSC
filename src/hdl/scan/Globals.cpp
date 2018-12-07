
#include <iostream> 
#include <boost/regex.hpp> // (just for exit)

#include "Globals.h"

tStringSet      Globals::_files;
tStringSet      Globals::_vfiles;
tStringSet      Globals::_ydirs;
tStringSet      Globals::_idirs;
tStringSet      Globals::_dmacros;
tStringSet      Globals::_libexts;
tStringSet      Globals::_libnames;
std::string     Globals::_exec_name = "";
unsigned        Globals::_no_elab   = 0;
unsigned        Globals::_ignore    = 0;
std::string     Globals::_regexpr   = ".*";
unsigned        Globals::_printall  = 1;
std::string     Globals::_topmod    = "";
std::string     Globals::_bsctopmod = "";
std::string     Globals::_cosimmod  = "";
std::string     Globals::_cosiminst = "";
std::string     Globals::_path      = ".";
std::string     Globals::_prefix    = "_SCAN";
unsigned        Globals::_size      = 1;
tExpr*          Globals::_width     = NULL;
//int           Globals::_error     = 0;

void Globals::AddYDir(char *path)
{
  _ydirs.insert(path);
}

void Globals::AddIDir(char *path)
{
  _idirs.insert(path);
}

void Globals::AddVFile(char *path)
{
  _vfiles.insert(path);
}

void Globals::AddFile(char *path)
{
  _files.insert(path);
}

void Globals::AddDefMacro(char *expr)
{
  _dmacros.insert(expr);
}

void Globals::AddLibExt(char *ext)
{
  _libexts.insert(ext);
}

void Globals::SetExecName(char *name)
{
  _exec_name.assign(name);
}

void Globals::SetNoElab()
{
  _no_elab = 1;
}

void Globals::SetIgnore()
{
  _ignore = 1;
}

void Globals::SetRegExpr(char *regexpr)
{
  _regexpr.assign(regexpr);
}

void Globals::SetPrintOnlyModified()
{
  _printall = 0;
}

void Globals::SetTopMod(char *name)
{
  if (_topmod.size() > 0) {
    fprintf(stderr, "The -e option can only be used once, Exiting.\n");
    exit(1);
  } else {
    _topmod.assign(name);
  }
}

void Globals::SetTopMod(const char *name)
{
  return Globals::SetTopMod((char*) name);
}

void Globals::SetBscTopMod(char *name)
{
  _bsctopmod.assign(name);
}

void Globals::SetBscTopMod(const char *name)
{
  return Globals::SetBscTopMod((char*) name);
}

void Globals::SetCosimMod(char *name)
{
  _cosimmod.assign(name);
}

void Globals::SetCosimMod(const char *name)
{
  return Globals::SetCosimMod((char*) name);
}

void Globals::SetCosimInst(char *name)
{
  _cosiminst.assign(name);
}

void Globals::SetCosimInst(const char *name)
{
  return Globals::SetCosimInst((char*) name);
}

void Globals::SetPath(char *path)
{
  _path.assign(path);
}

void Globals::SetPrefix(char *prefix)
{
  _prefix.assign(prefix);
}

void Globals::SetSize(unsigned size)
{
  _size = size;
}

void Globals::SetWidth(tExpr* width)
{
  _width = width;
}

// void Globals::SetError(int num)
// {
//   _error = num;
// }

// void Globals::ClearError()
// {
//   _error = 0;
// }

tStringSet* Globals::GetYDirs()
{
  return &_ydirs;
}

tStringSet* Globals::GetIDirs()
{
  return &_idirs;
}

tStringSet* Globals::GetVFiles()
{
  return &_vfiles;
}
 
tStringSet* Globals::GetFiles()
{
  return &_files;
}

tStringSet* Globals::GetDefMacros()
{
  return &_dmacros;
}

tStringSet* Globals::GetLibExts()
{
  return &_libexts;
}

const char* Globals::GetExecName()
{
  return _exec_name.data();
}

unsigned Globals::GetNoElab()
{
  return _no_elab;
}

unsigned Globals::GetIgnore()
{
  return _ignore;
}

const char* Globals::GetRegExpr()
{
  return _regexpr.data();
}

unsigned Globals::GetPrintOnlyModified()
{
  return !_printall;
}

const char* Globals::GetTopMod()
{
  return _topmod.data();
}

const char* Globals::GetBscTopMod()
{
  return _bsctopmod.data();
}

const char* Globals::GetCosimMod()
{
  return _cosimmod.data();
}

const char* Globals::GetCosimInst()
{
  return _cosiminst.data();
}

const char* Globals::GetPath()
{
  return _path.data();
}

const char* Globals::GetPrefix()
{
  return _prefix.data();
}

unsigned Globals::GetSize()
{
  return _size;
}

tExpr* Globals::GetWidth()
{
  Verific::VeriMapForCopy id_map_table;
  tExpr* cp = _width->CopyExpression(id_map_table);
  return cp;
}

