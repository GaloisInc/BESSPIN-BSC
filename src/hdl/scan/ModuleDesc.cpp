#include <iostream>         // cout
using std::cout ;

#include "ModuleDesc.h"
#include "VUtils.h"
#include "utils.h"

#include "V.h"
// #include "Strings.h"
#include "Dbg.h"

// #include "PI.h"
// #include <map>
// #include <set>
// #include <utility> // make_pair

// using std::cout ;

#include <boost/regex.hpp> // (just for exit)

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

ModuleDesc::ModuleDesc(Verific::VeriModule* module)
{
  _path          = NULL;
  _module        = module;
  _includes_all  = 0;
  _includes_none = 0;
  ModuleDescMap::Insert(*this);
}

ModuleDesc::~ModuleDesc()
{

}

unsigned ModuleDesc::IncludesAll()
{
  return _includes_all;
}

unsigned ModuleDesc::IncludesNone()
{
  return _includes_none;
}

void ModuleDesc::AllSet(unsigned value)
{
  _includes_all = value;
}

void ModuleDesc::NoneSet(unsigned value)
{
  _includes_none = value;
}

ScanPath* ModuleDesc::PathGet()
{
  return _path;
}

void ModuleDesc::PathSet(ScanPath* path)
{
  _path = path;
}

Verific::VeriModule* ModuleDesc::ModuleGet()
{
  return _module;
}

void ModuleDesc::ModuleSet(Verific::VeriModule* module)
{
  _module = module;
}


// "key" is a string with the module name followed by the state
// path.  If a ModuleDesc includes all state, the sequence is
// ommitted and replaced by a "|" character.
std::string ModuleDesc::KeyGet()
{
  const char* name_base = getModuleNameBase(_module);
  std::string key = name_base;
  if (_includes_all) {
    key += createParamsString(_module);
    key += "|||";
  } else if (_includes_none) {
    key += "||";
  } else if (_path) {
    key += createParamsString(_module);
    key += "|";
    key += _path->ToString();
    key += "|";
  } else {
    key += "||";
  }
  return key;
}

ModuleDesc* ModuleDescMap::GetDesc(ModuleDesc* mdi)
{
  std::string key = mdi->KeyGet();
  Verific::VeriModule* module = mdi->ModuleGet();
  tStringMap::iterator it = _smap.find(key.data());
  if (it == _smap.end()) {

    Dbg::printf("Adding: %s %s %d %d %d\n", key.data(), mdi->ModuleGet()->GetName(), _smap.size(), mdi->IncludesAll(), mdi->IncludesNone());
    _smap.insert(std::make_pair(key.data(),mdi));
    _smap.insert(std::make_pair(mdi->ModuleGet()->GetName(), mdi));
    _pmap.insert(std::make_pair(module, mdi));
    
    if (mdi->IncludesAll() && mdi->IncludesNone()) {
      mdi->AllSet(0);
      key = mdi->KeyGet();
      Dbg::printf("Adding: %s %s %d\n", key.data(), mdi->ModuleGet()->GetName(), _smap.size());
      _smap.insert(std::make_pair(key.data(),mdi));
      mdi->AllSet(1);
    }
    return mdi;


  } else {
    ModuleDesc* md = it->second;
    Dbg::printf(" Hit: %s %s %d %d\n", key.data(), mdi->ModuleGet()->GetName(), md->ModuleGet()->GetName(), _smap.size());
    _pmap.insert(std::make_pair(module, md));
    return md;
  }
}


ModuleDesc* ModuleDescMap::GetDesc(Verific::VeriModule* module)
{
  tModuleMap::iterator it = _pmap.find(module);
  if (it == _pmap.end()) {
    Dbg::printf(" MMiss: %s \n", module->GetName());
    return  NULL;
  } else {
    ModuleDesc* md = it->second;
    Dbg::printf("  MHit: %s \n", module->GetName());
    return md;
  }
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

ScanPath::ScanPath()
{
 tScanPathList    _paths;
 _name           = "";
 _size           = 0;
 _index          = -1;
 _width          = NULL;
 _md             = NULL;
 _path_flat      = NULL;
 ScanPathSet::Insert(*this);

}

ScanPath::ScanPath(char* name)
{
 tScanPathList    _paths;
 _name           = name;
 _size           = 0;
 _index          = -1;
 _width          = NULL;
 _md             = NULL;
 _path_flat      = NULL;
 ScanPathSet::Insert(*this);

}

ScanPath::ScanPath(char* name, unsigned size, int index)
{
 tScanPathList    _paths;
 _name           = name;
 _size           = size;
 _index          = index;
 _width          = NULL;
 _md             = NULL;
 _path_flat      = NULL;
 ScanPathSet::Insert(*this);
}

ScanPath::ScanPath(char* name, Verific::VeriExpression* width, int index)
{
 tScanPathList    _paths;
 _name           = name;
 _size           = 1;
 _index          = index;
 _width          = width;
 _md             = NULL;
 _path_flat      = NULL;
 ScanPathSet::Insert(*this);
}

ScanPath::ScanPath(const char* name, ModuleDesc* md)
{
 tScanPathList _paths;
 _name           = name;
 _size           = 0;
 _index          = -1;
 _width          = NULL;
 _md             = md;
 _path_flat      = NULL;
 ScanPathSet::Insert(*this);
}

ScanPath::~ScanPath()
{

}

unsigned ScanPath::Length()
{
  unsigned len = 0;
  if(_paths.size() > 0) {
    tScanPathList::iterator it;
    for( it = _paths.begin(); it != _paths.end(); it++ ) {
      len += it->Length();
    }
  } else if (_md) {
    ScanPath* path_mod = _md->PathGet();
    len = path_mod->Length();
  } else if (_size > 0) {
    len = _size;
  }
  return len;
}

unsigned ScanPath::Length(unsigned fixed)
{
  if(_paths.size() > 0) {
    unsigned len = 0;
    tScanPathList::iterator it;
    for( it = _paths.begin(); it != _paths.end(); it++ ) {
      len += it->Length(fixed);
    }
    return len;
  } else if (_md) {
    ScanPath* path_mod = _md->PathGet();
    return path_mod->Length(fixed);
  } else if (_size > 0) {
    if ((fixed && !_width) || (!fixed && _width)) {
      return _size;
    }
  }
  return 0;
}

// std::string ScanPath::ToString(char* prefix)
// {
//   std::string out ("");
//   if(_paths.size() > 0) {
//     tScanPathList::iterator it;
//     unsigned first = 1;
//     for( it = _paths.begin(); it != _paths.end(); it++ ) {
//       if (!first) {  out += " "; }
//       first = 0;
//       out += it->ToString(prefix);
//     }
//   } else if (_md) {
//     ScanPath* path_mod = _md->PathGet();
//     std::string full (prefix);
//     full+= "/";
//     full+= _md->ModuleGet()->GetName();
//     out += path_mod->ToString((char*) full.data());
//   } else if (_size > 0) {
//     out += "[reg ";
//     out += prefix;
//     out += "/";
//     out += _name;
//     out += " ";
//     if (_width) {
//       out += "width";
//     } else {
//       out += itoa(_size);
//     }
//     out += "]";
//   } else {
//     // Leave at ""
//   }
//   return out;
// }

std::string ScanPath::ToString(const char* reg_start, const char* reg_end, const char* prefix)
{
  std::string out ("");
  if(_paths.size() > 0) {
    tScanPathList::iterator it;
    unsigned first = 1;
    for( it = _paths.begin(); it != _paths.end(); it++ ) {
//      if (!first) {  out += " "; }
      first = 0;
      out += it->ToString(reg_start, reg_end, prefix);
    }
  } else if (_md) {
    ScanPath* path_mod = _md->PathGet();
    std::string full (prefix);
    full+= "/";
    full+= _name;
//    full+= _md->ModuleGet()->GetName();
    out += path_mod->ToString(reg_start, reg_end, (char*) full.data());
  } else if (_size > 0) {
    out += reg_start;
    out += prefix;
    out += "/";
    out += _name;
    out += " ";
    if (_width) {
      out += "width";
    } else {
      out += itoa(_size);
    }
    if (_index >= 0) {
      out += " ";
      out += itoa(_index);
    }
    out += reg_end;
  } else {
    // Leave at ""
  }
  return out;
}

std::string ScanPath::ToString(const char* prefix)
{
  return ScanPath::ToString("[reg ", "] ", prefix);
}

std::string ScanPath::ToString()
{
  std::string out ("");
  if(_paths.size() > 0) {
    // out += "[path ";
    //    out += _name;
    tScanPathList::iterator it;
    unsigned first = 1;
    for( it = _paths.begin(); it != _paths.end(); it++ ) {
      if (!first) {  out += " "; }
      first = 0;
      out += it->ToString();
    }
    // out += "]";
  } else if (_md) {
    out += "[mod ";
    out += _name;
    out += " ";
    out +=  _md->ModuleGet()->GetName();
    out += "]";
  } else if (_size > 0) {
    out += "[reg ";
    out += _name;
    out += " ";
    out += itoa(_size);
    if (_index >= 0) {
      out += " ";
      out += itoa(_index);
    }
    out += "]";
  } else {
    // Leave at ""
  }
  return out;
}

unsigned ScanPath::IsEmpty()
{
  if(_paths.size() > 0) {
    tScanPathList::iterator it;
    for( it = _paths.begin(); it != _paths.end(); it++ ) {
      if (!it->IsEmpty()) {
	return 0;
      }
    }
    return 1;
  } else if (_md) {
    return _md->IncludesNone();
  } else {
    return (_size == 0);
  }
}

void ScanPath::Prepend(ScanPath* path)
{
  if(_md) {
    exit(1);
  }
  _paths.push_back(*path);
  _path_flat = NULL;
}

void ScanPath::Postpend(ScanPath* path)
{
  if(_md) {
    exit(1);
  }
  _paths.push_front(*path);
  _path_flat = NULL;
}

void ScanPath::Pop()
{
  if(_md) {
    exit(1);
  }
  _paths.pop_back();
  _path_flat = NULL;
}

tScanPathList ScanPath::PathsGet()
{
  return _paths;
}

// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

tStringMap ModuleDescMap::_smap;
tModuleMap ModuleDescMap::_pmap;
tModuleDescList ModuleDescMap::_mdlist;

void ModuleDescMap::Insert(ModuleDesc md)
{
  _mdlist.push_back(md);
}

ModuleDesc* ModuleDescMap::GetAllDesc(Verific::VeriModule* module)
{
  ModuleDesc* mdi = new ModuleDesc(module);
  mdi->AllSet(1);
  std::string key = mdi->KeyGet();
  tStringMap::iterator it = _smap.find(key.data());
  if (it == _smap.end()) {
    Dbg::printf("All Miss: %s %s\n", key.data(),  mdi->ModuleGet()->GetName());
    return NULL;
  } else {
    ModuleDesc* md = it->second;
    Dbg::printf(" All Hit: %s %s %s\n", key.data(),  md->KeyGet().data(), md->ModuleGet()->GetName());
    return md;
  } 
}

ModuleDesc* ModuleDescMap::GetNoneDesc(Verific::VeriModule* module)
{
  ModuleDesc* mdi = new ModuleDesc(module);
  mdi->NoneSet(1);
  std::string key = mdi->KeyGet();
  tStringMap::iterator it = _smap.find(key.data());
  if (it == _smap.end()) {
    Dbg::printf("All Miss: %s %s\n", key.data(), mdi->ModuleGet()->GetName());
    return NULL;
  } else {
    ModuleDesc* md = it->second;
    Dbg::printf(" All Hit: %s %s\n", key.data(), md->ModuleGet()->GetName());
    return md;
  } 
}

void  ModuleDescMap::Clear()
{
  _smap.clear();
  _pmap.clear();
}


// -----------------------------------------------------------------------------
// 
// -----------------------------------------------------------------------------

tScanPathList ScanPathSet::_slist;

void ScanPathSet::Insert(ScanPath path)
{
  _slist.push_back(path);
}

