#include "ScanPath.h"
#include "Types.h"

ScanPath::ScanPath()
{
 tScanPathList    _paths;
 _name           = "";
 _size           = 0;
 _index          = -1;
 _width          = NULL;
 _mp             = NULL;
}

ScanPath::ScanPath(char* name)
{
 tScanPathList    _paths;
 _name           = name;
 _size           = 0;
 _index          = -1;
 _width          = NULL;
 _mp             = NULL;
}

ScanPath::ScanPath(char* name, unsigned size, int index)
{
 tScanPathList    _paths;
 _name           = name;
 _size           = size;
 _index          = index;
 _width          = NULL;
 _mp             = NULL;
}

ScanPath::ScanPath(char* name, Verific::VeriExpression* width, int index)
{
 tScanPathList    _paths;
 _name           = name;
 _size           = 1;
 _index          = index;
 _width          = width;
 _mp             = NULL;
}

ScanPath::ScanPath(const char* name, ScanPath* mp)
{
 tScanPathList _paths;
 _name           = name;
 _size           = 0;
 _index          = -1;
 _width          = NULL;
 _mp             = mp;
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
  } else if (_mp) {
    ScanPath* path_mod = _mp;
    len = path_mod->Length();
  } else if (_size > 0) {
    len = _size;
  }
  return len;
}

unsigned ScanPath::Length(bool fixed)
{
  if(_paths.size() > 0) {
    unsigned len = 0;
    tScanPathList::iterator it;
    for( it = _paths.begin(); it != _paths.end(); it++ ) {
      len += it->Length(fixed);
    }
    return len;
  } else if (_mp) {
    ScanPath* path_mod = _mp;
    return path_mod->Length(fixed);
  } else if (_size > 0) {
    if ((fixed && !_width) || (!fixed && _width)) {
      return _size;
    }
  }
  return 0;
}

std::string ScanPath::ToString(const char* reg_start, const char* reg_end, const char* prefix)
{
  std::string out ("");
  if(_paths.size() > 0) {
    tScanPathList::iterator it;
    //unsigned first = 1;
    for( it = _paths.begin(); it != _paths.end(); it++ ) {
      //if (!first) {  out += " "; }
      //first = 0;
      out += it->ToString(reg_start, reg_end, prefix);
    }
  } else if (_mp) {
    ScanPath* path_mod = _mp;
    std::string full (prefix);
    full+= "/";
    full+= _name;
//    full+= _mp->ModuleGet()->GetName();
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
  } else if (_mp) {
    out += "[mod ";
    out += _name;
//    out += " ";
//    out +=  _mp->ModuleGet()->GetName();
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
  } else if (_mp) {
    return _mp->IsEmpty();
  } else {
    return (_size == 0);
  }
}

void ScanPath::Prepend(ScanPath* path)
{
  if(_mp) {
    exit(1);
  }
  _paths.push_back(*path);
}

void ScanPath::Postpend(ScanPath* path)
{
  if(_mp) {
    exit(1);
  }
  _paths.push_front(*path);
}

void ScanPath::Pop()
{
  if(_mp) {
    exit(1);
  }
  _paths.pop_back();
}

tScanPathList ScanPath::PathsGet()
{
  return _paths;
}
