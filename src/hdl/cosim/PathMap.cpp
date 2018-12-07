
#include "PathMap.h"
#include "vpi_user.h"
#include <stdlib.h>
#include <string.h>
#include "VHandle.h"
#include "GenModule.h"
#include <stdio.h>

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

PathMap::PathMap(unsigned size)
 : _chains()
 , _chain(0)
 , _clocks()
 , _inst_length(0)
{
  _chains.resize(size);
  _clocks = new tStateDescList();
}

PathMap::~PathMap()
{

}

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

void PathMap::SetPath(unsigned probe, unsigned chain, unsigned size)
{

  unsigned num = getPathNumber(probe, chain);

  if (num > _chains.size()) {
    fprintf(stderr, "SetPath: ERROR\n");
  }

 //  if (_iter == _paths[_current].end()) {
//     //    printf("ITER DONE!\n");
//   }

  _chain = new ChainDesc();
  _chain->_size = size;
  _chain->_iter = _chain->_path->begin();
  _chains[num] = _chain;
  _chain->Clear();
}

void PathMap::SetPath(unsigned probe, unsigned chain)
{

  unsigned num = getPathNumber(probe, chain);

  if (num > _chains.size()) {
    fprintf(stderr, "SetPath: ERROR\n");
  }

 //  if (_iter == _paths[_current].end()) {
//     //    printf("ITER DONE!\n");
//   }

  _chain = _chains[num];
  _chain->_iter = _chain->_path->begin();
  _chain->Clear();
}

void PathMap::RepeatPath()
{
  _chain->_iter = _chain->_path->begin();
  _chain->Clear();
}

void PathMap::SetClock(const char* name)
{
  _chain->SetClock(name);
}

void PathMap::SetInst(const char* name)
{
  tPath path = GenModule::toPath(name);
  path.pop_front(); // remove top level
  path.push_front("top");
  path.push_front("main");
  std::string inst_path = GenModule::fromPath(path);
  _chain->SetInst(inst_path.c_str());
  _inst_length = strlen(inst_path.c_str());
}

bool PathMap::IsClock(const char* name)
{
  return _chain->IsClock(name);
}

unsigned PathMap::GetWidth()
{
  return  _chain->GetWidth();
}

StateDesc* PathMap::Next()
{
  return _chain->Next();
}

void  PathMap::Push(unsigned value)
{
  _chain->Push(value);
}

bool PathMap::NotEmpty()
{
  return _chain->NotEmpty();
}

void PathMap::Pop()
{
  _chain->Pop();
}


const char* PathMap::First()
{
  return _chain->First();
}

StateDesc* PathMap::GetDesc()
{
  return _chain->GetDesc();
}

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

bool PathMap::registerSignal(FILE* stream, GenModule& mod)
{
  return(registerSignal(stream, mod, false));
}

bool PathMap::registerSignal(FILE* stream, GenModule& mod, bool new_clock)
{

  bool cont, has_index ;
  bool is_clock;
  char name[1024];
  char size[1024];
  char index[1024];
  char line[1024];
  fgets(line, 1024, stream);
  std::string name_mod;
  int count = (sscanf (line, " %s %s %s", (char *) &name, (char*) &size, (char*) &index));
//  name_mod = "/top";
//  name_mod += name + _inst_length;
  name_mod = _chain->_inst;
  name_mod += name;
  char* local = (char*) name_mod.c_str();
  if (2 == count || 3 == count) {
    if (count == 2) {
      // printf("Register: %s %s\n", local, size);
    } else {
      // printf("Register: %s %s [%s]\n", local, size, index);
    }
    cont = true;
    has_index = (count == 3);
    unsigned n = 1;
    unsigned i = 0;
    bool ignore = false;
    if (!strcmp("width",size)) {
      n = GetWidth();
      ignore = true;
    } else {
      if (1 != sscanf(size, "%d", &n)) {
	fprintf (stderr, "Error in path map.\n");
	cont = false;
	exit(1);
      }
    }
    if (has_index && (1 != sscanf(index, "%d", &i))) {
      fprintf (stderr, "Error in path map.\n");
      cont = false;
      exit(1);
    }
    VHandle* h = NULL;
    if (ignore) {
//      printf("Register: (size=%2d) (ignore) %s\n", n, local);
    } else {
      if (new_clock) {
	is_clock = true;
      } else {
	is_clock = IsClock(local);
      }
      std::string full_name = local;
      if (is_clock) {
	_chain->_clock_found = true;
	// printf(" Found clock %s\n", local);
      }
      tPath path = GenModule::toPath(full_name);
      std::string signal_name = path.back();
      path.pop_back(); // remove signal name
//      cout << "PATH" << path << endl;
//      cout << "PATH" << GenModule::fromPath(path) << endl;
      VHandle* m = mod.getInst(path);
      if (m) {
	VHandle* s;
	if (has_index) {
//	  printf("INDEX: %s\n", index);
	  s = GenModule::getSignal(*m, signal_name, i);
	} else {
	  s = GenModule::getSignal(*m, signal_name);
	}
	if (s) {
	  h = s;
	} else {

	}
      } else {
	printf("ERROR: Couldn't find module with net %s\n", local);
      }
//      printf("Register: (size=%2d)          %s\n", n, local);
    }
    if (has_index) {
      StateDesc desc = StateDesc(local, n, ignore, h, i);
      _chain->_path->push_front(desc);
    } else if (is_clock) {
      StateDesc desc = StateDesc(local, n, ignore, true, h);
      _chain->_path->push_front(desc);
      _clocks->push_front(desc);
    } else {
      StateDesc desc = StateDesc(local, n, ignore, h);
      _chain->_path->push_front(desc);
    }
  } else {
    fprintf (stderr, "Error in path map.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

bool PathMap::pathStart(FILE* stream)
{

  bool cont ;
  unsigned probe, chain, size;
  if (3 == (fscanf (stream, " %d %d %d", &probe, &chain, &size))) {
    cont = true;
    printf(" Loading path %d %d (width = %d)\n", probe, chain, size);
    SetPath(probe, chain, size);
  } else {
    fprintf (stderr, "Error in path map.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

bool PathMap::setClock(FILE* stream)
{

  bool cont ;
  char name[1024];
  if (1 == (fscanf (stream, " %s", (char*) name))) {
    printf(" Setting clock %s\n", (char*) name);
    cont = true;
    SetClock(name);
  } else {
    fprintf (stderr, "Error in path map.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

bool PathMap::setDef(FILE* stream)
{

  bool cont ;
  char name[1024];
  unsigned int probe;
  if (2 == (fscanf (stream, " %d %s", &probe, (char*) name))) {
    printf(" Setting def %s\n", (char*) name);
    cont = true;
  } else {
    fprintf (stderr, "Error in path map.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

bool PathMap::setInst(FILE* stream)
{

  bool cont ;
  char name[1024];
  unsigned int probe;
  if (2 == (fscanf (stream, " %d %s", &probe, (char*) name))) {
    printf(" Setting inst %s\n", (char*) name);
    cont = true;
    SetInst(name);
  } else {
    fprintf (stderr, "Error in path map.\n");
    cont = false;
    exit(1);
  }
  return cont;
}

bool PathMap::defaultMap(const char buf[])
{
  printf("DEFAULT: %s\n", (char*) buf);
  return false;
}

unsigned PathMap::processMapStream (FILE* stream, GenModule & mod)
{

  unsigned ret = 0;
  char buf0[1024];
  bool continueLoop = true;
  while(continueLoop) {
    int scanres = fscanf (stream, " %s:", (char *) buf0);
    if (1 == scanres) {
      switch (buf0[0]) {
      case 'p': /*path start*/
        continueLoop = pathStart(stream);
        break;
      case 'r': /*signal*/
        continueLoop = registerSignal(stream, mod);
        break;
      case 'c': /*clock*/
        continueLoop = setClock(stream);
        break;
      case 'd':
        continueLoop = setDef(stream);
	break;
      case 'i':
        continueLoop = setInst(stream);
	break;
      default:
        continueLoop = defaultMap (buf0);
        break;
      }
    } else /*if (EOF == scanres)*/ {
      continueLoop = false;
    }
  }
  return ret;
}

void       PathMap::Load(const char* filename, GenModule & mod)
{

  std::string file_name = filename;
  FILE* map_file = fopen (file_name.data(), "r" );
  if (0 == map_file) {
    fprintf (stderr, "Error: could not open %s\n", file_name.data());
    exit (1);
  }
  processMapStream(map_file, mod);
  Check();
}

void       PathMap::Check()
{
  if (!_chain->_clock_found) {
    fprintf (stderr, "Error: Unable to resolve designated clock signal: %s\n",   _chain->_clock.c_str());
    exit(1);
  }
}

void PathMap::PulseClocks(Simulator sim, long long& half)
{
  long long offset = 0;

  if (sim == CVC) {
    offset = half + half;
  }
  static char val_str[8];
  tStateDescList::iterator iter;
  for (iter = (*_clocks).begin(); iter != (*_clocks).end(); iter++) {
    StateDesc* desc = &(*iter);


    static s_vpi_time tim;
    tim.type = vpiSimTime;
    vpi_get_time(NULL, &tim);
    VHandle* h = desc->Handle();
    if (h) {
      //      printf("CLOCK HAS HANDLE %d %d\n", tim.high, tim.low);
      static s_vpi_value val2;
      static s_vpi_time t2;
      t2.type = vpiSimTime;
      t2.high = 0;
      t2.low = offset;
      strcpy(val_str, "1");
      val2.format = vpiBinStrVal;
      val2.value.str = val_str;
      vpi_put_value (h->get(), &val2, &t2, vpiTransportDelay);
      if (sim == MODELSIM) {
	vpi_put_value (h->get(), &val2, &t2, vpiNoDelay);
      }


      static s_vpi_value val3;
      static s_vpi_time t3;
      t3.type = vpiSimTime;
      t3.high = 0;
      t3.low = offset + half;
      strcpy(val_str, "0");
      val3.format = vpiBinStrVal;
      val3.value.str = val_str;
      vpi_put_value (h->get(), &val3, &t3, vpiTransportDelay);
      if (sim == MODELSIM) {
	vpi_put_value (h->get(), &val3, &t3, vpiNoDelay);
      }

    } else {
      printf("CLOCK NO HANDLE\n");
    }
  }
}

unsigned PathMap::getPathNumber(unsigned probe, unsigned chain)
{
  return chain;
}

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */

ChainDesc::ChainDesc()
  : _size(0)
  , _path()
  , _queue()
  , _clock()
  , _clock_found(false)
  , _inst_length(0)
  , _inst("")
{
  _path = new tStateDescList();
  _iter = _path->begin();

}

ChainDesc::~ChainDesc()
{

}

void ChainDesc::SetClock(const char* name)
{
  std::string clk_name;
  clk_name = _inst;
  clk_name += name;
  _clock.assign(clk_name);
}

void ChainDesc::SetInst(const char* name)
{
  _inst_length = strlen(name);
  _inst = name;
}

bool ChainDesc::IsClock(const char* name)
{
  return(!strcmp(_clock.c_str(), name));
}

unsigned ChainDesc::GetWidth()
{

  return  _size;

}

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */


StateDesc* ChainDesc::Next()
{

  if (_iter == _path->end()) {
    fprintf(stderr, "NEXT: ERROR\n");
    return NULL;
  }

  StateDesc* desc = &(*_iter);
  _iter++;

  return  desc;

}

void  ChainDesc::Push(unsigned value)
{
  unsigned size = GetWidth();
  _queue.PushN(value, size);

}

bool ChainDesc::NotEmpty()
{

  if (_iter == _path->end()) {
    return false;
  }
  StateDesc* desc = &(*_iter);
  unsigned needed = desc->Size();
//  const char* name = desc->Name();
  return _queue.HasN(needed);
}

void ChainDesc::Pop()
{
  if (NotEmpty()) {
    StateDesc* desc = &(*_iter);
    unsigned needed = desc->Size();
    _queue.PopN(needed);
    _iter++;
  }
}

const char* ChainDesc::First()
{
  if (NotEmpty()) {
    StateDesc* desc = &(*_iter);
    unsigned needed = desc->Size();
    return _queue.FirstN(needed);
  } else {
    fprintf (stderr, "First() called with insuficient data.\n");
    exit(1);
  }
}

StateDesc* ChainDesc::GetDesc()
{
  if (_iter == _path->end()) {
    return NULL;
  }
  StateDesc* desc = &(*_iter);
  return  desc;

}

void  ChainDesc::Clear()
{
  _queue.Clear();
}

StateDesc::StateDesc(char* name, unsigned size)
{
  _name     = name;
  _size     = size;
  _index    = 0;
  _is_mem   = false;
  _ignore   = false;
  _is_clock = false;
  _handle   = NULL;

}

StateDesc::StateDesc(char* name, unsigned size, bool ignore)
{
  _name   = name;
  _size   = size;
  _index  = 0;
  _is_mem = false;
  _ignore = ignore;
  _is_clock = false;
  _handle   = NULL;

}

StateDesc::StateDesc(char* name, unsigned size, bool ignore, VHandle* handle)
{
  _name   = name;
  _size   = size;
  _index  = 0;
  _is_mem = false;
  _ignore = ignore;
  _is_clock = false;
  _handle   = handle;

}

StateDesc::StateDesc(char* name, unsigned size, bool ignore, bool is_clock, VHandle* handle)
{
  _name   = name;
  _size   = size;
  _index  = 0;
  _is_mem = false;
  _ignore = ignore;
  _is_clock = is_clock;
  _handle   = handle;

}

StateDesc::StateDesc(char* name, unsigned size, bool ignore, VHandle* handle, unsigned index)
{
  _name   = name;
  _size   = size;
  _index  = index;
  _is_mem = true;
  _ignore = ignore;
  _is_clock = false;
  _handle = handle;

}

StateDesc::~StateDesc()
{

}

const char* StateDesc::Name()
{
  return _name.data();
}

unsigned StateDesc::Size()
{
  return _size;
}

unsigned StateDesc::Index()
{
  return _index;
}

bool StateDesc::Ignore()
{
  return _ignore;
}

bool StateDesc::IsMemory()
{
  return _is_mem;
}

bool StateDesc::IsClock()
{
  return _is_clock;
}

VHandle*  StateDesc::Handle()
{
  return _handle;
}
