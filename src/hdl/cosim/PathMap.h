#ifndef _PATHMAP_H_
#define _PATHMAP_H_


#include <list>
#include <string>
#include <vector>
#include <queue>
#include "DataQueue.h"
#include "VHandle.h"

class PathMap;
class StateDesc;
class GenModule;

enum Simulator  { CVC, MODELSIM, VCS, UNKNOWN };

typedef std::list<StateDesc>                        tStateDescList;

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/

class ChainDesc
{

 public :
  // Constructors
  explicit ChainDesc();
  // Destructor
  ~ChainDesc();

 public:

  StateDesc*  Next();
  void        SetClock(const char* name);
  void        SetInst(const char* name);
  bool        IsClock(const char* name);
  void        Check();
  unsigned    GetWidth();
  void        Push(unsigned value);
  void        Pop();
  bool        NotEmpty();
  const char* First();
  StateDesc*  GetDesc();
  void        Clear();

  unsigned                    _size;
  tStateDescList*             _path;
  DataQueue                   _queue;
  std::string                 _clock;
  bool                        _clock_found;
  unsigned                    _inst_length;
  std::string                 _inst;
  tStateDescList::iterator    _iter;
};

class PathMap
{

  public :
  // Constructors
  explicit PathMap(unsigned size);
  // Destructor
  ~PathMap() ;

 public:

  StateDesc*  Next();
  void        SetPath(unsigned probe, unsigned chain, unsigned size);
  void        SetPath(unsigned probe, unsigned chain);
  void        RepeatPath();
  void        SetClock(const char* name);
  void        SetInst(const char* name);
  bool        IsClock(const char* name);
  void        PulseClocks(Simulator sim, long long& half);
  void        Check();
  unsigned    GetWidth();
  void        Load(const char* filename, GenModule &);
  void        Push(unsigned value);
  void        Pop();
  bool        NotEmpty();
  const char* First();
  StateDesc*  GetDesc();
  unsigned    getPathNumber(unsigned probe, unsigned chain);

 private:

  bool     registerSignal(FILE* stream, GenModule& mod);
  bool     registerSignal(FILE* stream, GenModule& mod, bool new_clock);
  bool     pathStart(FILE* stream);
  bool     setClock(FILE* stream);
  bool     setDef(FILE* stream);
  bool     setInst(FILE* stream);
  bool     defaultMap(const char buf[]);
  unsigned processMapStream (FILE* stream, GenModule & mod);

 std::vector<ChainDesc*>     _chains;
 ChainDesc*                  _chain;
 tStateDescList*             _clocks;
 unsigned                    _inst_length;
};

class StateDesc
{

 public :
  // Constructors
  explicit StateDesc(char* name, unsigned size);
  explicit StateDesc(char* name, unsigned size, bool ignore);
  explicit StateDesc(char* name, unsigned size, bool ignore, VHandle* handle);
  explicit StateDesc(char* name, unsigned size, bool ignore, bool is_clock, VHandle* handle);
  explicit StateDesc(char* name, unsigned size, bool ignore, VHandle* handle, unsigned index);
  // Destructor
  ~StateDesc() ;

 private :
   // Prevent compiler from defining the following
//   StateDesc& operator=(const StateDesc &) ; // Purposely leave unimplemented

 public:

 unsigned          Size();
 unsigned          Index();
 const char*       Name();
 bool              Ignore();
 bool              IsMemory();
 bool              IsClock();
 VHandle*          Handle();

 private:

 std::string _name;
 unsigned    _size;
 unsigned    _index;
 bool        _ignore;
 bool        _is_mem;
 bool        _is_clock;
 VHandle*    _handle;

};

#endif // #ifndef _PATHMAP_H_
