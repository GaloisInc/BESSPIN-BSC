//
// Copyright (c) 2014
// bluenoc.h - BlueNoC C++ Interface
//
#ifndef __BLUENOC_H__
#define __BLUENOC_H__

#include <list>
#include <vector>
#include <set>
#include <stdexcept>

#include "bluenoc_parameters.h"
#include "bluenoc_pipe.h"

#include "bluenoc_link.h"

extern "C" {
  typedef int (*BlueNoCServiceLoopHandler)(void *context, int pending);
}

typedef enum { NONE, SIMULATOR, TB } BlueNoCServiceOwner;

class bluenoc 
{
private:
  bluenoc_parameters *parameters;

  bluenoc_link *link;
  void *link_handle;
  void (*destroy_link_fn)(bluenoc_link *link);
  BlueNoCServiceOwner service_owner;
  bool service_thread_stop;

  std::list<std::pair<Packet*,uint64_t> >  pending_from_hw;

  std::vector<bluenoc_pipe*> pipes;

  bluenoc(const bluenoc_parameters *params);
  ~bluenoc();

public:
  static bluenoc *Init(const bluenoc_parameters *parameters);

  static bluenoc *Pointer();
  static void SetPointer(bluenoc *ptr);

  static void Shutdown(bluenoc *mct);

  int ServiceLoop(BlueNoCServiceLoopHandler g = 0, void *context = 0);

  BlueNoCServiceOwner GetServiceOwner() { return service_owner; }
  void SetServiceOwner(BlueNoCServiceOwner owner) { service_owner = owner; }
  void ServiceThreadStop(bool value) { service_thread_stop = value; }
  bool ServiceThreadStop() { return service_thread_stop; }

  bluenoc_parameters *parameter() { return parameters; }

  bluenoc_pipe *get_bluenoc_pipe(const char *endpoint_path);
  bluenoc_pipe *get_bluenoc_pipe(uint32_t nodeid);
};


#endif // __BLUENOC_H__
