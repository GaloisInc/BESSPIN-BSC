#ifndef __BLUENOC_LINKPLUGIN_H__
#define __BLUENOC_LINKPLUGIN_H__

#include "bluenoc_parameters.h"
#include "bluenoc_link.h"

typedef bluenoc_link *LinkCreateFn(bluenoc_parameters *, void *);
typedef void LinkDestroyFn(bluenoc_link*);

extern "C" {
  LinkCreateFn create_link;
  LinkDestroyFn destroy_link;
}

#endif //__BLUENOC_LINKPLUGIN_H__
