#ifndef __LINK_PLUGIN_H__
#define __LINK_PLUGIN_H__

#include "SceMiParameters.h"
#include "SceMiEC.h"
#include "Link.h"

typedef Link* LinkCreateFn(SceMiParameters*, SceMiErrorHandler, void*);
typedef void LinkDestroyFn(Link*);

extern "C" {
  LinkCreateFn create_link;
  LinkDestroyFn destroy_link;
}

#endif /* __LINK_PLUGIN_H__ */
