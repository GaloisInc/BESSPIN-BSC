#include <iostream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <dlfcn.h>
#include <ctype.h>
#include <cstdio>

#include "bluenoc.h"
#include "sized_types.h"

#include "bluenoc_linkplugin.h"

static bluenoc *the_bluenoc_object = NULL;

bluenoc::bluenoc(const bluenoc_parameters *params)
  : link(NULL)
  , link_handle(NULL)
  , destroy_link_fn(NULL)
  , service_owner(NONE)
  , service_thread_stop(false)
{
  if (params) {
    parameters = new bluenoc_parameters(*params);
  } else {
    parameters = NULL;
  }

  if (parameters == NULL) {
    throw std::string("bluenoc::bluenoc() no parameters supplied!");
  }

  if (parameters->NumberOfObjects("Link") != 1) {
    throw std::string("bluenoc::bluenoc() Link object parameter is either not supplied or too many link object parameters are supplied!");
  }

  const char *link_type = parameters->AttributeStringValue("Link", 0, "LinkType");
  if (link_type == NULL) {
    throw std::string("bluenoc::bluenoc() Link object parameter of type LinkType is not supplied!");
  }

  const char *bsdir = getenv("BLUESPECDIR");
  if (bsdir == NULL) {
    throw std::string("bluenoc::bluenoc() BLUESPECDIR environment variable is not set!");
  }

  // determine the C++ ABI family
  std::string cxx_family_cmd = bsdir;
  cxx_family_cmd += "/bin/bsenv c++_family";
  FILE *cmd_pipe = popen(cxx_family_cmd.c_str(), "r");
  char cxx_family[16];
  if (cmd_pipe == NULL) {
    throw std::string("bluenoc::bluenoc() Failed to call bsenv script for C++ family!");
  } else if (fgets(cxx_family, 16, cmd_pipe) == NULL) {
    throw std::string("bluenoc::bluenoc() Failed to determine C++ ABI family");
  }
  pclose(cmd_pipe);
  uint32_t i = 0;
  while (!isspace(cxx_family[i])) ++i;
  cxx_family[i] = '\0';

  // build the full name of the library we need to load
  std::string filename;
  filename = bsdir;
  filename += "/BlueNoC/";
  filename += cxx_family;
  filename += "/";
  const char *cptr = link_type;
  if (!strncmp(link_type, "PCIE_", 5))
    cptr = "PCIE";
  while(*cptr != '\0')
    filename += static_cast<char>(tolower(*cptr++));
  filename += "link.so";

  // attempt to dynamically load the library
  link_handle = dlopen(filename.c_str(), RTLD_LAZY);
  if (link_handle == NULL) {
    throw std::string("bluenoc::bluenoc() Failed to load communcation link library");
  }
  LinkCreateFn *create_link_fn = (LinkCreateFn*) dlsym(link_handle, "create_link");
  destroy_link_fn = (LinkDestroyFn*) dlsym(link_handle, "destroy_link");
  if (!create_link_fn || !destroy_link_fn) {
    destroy_link_fn = NULL;
    dlclose(link_handle);
    link_handle = NULL;
    throw std::string("bluenoc::bluenoc() Failed to resolve create_link_fn or destroy_link_fn");
  }

  // use the create_link_fn to instantiate the link instance
  link = create_link_fn(parameters, NULL);
  if (!link) {
    destroy_link_fn = NULL;
    dlclose(link_handle);
    link_handle = NULL;
    throw std::string("bluenoc::bluenoc() Failed to open communications link");
  }

  // Instance all pipe endpoints listed in the params file
  uint32_t index = parameters->NumberOfObjects("InPipe");
  while(index--) {
    const char *name = parameters->AttributeStringValue("InPipe", index, "PipeName");
    const char *xactor = parameters->AttributeStringValue("InPipe", index, "TransactorName");
    uint32_t width = parameters->AttributeIntegerValue("InPipe", index, "PipeWidth");
    uint32_t depth = parameters->AttributeIntegerValue("InPipe", index, "PipeDepth");
    uint32_t nodeid = parameters->AttributeIntegerValue("InPipe", index, "PipeNum");

    if (!name || !xactor) {
      for(std::vector<bluenoc_pipe*>::iterator piter = pipes.begin(); piter != pipes.end(); ++piter) {
	bluenoc_pipe *pipe = *piter;
	delete pipe;
      }
      if (destroy_link_fn != NULL) {
	destroy_link_fn(link);
	destroy_link_fn = NULL;
      }
      if (link_handle != NULL) {
	dlclose(link_handle);
	link_handle = NULL;
      }
      if (!name) {
	throw std::string("bluenoc::bluenoc() InPipe is missing PipeName attribute!");
      } else {
	throw std::string("bluenoc::bluenoc() InPipe is missing TransactorName attribute!");
      }
    }

    bluenoc_pipe *pipe = new bluenoc_inpipe(xactor, name, nodeid, width, depth, link);
    pipes.push_back(pipe);
  }

  index = parameters->NumberOfObjects("OutPipe");
  while(index--) {
    const char *name = parameters->AttributeStringValue("OutPipe", index, "PipeName");
    const char *xactor = parameters->AttributeStringValue("OutPipe", index, "TransactorName");
    uint32_t width = parameters->AttributeIntegerValue("OutPipe", index, "PipeWidth");
    uint32_t depth = parameters->AttributeIntegerValue("OutPipe", index, "PipeDepth");
    uint32_t nodeid = parameters->AttributeIntegerValue("OutPipe", index, "PipeNum");

    if (!name || !xactor) {
      for(std::vector<bluenoc_pipe*>::iterator piter = pipes.begin(); piter != pipes.end(); ++piter) {
	bluenoc_pipe *pipe = *piter;
	delete pipe;
      }
      if (destroy_link_fn != NULL) {
	destroy_link_fn(link);
	destroy_link_fn = NULL;
      }
      if (link_handle != NULL) {
	dlclose(link_handle);
	link_handle = NULL;
      }
      if (!name) {
	throw std::string("bluenoc::bluenoc() OutPipe is missing PipeName attribute!");
      } else {
	throw std::string("bluenoc::bluenoc() OutPipe is missing TransactorName attribute!");
      }
    }
  
    bluenoc_pipe *pipe = new bluenoc_outpipe(xactor, name, nodeid, width, depth, link);
    pipes.push_back(pipe);
  }
}

bluenoc::~bluenoc()
{
  if (destroy_link_fn != NULL) 
    destroy_link_fn(link);
  if (link_handle != NULL)
    dlclose(link_handle);
  the_bluenoc_object = NULL;
  
  delete parameters;
}

bluenoc *bluenoc::Init(const bluenoc_parameters *parameters)
{
  if (parameters == NULL) {
    throw std::string("bluenoc::Init() No parameters specified");
  }

  if (the_bluenoc_object != NULL) {
    throw std::string("bluenoc::Init() Re-initialization of existing bluenoc object!");
  }

  the_bluenoc_object = new bluenoc(parameters);
  if (the_bluenoc_object == NULL) {
    throw std::string("bluenoc::Init() Could not allocate bluenoc object!");
  }
  return the_bluenoc_object;
}

bluenoc *bluenoc::Pointer()
{
  return the_bluenoc_object;
}

void bluenoc::SetPointer(bluenoc *ptr)
{
  the_bluenoc_object = ptr;
}

void bluenoc::Shutdown(bluenoc *bluenoc_object)
{
  if (bluenoc_object == NULL)
    return;
  delete bluenoc_object;
  bluenoc_object = NULL;
}

int bluenoc::ServiceLoop(BlueNoCServiceLoopHandler g, void *context)
{
  Packet* pkt;
  uint64_t cycle;
  unsigned int count;
  const unsigned int maxmsgpercycle = 500;

  count = 0;
  int protocol;
  while ((protocol = link->recv_pkt(&cycle, &pkt)) > 0) {
    if ((++count) > maxmsgpercycle) {
      break;
    }
  }

  // process the pipe notification
  for (std::vector<bluenoc_pipe*>::iterator iter = pipes.begin(); iter != pipes.end(); ++ iter ) {
    (*iter)->process_notification();
  }

  return 0;
}

bluenoc_pipe *bluenoc::get_bluenoc_pipe(const char *endpoint_path)
{
  if (the_bluenoc_object) {
    std::vector<bluenoc_pipe*>::iterator it;
    for(it = pipes.begin(); it < pipes.end(); it++) {
      if (strcmp(endpoint_path, (*it)->endpoint_path()) == 0)
	return (*it);
    }
  }
  return NULL;
}

bluenoc_pipe *bluenoc::get_bluenoc_pipe(uint32_t nodeid)
{
  if (the_bluenoc_object) {
    return pipes[nodeid];
  }
  return NULL;
}
