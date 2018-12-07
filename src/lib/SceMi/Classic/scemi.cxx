// ****************************************************************
// This is an implementation of the SCE-MI API, as defined in the
// Accellera document:
//     Standard Co-Emulation Modeling Interface (SCE-MI)
//     Reference Manual
//     Version 2.0 Release
//     March 22nd, 2007
// and embodied in the file 'scemi.h' in Appendix E.1 of that document

// This code is called by (and linked into) the "software side"
// (typically a "testbench") of a SCE-MI-style co-emulation in order
// to communicate with the "hardware side".

// It uses TCP/IP-socket-based communications with the hardware side.

// Incomplete stuff below marked with 'TODO:'

// Copyright (c) 2008 Bluespec, Inc; all rights reserved
// ****************************************************************

#include <iostream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <dlfcn.h>
#include <ctype.h>
#include <cstdio>

using namespace std;

#include "scemi.h"
#include "sized_types.h"
#include "flexlm.h"


#include "LinkPlugin.h"

// ****************************************************************
// The following number is the 'interface version number'
// returned by SceMi::Version()

#define BSCEMI_VERSION 020200


// ****************************************************************
// This is a single common shared scemi object.
// Making SceMi a singleton in this way is specified in the standard,
// so we have to go along with it.

static SceMi *the_scemi_object = NULL;

// ****************************************************************
// class SceMi members

SceMi::SceMi(const SceMiParameters* params, SceMiEC* ec)
{
  link = NULL;
  link_handle = NULL;
  destroy_link_fn = NULL;
  service_thread_stop = false;

  // make internal copy of parameters
  if (params)
    parameters = new SceMiParameters(*params);
  else
    parameters = NULL;

  if (parameters == NULL) {
    raiseError ("SceMi::SceMi", "No parameters supplied", SceMiError, ec);
    return;
  }

  // determine the communication link type
  if (parameters->NumberOfObjects("Link") != 1) {
    raiseError ("SceMi::SceMi",
		"Link object parameter is either not supplied or too many link object parameters are supplied",
		SceMiError, ec);
    return;
  }

  const char* link_type = parameters->AttributeStringValue("Link", 0, "LinkType");
  if (link_type == NULL) {
    raiseError ("SceMi::SceMi",
		"Link object parameter of type LinkType is not supplied",
		SceMiError, ec);
    return;
  }

  // get the $BLUESPECDIR value
  const char* bsdir = getenv("BLUESPECDIR");
  if (bsdir == NULL) {
    raiseError ("SceMi::SceMi",
		"BLUESPECDIR environment variable is not set",
		SceMiError, ec);
    return;
  }

  // determine the C++ ABI family
  std::string cxx_family_cmd = bsdir;
  cxx_family_cmd += "/bin/bsenv c++_family";
  FILE* pipe = popen(cxx_family_cmd.c_str(), "r");
  char cxx_family[16];
  if (pipe == NULL) {
    raiseError ("SceMi::SceMi",
		"Failed to call bsenv script for C++ family",
		SceMiError, ec);
    return;
  } else if (fgets(cxx_family, 16, pipe) == NULL) {
    pclose(pipe);
    raiseError ("SceMi::SceMi",
		"Failed to determine C++ ABI family",
		SceMiError, ec);
    return;
  }
  pclose(pipe);
  unsigned int i = 0;
  while (!isspace(cxx_family[i])) ++i;
  cxx_family[i] = '\0';

  // build the full name of the library we need to load
  std::string file_name;
  file_name = bsdir;
  file_name += "/SceMi/Classic/";
  file_name += cxx_family;
  file_name += "/";
  const char* cptr = link_type;
  if (!strncmp(link_type, "PCIE_", 5))
    cptr = "PCIE";
  while (*cptr != '\0')
    file_name += static_cast<char>(tolower(*cptr++));
  file_name += "link.so";

  // EVE link type does not require a bluespec license
  if (0 != strncmp(link_type, "EVE", 3)) {
    if (getlm (false, bfl_scemi) != 0) {
      raiseError ("SceMi::SceMi",
		  "Failed to check out SceMi License",
		  SceMiError, ec);
      return;
    }
  }

  // attempt to dynamically load the library
  link_handle = dlopen(file_name.c_str(), RTLD_LAZY);
  if (link_handle == NULL) {
    raiseError ("SceMi::SceMi",
		"Failed to load communication link library",
		SceMiError, ec);
    return;
  }
  LinkCreateFn* create_link_fn = (LinkCreateFn*) dlsym(link_handle, "create_link");
  destroy_link_fn = (LinkDestroyFn*) dlsym(link_handle, "destroy_link");
  if ((create_link_fn == NULL) || (destroy_link_fn == NULL)) {
    destroy_link_fn = NULL;
    dlclose(link_handle);
    link_handle = NULL;
    raiseError ("SceMi::SceMi",
		"Failed to resolve create_link_fn or destroy_link_fn",
		SceMiError, ec);
    return;
  }

  // use the create_link_fn to instantiate the Link instance
  link = create_link_fn(parameters, ec_get_handler(), ec_get_context());
  if (link == NULL)
  {
    destroy_link_fn = NULL;
    dlclose(link_handle);
    link_handle = NULL;
    raiseError ("SceMi::SceMi",
		"Failed to open the communication link",
		SceMiError, ec);
    return;
  }
}

SceMi::~SceMi()
{
  // delete all port proxies that have been created
  for (std::vector<SceMiMessageInPortProxy*>::iterator it = inProxies.begin();
       it < inProxies.end(); it++)
    delete *it;

  for (std::vector<SceMiMessageOutPortProxy*>::iterator it = outProxies.begin();
       it < outProxies.end(); it++)
    delete *it;

  inProxies.clear();
  outProxies.clear();

  rellm(bfl_scemi);
  // shutdown the communication link
  if (destroy_link_fn != NULL)
    destroy_link_fn(link);
  if (link_handle != NULL)
    dlclose(link_handle);
  the_scemi_object = NULL;

  delete parameters;
}

int SceMi::Version ( const char* versionString)
{
    if (strcmp (versionString, SCEMI_VERSION_STRING) != 0)
        return -1;
    else
	return BSCEMI_VERSION;
}

SceMi* SceMi::Init (int version,
		    const SceMiParameters* parameters,
		    SceMiEC* ec)
{
    if (ec != NULL)
	ec->Type = SceMiOK;

    if (parameters == NULL) {
      raiseError ("SceMi::Init", "No parameters supplied", SceMiError, ec);
      return NULL;
    }

    if (the_scemi_object != NULL)
    {
      raiseError ("SceMi::Init", "Re-initialization of existing SceMi object", SceMiError, ec);
      return NULL;
    }

    if (version != BSCEMI_VERSION)
    {
      cerr << "SceMi::Init: Bad version number (" << version
	   << "); only " << BSCEMI_VERSION << " is supported"
	   << endl;
      raiseError ("SceMi::Init", "Bad version number", SceMiError, ec);
      return NULL;
    }

    the_scemi_object = new SceMi(parameters);
    if (the_scemi_object == NULL)
      raiseError ("SceMi::Init", "Couldn't allocate SceMi object", SceMiError, ec);
    return the_scemi_object;
}

SceMi* SceMi::Pointer(SceMiEC* ec)
{
  return the_scemi_object;
}

void SceMi::Shutdown (SceMi* scemi_object, SceMiEC* ec)
{
    if (ec != NULL)
	ec->Type = SceMiOK;
    if (scemi_object == NULL) return;

    delete scemi_object;
    scemi_object = NULL;
}

SceMiMessageInPortProxy* SceMi::BindMessageInPort(const char* transactorName,
						  const char* portName,
						  const SceMiMessageInPortBinding* binding,
						  SceMiEC* ec)
{
  // Search for the named port
  unsigned int index = parameters->NumberOfObjects("MessageInPort", ec);
  const char* xactor;
  const char* port;
  while (index--)
  {
    port = parameters->AttributeStringValue("MessageInPort",index,"PortName",ec);
    if ((port == NULL) || strcmp(port,portName))
      continue;

    xactor = parameters->AttributeStringValue("MessageInPort",index,"TransactorName",ec);
    if ((xactor == NULL) || strcmp(xactor,transactorName))
      continue;

    // if we get here, then we've found a match
    unsigned int width = parameters->AttributeIntegerValue("MessageInPort",index,"PortWidth",ec);
    unsigned int channel = parameters->AttributeIntegerValue("MessageInPort",index,"ChannelId",ec);
    SceMiMessageInPortProxy* proxy = new SceMiMessageInPortProxy(xactor,port,width,channel,link);
    if (proxy == NULL) {
      raiseError ("SceMi::BindMessageInPort", "new SceMiMessageInPortProxy failed", SceMiError, ec);
      return NULL;
    }
    if (channel >= inProxies.size())
      inProxies.resize(channel + 1);
    if (not_ready_channels.find(channel) == not_ready_channels.end())
      not_ready_channels.insert(channel);
    delete inProxies[channel];
    inProxies[channel] = proxy;
    proxy->ReplaceBinding(binding,ec);
    return proxy;
  }

  // if we get here, we did not find any matching port
  char msg[128];
  sprintf(msg, "Illegal transactor/port name: %s/%s", transactorName, portName);
  raiseError ("SceMi::BindMessageInPort", msg, SceMiError, ec);
  return NULL;
}

SceMiMessageOutPortProxy* SceMi::BindMessageOutPort(const char* transactorName,
						    const char* portName,
						    const SceMiMessageOutPortBinding* binding,
						    SceMiEC* ec)
{
  // Search for the named port
  unsigned int index = parameters->NumberOfObjects("MessageOutPort", ec);
  const char* xactor;
  const char* port;
  while (index--)
  {
    port = parameters->AttributeStringValue("MessageOutPort",index,"PortName",ec);
    if ((port == NULL) || strcmp(port,portName))
      continue;

    xactor = parameters->AttributeStringValue("MessageOutPort",index,"TransactorName",ec);
    if ((xactor == NULL) || strcmp(xactor,transactorName))
      continue;

    // if we get here, then we've found a match
    unsigned int width = parameters->AttributeIntegerValue("MessageOutPort",index,"PortWidth",ec);
    unsigned int channel = parameters->AttributeIntegerValue("MessageOutPort",index,"ChannelId",ec);
    SceMiMessageOutPortProxy* proxy = new SceMiMessageOutPortProxy(xactor,port,width,channel,link);
    if (proxy == NULL) {
      raiseError ("SceMi::BindMessageOutPort", "new SceMiMessageOutPortProxy failed", SceMiError, ec);
      return NULL;
    }
    if (channel >= outProxies.size())
      outProxies.resize(channel + 1);
    delete outProxies[channel];
    outProxies[channel] = proxy;
    proxy->ReplaceBinding(binding,ec);
    return proxy;
  }

  // if we get here, we did not find any matching port
  char msg[128];
  sprintf(msg, "Illegal transactor/port name: %s/%s", transactorName, portName);
  raiseError ("SceMi::BindMessageOutPort", msg, SceMiError, ec);
  return NULL;
}

int SceMi::ServiceLoop(SceMiServiceLoopHandler g,
		       void* context,
		       SceMiEC* ec)
{
  bool exit_service_loop = false;
  unsigned int service_request_count = 0;
  Packet* pkt;
  SceMiU64 cycle;
  unsigned int channel;
  unsigned int count;
  const unsigned int maxmsgpercycle = 500;
  std::set<unsigned int>::iterator not_ready_itr;

  // send out any messages waiting to go to HW side
  count = 0;
  while (link->send_pkt(&channel)) {
    not_ready_channels.insert(channel);
    if ( (++count) > maxmsgpercycle ) {
      break;
    }
  }

  link->loop();

  // process any messages received from the HW side
  count = 0;
  while ((pkt = link->recv_pkt(&cycle)) != NULL) {
    pending_from_hw.push_back(make_pair(pkt,cycle));
    if ( (++count) > maxmsgpercycle ) {
      break;
    }
  }

  // check for newly ready channels
  // we cannot invoke callbacks until the HW is ready
  for (not_ready_itr = not_ready_channels.begin();
       not_ready_itr != not_ready_channels.end();) {
    channel = *not_ready_itr;
    if (link->ready_to_send(channel)) {
      // erase within the loop, must be done very carefully!
      not_ready_channels.erase(not_ready_itr++);
      ready_channels.insert(channel);
    }
    else
      not_ready_itr++;
  }

  // make callbacks
  while (!exit_service_loop)
  {
    if (!ready_channels.empty())
    {
      unsigned int chan = *(ready_channels.begin());
      SceMiMessageInPortProxy* proxy = inProxies[chan];
      if (proxy && proxy->Binding().IsReady)
	proxy->Binding().IsReady(proxy->Binding().Context);
      ++service_request_count;
      ready_channels.erase(chan);
      if (g != NULL && (g(context,1) == 0))
	exit_service_loop = true;
    }
    else if (!pending_from_hw.empty())
    {
      pkt   = pending_from_hw.front().first;
      cycle = pending_from_hw.front().second;
      pending_from_hw.pop_front();
      SceMiMessageOutPortProxy* proxy = outProxies[pkt->channel];
      if (proxy && proxy->Binding().Receive)
      {
	SceMiMessageData msg(link, pkt, cycle);
	proxy->Binding().Receive(proxy->Binding().Context, &msg);
      }
      ++service_request_count;
      if (g != NULL && (g(context,1) == 0))
	exit_service_loop = true;
    }
    else if (g == NULL || (g(context,0) == 0))
      exit_service_loop = true;
  }

  return service_request_count;
}

void SceMi::RegisterErrorHandler(SceMiErrorHandler  errorHandler,
				 void              *context)
{
  ec_register_handler(errorHandler, context);
}

void SceMi::RegisterInfoHandler(SceMiInfoHandler infoHandler,
				void* context)
{
  cerr << "TODO: SceMi::RegisterInfoHandler: not yet implemented" << endl;
}

const char *SceMi::sceMiMessageInPortProxyName(unsigned int chan)
{
  if (chan > inProxies.size())
    return NULL;

  SceMiMessageInPortProxy* proxy = inProxies[chan];
  return proxy->PortName();
}

const char *SceMi::sceMiMessageOutPortProxyName(unsigned int chan)
{
  if (chan > outProxies.size())
    return NULL;

  SceMiMessageOutPortProxy* proxy = outProxies[chan];
  return proxy->PortName();
}

void *SceMi::scemi_extension_access_vendor_platform()
{
  if (the_scemi_object && the_scemi_object->link)
    return the_scemi_object->link->scemi_extension_access_vendor_platform();
  return NULL;
}

// ****************************************************************
