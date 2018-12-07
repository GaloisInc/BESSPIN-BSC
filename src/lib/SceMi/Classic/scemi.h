//
// Copyright Å© 2003-2007 by Accellera
// scemi.h - SCE-MI C++ Interface
//
#ifndef __SCEMI_H__
#define __SCEMI_H__

#include <list>
#include <vector>
#include <set>

/* Sce-Mi version information */

#define SCEMI_MAJOR_VERSION 2
#define SCEMI_MINOR_VERSION 0
#define SCEMI_PATCH_VERSION 0
#define SCEMI_VERSION_STRING "2.0.0"

/* include header files for the various Sce-Mi classes */

#include "SceMiTypes.h"      /* basic POD typedefs */
#include "SceMiEC.h"         /* Error Context */
#include "SceMiIC.h"         /* Information Context */
#include "SceMiParameters.h" /* Parameter parsing and access */
#include "SceMiProxies.h"    /* In-port and out-port proxies, message data and bindings */

/* include header file for communication link code */

#include "Link.h"

/* Type aliases for handler functions */
extern "C" {
    typedef int (*SceMiServiceLoopHandler)(void* context, int pending);
}

//
// class SceMi
//
// Description
// -----------
// This file defines the public interface to class SceMi.
//

class SceMi {

 private:
  SceMiParameters *parameters; // copy of parameters

  // communication link to HW
  Link* link;
  void* link_handle;
  void (*destroy_link_fn)(Link* link);
  bool service_thread_stop;

  // record of port proxies (indexed by channel number)
  std::vector<SceMiMessageInPortProxy*>  inProxies;
  std::vector<SceMiMessageOutPortProxy*> outProxies;

  std::list<std::pair<Packet*,SceMiU64> > pending_from_hw;  // data arrived from HW

  std::set<unsigned int> ready_channels; // set of channels which need ready callbacks
  std::set<unsigned int> not_ready_channels; // set of channels which are not ready

  SceMi(const SceMiParameters* params, SceMiEC* ec = NULL);

  ~SceMi();

 public:
    //
    // Check version string against supported versions.
    // Returns -1 if passed string not supported.
    // Returns interface version # if it is supported.
    // This interface version # can be passed to SceMi::Init().
    //
    static int Version(const char* versionString);

    //
    // This function wraps constructor of class SceMi. If an instance
    // of class SceMi has been established on a prior call to the
    // SceMi::Init() function, that pointer is returned since a single
    // instance of class SceMi is reusable among all C models.
    // Returns NULL if error occurred, check ec for status or register
    // an error callback.
    //
    // The caller is required to pass in the version of SceMi it is
    // expecting to work with. Call SceMi::Version to convert a version
    // string to an integer suitable for this version's "version" arguÅ¨ment.
    //
    // The caller is also expected to have instantiated a SceMiParameters
    // object, and pass a pointer to that object into this function.
    //
    static SceMi* Init(int version,
		       const SceMiParameters* parameters,
		       SceMiEC* ec = NULL);


    //
    // Get access to constructed SceMi object pointer
    //
    static SceMi* Pointer(SceMiEC* ec = NULL);

    //
    // Shut down the SCEMI interface.
    //
    static void Shutdown(SceMi* mct, SceMiEC* ec = NULL);

    //
    // Create proxy for message input port.
    //
    // Pass in the instance name in the bridge netlist of
    // the transactor and port to which binding is requested.
    //
    // The binding argument is a callback function and context
    // pointer tray. For more details, see the comments in
    // scemicommontypes.h by struct SceMiMessageInPortBinding.
    //
    SceMiMessageInPortProxy*
    BindMessageInPort(const char* transactorName,
		      const char* portName,
		      const SceMiMessageInPortBinding* binding = 0,
		      SceMiEC* ec = NULL);

    //
    // Create proxy for message output port.
    //
    // Pass in the instance name in the bridge netlist of
    // the transactor and port to which binding is requested.
    //
    // The binding argument is a callback function and context
    // pointer tray. For more details, see the comments in
    // scemicommontypes.h by struct SceMiMessageOutPortBinding.
    //
    SceMiMessageOutPortProxy*
    BindMessageOutPort(const char* transactorName,
		       const char* portName,
		       const SceMiMessageOutPortBinding* binding = 0,
		       SceMiEC* ec = NULL);

    //
    // Service arriving transactions from the portal.
    // Messages enqueued by SceMiMessageOutPortProxy methods, or which are
    // are from output transactions that pending dispatch to the
    // SceMiMessageOutPortProxy callbacks, may not be handled until
    // ServiceLoop() is called. This function returns the # of output
    // messages that were dispatched.
    //
    // Regarding the service loop handler (aka "g function"):
    // If g is NULL, check for transfers to be performed and
    // dispatch them returning immediately afterwards. If g is
    // non-NULL, enter into a loop of performing transfers and
    // calling 'g'. When 'g' returns 0 return from the loop.
    // When 'g' is called, an indication of whether there is at
    // least 1 message pending will be made with the 'pending' flag.
    //
    // The user context object pointer is uninterpreted by
    // ServiceLoop() and is passed straight to the 'g' function.
    //
    int ServiceLoop(SceMiServiceLoopHandler g = 0,
		    void* context = 0,
		    SceMiEC* ec = NULL);

    void              ServiceThreadStop(bool value) { service_thread_stop = value; }
    bool              ServiceThreadStop()           { return service_thread_stop; }

    //
    // Register an error handler which is called in the event
    // that an error occurs. If no handler is registered, the
    // default error handler is called.
    //
    static void RegisterErrorHandler(SceMiErrorHandler errorHandler,
				     void* context);

    //
    // Register an info handler which is called in the event
    // that a text message needs to be issued. If no handler
    // is registered, the message is printed to stdout in
    // Ikos message format.
    //
    static void RegisterInfoHandler(SceMiInfoHandler infoHandler,
				    void* context);

    //
    // Parameters method to return the parameter structure.
    //  This is a non-standard SceMi API (thus the lowercase function)
    //
    SceMiParameters *parameter() { return parameters; }


    // Returns the name of the In/Out Port Proxy
    const char *sceMiMessageInPortProxyName(unsigned int channel);
    const char *sceMiMessageOutPortProxyName(unsigned int channel);

    // Special extension for return platform specific object
    static void *scemi_extension_access_vendor_platform();
};

#endif /* __SCEMI_H__ */
