/*
  Parts Copyright © 2003-2007 by Accellera
  Parts Copyright (c) 2008-2009 Bluespec, Inc; all rights reserved
  scemi_capi.h - SCE-MI C Interface
*/

#ifndef __SCEMI_CAPI_H__
#define __SCEMI_CAPI_H__


typedef void SceMi;
typedef void SceMiParameters;
typedef void SceMiMessageData;
typedef void SceMiMessageInPortProxy;
typedef void SceMiMessageOutPortProxy;
typedef void SceMiMessageInPortBinding;
typedef void SceMiMessageOutPortBinding;
typedef int (*SceMiServiceLoopHandler)( void *context, int pending );

/* include header files for the various Sce-Mi classes */

#include "SceMiTypes.h"         /* Typedefs */
#include "SceMiEC.h"            /* Error Context */
#include "SceMiIC.h"            /* Information Context */
#include "scemi_c_parameters.h" /* Parameter parsing and access */
#include "scemi_c_proxies.h"    /* In-port and out-port proxies, message data and bindings */


#ifdef __cplusplus
extern "C" {
#endif

  /* SceMiEC - error handling */
  void SceMiRegisterErrorHandler(SceMiErrorHandler errorHandler,
				 void *context );

  /* Version discovery */
  int SceMiVersion(const char *versionString);

  /* Initialization */
  SceMi *SceMiInit(int version,
		   const SceMiParameters *parameterObjectHandle,
		   SceMiEC *ec );

  /* SceMi Object Pointer Access */
  SceMi *SceMiPointer(SceMiEC *ec);

  /* Shutdown */
  void SceMiShutdown(SceMi *sceMiHandle,
		     SceMiEC *ec);

  /* Service loop */
  int SceMiServiceLoop(SceMi *sceMiHandle,
		       SceMiServiceLoopHandler g,
		       void *context,
		       SceMiEC *ec);


#ifdef __cplusplus
};
#endif

#endif /* __SCEMI_CAPI_H__ */
