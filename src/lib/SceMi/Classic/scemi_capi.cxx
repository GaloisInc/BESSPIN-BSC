/*
  Parts Copyright © 2003-2007 by Accellera
  Parts Copyright (c) 2008 Bluespec, Inc; all rights reserved
*/

/* include header files for the various Sce-Mi classes */

#include "scemi.h"
#include <list>
#include <vector>


extern "C" {

  /* Forward declare the scemi callback funcions */
  void bscemi_message_inport_ready_CB(void* context);
  void bscemi_message_outport_ready_CB(void* context, const SceMiMessageData* data);


  /* SceMiEC - error handling */
  void SceMiRegisterErrorHandler(SceMiErrorHandler errorHandler,
				 void *context)
  {
    SceMi::RegisterErrorHandler(errorHandler, context);
  }

  /* Version discovery */
  int SceMiVersion(const char *versionString)
  {
    return SceMi::Version(versionString);
  }

  /* Initialization */
  SceMi *SceMiInit(int version,
		   const SceMiParameters *parameterObjectHandle,
		   SceMiEC *ec)
  {
    return SceMi::Init(version, parameterObjectHandle, ec);
  }

  /* SceMi Object Pointer Access */
  SceMi *SceMiPointer(SceMiEC *ec)
  {
    return SceMi::Pointer(ec);
  }

  /* Shutdown */
  void SceMiShutdown(SceMi *sceMiHandle,
		     SceMiEC *ec)
  {
    sceMiHandle->Shutdown(sceMiHandle, ec);
  }

  /* Service loop */
  int SceMiServiceLoop(SceMi *sceMiHandle,
		       SceMiServiceLoopHandler g,
		       void *context,
		       SceMiEC *ec)
  {
    return sceMiHandle->ServiceLoop(g, context, ec);
  }

};
