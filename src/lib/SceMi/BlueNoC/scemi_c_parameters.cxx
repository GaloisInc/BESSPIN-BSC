/*
  Parts Copyright © 2003-2007 by Accellera
  Parts Copyright (c) 2008 Bluespec, Inc; all rights reserved
*/

/* include header files for the various Sce-Mi classes */

#include "scemi.h"

extern "C" {

  /* SceMiParameters - parameter access */
  /* Constructor */
  SceMiParameters *SceMiParametersNew(const char *paramsFile,
				      SceMiEC *ec)
  {
    return new SceMiParameters(paramsFile, ec);
  }

  /* Destructor */
  void SceMiParametersDelete(SceMiParameters *parametersHandle)
  {
    delete parametersHandle;
  }


  /* Accessors */
  unsigned int SceMiParametersNumberOfObjects(const SceMiParameters *parametersHandle,
					      const char *objectKind,
					      SceMiEC *ec)
  {
    return parametersHandle->NumberOfObjects(objectKind, ec);
  }

  int SceMiParametersAttributeIntegerValue(const SceMiParameters *parametersHandle,
					   const char *objectKind,
					   unsigned int index,
					   const char *attributeName,
					   SceMiEC *ec)
  {
    return parametersHandle->AttributeIntegerValue(objectKind, index, attributeName, ec);
  }

  const char *SceMiParametersAttributeStringValue(const SceMiParameters *parametersHandle,
						  const char *objectKind,
						  unsigned int index,
						  const char *attributeName,
						  SceMiEC *ec)
  {
    return parametersHandle->AttributeStringValue(objectKind, index, attributeName, ec);
  }


  void SceMiParametersOverrideAttributeIntegerValue(SceMiParameters *parametersHandle,
						    const char *objectKind,
						    unsigned int index,
						    const char *attributeName,
						    int value,
						    SceMiEC *ec)
  {
    parametersHandle->OverrideAttributeIntegerValue(objectKind, index, attributeName, value, ec);
  }

  void SceMiParametersOverrideAttributeStringValue(SceMiParameters *parametersHandle,
						   const char *objectKind,
						   unsigned int index,
						   const char *attributeName,
						   const char *value,
						   SceMiEC *ec)
  {
    parametersHandle->OverrideAttributeStringValue(objectKind, index, attributeName, value, ec);
  }

};
