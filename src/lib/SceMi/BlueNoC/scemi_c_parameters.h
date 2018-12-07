/*
  Parts Copyright © 2003-2007 by Accellera
  Parts Copyright (c) 2008-2009 Bluespec, Inc; all rights reserved
*/
#ifndef __SCEMI_C_PARAMETERS_H__
#define __SCEMI_C_PARAMETERS_H__

#include "scemi_capi.h"

#ifdef __cplusplus
extern "C" {
#endif

  /* SceMiParameters - parameter access */
  /* Constructor */
  SceMiParameters *SceMiParametersNew(const char *paramsFile,
				      SceMiEC *ec);

  /* Destructor */
  void SceMiParametersDelete(SceMiParameters *parametersHandle);


  /* Accessors */
  unsigned int SceMiParametersNumberOfObjects(const SceMiParameters *parametersHandle,
					      const char *objectKind,
					      SceMiEC *ec);

  int SceMiParametersAttributeIntegerValue(const SceMiParameters *parametersHandle,
					   const char *objectKind,
					   unsigned int index,
					   const char *attributeName,
					   SceMiEC *ec);

  const char *SceMiParametersAttributeStringValue(const SceMiParameters *parametersHandle,
						  const char *objectKind,
						  unsigned int index,
						  const char *attributeName,
						  SceMiEC *ec);

  void SceMiParametersOverrideAttributeIntegerValue(SceMiParameters *parametersHandle,
						    const char *objectKind,
						    unsigned int index,
						    const char *attributeName,
						    int value,
						    SceMiEC *ec);

  void SceMiParametersOverrideAttributeStringValue(SceMiParameters *parametersHandle,
						   const char *objectKind,
						   unsigned int index,
						   const char *attributeName,
						   const char *value,
						   SceMiEC *ec);


#ifdef __cplusplus
};
#endif

#endif /* __SCEMI_C_PARAMETERS_H__ */
