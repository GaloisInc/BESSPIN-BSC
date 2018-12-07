// Copyright (c) 2008 Bluespec, Inc; all rights reserved

#include <cstdlib>
#include <cstdio>

#include "SceMiEC.h"

// ****************************************************************
// This is a single shared error handler

static SceMiErrorHandler   errorHandlerFn = NULL;
static void               *errorHandlerContext = NULL;
static SceMiEC             errorHandlerEc;

/* Function for registering an error handler */
void ec_register_handler(SceMiErrorHandler errorHandler,
                         void*             context)
{
  errorHandlerFn = errorHandler;
  errorHandlerContext = context;
}

/* Accessor functions for error handler info */
SceMiErrorHandler ec_get_handler()
{
  return errorHandlerFn;
}

void* ec_get_context()
{
  return errorHandlerContext;
}

/* Utility function for handling errors */
void raiseError (const char     *culprit,
		 const char     *message,
		 SceMiErrorType  errtype,
		 SceMiEC        *ec)
{
    if (ec != NULL) {
	ec->Culprit = culprit;
	ec->Message = message;
	ec->Type    = errtype;
    }
    else if (errorHandlerFn != NULL) {
	ec = & errorHandlerEc;
	ec->Culprit = culprit;
	ec->Message = message;
	ec->Type    = errtype;
	errorHandlerFn(errorHandlerContext, ec);
    }
    else
    {
      /* default error handler */
      if (culprit)
	fprintf(stderr, "%s\n", culprit);
      else
	fprintf(stderr, "SCE-MI Error\n");
      if (message)
	fprintf(stderr, ": %s\n", message);
      if (errtype == SceMiError)
        abort();
    }
}
