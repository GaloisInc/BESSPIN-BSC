// Parts Copyright © 2003-2007 by Accellera
// Parts Copyright (c) 2008-2009 Bluespec, Inc; all rights reserved

#ifndef __SCEMI_EC_H__
#define __SCEMI_EC_H__

/* SceMi Error Context */

typedef enum {
    SceMiOK,
    SceMiError
} SceMiErrorType;

typedef struct {
    const char* Culprit;  /* The offending function */
    const char* Message;  /* Descriptive message describing problem */
    SceMiErrorType Type;  /* Error code describing the nature of the error */
    int Id;               /* A code to uniquely identify each error */
} SceMiEC;

/* Type aliases for handler functions */
#ifdef __cplusplus
extern "C" {
#endif
  typedef void (*SceMiErrorHandler)(void* context, SceMiEC* ec);
#ifdef __cplusplus
}
#endif
/* Function for registering an error handler */
#ifdef __cplusplus
extern
#endif
void ec_register_handler(SceMiErrorHandler errorHandler,
			 void*             context);

/* Accessor functions for error handler info */
#ifdef __cplusplus
extern
#endif
SceMiErrorHandler ec_get_handler();
#ifdef __cplusplus
extern
#endif
void* ec_get_context();

/* Utility function for handling errors */
#ifdef __cplusplus
extern
#endif
void raiseError (const char     *culprit,
		 const char     *message,
		 SceMiErrorType  errtype,
		 SceMiEC        *ec);

#endif /* __SCEMI_EC_H__ */
