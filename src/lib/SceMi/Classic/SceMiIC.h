// Copyright © 2003-2007 by Accellera

#ifndef __SCEMI_IC_H__
#define __SCEMI_IC_H__

/* SceMi Information Message Context */

typedef enum {
    SceMiInfo,
    SceMiWarning,
    SceMiNonFatalError
} SceMiInfoType;

typedef struct {
    const char* Originator; /* The source of the message */
    const char* Message;    /* Informational message */
    SceMiInfoType Type;     /* Code describing the severity of the message */
    int Id;                 /* A code to uniquely identify each message */
} SceMiIC;

/* Type aliases for handler functions */
#ifdef __cplusplus
extern "C" {
#endif
    typedef void (*SceMiInfoHandler)(void* context, SceMiIC* ic);

#ifdef __cplusplus
}
#endif

#endif /* __SCEMI_IC_H__ */
