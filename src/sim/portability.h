/* Copyright 2004--2009 Bluespec, Inc.  All rights reserved. */

#ifndef __PORTABILITY_H__
#define __PORTABILITY_H__

/* This file contains declarations of utility functions
 * which enhance code portability.
 */

#include <cstdio>
#include <sys/types.h>
#include <limits.h>
#include <semaphore.h>

/* The defines for limits of long long values change across versions */
#ifndef LLONG_MAX
#define LLONG_MAX    9223372036854775807LL
#endif

#ifndef LLONG_MIN
#define LLONG_MIN    (-LLONG_MAX - 1LL)
#endif

#ifndef LONG_LONG_MIN
#define LONG_LONG_MIN LLONG_MIN
#endif

#ifndef LONG_LONG_MAX
#define LONG_LONG_MAX LLONG_MAX
#endif

extern "C" {

/* our own implementation of strdup() */
char* port_strdup(const char* str);
char* port_strndup(const char* str, unsigned int n);

/* older libraries don't have ftello() */
off_t port_ftello(FILE* stream);

/* exponentiation on integers */
unsigned long long powll(unsigned int base, unsigned int exp);

/* portable semaphore facade */

#ifdef __APPLE__
#define USE_NAMED_SEMAPHORES 1
#else
#define USE_NAMED_SEMAPHORES 0
#endif

#if USE_NAMED_SEMAPHORES
typedef struct
{
  char*  name;
  sem_t* sem;
} tSemaphore;
#else
typedef sem_t tSemaphore;
#endif

tSemaphore* create_semaphore();
void post_semaphore(tSemaphore* semaphore);
void trywait_on_semaphore(tSemaphore* semaphore);
void wait_on_semaphore(tSemaphore* semaphore);
void release_semaphore(tSemaphore* semaphore);

// asprintf allocates an output buffer for the caller to free
// asprintf is common but not standardized, so
// so we implement it with C99 va_copy and vsnprintf
#undef asprintf
#define asprintf new_asprintf
  int new_asprintf(char **strp, const char *fmt, ...) throw ();

} /* extern "C" */

#endif /* __PORTABILITY_H__ */
