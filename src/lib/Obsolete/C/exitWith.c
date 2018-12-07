/* Copyright 2000--2003 Bluespec, Inc.  All rights reserved. */

/* $Id: exitWith.c,v 1.2 2003/01/10 21:39:12 elf Exp $ */

#include <sys/types.h>
#include <setjmp.h>
#include "bsc.h"
#include "misc.h"

extern jmp_buf exitjmp ;

void
exitWith(varcp exitCode)
{
  global_exit_request = 1 ;
  global_exit_code = exitCode->data[0] ;
  longjmp(exitjmp, 3) ;
}
