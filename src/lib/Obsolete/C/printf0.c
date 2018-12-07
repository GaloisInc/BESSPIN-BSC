/* Copyright 2000--2003 Bluespec, Inc.  All rights reserved. */

/* $Id: printf0.c,v 1.4 2003/01/10 21:39:12 elf Exp $ */

#include "bsc.h"
#include "misc.h"
#include "printf0.h"

void printf0(varcp);

void
printf0(varcp s)
{
    char buf[1024] ;
    strlrev(buf, (const char *)s->data, sizeof buf); 
    printf(buf);
}
