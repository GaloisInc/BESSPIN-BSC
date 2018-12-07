/* Copyright 2000--2003 Bluespec, Inc.  All rights reserved. */

/* $Id: printf1.c,v 1.4 2003/01/10 21:39:12 elf Exp $ */

#include "bsc.h"
#include "misc.h"
#include "printf1.h"

void printf1(varcp, varcp);

void
printf1(varcp s, varcp x1)
{
    char buf[1024] ;
    strlrev(buf, (const char *)s->data, sizeof buf); 
    printf(buf, GETUINT(x1));
}
