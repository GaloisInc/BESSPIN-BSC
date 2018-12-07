/* Copyright 2000--2003 Bluespec, Inc.  All rights reserved. */

/* $Id: printf2.c,v 1.4 2003/01/10 21:39:12 elf Exp $ */

#include "bsc.h"
#include "misc.h"
#include "printf2.h"

void
printf2(varcp s, varcp x1, varcp x2)
{
    char buf[1024] ;
    strlrev(buf, (const char *)s->data, sizeof buf); 
    printf(buf, GETUINT(x1), GETUINT(x2));
}
