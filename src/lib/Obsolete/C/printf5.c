/* Copyright 2000--2003 Bluespec, Inc.  All rights reserved. */

/* $Id: printf5.c,v 1.4 2003/01/10 21:39:12 elf Exp $ */

#include "bsc.h"
#include "misc.h"
#include "printf5.h"

void
printf5(varcp s, varcp x1, varcp x2, varcp x3, varcp x4, varcp x5)
{
    char buf[1024] ;
    strlrev(buf, (const char *)s->data, sizeof buf); 
    printf(buf, GETUINT(x1), GETUINT(x2), GETUINT(x3), GETUINT(x4), GETUINT(x5));
}
