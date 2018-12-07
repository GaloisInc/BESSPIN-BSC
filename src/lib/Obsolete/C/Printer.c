/* Copyright 2000--2003 Bluespec, Inc.  All rights reserved. */

/* $Id: Printer.c,v 1.26 2003/01/10 21:39:11 elf Exp $ */

#include "bsc.h"
#include "misc.h"
#include "Printer.h"

#define MAXITEMS 1000		/* maximum number of items that can be printed in one cycle */

struct var_IBPrinter_ { OBJ_VAR(0,1,0) } ;
struct IBPrinter_ {
  /* public */
  struct obj base;
  struct var_IBPrinter_ varinfo ;
  /* private */
  struct {
    enum { STR, VARHEX, VARDEC, CYCLE } what;
    varcp item;
  } args[MAXITEMS];
  int nargs;
};

static void
Printer_pUpdate(void/*BPrinter_ aself*/)
{
  int i/*, dw*/;
  struct IBPrinter_ *self = (struct IBPrinter_ *) bsc_self ;
  char buf[FULLNAMESIZE] ;
  for (i = 0; i < self->nargs; i++) {
    switch (self->args[i].what) {
    case STR:
      printf("%s", (const char *)self->args[i].item->data);
      break;
    case VARHEX:
      show_var(buf, sizeof buf, (varcp)self->args[i].item);
      printf("%s", buf) ;
      break;
    case VARDEC:
      if (NWORDS(self->args[i].item->size) == 1) {
        /* dw is decimal field width. 0.301029996 is log 2/log 10 */
	/*	dw = 1 + 0.301029996 * self->args[i].item->size; */
	/* OBSOLETE: printf("%10u", self->args[i].item->data[0]); */
	printf("%u", self->args[i].item->data[0]);
      } else {
        show_var(buf, sizeof buf, (varcp)self->args[i].item);
        printf("%s", buf) ;
      }
      break;
    case CYCLE:
      printf("%llu", sim_time / 2);
      break;
    }
  }
  fflush(stdout);
  self->nargs = 0;
}

static void
Printer_pNoUpdate(void/*BPrinter_ aself*/)
{
  struct IBPrinter_ *self = (struct IBPrinter_ *) bsc_self ;
  self->nargs = 0;
}

static void
Printer_pDump(volatile objp aself, int i)
{
  struct IBPrinter_ *self = (struct IBPrinter_ *) aself ;
  /* Fool stupid gcc 2.96 into not thinking aself is const. */
  self = self;
}

void
Printer_m_print_(objp aself, varcp str)
{
  struct IBPrinter_ *self = (struct IBPrinter_ *) aself ;
  if (self->nargs >= MAXITEMS) {
    printf("Printer: lost string (too many printed in one cycle)\n");
  } else {
    self->args[self->nargs].what = STR;
    self->args[self->nargs++].item = str;
  }
}

void
Printer_m_printBit_(objp aself, varcp arg)
{
  struct IBPrinter_ *self = (struct IBPrinter_ *) aself ;
  if (self->nargs >= MAXITEMS) {
    printf("Printer: lost argument (too many printed in one cycle)\n");
  } else {
    self->args[self->nargs].what = VARHEX;
    self->args[self->nargs++].item = arg;
  }
}

void
Printer_m_printBitDec_(objp aself, varcp arg)
{
  struct IBPrinter_ *self = (struct IBPrinter_ *) aself ;
  if (self->nargs >= MAXITEMS) {
    printf("Printer: lost argument (too many printed in one cycle)\n");
  } else {
    self->args[self->nargs].what = VARDEC;
    self->args[self->nargs++].item = arg;
  }
}

void
Printer_m_exitWith_(volatile objp aself, varcp arg)
{
  struct IBPrinter_ *self = (struct IBPrinter_ *) aself ;
  self = self;
  exit(GETUINT(arg));
}

void
Printer_m_printCycle_(objp aself)
{
  struct IBPrinter_ *self = (struct IBPrinter_ *) aself ;
  if (self->nargs >= MAXITEMS) {
    printf("Printer: lost argument (too many printed in one cycle)\n");
  } else {
    self->args[self->nargs++].what = CYCLE;
  }
}
struct objconst printer_oconst =
{
  OBJCONST_CCODE(Printer_pUpdate, Printer_pNoUpdate, Printer_pDump, 1, NULL)
} ;

objp
new_Printer(objp parent, const struct varinfo *vinfo)
{
  struct IBPrinter_ *r = xalloc(sizeof *r) ;

  if (verbose > 1)
    printf("new Printer_ %s\n", vinfo->vi_name);
  initobj(&r->base, parent, vinfo, &printer_oconst);
  r->nargs = 0;
  return &r->base;
}
