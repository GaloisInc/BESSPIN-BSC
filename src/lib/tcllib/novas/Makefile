##   Makefile for tcl library
##  Copies the tcl libraries from vendor to the inst dir

VENDOR = ../../../../vendor/

TCLDIR = $(VENDOR)/tcltk/


# Location where files are installed
PWD:=$(shell pwd)
PREFIX?=$(PWD)/../../../../inst
LIBDIR=$(PREFIX)/lib

INSTALL ?= install

.PHONY: all
all: install


.PHONY: install
install: novaslibs

.PHONY: novaslibs
novaslibs: 
	install -d -m755 $(LIBDIR)/tcllib/novas
	make -C etc LIBDIR=$(LIBDIR)/ install



TAGS: */*.tcl
	ctags -R -e

.PHONY: clean realclean
realclean clean:
	rm -f TAGS
	make -C etc $@
