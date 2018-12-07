# Makefile for VPI access library
# Copyright 2008 Bluespec, Inc. All Rights Reserved
PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

INSTALL ?= $(TOP)/util/install

PREFIX?=$(TOP)/inst
LIBDIR=$(PREFIX)/lib
VPILIB=$(LIBDIR)/VPI

# Determine which g++ directory to build in
BUILDDIR = $(shell $(TOP)/util/scripts/bsenv c++_family)
$(warning generating libraries for $(BUILDDIR)) 
export BUILDDIR

# These get copied to the inst/lib/VPI area
HEADERS = bdpi.h vpi_user.h

.PHONY:	all
all:
	$(MAKE) -C $(BUILDDIR) all

# Static check of the header files before releasing them.
.PHONY: test-headers
test-headers:
	$(RM) test_hdr.c
	for hdr in $(HEADERS); do \
          echo "#include \"$$hdr\"" >> test_hdr.c ; \
	done
	$(CC) -c -o /dev/null test_hdr.c

.PHONY: install
install: all test-headers
	$(INSTALL) -m 755 -d $(VPILIB)
	$(INSTALL) -m 644 $(HEADERS) $(VPILIB)
	$(MAKE) -C $(BUILDDIR) install

.PHONY: clean
clean:
	$(RM) test_hdr.c
	$(MAKE) -C $(BUILDDIR) clean

realclean: clean

TAGS:	*.h *.c
	etags *.h *.c
