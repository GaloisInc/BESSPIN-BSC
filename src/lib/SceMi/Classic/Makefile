# Makefile for SCE-MI library
# Copyright 2008 Bluespec, Inc. All Rights Reserved
PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

VENDOR = $(TOP)/vendor

# definitions for platform dependent variables
include $(TOP)/platform.mk

INSTALL ?= $(TOP)/util/install

PREFIX?=$(TOP)/inst
LIBDIR=$(PREFIX)/lib
SCEMILIB=$(LIBDIR)/SceMi/Classic

# Determine which g++ directory to build in
BUILDDIR = $(shell $(TOP)/util/scripts/bsenv c++_family)
$(info generating libraries for $(BUILDDIR)) 
export BUILDDIR

# These get copied to the inst/lib/SceMi area
HEADERS = sized_types.h scemi.h Link.h \
          SceMiParameters.h SceMiEC.h SceMiIC.h SceMiProxies.h SceMiTypes.h \
	  bsv_scemi.h

HEADERS_C = sized_types.h Link.h \
            SceMiTypes.h SceMiEC.h SceMiIC.h \
            scemi_capi.h scemi_c_parameters.h scemi_c_proxies.h

# Triple escapes
BSVXACTORS_H = $(shell sed -n -e s/^.*\\\#include.*\\\"\\\(\\\.\\\.\\\/bsvxactor.*\\\)\\\"/\\\1/p bsv_scemi.h )

.PHONY:	all
all:
	$(MAKE) -C $(BUILDDIR) all

# Static check of the header files before releasing them.
.PHONY: test-headers
test-headers:
	$(RM) test_hdr.cxx
	for hdr in $(HEADERS); do \
          echo "#include \"$$hdr\"" >> test_hdr.cxx ; \
	done
	$(CXX) -std=gnu++98 -I. -c -o /dev/null test_hdr.cxx
	$(RM) test_hdr_c.c
	for hdr in $(HEADERS_C); do \
	  echo "#include \"$$hdr\"" >> test_hdr_c.c ; \
	done
	$(CXX) -std=c99 -c -o /dev/null test_hdr_c.c

.PHONY: install
install: all test-headers
	$(INSTALL) -m 755 -d $(SCEMILIB)
	$(INSTALL) -m 644 $(HEADERS) $(SCEMILIB)
	$(MAKE) -C $(BUILDDIR) install
#	$(INSTALL) -m 755 -d $(SCEMILIB)/bsvxactors
#	$(INSTALL) -m 644 $(BSVXACTORS_H)  $(SCEMILIB)/bsvxactors

.PHONY: clean
clean:
	$(RM) test_hdr.cxx test_hdr_c.c
	$(MAKE) -C $(BUILDDIR) clean

realclean: clean

TAGS:	*.h *.c
	etags *.h *.c
