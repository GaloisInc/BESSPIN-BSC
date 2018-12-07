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
SCEMILIB=$(LIBDIR)/SceMi/BlueNoC

# Determine which g++ directory to build in
BUILDDIR = $(shell $(TOP)/util/scripts/bsenv c++_family)
$(info generating libraries for $(BUILDDIR))
export BUILDDIR

# These get copied to the inst/lib/SceMi area
HEADERS = sized_types.h scemi.h Link.h \
	  SceMiParameters.h SceMiEC.h SceMiIC.h SceMiProxies.h SceMiTypes.h \
	  SceMiPipe.h SceMiPipeInfo.h bsv_scemi.h svdpi.h scemi_pipes.h DataBufferQ.h \
	tlm_xactors.h

HEADERS_C = sized_types.h Link.h \
	    SceMiTypes.h SceMiEC.h SceMiIC.h \
	    scemi_capi.h scemi_c_parameters.h scemi_c_proxies.h \
	    svdpi.h scemi_pipes.h

# The list of files to "install" is pulled from the top-level header file
# Triple escapes
BSVXACTORS_H = $(shell sed -n -e s/^.*\\\#include.*\\\"\\\(\\\.\\\.\\\/bsvxactors.*\\\)\\\"/\\\1/p bsv_scemi.h )
TLMXACTORS_H = $(shell sed -n -e s/^.*\\\#include.*\\\"\\\(\\\.\\\.\\\/tlmxactors.*\\\)\\\"/\\\1/p tlm_xactors.h )
SYSC_H       = $(shell sed -n -e s/^.*\\\#include.*\\\"\\\(\\\.\\\.\\\/tlmxactors.*\\\)\\\"/\\\1/p tlm_systemc.h )

ALLHEADERS = $(HEADERS)  $(HEADERS_C) $(BSVXACTORS_H) $(TLMXACTORS_H)


.PHONY:	all
all:
	$(MAKE) -C $(BUILDDIR) all

# Static check of the header files before releasing them.
test-headers: test_hdr.cxx

test_hdr.cxx: $(ALLHEADERS)
	@echo "Checking header files for self consistency at: `date`"
	@-$(RM) -f $@
	@$(foreach hf,$(ALLHEADERS), \
		$(RM) -f test_hdr.cxx && \
		echo "#include \"$(hf)\"" >> test_hdr.cxx  && \
		$(CXX) -std=gnu++98 -I. -c -o /dev/null test_hdr.cxx && \
	) \
	echo "Done at: `date`"
	touch $@


.PHONY: install
install: all test-headers
	$(INSTALL) -m 755 -d $(SCEMILIB)
	$(INSTALL) -m 644 $(HEADERS) $(SCEMILIB)
	$(MAKE) -C $(BUILDDIR) install
	$(INSTALL) -m 755 -d $(SCEMILIB)/../bsvxactors
	$(INSTALL) -m 644 $(BSVXACTORS_H)  $(SCEMILIB)/../bsvxactors
	$(INSTALL) -m 755 -d $(SCEMILIB)/../tlmxactors
	$(INSTALL) -m 644 $(TLMXACTORS_H)  $(SCEMILIB)/../tlmxactors
	$(INSTALL) -m 644 $(SYSC_H)        $(SCEMILIB)/../tlmxactors
	$(INSTALL) -m 644 tlm_systemc.h    $(SCEMILIB)

.PHONY: clean
clean:
	$(RM) test_hdr.cxx test_hdr_c.c
	$(MAKE) -C $(BUILDDIR) clean

realclean: clean

TAGS:	*.h *.c
	etags *.h *.c
