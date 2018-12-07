# Makefile for BlueNoC library
# Copyright 2011 Bluespec, Inc. All Rights Reserved
PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

INSTALL ?= $(TOP)/util/install

PREFIX?=$(TOP)/inst
LIBDIR=$(PREFIX)/lib
BNOCLIB=$(LIBDIR)/BlueNoC
DRVLIB=$(LIBDIR)/board_support/bluenoc

# Determine which g++ directory to build in
BUILDDIR = $(shell $(TOP)/util/scripts/bsenv c++_family)
$(info generating libraries for $(BUILDDIR))
export BUILDDIR

# These get copied to the inst/lib/BlueNoC area
HEADERS = bluenoc_tcp.h sized_types.h bluenoc.h core_link.h bluenoc_link.h bluenoc_linkplugin.h bluenoc_parameters.h \
bluenoc_pipe.h DataBufferQ.h pcie_link.h tcp_link.h xactors.h

XACTORS_H = $(shell sed -n -e s/^.*\\\#include.*\\\"\\\(xactors.*\\\)\\\"/\\\1/p xactors.h )

ALLHEADERS = $(HEADERS) $(XACTORS_H)

.PHONY:	all
all:
	$(MAKE) -C $(BUILDDIR) all

# Static check of the header files before releasing them.
.PHONY: test-headers
test-headers: test_hdr.cxx

test_hdr.cxx: $(ALLHEADERS)
	@echo "Checking header files for self consistency at: `date`"
	@echo "$(XACTORS_H)"
	@-$(RM) -f $@
	@$(foreach hf,$(ALLHEADERS), \
		$(RM) -f test_hdr.cxx && \
		echo "#include \"$(hf)\"" >> test_hdr.cxx  && \
		$(CXX) -I. -I$(DRVLIB) -c -o /dev/null test_hdr.cxx && \
	) \
	echo "Done at `date`"
	touch $@

.PHONY: install
install: all test-headers
	$(INSTALL) -m 755 -d $(BNOCLIB)
	$(INSTALL) -m 644 $(HEADERS) $(BNOCLIB)
	$(MAKE) -C $(BUILDDIR) install
	$(INSTALL) -m 755 -d $(BNOCLIB)/xactors
	$(INSTALL) -m 644 $(XACTORS_H) $(BNOCLIB)/xactors

.PHONY: clean
clean:
	$(RM) test_hdr.cxx test_hdr_c.c
	$(MAKE) -C $(BUILDDIR) clean

realclean: clean

TAGS:	*.h *.c
	etags *.h *.c
