# Makefile for Bluesim runtime
# Copyright 2005 Bluespec, Inc. All Rights Reserved
PWD:=$(shell pwd)
TOP:=$(PWD)/../..

INSTALL ?= $(TOP)/util/install

PREFIX?=$(TOP)/inst
LIBDIR=$(PREFIX)/lib
SIMLIB=$(LIBDIR)/Bluesim
COMPDIR=$(TOP)/src/comp

# Determine which g++ directory to build in
BUILDDIR = $(shell $(TOP)/util/scripts/bsenv c++_family)
$(info generating libraries for $(BUILDDIR))
export BUILDDIR

# These get copied to the inst/lib/Bluesim area
HEADERS = bluesim_kernel_api.h \
          bluesim_types.h \
          bluesim_primitives.h \
          bluesim_systemc.h \
          bluesim_probes.h \
          bs_wide_data.h \
          bs_prim_ops.h \
          bs_prim_mod_reg.h bs_prim_mod_wire.h bs_prim_mod_probe.h \
          bs_prim_mod_fifo.h bs_prim_mod_regfile.h bs_prim_mod_bram.h \
          bs_prim_mod_counter.h \
          bs_prim_mod_clockgen.h \
          bs_prim_mod_synchronizers.h \
          bs_prim_mod_gatedclock.h \
          bs_prim_mod_clockmux.h \
          bs_prim_mod_resets.h \
          bs_reset.h \
          bs_symbol.h \
          bs_system_tasks.h \
          bs_mem_defines.h bs_mem_file.h bs_range_tracker.h \
          bs_vcd.h bs_module.h bs_target.h \
          bs_model.h

# These ld export maps get copies to the inst/lib/Bluesim area
LINKFILES = bs_linux_export_map.txt \
            bs_darwin_export_map.txt

.PHONY:	all
all: version
	$(MAKE) -C $(BUILDDIR) all

.PHONY: version
version:
	./gen_version_h $(COMPDIR)/Version.hs > version.h

# Static check of the header files before releasing them.
# Don't include headers which require systemC.
.PHONY: test-headers
test-headers:
	$(RM) test_hdr.cxx
	for hdr in $(HEADERS); do \
	  if [ $$hdr != "bluesim_systemc.h" ]; then \
	    echo "#include \"$$hdr\"" >> test_hdr.cxx ; \
	  fi ; \
	done
	$(CXX) -std=gnu++98 -c -o /dev/null test_hdr.cxx

.PHONY: install
install: all test-headers
	$(INSTALL) -m 755 -d $(SIMLIB)
	$(INSTALL) -m 644 $(HEADERS) $(SIMLIB)
	$(INSTALL) -m 644 $(LINKFILES) $(SIMLIB)
	$(MAKE) -C $(BUILDDIR) install

.PHONY: clean
clean:
	$(RM) test_hdr.cxx version.h
	$(MAKE) -C $(BUILDDIR) clean

realclean: clean

TAGS:	*.h *.c *.cxx
	etags *.h *.c *.cxx
