# BSCSource/SceMi/BlueNoC/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../../..

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
PREFIX?=$(abspath $(PWD)/../../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Libraries/BlueNoC)
VDIR= $(abspath $(BUILDDIR)/Libraries)
BSCFLAGS+= -p .:$(BUILDDIR)/Prelude:.. -bdir $(BDIR) -vdir $(VDIR)

# Files to release publically
SHOW_FILE = \
	SceMiDefines.bsv \
	SceMiXactors.bsv


SRC_FILE = $(wildcard *.bsv)
RM_SRC = $(filter-out $(SHOW_FILE),$(SRC_FILE))


.PHONY: clean sanitize realclean build

build: *.bsv $(BDIR)
	$(BSC) -u -verilog $(BSCFLAGS) SceMi.bsv
	$(BSC) -u -sim     $(BSCFLAGS) SceMi.bsv

$(BDIR):
	mkdir -p $@

install:
	$(MAKE) -C .. install

clean:
	@rm -f *.bi *.bo *.ba *.v vpi_wrapper* *.info *~

sanitize: clean
	rm -rf Makefile .svn $(RM_SRC)

realclean: clean
	rm -f $(addprefix $(BDIR)/,$(addsuffix .bo,$(basename $(notdir $(wildcard *.bsv *.bs ../*.bsv)))))
