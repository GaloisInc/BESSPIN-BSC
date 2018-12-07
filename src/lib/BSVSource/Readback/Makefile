# BSCSource/ReadBack/Makefile


# Note:   The Readback package only is used with BlueNoC

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
PREFIX?=$(abspath $(PWD)/../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Libraries/BlueNoC)
VDIR= $(abspath $(BUILDDIR)/Libraries)
BSCFLAGS+= -p .:$(BUILDDIR)/Prelude -bdir $(BDIR) -vdir $(VDIR)

# Files to release publically
SHOW_FILE =


SRC_FILE = $(wildcard *.bsv)
RM_SRC = $(filter-out $(SHOW_FILE),$(SRC_FILE))


.PHONY: clean sanitize realclean build

build: $(BDIR)
	$(BSC) -u -verilog $(BSCFLAGS)  $(notdir $(CURDIR)).bsv
	$(BSC) -u -sim     $(BSCFLAGS)  $(notdir $(CURDIR)).bsv
	cp -p Readback.defines $(VDIR)

$(BDIR):
	mkdir -p $@

install:
	$(MAKE) -C .. install
clean:
	@rm -f *.bi *.bo *.ba *.v vpi_wrapper* *.info *~

sanitize: clean
	@rm -rf Makefile .svn $(RM_SRC)

realclean: clean
	rm -f $(addprefix $(BDIR)/,$(addsuffix .bo,$(basename $(wildcard *.bsv *.bs))))
