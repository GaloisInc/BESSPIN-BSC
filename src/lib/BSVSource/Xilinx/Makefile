# BSVSource/Xilinx/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BUILDDIR= $(TOP)/build/bsvlib
PREFIX?=$(abspath $(PWD)/../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Prelude)
VDIR= $(abspath $(BUILDDIR)/Libraries)
BSCFLAGS+= -p . -bdir $(BDIR) -vdir $(VDIR)


.PHONY: clean sanitize realclean build

build:
	$(BSC) -u -verilog $(BSCFLAGS) $(notdir $(CURDIR)).bsv
	$(BSC) -u -sim     $(BSCFLAGS) $(notdir $(CURDIR)).bsv

install:
	$(MAKE) -C .. install


clean:
	@rm -f *.bi *.bo *.ba vpi_wrapper* *.info *~ mk*.v

sanitize: clean
	@rm -rf Makefile .svn

realclean: clean
	rm -f $(addprefix $(BDIR)/,$(addsuffix .bo,$(basename $(wildcard *.bsv *.bs))))
