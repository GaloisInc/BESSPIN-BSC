# BSVSource/Xilinix/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BUILDDIR= $(TOP)/build/bsvlib
PREFIX?=$(abspath $(PWD)/../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Prelude)
BSCFLAGS+= -p . -bdir $(BDIR)


.PHONY: clean sanitize realclean build

build:
	$(BSC) -u $(BSCFLAGS) $(notdir $(CURDIR)).bsv

install:
	$(MAKE) -C .. install

clean:
	@rm -f *.bi *.bo *.ba *.v vpi_wrapper* *.info *~

sanitize: clean
	@rm -rf Makefile .svn

realclean: clean
	rm -f $(addprefix $(BDIR)/,$(addsuffix .bo,$(basename $(wildcard *.bsv *.bs))))
