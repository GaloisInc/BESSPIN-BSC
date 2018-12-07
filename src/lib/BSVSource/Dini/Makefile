# BSVSource/Dini/Makefile

DIRS = \
	Classic \
	BlueNoC

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
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true

install:
	$(MAKE) -C .. install
clean:
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true
	@rm -f *.bi *.bo *.ba vpi_wrapper* *.info *~ *.v

sanitize: clean
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true
	@rm -rf Makefile .svn

realclean: clean
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true
	rm -f $(addprefix $(BDIR)/,$(addsuffix .bo,$(basename $(wildcard *.bsv *.bs))))
