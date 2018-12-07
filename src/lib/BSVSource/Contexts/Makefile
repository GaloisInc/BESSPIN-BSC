# BSVSource/Context/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BUILDDIR= $(TOP)/build/bsvlib
PREFIX?=$(abspath $(PWD)/../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Prelude)
LDIR= $(abspath $(BUILDDIR)/Libraries)
BSCFLAGS+= -p . -bdir $(BDIR)


.PHONY: clean sanitize realclean build

build: $(LDIR)
	$(BSC) -u $(BSCFLAGS) $(notdir $(CURDIR)).bsv
	cp -p Contexts.defines $(LDIR)

$(LDIR):
	mkdir -p $@

install:
	$(MAKE) -C .. install

clean:
	@rm -f *.bi *.bo *.ba vpi_wrapper* *.info *~

# remove the classic source   ModuleContext
sanitize: clean
	@rm -rf Makefile .svn
	@rm -f *.bs

realclean: clean
	rm -f $(addprefix $(BDIR)/,$(addsuffix .bo,$(basename $(wildcard *.bsv *.bs))))
