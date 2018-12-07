# BSVSource/Contrib/NonPipelinedMath/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../../..

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
PREFIX?=$(abspath $(PWD)/../../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Libraries/Contrib/NonPipelinedMath)

BSCFLAGS+= -p .:$(BUILDDIR)/Prelude -bdir $(BDIR)


.PHONY: clean sanitize realclean build

build: $(BDIR)
	$(BSC) -u $(BSCFLAGS) $(notdir $(CURDIR)).bsv

$(BDIR):
	mkdir -p $@

install:
	$(MAKE) -C .. install

clean:
	@rm -f *.bi *.bo *.ba *.v vpi_wrapper* *.info *~

sanitize:
	@rm -rf Makefile .svn doc

realclean: clean
	rm -rf $(BDIR)
