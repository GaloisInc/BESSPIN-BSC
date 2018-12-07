# BSVSource/Contrib/TLM/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../../..

BUILDDIR= $(TOP)/build/bsvlib
PREFIX?=$(abspath $(PWD)/../../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Libraries/Contrib/TLM)

BSCFLAGS+= -p .:$(BUILDDIR)/Prelude -bdir $(BDIR)



.PHONY: clean sanitize realclean build


build: $(BDIR)
	-rm -f $(BDIR)/TLM.defines
	$(BSC) -u $(BSCFLAGS) TLM.bsv
	$(BSC) -u $(BSCFLAGS) TLM2.bsv
	cp -p TLM.defines $(BDIR)

$(BDIR):
	mkdir -p $@

install:
	$(MAKE) -C .. install

clean:
	@rm -f *.bi *.bo *.ba *.v vpi_wrapper* *.info *~

sanitize:
	rm -rf Makefile .svn doc

realclean: clean
	rm -rf $(BDIR)
