# BSVSource/Contrib/Axi/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../../..

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
PREFIX?=$(abspath $(PWD)/../../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
TLM= $(abspath $(BUILDDIR)/Libraries/Contrib/TLM)
BDIR= $(abspath $(BUILDDIR)/Libraries/Contrib/Axi)

BSCFLAGS+= -p .:$(BUILDDIR)/Prelude:$(TLM) -bdir $(BDIR)


.PHONY: clean sanitize realclean build

build: $(BDIR)
	-rm -f $(BDIR)/Axi.defines
	$(BSC) -u $(BSCFLAGS) $(notdir $(CURDIR)).bsv
	cp -p Axi.defines $(BDIR)

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
