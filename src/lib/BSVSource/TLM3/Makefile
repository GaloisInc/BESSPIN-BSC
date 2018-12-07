# BSVSource/TLM3/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
PREFIX?=$(abspath $(PWD)/../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Libraries/TLM3)
BSCFLAGS+= -p .:$(BUILDDIR)/Prelude -bdir $(BDIR)

SHOW_FILE = \
	TLM3Defines.bsv



SRC_FILE = $(wildcard *.bsv)
RM_SRC = $(filter-out $(SHOW_FILE),$(SRC_FILE))
RM_SRC += ram_init.text SYN.defines


.PHONY: clean sanitize realclean build

build: $(BDIR)
	-rm -f $(BDIR)/TLM.defines
	$(BSC) -u $(BSCFLAGS) $(notdir $(CURDIR)).bsv
	cp -p TLM.defines $(BDIR)
	cp -p SYN.defines $(BDIR)

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
	rm -f $(addprefix $(BDIR)/, TLM.defines)
