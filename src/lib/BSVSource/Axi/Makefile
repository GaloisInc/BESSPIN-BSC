# BSVSource/Axi/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
PREFIX?=$(abspath $(PWD)/../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
TLM3= $(abspath $(BUILDDIR)/Libraries/TLM3)
BDIR= $(abspath $(BUILDDIR)/Libraries/Axi)
BSCFLAGS+= -p .:$(BUILDDIR)/Prelude:$(TLM3) -bdir $(BDIR)

# Files to release publically
SHOW_FILE = \




SRC_FILE = $(wildcard *.bsv)
RM_SRC = $(filter-out $(SHOW_FILE),$(SRC_FILE))


.PHONY: clean sanitize realclean build

build: *.bsv $(BDIR)
	$(BSC) -u $(BSCFLAGS) $(notdir $(CURDIR)).bsv
	cp -p Axi.defines $(BDIR)

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
