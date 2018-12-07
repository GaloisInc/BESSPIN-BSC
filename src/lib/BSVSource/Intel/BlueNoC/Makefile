# BSCSource/Intel/BlueNoC/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../../..

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
PREFIX?=$(abspath $(PWD)/../../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../../common.mk

BSCFLAGS?= $(BSCFLAGS_EXT)
BDIR= $(abspath $(BUILDDIR)/Libraries/BlueNoC)
VDIR= $(abspath $(BUILDDIR)/Libraries)
BSCFLAGS+= -p .:$(BUILDDIR)/Prelude:.. -bdir $(BDIR) -vdir $(VDIR)

# Files to release publically
SHOW_FILES =

.PHONY: clean sanitize realclean build

build: *.bsv $(BDIR)
	$(BSC) -u -verilog $(BSCFLAGS) Intel.bsv
	$(BSC) -u -sim     $(BSCFLAGS) Intel.bsv

$(BDIR):
	mkdir -p $@

install:
	$(MAKE) -C .. install

clean:
	@rm -f *.bi *.bo *.ba vpi_wrapper* *.info *~ *.v

sanitize: clean
	@rm -rf Makefile .svn

realclean: clean
	rm -f $(addprefix $(BDIR)/,$(addsuffix .bo,$(basename $(notdir $(wildcard *.bsv *.bs ../*.bsv)))))
