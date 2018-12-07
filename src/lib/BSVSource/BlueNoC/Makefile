# BSVSource/BlueNoC/Makefile

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


SHOW_FILE = \
	BlueNoC.bsv \
	BypassReg.bsv \
	ByteBuffer.bsv \
	LRU.bsv \
	MsgFormat.bsv \



SRC_FILE = $(wildcard *.bsv)
RM_SRC = $(filter-out $(SHOW_FILE),$(SRC_FILE))

.PHONY: clean sanitize realclean

build: $(BDIR)
	$(BSC) -u -verilog $(BSCFLAGS) $(notdir $(CURDIR)).bsv
	$(BSC) -u -sim $(BSCFLAGS) $(notdir $(CURDIR)).bsv

$(BDIR):
	mkdir -p $@

install:
	$(MAKE) -C .. install

clean:
	@rm -f *.bi *.bo *.ba vpi_wrapper* *.info *~ mk*.v

sanitize: clean
	@rm -rf Makefile .svn $(RM_SRC)

realclean: clean
	rm -f $(addprefix $(BDIR)/,$(addsuffix .bo,$(basename $(wildcard *.bsv *.bs))))
