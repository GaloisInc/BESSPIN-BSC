# BSVSource/TLM3/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
PREFIX?=$(abspath $(PWD)/../../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)
include ../common.mk

########  BlueNoC ONLY  #######################

BSCFLAGS?= $(BSCFLAGS_EXT)
TLM3= $(abspath $(BUILDDIR)/Libraries/TLM3)
APB= $(abspath $(BUILDDIR)/Libraries/Apb)
AHB= $(abspath $(BUILDDIR)/Libraries/Ahb)
AXI3= $(abspath $(BUILDDIR)/Libraries/Axi)
AXI4= $(abspath $(BUILDDIR)/Libraries/Axi4)
SCEMI= $(abspath $(BUILDDIR)/Libraries/BlueNoC)
BDIR= $(abspath $(BUILDDIR)/Libraries/Xactors)
BSCFLAGS+= -p .:$(BUILDDIR)/Prelude:$(TLM3):$(APB):$(AHB):$(AXI3):$(AXI4):$(SCEMI) -bdir $(BDIR)

SHOW_FILE = \
	XactorsDefines.bsv 



SRC_FILE = $(wildcard *.bsv)
RM_SRC = $(filter-out $(SHOW_FILE),$(SRC_FILE))


.PHONY: clean sanitize realclean build

build: $(BDIR)
	$(BSC) -u $(BSCFLAGS) $(notdir $(CURDIR)).bsv

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
