PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

include $(TOP)/platform.mk

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
INSTALLDIR=$(LIBDIR)/board_support/scripts

INSTALL?= $(TOP)/util/install

# Files to release
FILES = \
	altera_timing \
	Generate_BSV_wrappers.py \
	Generate_TestBench.py \
	Generate_TBTemplate.py \
	Generate_SimTBTemplate.py \
	ParsePin.py \
	prepare_pa_project \
	prepare_xst_project \
	xilinx_timing \


.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m755 $(FILES) $(INSTALLDIR)

.PHONY: clean realclean
clean realclean:

