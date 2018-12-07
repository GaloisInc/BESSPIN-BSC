#
# $Id: Makefile 34176 2014-11-03 04:38:46Z todd $
#

PWD:=$(shell pwd)
TOP:=$(PWD)/../..

# INSTALL ?= $(TOP)/util/install

PREFIX?=$(TOP)/inst
LIBDIR=$(PREFIX)/lib

export BLUESPECDIR = $(LIBDIR)

INSTALL?=install

.PHONY: all install clean realclean

all: install

install:
	cd Verilog && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd Verilog.Vivado && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd Verilog.Quartus && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd Prelude && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd Libraries && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR)  install
	cd exec && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd VPI && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd board_support && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd Readback && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd BlueNoC && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd SceMi && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install
	cd tcllib && \
	  $(MAKE) PREFIX=$(PREFIX) LIBDIR=$(LIBDIR) install

clean:
	cd Prelude && $(MAKE) clean
	cd Libraries && $(MAKE) clean
	cd Verilog && $(MAKE) clean
	cd Verilog.Vivado && $(MAKE) clean
	cd Verilog.Quartus && $(MAKE) clean
	cd exec && $(MAKE) clean
	cd VPI && $(MAKE) clean
	$(MAKE) -C tcllib $@
	$(MAKE) -C board_support $@
	$(MAKE) -C Readback $@
	$(MAKE) -C BlueNoC $@
	$(MAKE) -C SceMi $@

realclean:
	cd Prelude && $(MAKE) realclean
	cd Libraries && $(MAKE) realclean
	cd Verilog && $(MAKE) realclean
	cd Verilog.Vivado && $(MAKE) realclean
	cd Verilog.Quartus && $(MAKE) realclean
	cd exec && $(MAKE) clean
	cd VPI && $(MAKE) realclean
	$(MAKE) -C tcllib $@
	$(MAKE) -C board_support $@
	$(MAKE) -C Readback $@
	$(MAKE) -C BlueNoC $@
	$(MAKE) -C SceMi $@
