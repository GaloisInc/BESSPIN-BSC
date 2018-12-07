PWD:=$(shell pwd)
TOP=$(PWD)/../../../../../..

include $(TOP)/platform.mk

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
SUPPORTDIR=$(LIBDIR)/board_support/bluenoc
INSTALLDIR=$(SUPPORTDIR)/altera/verilog

INSTALL?= $(TOP)/util/install

.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 *.v $(INSTALLDIR)
	install -d -m755 $(INSTALLDIR)/ddr2_v12_1
	install -d -m755 $(INSTALLDIR)/ddr2_v12_1/ddr2_v12_1
	install -m644 ddr2_v12_1/*.v $(INSTALLDIR)/ddr2_v12_1
	install -m644 ddr2_v12_1/*.qip $(INSTALLDIR)/ddr2_v12_1
	install -m644 ddr2_v12_1/ddr2_v12_1/*.v $(INSTALLDIR)/ddr2_v12_1/ddr2_v12_1
	install -m644 ddr2_v12_1/ddr2_v12_1/*.sv $(INSTALLDIR)/ddr2_v12_1/ddr2_v12_1
	install -m644 ddr2_v12_1/ddr2_v12_1/*.sdc $(INSTALLDIR)/ddr2_v12_1/ddr2_v12_1
	install -m644 ddr2_v12_1/ddr2_v12_1/*.iv $(INSTALLDIR)/ddr2_v12_1/ddr2_v12_1
	install -m644 ddr2_v12_1/ddr2_v12_1/*.hex $(INSTALLDIR)/ddr2_v12_1/ddr2_v12_1
	install -m644 ddr2_v12_1/ddr2_v12_1/*.tcl $(INSTALLDIR)/ddr2_v12_1/ddr2_v12_1
	install -m644 ddr2_v12_1/ddr2_v12_1/*.ppf $(INSTALLDIR)/ddr2_v12_1/ddr2_v12_1
	install -d -m755 $(INSTALLDIR)/ddr2_v12_1_iv
	install -d -m755 $(INSTALLDIR)/ddr2_v12_1_iv/ddr2_v12_1
	install -m644 ddr2_v12_1_iv/*.v $(INSTALLDIR)/ddr2_v12_1_iv
	install -m644 ddr2_v12_1_iv/*.qip $(INSTALLDIR)/ddr2_v12_1_iv
	install -m644 ddr2_v12_1_iv/ddr2_v12_1/*.v $(INSTALLDIR)/ddr2_v12_1_iv/ddr2_v12_1
	install -m644 ddr2_v12_1_iv/ddr2_v12_1/*.sv $(INSTALLDIR)/ddr2_v12_1_iv/ddr2_v12_1
	install -m644 ddr2_v12_1_iv/ddr2_v12_1/*.sdc $(INSTALLDIR)/ddr2_v12_1_iv/ddr2_v12_1
	install -m644 ddr2_v12_1_iv/ddr2_v12_1/*.iv $(INSTALLDIR)/ddr2_v12_1_iv/ddr2_v12_1
	install -m644 ddr2_v12_1_iv/ddr2_v12_1/*.hex $(INSTALLDIR)/ddr2_v12_1_iv/ddr2_v12_1
	install -m644 ddr2_v12_1_iv/ddr2_v12_1/*.tcl $(INSTALLDIR)/ddr2_v12_1_iv/ddr2_v12_1
	install -m644 ddr2_v12_1_iv/ddr2_v12_1/*.ppf $(INSTALLDIR)/ddr2_v12_1_iv/ddr2_v12_1
	install -d -m755 $(INSTALLDIR)/StratixIV
	install -d -m755 $(INSTALLDIR)/StratixIII
	install -m644 StratixIV/*.v  $(INSTALLDIR)/StratixIV
	install -m644 StratixIII/*.v $(INSTALLDIR)/StratixIII

.PHONY: clean realclean
clean realclean:

