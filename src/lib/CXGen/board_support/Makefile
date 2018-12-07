PWD:=$(shell pwd)
TOP=$(PWD)/../../../..

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
INSTALLDIR=$(LIBDIR)/board_support/bluenoc

.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)/dini/DNVUF4A
	install -m644 ptm_bridge_dnvuf4a.xdc $(INSTALLDIR)/dini/DNVUF4A/ptm_bridge.xdc

.PHONY: clean realclean
clean realclean:
