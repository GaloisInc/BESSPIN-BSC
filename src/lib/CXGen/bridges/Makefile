PWD:=$(shell pwd)
TOP=$(PWD)/../../../..

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
SUPPORTDIR=$(LIBDIR)/board_support/bluenoc
INSTALLDIR=$(SUPPORTDIR)/bridges

.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 *.defines $(INSTALLDIR)

.PHONY: clean realclean
clean realclean:
