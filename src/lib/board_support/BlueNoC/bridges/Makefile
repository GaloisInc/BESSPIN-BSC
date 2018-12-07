PWD:=$(shell pwd)
TOP=$(PWD)/../../../../..

include $(TOP)/platform.mk

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
SUPPORTDIR=$(LIBDIR)/board_support/bluenoc
INSTALLDIR=$(SUPPORTDIR)/bridges

INSTALL?= $(TOP)/util/install

.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 *.bsv $(INSTALLDIR)

.PHONY: clean realclean
clean realclean:

