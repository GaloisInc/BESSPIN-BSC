PWD:=$(shell pwd)
TOP=$(PWD)/../../../../..

include $(TOP)/platform.mk

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
BINDIR=$(PREFIX)/bin

SUPPORTDIR=$(LIBDIR)/board_support/bluenoc
INSTALLDIR=$(SUPPORTDIR)/bluenoc
BSCDIR=$(LIBDIR)/bin/$(KERNELNAME)$(W32or64)

INSTALL?= $(TOP)/util/install

.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 bluenoc.c clocking.h clocking.cpp $(INSTALLDIR)
	install -m644 bluenoc.mk $(INSTALLDIR)/Makefile
ifneq ($(OSTYPE), Darwin)
	make -f bluenoc.mk bluenoc
	install -d $(BSCDIR)
	install -m 755 bluenoc $(BSCDIR)
	install -d $(BINDIR)
	install -m 755 bluenoc-wrapper $(BINDIR)/bluenoc
endif

.PHONY: clean realclean
clean realclean:
	make -f bluenoc.mk $@

