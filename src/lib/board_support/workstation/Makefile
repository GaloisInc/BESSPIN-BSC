PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

include $(TOP)/platform.mk

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
INSTALLDIR=$(LIBDIR)/board_support/workstation

INSTALL?= $(TOP)/util/install

# Files to release
FILES = \
	project.bspec.template \


.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 $(FILES) $(INSTALLDIR)

.PHONY: clean realclean
clean realclean:

