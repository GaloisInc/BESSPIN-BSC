PWD:=$(shell pwd)
TOP=$(PWD)/../../../../..

include $(TOP)/platform.mk

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
SUPPORTDIR=$(LIBDIR)/board_support/bluenoc
INSTALLDIR=$(SUPPORTDIR)/drivers

INSTALL?= $(TOP)/util/install

VERSCRIPT=./kernel-module-version-set.pl

.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 drivers.mk $(INSTALLDIR)/Makefile
# BLUESPEC_VERSION gets set by the release script.
# Note well that the variable name must BLUESPEC_VERSION and not VERSION or else it
# interferes with vendor/cudd/Makefile.
	set -x ; if [ -z $(BLUESPEC_VERSION) ]	; \
	  then install -m644 bluenoc.c $(INSTALLDIR) ; \
	  else perl $(VERSCRIPT) $(BLUESPEC_VERSION) < bluenoc.c > \
	    $(INSTALLDIR)/bluenoc.c && \
	    chmod 644 $(INSTALLDIR)/bluenoc.c ; \
	fi
	install -m644 bluenoc.h $(INSTALLDIR)
	install -m644 99-bluespec.rules $(INSTALLDIR)

.PHONY: clean realclean
clean realclean:

