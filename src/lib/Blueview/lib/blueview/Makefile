
RM = rm -f 

INSTALL?=install -c

PWD:=$(shell pwd)
PREFIX?=$(PWD)/../../../../../inst
BINDIR=$(PREFIX)/bin

LIBDIR=$(PREFIX)/lib
BSLIBDIR=$(LIBDIR)/


.PHONY: all install install-tar clean realclean

all:

install:
	$(INSTALL) -m 755 -d $(BSLIBDIR)/Blueview
	$(INSTALL) -m 755 -d $(BSLIBDIR)/Blueview/lib
	$(INSTALL) -m 755 -d $(BSLIBDIR)/Blueview/lib/blueview
	@for i in *.tcl  ; do \
	    $(INSTALL) -m 444  $$i $(BSLIBDIR)/Blueview/lib/blueview ; \
	done;


clean:
	$(RM) *.~

realclean: clean
