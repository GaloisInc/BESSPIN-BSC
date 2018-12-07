PWD:=$(shell pwd)
TOP:=$(PWD)/../../../

RM = rm -f 

INSTALL?=install -c

PREFIX?=$(TOP)/inst
BINDIR=$(PREFIX)/bin

LIBDIR=$(PREFIX)/lib

.PHONY: all install install-tar clean realclean

all:

install:
	cd lib && $(MAKE) install 
	$(INSTALL) -m 755 -d $(BINDIR)	
	$(INSTALL) -m 555 blueview.tcl $(BINDIR)/blueview
clean:
	cd lib && $(MAKE) clean
	$(RM) *.~

realclean: clean
