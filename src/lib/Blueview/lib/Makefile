
RM = rm -f 

INSTALL?=install -c

PWD:=$(shell pwd)
PREFIX?=$(PWD)/../../../../inst
BINDIR=$(PREFIX)/bin

LIBDIR=$(PREFIX)/lib

.PHONY: all install clean realclean

all:

install:
	$(INSTALL) -m 755 -d $(LIBDIR)/Blueview
	cd blueview && $(MAKE) install
	cd BWidget-1.7.0 && $(MAKE) install

clean:
	$(RM) *.~
	cd blueview && $(MAKE) clean
	cd BWidget-1.7.0 && $(MAKE) clean

realclean: clean
