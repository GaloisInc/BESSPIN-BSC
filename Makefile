
RM = rm -f
LN = ln -sf

TOP=.
include $(TOP)/platform.mk

PWD := $(shell pwd)
PREFIX   ?= $(PWD)/inst
BUILDDIR ?= $(TOP)/build
INSTALL  ?= $(PWD)/util/install

default: rem_inst install-bin

all: rem_inst install

.PHONY:	rem_inst
rem_inst:
	rm -fr $(PREFIX)

.PHONY:	rem_build
rem_build:
	rm -fr $(BUILDDIR)


.PHONY: noopt
noopt:
	make -C . BSC_BUILD=NOOPT default

# -------------------------

install: install-bin install-full

install-bin:
	cd util    && $(MAKE) PREFIX=$(PREFIX)  install
	cd vendor  && $(MAKE) PREFIX=$(PREFIX)  install
	cd src     && $(MAKE) PREFIX=$(PREFIX)  install

#This target is useful if all you want is systemc, for example for ad
#hoc testing a release tarball.  It needs to be called from the top
#level because the vendor Makefile assumes platform.mk has already
#been included.
.PHONY: systemc
systemc:
	$(MAKE) -C vendor systemc

install-full:
	cd util     && $(MAKE) PREFIX=$(PREFIX) release
	cd vendor   && $(MAKE) PREFIX=$(PREFIX) release

# -------------------------

clean:	rem_inst rem_build
	-cd vendor && $(MAKE) clean
	-cd src && $(MAKE) clean

realclean: rem_inst rem_build
	-cd vendor && $(MAKE) realclean
	-cd src && $(MAKE) realclean

# -------------------------

