#
# $Id$
#

PWD:=$(shell pwd)
PREFIX?=$(PWD)/../inst
RELEASEDIR?=$(PREFIX)/util

INSTALL?=install -c

INSTALL_FILES = \
	convert_bsc30.sh


.PHONY: all 
.PHONY: emacs

all: emacs

emacs:
	cd emacs && $(MAKE)

.PHONY: install
install:
	$(INSTALL) -m 755 -d $(RELEASEDIR)
	cd scripts  && $(MAKE) PREFIX=$(PREFIX) install
	cd emacs    && $(MAKE) PREFIX=$(RELEASEDIR) install

# release is the target for code distribution
.PHONY: release
release: install
	cd emacs    && $(MAKE) PREFIX=$(RELEASEDIR) install
	cd jedit    && $(MAKE) PREFIX=$(RELEASEDIR) install
	cd vim      && $(MAKE) PREFIX=$(RELEASEDIR) install
	cd enscript && $(MAKE) PREFIX=$(RELEASEDIR) install
	cd bsim_standalone && $(MAKE) PREFIX=$(RELEASEDIR) install
#	cd blueview && $(MAKE) PREFIX=$(RELEASEDIR) install
	## script go into the main level bin
	cd scripts  && $(MAKE) PREFIX=$(PREFIX) release

clean:
	cd emacs    && $(MAKE) clean
#	cd blueview && $(MAKE) clean

realclean:
	cd emacs && $(MAKE) realclean
#	cd blueview && $(MAKE) realclean
