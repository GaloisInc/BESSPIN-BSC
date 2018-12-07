##   Makefile for tcl library
##  Copies the tcl libraries from vendor to the inst dir

VENDOR = ../../../vendor/

TCLDIR = $(VENDOR)/tcltk/


# Location where files are installed
PWD:=$(shell pwd)
PREFIX?=$(PWD)/../../../inst
LIBDIR=$(PREFIX)/lib

INSTALL ?= install

.PHONY: all
all: install


.PHONY: install
install: stdlibs bluelibs

.PHONY: bluelibs
bluelibs: stdlibs
	install -d -m755 $(LIBDIR)/tcllib/
	make -C bluespec LIBDIR=$(LIBDIR)/ install
	make -C workstation LIBDIR=$(LIBDIR)/ install
	make -C emacs LIBDIR=$(LIBDIR)/ install
	make -C gvim LIBDIR=$(LIBDIR)/ install
	make -C tk LIBDIR=$(LIBDIR)/ install
	make -C include LIBDIR=$(LIBDIR)/ install
	make -C scemi LIBDIR=$(LIBDIR)/ install
	make -C novas LIBDIR=$(LIBDIR)/ install
	make -C simtb LIBDIR=$(LIBDIR)/ install
	make -C semu LIBDIR=$(LIBDIR)/ install

.PHONY: stdlibs
stdlibs:
	install -d -m755 $(LIBDIR)/tcllib
	make -C $(TCLDIR)/ LIBDIR=$(LIBDIR)/tcllib/ install


TAGS: */*.tcl
	etags --language none \
		--regex="/[ \t]*\(private\|protected\|public\)?[ \t]*method[ \t]+\([^ \t]+\)/\2/" \
		--regex="/[ \t]*proc[ \t]+\([^ \t]+\)/\1/" \
		$+

$+

.PHONY: clean realclean
realclean clean:
	rm -f TAGS
	make -C bluespec $@
	make -C workstation $@
	make -C tk $@
	make -C scemi $@
	make -C novas $@
	make -C semu  $@
	make -C simtb $@
