PREFIX ?= $(PWD)/../../../../inst
LIBDIR ?= $(PREFIX)/lib/

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BLUETCL_EXEC?=$(LIBDIR)/bin/bluetcl
BDIR=$(LIBDIR)
export BLUESPECDIR = $(LIBDIR)

EXEFILES = \
	   dbg-gui \

TCLFILES = \
	ignore.tcl \

PACKAGES =

FILES = $(TCLFILES) $(PACKAGES) tclIndex pkgIndex.tcl

.PHONY: all
all: install

tclIndex: *.tcl
	TCLSH=$(BLUETCL_EXEC) BLUESPECDIR=$(LIBDIR) ./tclIndex.sh "$(TCLFILES)" "$(PACKAGES)"

install: tclIndex 
	install -d -m755 $(LIBDIR)/tcllib/semu
	install -m644 $(FILES) $(LIBDIR)/tcllib/semu
	install -m755 $(EXEFILES) $(LIBDIR)/tcllib/semu
	install -d -m755 $(LIBDIR)/bin
	install -m755 semulite $(LIBDIR)/bin/lumina
	install -m755 semulite_scemi $(LIBDIR)/bin/lumina_scemi
	install -m755 semulite_socket $(LIBDIR)/bin/lumina_socket
	install -m755 luminaplus $(LIBDIR)/bin/luminaplus

.PHONY: clean realclean
clean realclean:
	@rm -rf tclIndex pkgIndex.tcl 
