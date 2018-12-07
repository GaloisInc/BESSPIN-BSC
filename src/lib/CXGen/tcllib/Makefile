PWD:=$(shell pwd)
TOP=$(PWD)/../../../..

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
INSTALLDIR=$(LIBDIR)/tcllib/bluespec

define ADDPACKAGE
	if ! grep " $(1) " $(INSTALLDIR)/pkgIndex.tcl > /dev/null ; then \
		echo "Adding $(1) to $(INSTALLDIR)/pkgIndex.tcl" ; \
		echo 'package ifneeded $(1) 1.1 [list source [file join $$dir $(1).tcl]]' >> $(INSTALLDIR)/pkgIndex.tcl ; \
	 fi
endef

PKG_FILES = \
	CXGen.tcl \
	CXFields.tcl \
	CXactors.tcl \

EXE_FILES = \
	CXGen_adjust_top.tcl \
	CXGen_blackbox.tcl \

.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 $(PKG_FILES) $(INSTALLDIR)
	install -m755 $(EXE_FILES) $(INSTALLDIR)
	@$(foreach pkg, $(PKG_FILES), $(call ADDPACKAGE,$(basename $(pkg)));)

.PHONY: clean realclean
clean realclean:
