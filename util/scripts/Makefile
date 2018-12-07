PWD:=$(shell pwd)
PREFIX ?= $(PWD)/../../inst

TOP ?= $(PWD)/../../
include $(TOP)/platform.mk

INSTALL ?= install

LIBDIR=$(PREFIX)/lib
BINDIR=$(PREFIX)/bin
EXEDIR=$(LIBDIR)/bin

PLATDIR=$(EXEDIR)/$(KERNELNAME)$(W32or64)

CC	= gcc

.PHONY: all install release clean realclean

all:

$(BINDIR):
	$(INSTALL) -m 755 -d $(BINDIR)

$(LIBDIR):
	$(INSTALL) -m 755 -d $(LIBDIR)

$(EXEDIR): $(LIBDIR)
	$(INSTALL) -m 755 -d $(EXEDIR)

$(PLATDIR): $(EXECDIR)
	$(INSTALL) -m 755 -d $(PLATDIR)

enum.exe: enum.c
	$(CC) enum.c -o enum.exe

install: $(EXEDIR) $(BINDIR) $(PLATDIR) enum.exe
	$(INSTALL) -m 555 bsenv              $(EXEDIR)/bsenv
	$(INSTALL) -m 555 fixvcd.pl          $(EXEDIR)/fixvcd
	$(INSTALL) -m 555 edit_for_elab.pl   $(EXEDIR)/edit_for_elab
	$(INSTALL) -m 555 v2vhdl-wrapper.pl  $(EXEDIR)/v2vhdl-wrapper
	$(INSTALL) -m 555 basicinout.pl      $(EXEDIR)/basicinout
	$(INSTALL) -m 555 blackbox.pl        $(EXEDIR)/blackbox
	$(INSTALL) -m 555 cclock_view.pl     $(EXEDIR)/cclock_view
	$(INSTALL) -m 555 tarmacgen.pl       $(EXEDIR)/tarmacgen
	$(INSTALL) -m 555 build              $(EXEDIR)/build
	$(INSTALL) -m 555 bluenoc_hotswap    $(BINDIR)/bluenoc_hotswap
	$(INSTALL) -m 555 bluenoc_hotswap    $(EXEDIR)/bluenoc_hotswap
	$(INSTALL) -m 555 btags              $(EXEDIR)/
	$(INSTALL) -m 555 build-wrapper      $(BINDIR)/build
	$(INSTALL) -m 555 enum               $(EXEDIR)/enum
	$(INSTALL) -m 555 enum-wrapper       $(BINDIR)/enum
	$(INSTALL) -m 755 enum.exe           $(PLATDIR)/enum
	$(MAKE) -C install

clean:
	$(RM) -f enum.exe

realclean: clean
