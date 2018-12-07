PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
INSTALLDIR=$(LIBDIR)/Libraries/Xactors

# ---------------

# Build using the files installed at $(PREFIX)
export BLUESPECDIR = $(LIBDIR)
export BSC = $(LIBDIR)/bin/bsc

# Expect to find the following libraries installed at $(PREFIX)
PRELUDE= $(LIBDIR)/Prelude
TLM3= $(LIBDIR)/Libraries/TLM3
APB= $(LIBDIR)/Libraries/Apb
AHB= $(LIBDIR)/Libraries/Ahb
AXI3= $(LIBDIR)/Libraries/Axi
AXI4= $(LIBDIR)/Libraries/Axi4
SCEMI= $(LIBDIR)/Libraries/BlueNoC
XACT= $(LIBDIR)/Libraries/Xactors

# Put generated files in a local work directory
BDIR= bdir

BSCFLAGS = -stdlib-names +RTS -K32M -RTS
BSCFLAGS+= -p .:$(PRELUDE):$(TLM3):$(APB):$(AHB):$(AXI3):$(AXI4):$(SCEMI):$(XACT) -bdir $(BDIR)

.PHONY: build
build: $(BDIR)
	$(BSC) -u $(BSCFLAGS) CXactors.bsv

$(BDIR):
	mkdir -p $@

.PHONY: install
install: build
	rm -rf $(LIBDIR)/Libraries/CXactors
	install -d -m755 $(LIBDIR)/Libraries/CXactors
	install -m644 $(BDIR)/*.bo $(INSTALLDIR)

.PHONY: clean
clean:

.PHONY: realclean
realclean: clean
	rm -rf $(BDIR)
