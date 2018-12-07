# Prelude/Makefile

PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

include $(TOP)/platform.mk

PREFIX?=$(TOP)/inst
LIBDIR=$(PREFIX)/lib

CXXFAMILY=$(shell $(TOP)/util/scripts/bsenv c++_family)
SATDIR=$(LIBDIR)/SAT/$(CXXFAMILY)

export BLUESPECDIR = $(LIBDIR)
export LD_LIBRARY_PATH := $(BLUESPEC_LD_LIBRARY_PATH):$(LD_LIBRARY_PATH):$(SATDIR)
export DYLD_LIBRARY_PATH := $(BLUESPEC_LD_LIBRARY_PATH):$(DYLD_LIBRARY_PATH):$(SATDIR)

# By depending the Prelude on BSC from "src/comp" rather than from "inst",
# we avoid recompiling the libraries every time the "inst" area is populated
# and only when BSC has actually changed.
#
BSC=$(TOP)/src/comp/bsc
BSCFLAGS=

INSTALL?=install -c

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
BDIR= $(BUILDDIR)/Prelude

BSCFLAGS+=  -stdlib-names -p . -bdir $(BDIR)  -vsearch $(LIBDIR)


BOFILES = \
	ActionSeq.bo \
	Assert.bo \
	Clocks.bo \
	ConfigReg.bo \
	Connectable.bo \
	Counter.bo \
	Enum.bo \
	Environment.bo \
	FIFO.bo \
	FIFOF.bo \
	FIFOF_.bo \
	FIFOLevel.bo \
	FoldFIFO.bo \
	FoldFIFOF.bo \
	Fork.bo \
	FShow.bo \
	GetPut.bo \
	Inout.bo \
	List.bo \
	ListN.bo \
	RevertingVirtualReg.bo \
	LevelFIFO.bo \
	Once.bo \
	Probe.bo \
	ProbeWire.bo \
	Array.bo \
	Real.bo \
	RegFile.bo \
	Reserved.bo \
	Vector.bo \
	RWire.bo


MODS = $(addprefix $(BDIR)/,$(BOFILES))

.SUFFIXES: .bs .bsv .bo
vpath %.bo $(BDIR)

.PHONY: all clean realclean clean-tix

all: build

build:	$(BDIR) $(BDIR)/Prelude.bo $(BDIR)/PreludeBSV.bo ${MODS}

install: build
	$(INSTALL) -m 755 -d $(LIBDIR)/Prelude
	$(INSTALL) -m644 $(BUILDDIR)/Prelude/*.bo $(LIBDIR)/Prelude/


clean: clean-tix
	rm -f *.bo

clean-tix:
	rm -f *.tix

realclean: clean
	rm -fr $(BDIR)

$(BDIR)/Prelude.bo:	Prelude.bs ${BSC}
	${BSC} ${BSCFLAGS} -no-use-prelude -stdlib-names Prelude.bs
$(BDIR)/PreludeBSV.bo:	PreludeBSV.bsv ${BSC}
	${BSC} ${BSCFLAGS} -no-use-prelude -stdlib-names PreludeBSV.bsv

# .bs.bo:
$(BDIR)/%.bo: %.bs
	${BSC} ${BSCFLAGS} $<

# .bsv.bo:
$(BDIR)/%.bo: %.bsv
	${BSC} ${BSCFLAGS} $<

$(BDIR):
	mkdir -p $@

# remake the dependency file,  require bluetcl and more to boot strap
.PHONEY: depends
depends:
	bluetcl -exec makedepend -bdir "$(BDIR)" "*.bs*" | sed -e 's|$(BDIR)|$$(BDIR)|g' > depends.mk


sinclude depends.mk
