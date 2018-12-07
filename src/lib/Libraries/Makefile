# Libraries/Makefile
# $Id$

PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

PREFIX?=$(abspath $(TOP)/inst)
LIBDIR=$(abspath $(PREFIX)/lib)

export BLUESPECDIR = $(LIBDIR)

BSC=$(LIBDIR)/bin/bsc
BSCFLAGS= -stdlib-names -no-elab -p . -vsearch "" +RTS -K32M -RTS

# This flags are passed to makes in the BSVSource area.
export BSCFLAGS_EXT= -stdlib-names  +RTS -K32M -RTS

INSTALL?=install -c

BUILDDIR= $(abspath $(TOP)/build/bsvlib)
BDIR= $(abspath $(BUILDDIR)/Prelude)
BSCFLAGS+= -bdir $(BDIR)

PRIVATE_MODS = \
	FlexBitArith.bo \

PUBLIC_MODS = \
	BGetPut.bo \
	BRAMCore.bo \
	BRAM_Compat.bo \
	BitonicSort.bo \
	Boolify.bo \
	CGetPut.bo \
	ClientServer.bo \
	CompletionBuffer.bo \
	DPSRAM.bo \
	DReg.bo \
	EqFunction.bo \
	Esl_defs.bo \
	IVec.bo \
	LFSR.bo \
	ListFIFO.bo \
	ListReg.bo \
	Mcp.bo \
	ModuleAugmented.bo \
	OInt.bo \
	OVLAssertions.bo \
	PopCount.bo \
	Pull.bo \
	Push.bo \
	RAM.bo \
	RPush.bo \
	RegTwo.bo \
	SPSRAM.bo \
	SRAM.bo \
	SRAMFile.bo \
	STRAM.bo \
	StmtFSM.bo \
        ToString.bo \
	TurboFIFO.bo \
	SplitTRAM.bo \
	SVA.bo \
	SyncSRAM.bo \
	TRAM.bo \
	Tabulate.bo \
	TriState.bo \
	UIntRange.bo \
	UniqueWrappers.bo \
	Wallace.bo \
	ZBusUtil.bo \
	ZBus.bo 

MODS = $(addprefix $(BDIR)/,${PUBLIC_MODS} ${PRIVATE_MODS})

SOURCEPREFIX?= ..
SOURCENAME?=   	      BSVSource
SOURCEDIR?=    	      $(SOURCEPREFIX)/$(SOURCENAME)


ifeq ($(NOAZURE), 1)

else

SRC_DIR_BUILD=azure
BLUENOCSHARE?= $(wildcard $(BUILDDIR)/*/BlueNoC/bsvs*.ba $(BUILDDIR)/*/BlueNoC/emu_*.ba $(BUILDDIR)/*/BlueNoC/timer_*.ba $(BUILDDIR)/*/BlueNoC/vpi_wrapper_* $(BUILDDIR)/*/BlueNoC/*.v)
CLASSICSHARE?= $(wildcard $(BUILDDIR)/*/Classic/bsvs*.ba $(BUILDDIR)/*/Classic/emu_*.ba $(BUILDDIR)/*/Classic/timer_*.ba $(BUILDDIR)/*/Classic/vpi_wrapper_* $(BUILDDIR)/*/Classic/*.v)

endif




.SUFFIXES: .bs .bsv .bo

all: build install

#########################################
## Build targets
#########################################
.PHONY: prelude azure

build: prelude $(MODS) $(SRC_DIR_BUILD) header

prelude:
	-rm -f  $(BUILDDIR)/Prelude/std_ovl_defines.h
	make -C ../Prelude build

azure: | prelude $(MODS)
	make -C $(SOURCEDIR) BSC=$(BSC) build

header:
	cp std_ovl_defines.h $(BUILDDIR)/Prelude

$(MODS): | prelude


#########################################
## Install
#########################################
.PHONY: install install-bo install-src
install: build install-bo install-src
reinstall: install-bo install-src

install-bo: | build
	rm -rf $(LIBDIR)/Prelude
	rm -rf $(LIBDIR)/Libraries
	install -d $(LIBDIR)/Prelude  $(LIBDIR)/Libraries
	install -m644 $(BUILDDIR)/Prelude/*.bo $(LIBDIR)/Prelude/
	install -m644 $(BUILDDIR)/Prelude/*.ba $(LIBDIR)/Libraries
ifeq ($(NOAZURE), 1)
else
	cp -r $(BUILDDIR)/Libraries $(LIBDIR)/
	$(INSTALL) -m 644 $(BLUENOCSHARE)   $(LIBDIR)/Libraries
	$(INSTALL) -m 644 $(CLASSICSHARE)   $(LIBDIR)/Libraries
endif


install-src:
	rm -rf $(LIBDIR)/$(SOURCENAME)
	cp -r $(SOURCEDIR) $(LIBDIR)
	-$(MAKE) -C $(LIBDIR)/$(SOURCENAME) sanitize

#########################################
## clean
#########################################
clean:
	$(MAKE) -C $(SOURCEDIR) clean

realclean:
	rm -f *.bo *.ba *.v
	rm -fr $(BUILDDIR)
	$(MAKE) -C $(SOURCEDIR) $@


#########################################
# rules for building bo files
#########################################

#.bs.bo:
$(BDIR)/%.bo: %.bsv
	${BSC} ${BSCFLAGS} $<

#.bsv.bo:
$(BDIR)/%.bo: %.bs
	${BSC} ${BSCFLAGS} $<


#########################################
# remake the dependency file,  require bluetcl and more to boot strap
# This is commited to SVN
#########################################
.PHONEY: depends
depends:
	bluetcl -exec makedepend -bdir "$(BDIR)" "*.bs*" | sed -e 's|$(BDIR)|$$(BDIR)|g' > depends.mk


sinclude depends.mk

