#

PWD:=$(shell pwd)
TOP:=$(PWD)/../
export PREFIX?=$(TOP)/inst

include $(TOP)/platform.mk

RM = rm -f
LN = ln -sf

#systemc is required to run the testsuite, but not needed if all
#you want to do is build the compiler
ifndef NOSYSTEMC
BUILD_SYSTEMC=systemc
endif

all: install

.PHONY: install
install: cudd yices stp flexlm tcltk $(BUILD_SYSTEMC) verific libftdi

.PHONY: release
release: flexlm
	cd flexlm && $(MAKE) release

.PHONY: cudd
cudd:
	cd cudd && $(MAKE)

.PHONY: yices
yices:
	cd yices && $(MAKE)

.PHONY: stp
stp:
	cd stp && $(MAKE)

.PHONY: flexlm
flexlm:
	cd flexlm && $(MAKE)

.PHONY: tcltk
tcltk:
	$(MAKE) -C tcltk

# usb-driver contains no license information, so it is fully copyrighted by default
# (Hague Convention), meaning Bluespec cannot redistribute it.  So do not compile and
# install it by default.
.PHONY: usb-driver
usb-driver:
ifneq ($(OSTYPE), Darwin)
	$(MAKE) -C $@ -f Makefile.bluespec
endif

# systemc requires OSTYPE and MACHTYPE to be set according to ../platform.mk, 
# not the standard values
.PHONY: systemc
systemc:
	$(MAKE) -C systemc
# The following might be useful when debugging tinderbox
#	$(MAKE) -C systemc > systemc.log 2>&1

.PHONY: boost
boost:
	$(MAKE) -C $@

.PHONY: verific
verific:
	$(MAKE) -C $@

.PHONY: iverilog
iverilog:
	$(MAKE) -C $@

.PHONY: libftdi
libftdi:
	$(MAKE) -C $@

.PHONY: clean
clean:
	-cd lib && $(MAKE) clean
	-cd cudd && $(MAKE) clean
	-cd Parsec && $(MAKE) clean
	-cd sha1 && $(MAKE) clean
	-cd flexlm && $(MAKE) clean
	-cd yices && $(MAKE) clean
	-cd stp && $(MAKE) clean
	-$(MAKE) -C tcltk $@
	-$(MAKE) -C systemc $@
	-$(MAKE) -C verific $@
	-$(MAKE) -C boost $@
	-$(MAKE) -C iverilog $@
	-$(MAKE) -C usb-driver -f Makefile.bluespec $@
	-$(MAKE) -C libftdi $@

.PHONY: realclean
realclean:
	-cd lib && $(MAKE) realclean
	-cd cudd && $(MAKE) realclean
	-cd Parsec && $(MAKE) realclean
	-cd sha1 && $(MAKE) realclean
	-cd flexlm && $(MAKE) realclean
	-cd yices && $(MAKE) realclean
	-cd stp && $(MAKE) realclean
	-$(MAKE) -C tcltk $@
	-$(MAKE) -C systemc $@
	-$(MAKE) -C verific $@
	-$(MAKE) -C boost $@
	-$(MAKE) -C iverilog $@
	-$(MAKE) -C usb-driver -f Makefile.bluespec $@
	-$(MAKE) -C libftdi $@
