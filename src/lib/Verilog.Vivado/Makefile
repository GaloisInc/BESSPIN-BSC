#
# $Id$
#

PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

PREFIX?=$(PWD)/../../../inst
LIBDIR=$(PREFIX)/lib

VCS ?= vcs
VCSCMD = $(VCS)  +v2k +libext+.v -y . +warn=all +define+TOP=Empty 

IVERILOG = iverilog -tnull -Wall -y . -DTOP=Empty
CVC = cvc +libext+.v -y . +warn=all +define+TOP=Empty

PD = +define+$(1)
PDARGS = $(foreach a, $(1), $(call PD,$a))

MD = -D$(1)
MDARGS = $(foreach a, $(1), $(call MD,$a))

RM = rm -f

INSTALL?=install -c

COPYMOD = $(PWD)/copy_module.pl

VERIS = \
	MakeClock.v \
	RegFile.v \
	SizedFIFO.v \
	BRAM1.v \
	BRAM1BE.v \
	BRAM1BELoad.v \
	BRAM1Load.v \
	BRAM2.v \
	BRAM2BE.v \
	BRAM2BELoad.v \
	BRAM2Load.v \

#OTHERFILES = \

.PHONY: all install clean realclean

all: test


# ----------

# check that the files compile.
test_vcs:	$(VERIS)
	$(VCSCMD) $+
	-$(RM) simv

# test whether the Verilog libs parse with various parametrizing macros defined
.PHONY: test
test:	$(VERIS)
	$(IVERILOG)	$+
	$(IVERILOG) $(call MDARGS, BSV_NO_INITIAL_BLOCKS=1) $+
	$(IVERILOG) $(call MDARGS, BSV_ASSIGNMENT_DELAY='#0')  $+
	$(IVERILOG) $(call MDARGS, BSV_ASSIGNMENT_DELAY='#1')  $+
	$(IVERILOG) $(call MDARGS, BSV_POSITIVE_RESET) $+
	$(IVERILOG) $(call MDARGS, BSV_ASYNC_FIFO_RESET) $+
	$(IVERILOG) $(call MDARGS, BSV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+
	$(IVERILOG) $(call MDARGS, BSV_ASYNC_FIFO_RESET BSV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+
	$(IVERILOG) $(call MDARGS, BSV_POSITIVE_RESET BSV_ASYNC_FIFO_RESET BSV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+


testvcs:	$(VERIS)
	$(VCSCMD)  $+
	$(VCSCMD) $(call PDARGS, BSV_NO_INITIAL_BLOCKS=1)  $+
	$(VCSCMD) $(call PDARGS, BSV_ASSIGNMENT_DELAY='#0') $+
	$(VCSCMD) $(call PDARGS, BSV_ASSIGNMENT_DELAY='#1') $+
	$(VCSCMD) $(call PDARGS, BSV_POSITIVE_RESET) $+
	$(VCSCMD) $(call PDARGS, BSV_ASYNC_FIFO_RESET) $+
	$(VCSCMD) $(call PDARGS, BSV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+
	$(VCSCMD) $(call PDARGS, BSV_ASYNC_FIFO_RESET BSV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+
	$(VCSCMD) $(call PDARGS, BSV_POSITIVE_RESET BSV_ASYNC_FIFO_RESET BSV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+
	-$(RM) simv

testcvc:	$(VERIS)
	$(CVC) $+
	$(CVC) $(call PDARGS,  BSV_NO_INITIAL_BLOCKS=1) $+
	$(CVC) $(call PDARGS,  BSV_ASSIGNMENT_DELAY='#0') $+
	$(CVC) $(call PDARGS,  BSV_ASSIGNMENT_DELAY='#1') $+
	$(CVC) $(call PDARGS,  BSV_POSITIVE_RESET) $+
	$(CVC) $(call PDARGS,  BSV_ASYNC_FIFO_RESET) $+
	$(CVC) $(call PDARGS,  BSV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+
	$(CVC) $(call PDARGS,  BSV_ASYNC_FIFO_RESET SV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+
	$(CVC) $(call PDARGS,  BSV_POSITIVE_RESET BSV_ASYNC_FIFO_RESET SV_RESET_FIFO_HEAD BSV_RESET_FIFO_ARRAY) $+
	-$(RM) cvcsim verilog.log


.PHONY: test_race
test_race: $(VERIS)
	$(VCSCMD) +race=all $+
	cat race.out.static

.PHONY: testvcs
install: $(VERIS)
	$(INSTALL) -m 755 -d $(LIBDIR)/Verilog.Vivado
	$(INSTALL) -m 644 $(VERIS) $(LIBDIR)/Verilog.Vivado
#	$(INSTALL) -m 644 $(OTHERFILES) $(LIBDIR)/Verilog.Vivado

clean:
	-$(RM) *.dump a.out sim *.testout *.fsdb race.out.static simv
	-$(RM) -r csrc simv.daidir cvcsim verilog.log

realclean: clean
	# remove the copied modules, too
	$(RM)

VERILOG = $(CVC) +define+testBluespec
##VERILOG = iverilog -o simv -Wall -y . -DtestBluespec

.PHONY:	testout
testout:	

.PHONY: %.testout
.KEEP:	%.testout
%.testout:	%.v
	$(RM) ./simv
	$(VERILOG) $<
	./cvcsim | tee $@

%.testdiff: %.testout
	diff $< $<.expected | tee $@
