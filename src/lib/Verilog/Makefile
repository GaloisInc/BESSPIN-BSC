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
	ASSIGN1.v \
	BRAM1.v \
	BRAM1Load.v \
	BRAM1BE.v \
	BRAM1BELoad.v \
	BRAM2.v \
	BRAM2Load.v \
	BRAM2BE.v \
	BRAM2BELoad.v \
	BypassCrossingWire.v \
	BypassWire.v \
	BypassWire0.v \
	ClockDiv.v \
	ClockGater.v \
	ClockGen.v \
	ClockInverter.v \
	ClockMux.v \
	ClockSelect.v \
	ConfigRegN.v \
	ConfigRegUN.v \
	ConfigRegA.v \
	ConstrainedRandom.v \
	ConvertFromZ.v \
	ConvertToZ.v \
	Counter.v \
	CRegA5.v \
	CRegN5.v \
	CRegUN5.v \
	CrossingBypassWire.v \
	CrossingRegA.v	\
	CrossingRegN.v \
	CrossingRegUN.v \
	DualPortRam.v \
	FIFO1.v \
	FIFO10.v \
	FIFO2.v \
	FIFO20.v \
	FIFOL1.v \
	FIFOL10.v \
	FIFOL2.v \
	FIFOL20.v \
	Fork.v \
	GatedClock.v \
	GatedClockDiv.v \
	GatedClockInverter.v \
	InitialReset.v \
	InoutConnect.v \
	LatchCrossingReg.v \
	MakeClock.v \
	MakeReset.v \
	MakeResetA.v \
	MakeReset0.v \
	McpRegUN.v \
	ProbeCapture.v \
	ProbeWire.v \
	ProbeHook.v \
	ProbeMux.v \
	ProbeTrigger.v \
	ProbeValue.v \
	RWire.v \
	RWire0.v \
	RegA.v	\
	RegAligned.v \
	RegFile.v \
	RegFileLoad.v \
	RegN.v \
	RegTwoA.v \
	RegTwoN.v \
	RegTwoUN.v \
	RegUN.v \
	ResetMux.v \
	ResetEither.v \
	ResetToBool.v \
	ResetInverter.v \
	ResolveZ.v \
	RevertReg.v \
	SampleReg.v \
	ScanIn.v \
	SizedFIFO.v \
	SizedFIFO0.v \
	SizedFIFOL.v \
	SizedFIFOL0.v \
	SyncBit.v \
	SyncBit15.v \
	SyncBit1.v \
	SyncBit05.v \
	SyncFIFO.v \
	SyncFIFO0.v \
	SyncFIFO1.v \
	SyncFIFO10.v \
	SyncFIFOLevel.v \
	SyncFIFOLevel0.v \
	SyncHandshake.v \
	SyncPulse.v \
	SyncRegister.v \
	SyncReset.v \
	SyncResetA.v \
	SyncReset0.v \
	SyncWire.v \
	TriState.v \
	UngatedClockMux.v \
	UngatedClockSelect.v \
	main.v \
	Empty.v \


OTHERFILES = \
	Bluespec.xcf \
	icap_fsm.txt \
	SceMiInPipeProxyF.v \
	SceMiOutPipeProxyF.v \
	SceMiInPipeProxyPut.v \
	SceMiOutPipeProxyGet.v \
	SceMiPipeProxyRS.v \
	SceMiPipeProxySR.v \
	SimTbInterp.v \


.PHONY: all install clean realclean

all: test

# ----------
# some modules are duplicates of other modules

RegAligned.v: RegA.v
	$(COPYMOD) RegA RegAligned

ConfigRegN.v: RegN.v
	$(COPYMOD) RegN ConfigRegN

ConfigRegUN.v: RegUN.v
	$(COPYMOD) RegUN ConfigRegUN

ConfigRegA.v: RegA.v
	$(COPYMOD) RegA ConfigRegA

BypassCrossingWire.v: BypassWire.v
	$(COPYMOD) BypassWire BypassCrossingWire

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
	$(INSTALL) -m 755 -d $(LIBDIR)/Verilog
	$(INSTALL) -m 644 $(VERIS) $(LIBDIR)/Verilog
	$(INSTALL) -m 644 $(OTHERFILES) $(LIBDIR)/Verilog

clean:
	-$(RM) *.dump a.out sim *.testout *.fsdb race.out.static simv
	-$(RM) -r csrc simv.daidir cvcsim verilog.log

realclean: clean
	# remove the copied modules, too
	$(RM) RegAligned.v ConfigRegN.v ConfigRegUN.v ConfigRegA.v BypassCrossingWire.v

VCDGEN = SyncRegister.testout \
	SyncFIFO.testout \
	ClockGen.testout

VERILOG = $(CVC) +define+testBluespec
##VERILOG = iverilog -o simv -Wall -y . -DtestBluespec

.PHONY:	testout
testout:	$(VCDGEN)

.PHONY: %.testout
.KEEP:	%.testout
%.testout:	%.v
	$(RM) ./simv
	$(VERILOG) $<
	./cvcsim | tee $@

%.testdiff: %.testout
	diff $< $<.expected | tee $@
