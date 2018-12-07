PWD:=$(shell pwd)
TOP=$(PWD)/../../../../../..

include $(TOP)/platform.mk

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

LIBDIR=$(PREFIX)/lib
SUPPORTDIR=$(LIBDIR)/board_support/bluenoc
INSTALLDIR=$(SUPPORTDIR)/intel/ARRIA10GX

INSTALL?= $(TOP)/util/install

# note: p1b/sim and p1b/synth omitted for now...
.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 quartus_script_template.tcl $(INSTALLDIR)
	install -m644 quartus_sdc_template.sdc $(INSTALLDIR)
	install -d -m755 $(INSTALLDIR)/verilog
	install -m644 verilog/*.ip $(INSTALLDIR)/verilog
	install -m644 verilog/*.v $(INSTALLDIR)/verilog
	install -d -m755 $(INSTALLDIR)/verilog/p1b
	install -d -m755 $(INSTALLDIR)/verilog/pll2
	install -d -m755 $(INSTALLDIR)/verilog/clkbuf
	install -d -m755 $(INSTALLDIR)/verilog/p1b/synth
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_xcvr_fpll_a10_180
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_xcvr_native_a10_180
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180/sim
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180/synth
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180/synth/debug
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180/synth/debug/stp
	install -m644 verilog/p1b/synth/* $(INSTALLDIR)/verilog/p1b/synth
	install -m644 verilog/p1b/altera_pcie_a10_hip_180/sim/* $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180/sim
	install -m644 verilog/p1b/altera_pcie_a10_hip_180/synth/*v $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180/synth
	install -m644 verilog/p1b/altera_pcie_a10_hip_180/synth/*sdc $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180/synth
	install -m644 verilog/p1b/altera_pcie_a10_hip_180/synth/debug/stp/*xml $(INSTALLDIR)/verilog/p1b/altera_pcie_a10_hip_180/synth/debug/stp
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_xcvr_fpll_a10_180/sim
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_xcvr_fpll_a10_180/synth
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_xcvr_fpll_a10_180/sim/mentor
	install -m644 verilog/p1b/altera_xcvr_fpll_a10_180/sim/*v $(INSTALLDIR)/verilog/p1b/altera_xcvr_fpll_a10_180/sim
	install -m644 verilog/p1b/altera_xcvr_fpll_a10_180/sim/*txt $(INSTALLDIR)/verilog/p1b/altera_xcvr_fpll_a10_180/sim
	install -m644 verilog/p1b/altera_xcvr_fpll_a10_180/sim/mentor/* $(INSTALLDIR)/verilog/p1b/altera_xcvr_fpll_a10_180/sim/mentor
	install -m644 verilog/p1b/altera_xcvr_fpll_a10_180/synth/* $(INSTALLDIR)/verilog/p1b/altera_xcvr_fpll_a10_180/synth
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_xcvr_native_a10_180/sim
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_xcvr_native_a10_180/synth
	install -d -m755 $(INSTALLDIR)/verilog/p1b/altera_xcvr_native_a10_180/sim/mentor
	install -m644 verilog/p1b/altera_xcvr_native_a10_180/sim/*v $(INSTALLDIR)/verilog/p1b/altera_xcvr_native_a10_180/sim
	install -m644 verilog/p1b/altera_xcvr_native_a10_180/sim/*txt $(INSTALLDIR)/verilog/p1b/altera_xcvr_native_a10_180/sim
	install -m644 verilog/p1b/altera_xcvr_native_a10_180/sim/mentor/* $(INSTALLDIR)/verilog/p1b/altera_xcvr_native_a10_180/sim/mentor
	install -m644 verilog/p1b/altera_xcvr_native_a10_180/synth/* $(INSTALLDIR)/verilog/p1b/altera_xcvr_native_a10_180/synth
	install -m644 verilog/p1b/p1b* $(INSTALLDIR)/verilog/p1b
	install -d -m755 $(INSTALLDIR)/verilog/pll2/altera_iopll_180
	install -d -m755 $(INSTALLDIR)/verilog/pll2/altera_iopll_180/synth
	install -d -m755 $(INSTALLDIR)/verilog/pll2/synth
	install -m644 verilog/pll2/pll2* $(INSTALLDIR)/verilog/pll2
	install -m644 verilog/pll2/synth/* $(INSTALLDIR)/verilog/pll2/synth
	install -m644 verilog/pll2/altera_iopll_180/synth/* $(INSTALLDIR)/verilog/pll2/altera_iopll_180/synth
	install -d -m755 $(INSTALLDIR)/verilog/clkbuf/altclkctrl_180
	install -d -m755 $(INSTALLDIR)/verilog/clkbuf/altclkctrl_180/synth
	install -d -m755 $(INSTALLDIR)/verilog/clkbuf/synth
	install -m644 verilog/clkbuf/clkbuf* $(INSTALLDIR)/verilog/clkbuf
	install -m644 verilog/clkbuf/synth/* $(INSTALLDIR)/verilog/clkbuf/synth
	install -m644 verilog/clkbuf/altclkctrl_180/synth/* $(INSTALLDIR)/verilog/clkbuf/altclkctrl_180/synth

.PHONY: clean realclean
clean realclean:
