##  Makefile for board support libraries
##  Copies the board support libraries to the inst dir

VENDOR = ../../../../vendor/

XILINXDIR = $(VENDOR)/xilinx/
ALTERADIR = $(VENDOR)/altera/
DINIDIR   = $(VENDOR)/dini/

# Location where files are installed
PWD:=$(shell pwd)
PREFIX?=$(PWD)/../../../../inst
LIBDIR=$(PREFIX)/lib
SUPPORTDIR=$(LIBDIR)/board_support/classic

INSTALL ?= install

.PHONY: all
all: install

.PHONY: install
install: xilinx_src altera_src dini_src bs_utils bridges drivers

.PHONY: xilinx_src
xilinx_src:
	install -d -m755 $(SUPPORTDIR)/xilinx
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507
	install -m644 xilinx/ML507/default.*.template $(SUPPORTDIR)/xilinx/ML507
	install -m644 xilinx/ML507/*_constraints $(SUPPORTDIR)/xilinx/ML507
	install -m644 xilinx/ML507/bitgen.ut $(SUPPORTDIR)/xilinx/ML507
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/ddr2_v3_5
	install -m644 xilinx/ML507/verilog/ddr2_v3_5/*.v $(SUPPORTDIR)/xilinx/ML507/verilog/ddr2_v3_5
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/ddr2_v3_5/docs
	install -m644 xilinx/ML507/verilog/ddr2_v3_5/docs/* $(SUPPORTDIR)/xilinx/ML507/verilog/ddr2_v3_5/docs
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/ddr2_v3_5/user_design/rtl
	install -m644 xilinx/ML507/verilog/ddr2_v3_5/user_design/rtl/* $(SUPPORTDIR)/xilinx/ML507/verilog/ddr2_v3_5/user_design/rtl
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/ddr2_v3_5/user_design/sim
	install -m644 xilinx/ML507/verilog/ddr2_v3_5/user_design/sim/* $(SUPPORTDIR)/xilinx/ML507/verilog/ddr2_v3_5/user_design/sim
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/*.txt $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/doc
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/doc/* $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/doc
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/example_design
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/example_design/*.v $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/example_design
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/example_design/*.ucf $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/example_design
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/implement
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/implement/* $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/implement
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/*.v $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/*.f $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/dsport
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/dsport/* $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/dsport
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/functional
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/functional/* $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/functional
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/tests
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/tests/* $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/simulation/tests
	install -d -m755 $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/source
	install -m644 xilinx/ML507/verilog/endpoint_blk_plus_v1_14/source/* $(SUPPORTDIR)/xilinx/ML507/verilog/endpoint_blk_plus_v1_14/source
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5
	install -m644 xilinx/XUPV5/default.*.template $(SUPPORTDIR)/xilinx/XUPV5
	install -m644 xilinx/XUPV5/*_constraints $(SUPPORTDIR)/xilinx/XUPV5
	install -m644 xilinx/XUPV5/bitgen.ut $(SUPPORTDIR)/xilinx/XUPV5
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/ddr2_v3_5
	install -m644 xilinx/XUPV5/verilog/ddr2_v3_5/*.v $(SUPPORTDIR)/xilinx/XUPV5/verilog/ddr2_v3_5
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/ddr2_v3_5/docs
	install -m644 xilinx/XUPV5/verilog/ddr2_v3_5/docs/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/ddr2_v3_5/docs
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/ddr2_v3_5/user_design/rtl
	install -m644 xilinx/XUPV5/verilog/ddr2_v3_5/user_design/rtl/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/ddr2_v3_5/user_design/rtl
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/ddr2_v3_5/user_design/sim
	install -m644 xilinx/XUPV5/verilog/ddr2_v3_5/user_design/sim/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/ddr2_v3_5/user_design/sim
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/*.txt $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/doc
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/doc/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/doc
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/example_design
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/example_design/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/example_design
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/implement
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/implement/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/implement
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/*.v $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/*.f $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/dsport
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/dsport/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/dsport
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/functional
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/functional/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/functional
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/tests
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/tests/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/simulation/tests
	install -d -m755 $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/source
	install -m644 xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/source/* $(SUPPORTDIR)/xilinx/XUPV5/verilog/endpoint_blk_plus_v1_14/source
	install	-d -m755 $(SUPPORTDIR)/xilinx/ML605
	install -m644 xilinx/ML605/default.*.template $(SUPPORTDIR)/xilinx/ML605
	install -m644 xilinx/ML605/*_constraints $(SUPPORTDIR)/xilinx/ML605
	install -m644 xilinx/ML605/bitgen.ut $(SUPPORTDIR)/xilinx/ML605
	install	-d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5
	install -m644 xilinx/ML605/verilog/ddr3_v3_5/*.v $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/docs
	install -m644 xilinx/ML605/verilog/ddr3_v3_5/docs/* $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/docs
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/controller
	install -m644 xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/controller/* $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/controller
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ecc
	install -m644 xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ecc/* $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ecc
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ip_top
	install -m644 xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ip_top/* $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ip_top
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/phy
	install -m644 xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/phy/* $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/phy
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ui
	install -m644 xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ui/* $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/rtl/ui
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/sim
	install -m644 xilinx/ML605/verilog/ddr3_v3_5/user_design/sim/* $(SUPPORTDIR)/xilinx/ML605/verilog/ddr3_v3_5/user_design/sim
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7
	install -m644 xilinx/ML605/verilog/integrated_blk_v1_7/*.txt $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7/doc
	install -m644 xilinx/ML605/verilog/integrated_blk_v1_7/doc/* $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7/doc
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7/example_design
	install -m644 xilinx/ML605/verilog/integrated_blk_v1_7/example_design/* $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7/example_design
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7/implement
	install -m644 xilinx/ML605/verilog/integrated_blk_v1_7/implement/* $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7/implement
	install -d -m755 $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7/source
	install -m644 xilinx/ML605/verilog/integrated_blk_v1_7/source/*.v $(SUPPORTDIR)/xilinx/ML605/verilog/integrated_blk_v1_7/source
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705
	install -m644 xilinx/KC705/default.*.template $(SUPPORTDIR)/xilinx/KC705
	install -m644 xilinx/KC705/*_constraints $(SUPPORTDIR)/xilinx/KC705
	install -m644 xilinx/KC705/bitgen.ut $(SUPPORTDIR)/xilinx/KC705
	install	-d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6
	install -m644 xilinx/KC705/verilog/k7_pcie_v1_6/*.txt $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/example_design
	install -m644 xilinx/KC705/verilog/k7_pcie_v1_6/example_design/* $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/example_design
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/implement
	install -m644 xilinx/KC705/verilog/k7_pcie_v1_6/implement/*.bat $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/implement
	install -m755 xilinx/KC705/verilog/k7_pcie_v1_6/implement/*.sh $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/implement
	install -m644 xilinx/KC705/verilog/k7_pcie_v1_6/implement/*.tcl $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/implement
	install -m644 xilinx/KC705/verilog/k7_pcie_v1_6/implement/*.prj $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/implement
	install -m644 xilinx/KC705/verilog/k7_pcie_v1_6/implement/*.xcf $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/implement
	install -m644 xilinx/KC705/verilog/k7_pcie_v1_6/implement/*.xst $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/implement
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/source
	install -m644 xilinx/KC705/verilog/k7_pcie_v1_6/source/* $(SUPPORTDIR)/xilinx/KC705/verilog/k7_pcie_v1_6/source
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/*.csv $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/docs
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/docs/* $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/docs
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/*.v $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/clocking
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/clocking/*.v $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/clocking
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/controller
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/controller/*.v $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/controller
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ecc
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ecc/*.v $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ecc
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ip_top
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ip_top/*.v $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ip_top
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/phy
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/phy/*.v $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/phy
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/phy_traffic_gen
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/phy_traffic_gen/*.v $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/phy_traffic_gen
	install -d -m755 $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ui
	install -m644 xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ui/*.v $(SUPPORTDIR)/xilinx/KC705/verilog/ddr3_v1_6/user_design/rtl/ui

PHONY: altera_src
altera_src:
	install -d -m755 $(SUPPORTDIR)/altera
	install -d -m755 $(SUPPORTDIR)/altera/verilog
	install -m644 altera/verilog/*.v $(SUPPORTDIR)/altera/verilog
	install -d -m755 $(SUPPORTDIR)/altera/verilog/ddr2_v11_0
	install -d -m755 $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -d -m755 $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0/software
	install -m644 altera/verilog/ddr2_v11_0/*.v $(SUPPORTDIR)/altera/verilog/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/*.qip $(SUPPORTDIR)/altera/verilog/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/*.v $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/*.sv $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/*.sdc $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/*.iv $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/*.hex $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/*.tcl $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/*.ppf $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/*.mif $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0/ddr2_v11_0/software/* $(SUPPORTDIR)/altera/verilog/ddr2_v11_0/ddr2_v11_0/software
	install -d -m755 $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv
	install -d -m755 $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -d -m755 $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/software
	install -m644 altera/verilog/ddr2_v11_0_iv/*.v $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv
	install -m644 altera/verilog/ddr2_v11_0_iv/*.qip $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/*.v $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/*.sv $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/*.sdc $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/*.iv $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/*.hex $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/*.tcl $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/*.ppf $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/*.mif $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0
	install -m644 altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/software/* $(SUPPORTDIR)/altera/verilog/ddr2_v11_0_iv/ddr2_v11_0/software
	install -d -m755 $(SUPPORTDIR)/altera/verilog/StratixIV
	install -d -m755 $(SUPPORTDIR)/altera/verilog/StratixIII
	install -m644 altera/verilog/StratixIV/*.v  $(SUPPORTDIR)/altera/verilog/StratixIV
	install -m644 altera/verilog/StratixIII/*.v $(SUPPORTDIR)/altera/verilog/StratixIII

.PHONY: dini_src
dini_src:
	install -d -m755 $(SUPPORTDIR)/dini
	install -d -m755 $(SUPPORTDIR)/dini/verilog
	install -m644 dini/verilog/*.v $(SUPPORTDIR)/dini/verilog
	install -d -m755 $(SUPPORTDIR)/dini/7002
	install -m644 dini/7002/fpga_?.* $(SUPPORTDIR)/dini/7002
	install -m644 dini/7002/*pin_assignments $(SUPPORTDIR)/dini/7002
	install -m644 dini/7002/*sodimm_attributes $(SUPPORTDIR)/dini/7002
	install -m644 dini/7002/*synthesis_constraints $(SUPPORTDIR)/dini/7002
	install -m644 dini/7002/d7002* $(SUPPORTDIR)/dini/7002
	install -m644 dini/7002/7002* $(SUPPORTDIR)/dini/7002
	install -d -m755 $(SUPPORTDIR)/dini/7006
	install -m644 dini/7006/fpga_?.* $(SUPPORTDIR)/dini/7006
	install -m644 dini/7006/*pin_assignments $(SUPPORTDIR)/dini/7006
	install -m644 dini/7006/*sodimm_attributes $(SUPPORTDIR)/dini/7006
	install -m644 dini/7006/*synthesis_constraints $(SUPPORTDIR)/dini/7006
	install -m644 dini/7006/d7006* $(SUPPORTDIR)/dini/7006
	install -m644 dini/7006/7006* $(SUPPORTDIR)/dini/7006
	install -d -m755 $(SUPPORTDIR)/dini/7006/fpga_q
	install -d -m755 $(SUPPORTDIR)/dini/7006/fpga_q_lx50t
	install -m644 dini/7006/fpga_q/* $(SUPPORTDIR)/dini/7006/fpga_q
	install -m644 dini/7006/fpga_q_lx50t/* $(SUPPORTDIR)/dini/7006/fpga_q_lx50t
	install -d -m755 $(SUPPORTDIR)/dini/7406
	install -m644 dini/7406/fpga_?.* $(SUPPORTDIR)/dini/7406
	install -m644 dini/7406/*pin_assignments $(SUPPORTDIR)/dini/7406
	install -m644 dini/7406/*sodimm_attributes $(SUPPORTDIR)/dini/7406
	install -m644 dini/7406/*synthesis_constraints $(SUPPORTDIR)/dini/7406
	install -m644 dini/7406/d7406* $(SUPPORTDIR)/dini/7406
	install -m644 dini/7406/7406* $(SUPPORTDIR)/dini/7406
	install -d -m755 $(SUPPORTDIR)/dini/7406/fpga_q
	install -m644 dini/7006/fpga_q/* $(SUPPORTDIR)/dini/7406/fpga_q
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL
	install -m644 dini/10GHXTLL/default.*.template $(SUPPORTDIR)/dini/10GHXTLL
	install -m644 dini/10GHXTLL/bitgen.ut $(SUPPORTDIR)/dini/10GHXTLL
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp0
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp1
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp2
	install -m644 dini/10GHXTLL/verilog/sfp0/*.ngc $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp0
	install -m644 dini/10GHXTLL/verilog/sfp0/*.v $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp0
	install -m644 dini/10GHXTLL/verilog/sfp1/*.ngc $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp1
	install -m644 dini/10GHXTLL/verilog/sfp1/*.v $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp1
	install -m644 dini/10GHXTLL/verilog/sfp2/*.ngc $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp2
	install -m644 dini/10GHXTLL/verilog/sfp2/*.v $(SUPPORTDIR)/dini/10GHXTLL/verilog/sfp2
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7
	install -m644 dini/10GHXTLL/verilog/integrated_blk_v1_7/*.txt $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7/doc
	install -m644 dini/10GHXTLL/verilog/integrated_blk_v1_7/doc/* $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7/doc
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7/example_design
	install -m644 dini/10GHXTLL/verilog/integrated_blk_v1_7/example_design/* $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7/example_design
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7/implement
	install -m644 dini/10GHXTLL/verilog/integrated_blk_v1_7/implement/* $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7/implement
	install -d -m755 $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7/source
	install -m644 dini/10GHXTLL/verilog/integrated_blk_v1_7/source/*.v $(SUPPORTDIR)/dini/10GHXTLL/verilog/integrated_blk_v1_7/source
	install -d -m755 $(SUPPORTDIR)/dini/tool
	install -d -m755 $(SUPPORTDIR)/dini/tool/driver
	install -m644 dini/tool/Makefile $(SUPPORTDIR)/dini/tool
	install -m644 dini/tool/*.cpp $(SUPPORTDIR)/dini/tool
	install -m644 dini/tool/*.h $(SUPPORTDIR)/dini/tool
	install -m644 dini/tool/driver/*.c $(SUPPORTDIR)/dini/tool/driver
	install -m644 dini/tool/driver/*.h $(SUPPORTDIR)/dini/tool/driver

.PHONY: bs_utils
bs_utils:
	install -d -m755 $(SUPPORTDIR)/utils

.PHONY: bridges
bridges:
	install -d -m755 $(SUPPORTDIR)/bridges
	install -m644 bridges/*.bsv $(SUPPORTDIR)/bridges

.PHONY: drivers
drivers:
	install -d -m755 $(SUPPORTDIR)/drivers
	install -m644 drivers/Makefile $(SUPPORTDIR)/drivers
	install -m755 drivers/bluespec-rules.sh $(SUPPORTDIR)/drivers
	install -m644 drivers/50-bluespec.rules $(SUPPORTDIR)/drivers

.PHONY: clean realclean
realclean clean:
