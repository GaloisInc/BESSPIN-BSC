# dummy makefile
PWD:=$(shell pwd)
TOP:=$(PWD)/../../../

INSTALL?= $(TOP)/util/install

#INSTALL?=install -c

PREFIX?=$(TOP)/inst
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib
LIBBINDIR=$(LIBDIR)/bin
EXEDIR=$(LIBDIR)/bin

.PHONY: check localcheck clean realclean install

check:
	@echo "no check in exec"

localcheck:
	@echo "no localcheck in exec"

clean:
	@echo "no clean in exec"

realclean:
	@echo "no realclean in exec"


install: $(LIB) $(LIB_P)
	$(INSTALL) -m 755 -d $(LIBBINDIR)/
	$(INSTALL) -m 755 bsc_build_vsim_vcs $(LIBBINDIR)/bsc_build_vsim_vcs
	$(INSTALL) -m 755 bsc_build_vsim_vcs $(LIBBINDIR)/bsc_build_vsim_VCS
	$(INSTALL) -m 755 bsc_build_vsim_vcsi $(LIBBINDIR)/bsc_build_vsim_vcsi
	$(INSTALL) -m 755 bsc_build_vsim_vcsi $(LIBBINDIR)/bsc_build_vsim_VCSi
	$(INSTALL) -m 755 bsc_build_vsim_vcsi $(LIBBINDIR)/bsc_build_vsim_VCSI
	$(INSTALL) -m 755 bsc_build_vsim_ncverilog $(LIBBINDIR)/bsc_build_vsim_ncverilog
	$(INSTALL) -m 755 bsc_build_vsim_ncverilog $(LIBBINDIR)/bsc_build_vsim_NCVerilog
	$(INSTALL) -m 755 bsc_build_vsim_ncverilog $(LIBBINDIR)/bsc_build_vsim_nc
	$(INSTALL) -m 755 bsc_build_vsim_ncverilog $(LIBBINDIR)/bsc_build_vsim_ncv
	$(INSTALL) -m 755 bsc_build_vsim_ncverilog $(LIBBINDIR)/bsc_build_vsim_ncvlog
	$(INSTALL) -m 755 bsc_build_vsim_ncverilog $(LIBBINDIR)/bsc_build_vsim_ncsim
	$(INSTALL) -m 755 bsc_build_vsim_modelsim $(LIBBINDIR)/bsc_build_vsim_modelsim
	$(INSTALL) -m 755 bsc_build_vsim_modelsim $(LIBBINDIR)/bsc_build_vsim_Modelsim
	$(INSTALL) -m 755 bsc_build_vsim_modelsim $(LIBBINDIR)/bsc_build_vsim_ModelSim
	$(INSTALL) -m 755 bsc_build_vsim_iverilog $(LIBBINDIR)/bsc_build_vsim_iverilog
	$(INSTALL) -m 755 bsc_build_vsim_iverilog $(LIBBINDIR)/bsc_build_vsim_icarus
	$(INSTALL) -m 755 bsc_build_vsim_veriwell $(LIBBINDIR)/bsc_build_vsim_veriwell
	$(INSTALL) -m 755 bsc_build_vsim_cver $(LIBBINDIR)/bsc_build_vsim_cver
	$(INSTALL) -m 755 bsc_build_vsim_cvc $(LIBBINDIR)/bsc_build_vsim_cvc
	$(INSTALL) -m 755 bsc_build_vsim_cvc64 $(LIBBINDIR)/bsc_build_vsim_cvc64
	$(INSTALL) -m 755 bsc_build_vsim_isim $(LIBBINDIR)/bsc_build_vsim_isim
	$(INSTALL) -m 755 bsc_build_vsim_xsim $(LIBBINDIR)/bsc_build_vsim_xsim



