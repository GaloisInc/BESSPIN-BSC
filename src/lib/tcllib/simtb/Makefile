PREFIX?=$(PWD)/../../../../inst
LIBDIR = $(PREFIX)/lib
TARGETDIR = $(LIBDIR)/tcllib/simtb

FILES = \
	tclIndex \
	pkgIndex.tcl

# tcl files which need each proc listed in tclIndex
# utils.tcl is really a package, but some initization code require it
TCLFILES = \
	simtb_gui_dut.tcl

# tcl packages
PACKAGES = \
	boot.tcl \
	cmdline.tcl \
	CmdOptions.tcl \
	comm.tcl \
	Interp.tcl \
	Redirect.tcl \
	SimulationTestbench.tcl

.PHONY: clean all install tclIndex

all: tclIndex

# pkg_mkIndex.tcl is produced here
tclIndex: *.tcl
	./tclIndex.sh "$(TCLFILES)" "$(PACKAGES)"

install: tclIndex
	install -d -m755 $(TARGETDIR)
	install -m644 $(FILES) $(PACKAGES) $(TCLFILES) $(TARGETDIR)

.PHONY: TAGS
TAGS:
	make -C .. TAGS
	ln -s ../TAGS

.PHONY: clean realclean
realclean: clean
	-rm -f TAGS

clean:
	@-rm -f tclIndex pkg_mkIndex.tcl pkgIndex.tcl
