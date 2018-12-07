PREFIX?=$(PWD)/../../../../inst
LIBDIR = $(PREFIX)/lib
TARGETDIR = $(LIBDIR)/tcllib/scemi

# NOTE  tclIndex is not included in the list

FILES = \
	pkgIndex.tcl \

# tcl files which need each proc listed in tclIndex
# utils.tcl is really a package, but some initization code require it
TCLFILES = \

# tcl packages
PACKAGES = \
	ProbeGui.tcl \
	MemoryGui.tcl \
	ControlGui.tcl \
        VCDGui.tcl 

# top level user scripts
EXEFILES = \


.PHONY: clean all install tclIndex

all: tclIndex

# pkg_mkIndex.tcl is produced here
tclIndex: *.tcl
	./tclIndex.sh "$(TCLFILES)" "$(PACKAGES)"

install: tclIndex
	install -d -m755 $(TARGETDIR)
	install -m644 $(FILES) $(PACKAGES) $(TCLFILES) $(TARGETDIR)
	#install -m755 $(EXEFILES) $(TARGETDIR)

.PHONY: TAGS
TAGS: ../TAGS

.PHONY: clean realclean
realclean: clean

clean:
	@-rm -f tclIndex pkg_mkIndex.tcl pkgIndex.tcl
