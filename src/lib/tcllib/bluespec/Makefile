PREFIX?=$(PWD)/../../../../inst
LIBDIR = $(PREFIX)/lib
TARGETDIR = $(LIBDIR)/tcllib/bluespec

FILES = \
	tclIndex \
	pkgIndex.tcl \
	bluespec.tcl

# tcl files which need each proc listed in tclIndex
# utils.tcl is really a package, but some initization code require it
TCLFILES = \
        Editor.tcl \
        Emacs.tcl \
        Gvim.tcl \
	gtkwave_eval.tcl \
	utils.tcl

# tcl packages
PACKAGES = \
        BluesimP.tcl \
        EvalChannel.tcl \
        FileSupport.tcl \
        GtkWave.tcl \
        GtkWaveMac.tcl \
        InstSynth.tcl \
        MathSupport.tcl \
        NovasSupport.tcl \
        NovasSupportSendSignal.tcl \
        SceMiMsg.tcl \
        SceMiParams.tcl \
        SignalTypes.tcl \
        Trace.tcl \
        TypeSupport.tcl \
        Unique.tcl \
        ViewerCommon.tcl \
	VisitorPattern.tcl \
        Waves.tcl \
        portUtil.tcl \
        types.tcl \
        virtual.tcl \
        functional.tcl

# top level user scripts
EXEFILES = \
	bluesim.tcl \
	expandPorts.tcl \
	dumpStructures.tcl \
	listFiles.tcl \
	generateSceMiMsgData.tcl \
	generateSceMiHeaders.tcl \
	makedepend.tcl \


.PHONY: clean all install tclIndex

all: tclIndex

# pkg_mkIndex.tcl is produced here
tclIndex: *.tcl
	./tclIndex.sh "$(TCLFILES)" "$(PACKAGES)"

install: tclIndex
	install -d -m755 $(TARGETDIR)
	install -m644 $(FILES) $(PACKAGES) $(TCLFILES) $(TARGETDIR)
	install -m755 $(EXEFILES) $(TARGETDIR)
	# without verific,  use the older listFiles.tcl code
	install -m755 listFiles.tcl $(TARGETDIR)/listVlogFiles.tcl


.PHONY: TAGS
TAGS:
	make -C .. TAGS
	ln -s ../TAGS

.PHONY: clean realclean
realclean: clean
	-rm -f TAGS

clean:
	@-rm -f tclIndex pkg_mkIndex.tcl pkgIndex.tcl
