#
# $Id: Makefile,v 1.6 2003/08/20 17:36:02 augustss Exp $
#

# MANUAL STEPS UNTIL ActiveTcl compiler works on debian!!!!
#  rlogin to sapphire, compile it there with "tclcompile"
#  copy blueview.tbc back to here as blueview (check it in, etc)

BSSRC = $(HOME)/bsc/src


EXEFILES = \
	blueview

LIBFILES = \
	blueview-preferences-example.tcl \
	wave.menu

INSTALL?=install -c

PREFIX?=/usr/local/bluespec
LIBDIR=$(PREFIX)/lib
BINDIR=$(PREFIX)/bin

.PHONY: default all install release install-tar clean realclean

default:
	@echo "you probably should be running this"

all:
	make update
	make compile


release install install-tar:
	$(INSTALL) -m 755 -d $(BINDIR)
	$(INSTALL) -m 644 $(LIBFILES) $(BINDIR)
	$(INSTALL) -m 755 $(EXEFILES) $(BINDIR)

# update gExpDate and gVersion - assumes you have 
# needed build files at /home/{USER}/bsc/src/comp/Version.hs, etc
update:
	perl bv-update-version.pl $(BSSRC)/comp/Expiry.hs $(BSSRC)/comp/Version.hs

compile:
	# TODO: right now this only runs on clef!!!!
	#  this is a manual step, so it's not correct yet, but no
	#  choice until we get a tcl compiler on every machine...
	#rm -f blueview
	# tclcompiler blueview.tcl
	#mv blueview.tbc blueview
	# chmod +x blueview
	cp -p blueview.tcl blueview

testall:
	(cd /home/sallen/bsc/demos/mesa1; make build testwaves)
	(cd /home/sallen/bsc/testsuite/bsc.interra/IDCT/BSV; make -f my.mk build testwaves)
	(cd /home/sallen/bsc/testsuite/bsc.interra/rtl_quality/sequential_multiplier; make -f my.mk build testwaves)
	(cd /home/sallen/bsc/testsuite/bsc.interra/rtl_quality/pattern_detector; make -f my.mk build testwaves)
	(cd /home/sallen/customers/Infineon/infineon-bv-issues; make build testwaves)
clean:

realclean: clean
