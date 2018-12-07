PREFIX?=$(PWD)/../../../../inst
LIBDIR = $(PREFIX)/lib

TCLFILES = \
	msgbox.tcl  \
	tkfbox.tcl



.PHONY: clean all install

all: tclIndex

tclIndex: *.tcl
	./tclIndex.sh "$(TCLFILES)" ""


install: tclIndex
	install -d -m755 $(LIBDIR)/tcllib/tk
	install -m644 tclIndex $(TCLFILES) $(LIBDIR)/tcllib/tk


.PHONY: clean realclean	
realclean: clean

clean:
	rm -f pkgIndex.tcl tclIndex
