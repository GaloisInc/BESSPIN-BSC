PREFIX?=$(PWD)/../../../../../../inst
LIBDIR = $(PREFIX)/lib
TARGETDIR = $(LIBDIR)/tcllib/novas/etc/wave

# NOTE  tclIndex is not included in the list

FILES = \
	wave.menu


.PHONY: clean all install

all: install

install: 
	install -d -m755 $(TARGETDIR)
	install -m644 $(FILES) $(TARGETDIR)


.PHONY: TAGS
TAGS: ../TAGS

.PHONY: clean realclean
realclean: clean

clean:
	@-rm -f tclIndex pkg_mkIndex.tcl pkgIndex.tcl
