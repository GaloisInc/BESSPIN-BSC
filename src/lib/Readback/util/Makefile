PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# PREFIX is where things are installed
PREFIX?=$(TOP)/inst

INSTALL?= $(TOP)/util/install

# -------------------------

# Install location
INSTALLDIR=$(PREFIX)/lib/Readback

# -------------------------

.PHONY: install
install:
	install -d -m755 $(INSTALLDIR)
	install -m644 create_xrf.cxx $(INSTALLDIR)

.PHONY: clean realclean
clean realclean:

# --------------------
# for testing purposes

CXXFAMILY = $(shell $(TOP)/util/scripts/bsenv c++_family)

RDBACK_INCDIR = $(INSTALLDIR)
RDBACK_LIBDIR = $(INSTALLDIR)/$(CXXFAMILY)

INCFLAGS = \
	-I $(RDBACK_INCDIR) \

LIBFLAGS = \
	-L $(RDBACK_LIBDIR) \
	-lreadback \
	-lz \

create_xrf: create_xrf.cxx
	$(CXX) -o create_xrf create_xrf.cxx $(INCFLAGS) $(LIBFLAGS)

clean realclean:
	$(RM) -f create_xrf

# --------------------

