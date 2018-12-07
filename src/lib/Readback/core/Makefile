PWD := $(shell pwd)
TOP := $(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX ?= $(TOP)/inst

RM ?= rm -f
INSTALL ?= install

# -------------------------

# Install location for the header files
RDBK_HDR_DIR = $(PREFIX)/lib/Readback

# Install location for the library files
CXXFAMILY = $(shell $(TOP)/util/scripts/bsenv c++_family)
RDBK_LIB_DIR = $(PREFIX)/lib/Readback/$(CXXFAMILY)

# For now, don't install the library into the release
# Just keep it local, so the tcllib Makefile can compile
# the Semu and Lumina tcl packages using it

# Local directory for the compiled libraries
OBJDIR = obj-$(KERNELNAME)$(W32or64)

# -------------------------

# We compile a library with and a library without gzstream
# (which is protected by a lesser GPL)
OBJECTS_NOGZ =  \
	DataHandler.o \
	Design.o \
	Export.o \
	Module.o \
	Netlister.o \
	RdBackVCDWriter.o \
	ReadBackProbe.o \
	TokenMap.o \
	Utils.o \

OBJECTS = gzstream.o $(OBJECTS_NOGZ)

OBJECTS      := $(addprefix $(OBJDIR)/,$(OBJECTS))
OBJECTS_NOGZ := $(addprefix $(OBJDIR)/,$(OBJECTS_NOGZ))

# Headers to be installed
HEADERS = \
	DataHandler.hpp \
	Design.hpp \
	Export.hpp \
	gzstream.hpp \
	Module.hpp \
	Netlister.hpp \
	RdBackControl.hpp \
	RdBackVCDWriter.hpp \
	ReadBackProbe.hpp \
	TokenMap.hpp \
	Utils.hpp \

# -------------------------

# Compiler FLAGS
#CXXFLAGS += -g
CXXFLAGS += -O3
CXXFLAGS += -Wall
CXXFLAGS += -fPIC
CXXFLAGS += -I .
CXXFLAGS += -D GZSTREAM_NAMESPACE=gzs
CXXFLAGS += -D RDBACKCONTROL

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CXXFLAGS += -I /opt/local/include -I /usr/local/include
endif

# -------------------------

.PHONY: all
all: install

$(OBJECTS): $(OBJDIR)/%.o: %.cpp *.hpp | $(OBJDIR)
	$(CXX) -c  -o $@  $(CXXFLAGS) $<

$(OBJDIR)/libreadback.a: $(OBJECTS) | $(OBJDIR)
	$(AR) vq $@ $^

$(OBJDIR)/libreadback_nogz.a: $(OBJECTS_NOGZ) | $(OBJDIR) $(OBJDIR)/libreadback.a
	$(AR) vq $@ $^

.PHONY: install
install: $(OBJDIR) $(OBJDIR)/libreadback.a $(OBJDIR)/libreadback_nogz.a
	$(INSTALL) -m 755 -d $(RDBK_HDR_DIR)
	$(INSTALL) -m 644 $(HEADERS) $(RDBK_HDR_DIR)
	$(INSTALL) -m 755 -d $(RDBK_LIB_DIR)
	$(INSTALL) -m 755 $(OBJDIR)/libreadback_nogz.a $(RDBK_LIB_DIR)
	$(INSTALL) -m 755 $(OBJDIR)/libreadback.a $(RDBK_LIB_DIR)

$(OBJDIR):
	mkdir -p $(OBJDIR)

.PHONY: clean
clean:
	$(RM) $(OBJDIR)/*.o $(OBJDIR)/*.so $(OBJDIR)/*.a

.PHONY: realclean
realclean: clean
	$(RM) -r $(OBJDIR)

# -------------------------
