# Makefile ASSUMES that vendor/tcktk has been built

PWD := $(shell pwd)
TOP := $(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX ?= $(TOP)/inst

RM ?= rm -f
LN ?= ln -sf
INSTALL ?= install

# -------------------------

# Install location for header files
CAPILIB=$(PREFIX)/lib/SceMi/capi

# Header files to install
HEADERS = DutXactor.h TBXactor.h semu_capi.h usertb.h

# Install location for the library
LIBDIRI=$(PREFIX)/lib/tcllib
SOLIBDIR=$(LIBDIRI)/lib.$(KERNELNAME)$(W32or64)

# -------------------------

VENDOR = $(TOP)/vendor

# Source location for Tcl include files
TCL_ROOT ?= $(VENDOR)/tcltk/inst
TCL_INCLUDE = $(TCL_ROOT)/include

# Source location for Bluespec Tcl include files
TCLLIB=$(TOP)/src/lib/tcllib

# Local work directory
OBJDIR := obj-$(KERNELNAME)$(W32or64)

# -------------------------

# Ordinary object files (to go in the library)
CXXOBJECTS = \
	DutXactor.o \
	TBXactor.o \
	semu_capi.o \

# Object files (to go in the library) that require building with Tcl
TCLOBJECTS =  \
        usertb_base.o \

CXXOBJECTS := $(addprefix $(OBJDIR)/,$(CXXOBJECTS))
TCLOBJECTS := $(addprefix $(OBJDIR)/,$(TCLOBJECTS))

# -------------------------

# Dependencies
SRC_INC_DIRS = \
	. \
	$(TOP)/src/lib/SceMi/bsvxactors \
	$(TOP)/src/lib/SceMi/BlueNoC \
	$(TOP)/src/lib/SceMi/semu \
	$(TOP)/src/lib/Readback/core \

INC_DEPS = $(foreach dir,$(SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

# Additional dependencies for TCLOBJECTS
TCL_SRC_INC_DIRS = \
	$(TCL_INCLUDE) \
	$(TCLLIB)/include \

TCL_INC_DEPS = $(foreach dir,$(TCL_SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

# -------------------------

# Compiler FLAGS
#CXXFLAGS += -g
CXXFLAGS += -std=gnu++98
CXXFLAGS += -O3
CXXFLAGS += -Wall
CXXFLAGS += -fPIC
CXXFLAGS += $(foreach dir,$(SRC_INC_DIRS),-I$(dir))

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CXXFLAGS += -I /opt/local/include
endif

# Additional compiler flags for TCLOBJECTS
TCLCXXFLAGS = \
	-DUSE_TCL_STUBS \
	$(foreach dir,$(TCL_SRC_INC_DIRS),-I$(dir)) \

# -------------------------

# top target is shared object file for linking to tcl
.PHONY: all
all: $(OBJDIR)/libsemucapi.a

$(OBJDIR)/libsemucapi.a: $(CXXOBJECTS) $(TCLOBJECTS) | $(OBJDIR)
	$(AR) vr $@ $^

# Static pattern rule for compiling
$(CXXOBJECTS): $(OBJDIR)/%.o: %.cpp $(INC_DEPS) | $(OBJDIR)
	$(CXX) -c -o $@ $(CXXFLAGS) $<

# TCL objects need extra flags
$(TCLOBJECTS): $(OBJDIR)/%.o: %.cpp $(INC_DEPS) $(TCL_INC_DEPS) | $(OBJDIR)
	$(CXX) -c -o $@ $(CXXFLAGS) $(TCLCXXFLAGS) $<

.PHONY: install
install: all
	$(INSTALL) -m 755 -d $(CAPILIB)
	$(INSTALL) -m 644 $(HEADERS) $(CAPILIB)
	$(INSTALL) -m 755 -d $(SOLIBDIR)
	$(INSTALL) -m 755 $(OBJDIR)/libsemucapi.a $(SOLIBDIR)

$(OBJDIR):
	mkdir -p $(OBJDIR)

.PHONY: clean
clean:
	$(RM) $(OBJDIR)/*.o $(OBJDIR)/*.oo  $(OBJDIR)/*.so $(OBJDIR)/*.a
	$(RM) $(EXEFILES)

.PHONY: realclean
realclean: clean
	$(RM) -r $(OBJDIR)

# -------------------------
