# Makefile ASSUMES that vendor/tcktk has been built

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX?=$(TOP)/inst

# -------------------------

# Install location for library
TCLLIBDIR = $(shell $(TOP)/util/scripts/bsenv tcl_libdir)
SOLIBDIR = $(PREFIX)/lib/tcllib/$(TCLLIBDIR)

# -------------------------

VENDOR = $(TOP)/vendor

# Local directory for the compiled libraries
OBJDIR = obj-$(KERNELNAME)$(W32or64)

# Location for Tcl source include and lib files
TCL_ROOT = $(VENDOR)/tcltk/inst

# Source location for Bluespec Tcl include files
TCLLIB=$(TOP)/src/lib/tcllib

# Location for Readback core and core_tcl source include and lib files
RDBACK_ROOT = $(TOP)/src/lib/Readback

# Location for FTDI source include and lib files
FTDI_ROOT = $(VENDOR)/libftdi/inst

# Source location for JTAG files
JTAG_DIR = $(RDBACK_ROOT)/jtag

# -------------------------

# JTAG files to be compiled into the library
JTAG_CFILES = \
	$(JTAG_DIR)/log.cpp \
	$(JTAG_DIR)/jtag.cpp \
	$(JTAG_DIR)/cable.cpp \
	$(JTAG_DIR)/sim.cpp \
	$(JTAG_DIR)/control.cpp \
	$(JTAG_DIR)/fpga.cpp \
	$(JTAG_DIR)/xpc.cpp \
	$(abspath $(wildcard $(JTAG_DIR)/xc*.cpp)) \
	$(abspath $(wildcard $(JTAG_DIR)/ft*.cpp)) \

# -------------------------

# Dependencies
SRC_INC_DIRS = \
	. \
	$(RDBACK_ROOT)/core \
	$(RDBACK_ROOT)/core_tcl \
	$(TCL_ROOT)/include \
	$(TCLLIB)/include \
	$(FTDI_ROOT)/include \
	$(JTAG_DIR) \

INC_DEPS = $(foreach dir,$(SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

SRC_LIB_DIRS = \
	$(RDBACK_ROOT)/core_tcl/$(OBJDIR) \
	$(TCL_ROOT)/lib \
	$(FTDI_ROOT)/lib \

# XXX be more specific about the lib dependencies?
LIB_DEPS = $(foreach dir,$(SRC_LIB_DIRS),$(wildcard $(dir)/*.so) $(wildcard $(dir)/*.a))

# -------------------------

# Compiler FLAGS
#CXXFLAGS += -g
CXXFLAGS += -O3
CXXFLAGS += -Wall
CXXFLAGS += -fPIC
CXXFLAGS += -DUSE_TCL_STUBS
CXXFLAGS += $(foreach dir,$(SRC_INC_DIRS),-I$(dir))
CXXFLAGS += $(foreach dir,$(SRC_LIB_DIRS),-L$(dir))

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CXXFLAGS += -I/opt/local/include
CXXFLAGS += -L/opt/local/lib
endif

# Compiler library arguments
CXXLIBS += \
	-ltclstub8.5 \
	-lz \
	-lusb \
	-lftdi \

ifeq ($(OSTYPE), Darwin)
SHARED ?= -dynamiclib -undefined dynamic_lookup
else
SHARED ?= -shared
endif

# -------------------------

# Libraries to be created
LIBS = \
	libsemulite.so \
	libsemulite_nogz.so \

LIBS := $(addprefix $(OBJDIR)/,$(LIBS))

.PHONY: all
all: $(LIBS)

$(OBJDIR)/libsemulite.so: SemuLiteTcl.cpp $(JTAG_CFILES) $(INC_DEPS) $(LIB_DEPS) | $(OBJDIR)
	$(CXX) -o $@ $(SHARED) $(CXXFLAGS) $< $(JTAG_CFILES) -ldesign $(CXXLIBS)

$(OBJDIR)/libsemulite_nogz.so: SemuLiteTcl.cpp $(JTAG_CFILES) $(INC_DEPS) $(LIB_DEPS) | $(OBJDIR)
	$(CXX) -o $@ $(SHARED) $(CXXFLAGS) $< $(JTAG_CFILES) -ldesign_nogz $(CXXLIBS)

$(OBJDIR):
	mkdir -p $(OBJDIR)

.PHONY: install
install: all
	install -d -m755 $(SOLIBDIR)
	install -m755 $(LIBS) $(SOLIBDIR)

.PHONY: realclean
realclean: clean

.PHONY: clean
clean:
	rm -rf $(OBJDIR)

# -------------------------
