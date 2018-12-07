
PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX?=$(TOP)/inst

# -------------------------

# Install location for executable
INSTALLDIR = $(PREFIX)/lib/bin

# Executable name
PROGNAME = luminaplus_prompt

# -------------------------

VENDOR = $(TOP)/vendor

# Local directory for the compiled libraries
OBJDIR = obj-$(KERNELNAME)$(W32or64)

# Location for Readback core source include and lib files
RDBACK_ROOT = $(TOP)/src/lib/Readback

# -------------------------

# LuminaPlus files to be compiled into the executable
LUMINAPLUS_CFILES = \
	$(RDBACK_ROOT)/luminaplus/LuminaPlusControl.cpp \

# -------------------------

# Dependencies
SRC_INC_DIRS = \
	. \
	$(RDBACK_ROOT)/core \
	$(RDBACK_ROOT)/luminaplus \

INC_DEPS = $(foreach dir,$(SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

SRC_LIB_DIRS = \
	$(RDBACK_ROOT)/core/$(OBJDIR) \

# XXX be more specific about the lib dependencies?
LIB_DEPS = $(foreach dir,$(SRC_LIB_DIRS),$(wildcard $(dir)/*.so) $(wildcard $(dir)/*.a))

# -------------------------

# Compiler FLAGS
#CXXFLAGS += -g
CXXFLAGS += -O3
CXXFLAGS += -Wall
CXXFLAGS += -fPIC
CXXFLAGS += $(foreach dir,$(SRC_INC_DIRS),-I$(dir))
CXXFLAGS += $(foreach dir,$(SRC_LIB_DIRS),-L$(dir))

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CXXFLAGS += -I/opt/local/include
CXXFLAGS += -L/opt/local/lib
endif

# Compiler library arguments
CXXLIBS += \
	-lreadback \
	-lz \
	-lpthread \

SRC_CFILES = \
	$(PROGNAME).cxx \
	CompileBoolExpr.o \
	LuminaPlus.cxx \

# -------------------------

.PHONY: all
all: $(PROGNAME)

$(PROGNAME): $(SRC_CFILES) $(LUMINAPLUS_CFILES) $(INC_DEPS) $(LIB_DEPS)
	$(CXX) $(CXXFLAGS) -o $@ $(SRC_CFILES) $(LUMINAPLUS_CFILES) $(CXXLIBS)

# XXX Don't release this without a way for the user to rebuild,
# XXX because of the license on gzstream
.PHONY: install
install: all
#	install -d -m755 $(INSTALLDIR)
#	install -m755 $(PROGNAME) $(INSTALLDIR)

.PHONY: realclean
realclean: clean

.PHONY: clean
clean:
	rm -rf *.o
	rm -rf $(PROGNAME)

# -------------------------
