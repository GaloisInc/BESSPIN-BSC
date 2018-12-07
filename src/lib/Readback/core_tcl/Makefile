# Makefile ASSUMES that vendor/tcktk has been built

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX?=$(TOP)/inst

# -------------------------

# Install location for header files
TARGETDIR = $(PREFIX)/lib/tcllib/include

# Header files to install
HEADERS = \
	designtcl.h \

# Install location for libraries
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

# Location for Readback source include and lib files
RDBACK_ROOT = $(TOP)/src/lib/Readback/core

# Readback libraries to include in the Semu libraries
RDBACK_OBJ = $(RDBACK_ROOT)/$(OBJDIR)/libreadback.a
RDBACK_OBJ_NOGZ = $(RDBACK_ROOT)/$(OBJDIR)/libreadback_nogz.a

# -------------------------

OBJECTS = \
	designtcl.o \

OBJECTS := $(addprefix $(OBJDIR)/,$(OBJECTS))

# -------------------------

# Dependencies
SRC_INC_DIRS = \
	. \
	$(RDBACK_ROOT) \
	$(TCL_ROOT)/include \
	$(TCLLIB)/include \

INC_DEPS = $(foreach dir,$(SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

# -------------------------

# Compiler FLAGS
#CXXFLAGS += -g
CXXFLAGS += -O3
CXXFLAGS += -Wall
CXXFLAGS += -fPIC
CXXFLAGS += -DUSE_TCL_STUBS
CXXFLAGS += $(foreach dir,$(SRC_INC_DIRS),-I$(dir))

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CXXFLAGS += -I/opt/local/include
endif

# -------------------------

# Libraries to be created
LIBS = \
	libdesign.a \
	libdesign_nogz.a \

LIBS := $(addprefix $(OBJDIR)/,$(LIBS))

.PHONY: all
all: $(LIBS)

$(OBJECTS): $(OBJDIR)/%.o: %.cpp $(INC_DEPS) | $(OBJDIR)
	$(CXX) -c -o $@ $(CXXFLAGS) $<

$(OBJDIR)/libdesign.a: $(OBJECTS) $(RDBACK_OBJ) | $(OBJDIR)
	cp $(RDBACK_OBJ) $@
	$(AR) vq $@ $(OBJECTS)

$(OBJDIR)/libdesign_nogz.a: $(OBJECTS) $(RDBACK_OBJ_NOGZ) | $(OBJDIR)
	cp $(RDBACK_OBJ_NOGZ) $@
	$(AR) vq $@ $(OBJECTS)

$(OBJDIR):
	mkdir -p $(OBJDIR)

.PHONY: install
install: all
	install -d -m755 $(TARGETDIR)
	install -m644 $(HEADERS) $(TARGETDIR)
	install -d -m755 $(SOLIBDIR)
	install -m 755 $(LIBS) $(SOLIBDIR)

.PHONY: realclean
realclean: clean

.PHONY: clean
clean:
	rm -rf $(OBJDIR)

# -------------------------
