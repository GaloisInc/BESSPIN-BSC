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
SIMLIB=$(PREFIX)/lib/SceMi/simtb

# Header files to install
HEADERS = simtb.h

# Install location for the library
LIBDIRI=$(PREFIX)/lib/tcllib
SOLIBDIR=$(LIBDIRI)/lib.$(KERNELNAME)$(W32or64)

# -------------------------

VENDOR = $(TOP)/vendor

# Source location for Tcl include and lib files
TCL_ROOT ?= $(VENDOR)/tcltk/inst
TCL_INCLUDE = $(TCL_ROOT)/include
TCL_LIB     = $(TCL_ROOT)/lib

# Source location for SceMi include files
SCEMILIB=$(PREFIX)/lib/SceMi/BlueNoC

# Local work directory
OBJDIR := obj-$(KERNELNAME)$(W32or64)

# -------------------------

# Ordinary object files (to go in the library)
CXXOBJECTS = \

# Object files (to go in the library) that require building with Tcl
TCLOBJECTS =  \
	simtb.o

CXXOBJECTS := $(addprefix $(OBJDIR)/,$(CXXOBJECTS))
TCLOBJECTS := $(addprefix $(OBJDIR)/,$(TCLOBJECTS))

# -------------------------

# Dependencies
SRC_INC_DIRS = \
	. \
	$(SCEMILIB) \

INC_DEPS = $(foreach dir,$(SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

# Additional dependencies for TCLOBJECTS
TCL_SRC_INC_DIRS = \
	$(TCL_INCLUDE) \

TCL_INC_DEPS = $(foreach dir,$(TCL_SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

# -------------------------

# Compiler FLAGS
#CXXFLAGS += -g
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
	$(foreach dir,$(TCL_SRC_INC_DIRS),-I$(dir)) \
	-L$(TCL_LIB) \
	-ltcl8.5 \

# -------------------------

# top target is shared object file for linking to tcl
.PHONY: all
all: $(OBJDIR)/libsimtb.a

$(OBJDIR)/libsimtb0.a: $(CXXOBJECTS) $(TCLOBJECTS) | $(OBJDIR)
	$(AR) vr $@ $^

# objcopy does not work for OS X
# (Also, objcopy will work even if the user does not have write permission,
# so we need to change the permission or delete the file first, since
# the tcltk library is generated with mode 555 -- no write permission.)
$(OBJDIR)/libttcl8.5.a: $(TCL_LIB)/libtcl8.5.a | $(OBJDIR)
ifeq ($(OSTYPE), Darwin)
	rm -f $(OBJDIR)/libttcl8.5.a
	cp $(TCL_LIB)/libtcl8.5.a $(OBJDIR)/libttcl8.5.a
else
	objcopy --redefine-syms redefs.text \
		$(TCL_LIB)/libtcl8.5.a $(OBJDIR)/libttcl8.5.a
endif

$(OBJDIR)/libsimtb.a: $(OBJDIR)/libsimtb0.a $(OBJDIR)/libttcl8.5.a | $(OBJDIR)
	$(RM) -rf ar_build 
	mkdir ar_build
	$(AR) -x $(OBJDIR)/libsimtb0.a
	$(AR) -x $(OBJDIR)/libttcl8.5.a
	mv *.o ar_build
	$(AR) r $(OBJDIR)/libsimtb.a ar_build/*.o

# Static pattern rule for compiling
$(CXXOBJECTS): $(OBJDIR)/%.o: %.cpp $(INC_DEPS) | $(OBJDIR)
	$(CXX) -c -o $@ $(CXXFLAGS) $<

# TCL object need extra flags
$(TCLOBJECTS): $(OBJDIR)/%.o: %.cpp $(INC_DEPS) $(TCL_INC_DEPS) | $(OBJDIR)
	$(CXX) -c -o $@ $(CXXFLAGS) $(TCLCXXFLAGS) $<

.PHONY: install
install: all
	$(INSTALL) -m 755 -d $(SIMLIB)
	$(INSTALL) -m 644 $(HEADERS) $(SIMLIB)
	$(INSTALL) -m 755 -d $(SOLIBDIR)
	$(INSTALL) -m 755 $(OBJDIR)/libsimtb.a $(SOLIBDIR)

$(OBJDIR):
	mkdir -p $(OBJDIR)

.PHONY: clean
clean:
	$(RM) $(OBJDIR)/*.o $(OBJDIR)/*.oo  $(OBJDIR)/*.so $(OBJDIR)/*.a
	$(RM) -rf ar_build __.SYMDEF\ SORTED
	$(RM) $(EXEFILES)

.PHONY: realclean
realclean: clean
	$(RM) -r $(OBJDIR)

# -------------------------
