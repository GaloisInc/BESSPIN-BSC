# Makefile for Verific-based HDL analyzer
# Makefile ASSUMES that vendor/tcktk has been built.   Dependencies are not checked

.PHONY: default
default: all

PWD := $(shell pwd)
TOP := $(PWD)/../../..

VENDOR = ../../../vendor

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX ?= $(TOP)/inst
LIBDIR=$(PREFIX)/lib/tcllib
BINDIR=$(PREFIX)/bin
SOLIBDIR=$(LIBDIR)/lib.$(KERNELNAME)$(W32or64)
EXEDIR=$(PREFIX)/lib/bin
BSCDIR=$(EXEDIR)/$(KERNELNAME)$(W32or64)

RM ?= rm -f
LN ?= ln -sf
INSTALL ?= install

# location for object files
OBJDIR := obj-$(KERNELNAME)$(W32or64)

####################   Setup for Verific ###########################
VERIFIC ?= $(TOP)/vendor/verific
VERIFIC_SRC =  verilog util containers
VERIFIC_DIRS = $(addprefix $(VERIFIC)/,$(VERIFIC_SRC))
VERIFIC_INC = $(VERIFIC)/include
VERIFIC_LIBS =  $(foreach V,$(VERIFIC_SRC),$(VERIFIC)/lib-$(KERNELNAME)$(W32or64)/$(V)-$(KERNELNAME)$(W32or64).a)
VCDWRITER_INC = 

CXXFLAGS += -I. -I $(VERIFIC_INC)
LOADLIBES += -L $(VERIFIC_LIBS)

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CXXFLAGS += -I /opt/local/include
endif

################################# Location for TCL include files ###############
TCL_ROOT ?= $(VENDOR)/tcltk/inst
TCL_INCLUDE = $(TCL_ROOT)/include
TCL_LIB = $(TCL_ROOT)/lib

################ BOOST link ############################

ifeq ($(OSTYPE), Darwin)

  ifneq ($(wildcard /opt/local/include/boost/.*),)
    $(info Using MacPorts boost area)
    BOOST_INC = -I/opt/local/include/boost/
    ifeq ($(wildcard /opt/local/lib/libboost_regex-mt.a),)
      BOOST_LIB = /opt/local/lib/libboost_regex.a
    else
      BOOST_LIB = /opt/local/lib/libboost_regex-mt.a
    endif
  else
    $(info Using HomeBrew boost area)
    BOOST_INC = -I/usr/local/include/boost/
    ifeq ($(wildcard /usr/local/lib/libboost_regex-mt.a),)
      BOOST_LIB = /usr/local/lib/libboost_regex.a
    else
      BOOST_LIB = /usr/local/lib/libboost_regex-mt.a
    endif
  endif

else

# non-Mac
BOOST_LIB = -lboost_regex

endif


################ FLEX ############################
# Common makefile code for linking in FlexLM licensing
# target is FLEXOBJ and is named here
FLEXOBJ = $(OBJDIR)/hdledit_ccore.o
LM_BSC_DIR ?= $(TOP)/vendor/flexlm/cinterface
include $(LM_BSC_DIR)/Makefile.inc


# Compiler FLAGS
CFLAGS += -g
CFLAGS += -Wall
CFLAGS += -I . $(BOOST_INC) -I $(VERIFIC_INC) -I $(LM_BSC_DIR)
CFLAGS += -fPIC

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CFLAGS += -I /opt/local/include
endif

#  Listing of object files
CXXOBJECTS = \
	HdlUtils.o \
	TestBenchGenerator.o \
	ApplyChanges.o \
	CktMod.o \
	ECPass0.o \
	ECPass12.o \
	ScanPath.o \
	Types.o \
	VeriNodeInfo.o \

# Special handling for files requiring the tcl.h interface
TCLOBJECTS =  \
	CktEdits.o \
	CktEdits_paramfile.o \
	CrossReference.o \
	EditCompile.o \
	EditCompilePartition.o \
	EditCompilePartitionData.o \
	TclUtils.o \
	TclModelUtils.o \
	tclModifier.o \
	tclHdlEdit.o \

OBJECTS = $(TCLOBJECTS) $(CXXOBJECTS)

CXXOBJECTS := $(addprefix $(OBJDIR)/,$(CXXOBJECTS))
TCLOBJECTS := $(addprefix $(OBJDIR)/,$(TCLOBJECTS))

ifeq ($(OSTYPE), Darwin)
SHARED ?= -dynamiclib -undefined dynamic_lookup
EXEFLAG ?=  -undefined dynamic_lookup
else
SHARED ?= -shared
EXEFLAG ?=
endif

EXEFILES = \
	listVlogFiles

# top target is shared object file for linking to tcl
all: $(OBJDIR) $(OBJDIR)/libbhdledit.so $(EXEFILES)

# link to shared object
$(OBJDIR)/libbhdledit.so: $(CXXOBJECTS)  $(TCLOBJECTS) $(FLEXOBJ) $(VERIFIC_LIBS)
	$(CXX) -o $@ $(SHARED) $+  $(VERIFIC_LIBS) $(TCL_LIB)/libtclstub8.5.a $(BOOST_LIB)

# Static pattern rule for compiling
$(CXXOBJECTS): $(OBJDIR)/%.o: %.cxx *.h $(wildcard $(VERIFIC_INC)/*.h)
	$(CXX) -c -o $@ $(CFLAGS) $<

# TCL object need extra flags
$(TCLOBJECTS): $(OBJDIR)/%.o: %.cxx *.h $(wildcard $(VERIFIC_INC)/*.h)
	$(CXX) -c  -o $@  $(CFLAGS) -DUSE_TCL_STUBS  -I$(TCL_INCLUDE) $<


TAGS: *.h *.cxx $(VERIFIC_INC)/*.h $(wildcard $(addsuffix /*.cpp, $(addprefix $(VERIFIC)/verificsvn/, $(VERIFIC_SRC))))
	etags $+

genvch: NetAliases.cxx $(CXXOBJECTS) $(TCLOBJECTS) $(FLEXOBJ) $(VERIFIC_LIBS)
	$(CXX) -o $@ $(CXXFLAGS) $< $(EXEFLAG) $(CXXOBJECTS) $(TCLOBJECTS) $(FLEXOBJ) $(VERIFIC_LIBS) $(TCL_LIB)/libtclstub8.5.a $(BOOST_LIB) -lpthread


listVlogFiles: listVlogFiles.cxx Types.cxx HdlUtils.cxx ScanPath.cxx $(VERIFIC_LIBS)
	$(CXX) -o $@ $(CXXFLAGS) $(CFLAGS) $(EXEFLAG) $(BOOST_INC) $< Types.cxx HdlUtils.cxx ScanPath.cxx $(VERIFIC_LIBS) $(BOOST_LIB)


.PHONY: install
install: all install-genvch
	$(INSTALL) -m 755 -d $(SOLIBDIR)
	$(INSTALL) -m 644 $(OBJDIR)/libbhdledit.so $(SOLIBDIR)

install-genvch: genvch genvch-wrapper1 genvch-wrapper $(BINDIR) $(EXEDIR)
	$(INSTALL) -m 755 -d $(BSCDIR)
	$(INSTALL) -m 755 genvch $(BSCDIR)/genvch
	$(INSTALL) -m 755 -d $(EXEDIR)
	$(INSTALL) -m 755 genvch-wrapper1 $(EXEDIR)/genvch
	$(INSTALL) -m 755 -d $(BINDIR)
	$(INSTALL) -m 755 genvch-wrapper $(BINDIR)/genvch


$(OBJDIR):
	mkdir -p $(OBJDIR)


.PHONY: clean
clean:
	$(RM) *.o $(OBJDIR)/*.o TAGS $(EXEFILES)

.PHONY: realclean
realclean: clean
	$(RM) -r $(OBJDIR)
