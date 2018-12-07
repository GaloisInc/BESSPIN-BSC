# Makefile for Verific-based scanb chain insertion
# Makefile ASSUMES that vendor has been built.   Dependencies are not checked

.PHONY: default
default: all

PWD := $(shell pwd)
TOP := $(PWD)/../../..

VENDOR = $(TOP)/vendor

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX ?= $(TOP)/inst
BINDIR=$(PREFIX)/lib/bin/$(KERNELNAME)$(W32or64)

RM ?= rm -f
LN ?= ln -sf
INSTALL ?= install

# location for object files
OBJDIR := obj-$(KERNELNAME)$(W32or64)
EXEC = addscan

####################   Setup for Verific ###########################
VERIFIC ?= $(TOP)/vendor/verific
VERIFIC_SRC =  verilog util containers vhdl
VERIFIC_DIRS = $(addprefix $(VERIFIC)/,$(VERIFIC_SRC))
VERIFIC_INC = $(VERIFIC)/include
VERIFIC_LIBS =  $(foreach V,$(VERIFIC_SRC),$(VERIFIC)/lib-$(KERNELNAME)$(W32or64)/$(V)-$(KERNELNAME)$(W32or64).a)

CXXFLAGS += -I. -I $(VERIFIC_INC)
LOADLIBES += -L $(VERIFIC_LIBS)

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CXXFLAGS += -I /opt/local/include
endif

################ BOOST link ############################
BOOST_LIB = boost_regex-mt
ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
BOOST_LIBDIR = -L/opt/local/lib
else
BOOST_LIBDIR = 
endif


# Common makefile code for linking in FlexLM licensing
# target is FLEXOBJ and is named here
FLEXOBJ = $(OBJDIR)/hdledit_ccore.o
LM_BSC_DIR ?= $(TOP)/vendor/flexlm/cinterface
include $(LM_BSC_DIR)/Makefile.inc


# Compiler FLAGS
CFLAGS += -g
CFLAGS += -Wall
CFLAGS += -I . -I $(VERIFIC_INC) -I $(LM_BSC_DIR)
CFLAGS += -fPIC

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CFLAGS += -I /opt/local/include
endif

#  Listing of object files
CXXOBJECTS = main.o utils.o process.o Scan.o VUtils.o options.o ModuleDesc.o Dbg.o Globals.o

# # Special handling for files requiring the tcl.h interface
# TCLOBJECTS =  \
# 	CktEdits.o \
# 	CktEdits_paramfile.o \
# 	EditCompile.o \
# 	TclUtils.o \
# 	TclModelUtils.o \
# 	tclModifier.o \
# 	tclHdlEdit.o \

OBJECTS = $(CXXOBJECTS)

CXXOBJECTS := $(addprefix $(OBJDIR)/,$(CXXOBJECTS))

# top target is shared object file for linking to tcl
all: $(OBJDIR) $(OBJDIR)/$(EXEC)


# link to shared object
# Note that the return status is ignored to work around a chroot bug in releases...
$(OBJDIR)/$(EXEC): $(CXXOBJECTS)  $(FLEXOBJ) $(VERIFIC_LIBS) 
	-$(CXX) -o $@ $+  $(VERIFIC_LIBS) $(BOOST_LIBDIR) -l$(BOOST_LIB) -lpthread

# Static pattern rule for compiling
$(CXXOBJECTS): $(OBJDIR)/%.o: %.cpp *.h $(wildcard $(VERIFIC_INC)/*.h)
	$(CXX) -c -o $@ $(CFLAGS) $<

TAGS: *.h *.cpp $(VERIFIC_INC)/*.h $(addsuffix /*.cpp, $(addprefix $(VERIFIC)/verificsvn/, $(VERIFIC_SRC)))
	etags $+


.PHONY: install
install: all
	$(INSTALL) -m 755 -d $(BINDIR)
	$(INSTALL) -m 755 $(OBJDIR)/$(EXEC) $(BINDIR)
	$(INSTALL) -m 755 $(EXEC).trampoline $(BINDIR)/../$(EXEC)

.PHONY: $(OBJDIR)
$(OBJDIR):
	mkdir -p $(OBJDIR)


.PHONY: clean
clean:
	$(RM) *.o $(OBJDIR)/* TAGS

.PHONY: realclean
realclean: clean
	$(RM) *.a *.so

