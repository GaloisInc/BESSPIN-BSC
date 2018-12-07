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
LIBDIR=$(PREFIX)/lib/tcllib
SOLIBDIR=$(LIBDIR)/lib.$(KERNELNAME)$(W32or64)

RM ?= rm -f
LN ?= ln -sf
INSTALL ?= install

# location for object files
OBJDIR := obj-$(KERNELNAME)$(W32or64)

##########################################################################################
##########################################################################################

VPISO = $(OBJDIR)/cosim.so

PLI_INCS = ./pli_incs

CFLAGS += -g -Wall -fPIC
CFLAGS += -I $(PLI_INCS)
CFLAGS += -fPIC
LD = g++
ifeq ($(OSTYPE), Darwin)
# for Mac OS X, declare that the binary is dynamic and undefined references
# will be looked up later when the library is linked
LDFLAGS = -dynamiclib -undefined dynamic_lookup
else
LDFLAGS = -shared
endif



OBJS = BCosim.o BCPort.o BCModule.o VValue.o PathMap.o DataQueue.o GenModule.o utils.o

OBJS := $(addprefix $(OBJDIR)/,$(OBJS))

all: $(VPISO)  $(OBJDIR)

# Static pattern rule for compiling
$(OBJS): $(OBJDIR)/%.o: %.cpp *.h  $(OBJDIR)
	$(CXX) -c -o $@ $(CFLAGS) $<

$(VPISO): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $+

.PHONY: $(OBJDIR)
$(OBJDIR):
	mkdir -p $(OBJDIR)

.PHONY: install
install: all
	$(INSTALL) -m 755 -d $(SOLIBDIR)
	$(INSTALL) -m 644 $(VPISO) $(SOLIBDIR)

.PHONY: clean
clean:
	$(RM) *.o $(OBJDIR)/* TAGS

.PHONY: realclean
realclean: clean
	$(RM) *.a *.so
	$(RM) -r obj*

