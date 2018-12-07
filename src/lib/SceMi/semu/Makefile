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
	bsdebug_common.cpp \
	bsdebug_common.h \
	SceMiProbes.cpp \
	ReadBackControl.h \
	SimTb.cpp \
	VcdUtils.cpp \
	VcdUtils.h \

# -------------------------

.PHONY: all
all:

.PHONY: install
install: all
	install -d -m755 $(TARGETDIR)
	install -m644 $(HEADERS) $(TARGETDIR)

.PHONY: clean realclean
clean realclean:

# -------------------------
