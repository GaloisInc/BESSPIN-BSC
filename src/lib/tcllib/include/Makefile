PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX?=$(TOP)/inst

# -------------------------

# Install locations for headers
TARGETDIR = $(PREFIX)/lib/tcllib/include

HEADERS = \
	bstcl_base.h \

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
