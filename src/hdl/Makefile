#

PWD:=$(shell pwd)
TOP:=$(PWD)/../..
INCLUDE?= $(TOP)/util/install

PREFIX?=$(TOP)/inst

.PHONY: all install clean realclean

include $(TOP)/platform.mk

all: install

install:
	@echo "Nothing to be done for install  -- code obsolete"

clean:
	cd scan     && $(MAKE) clean
	cd rtl_edit && $(MAKE) clean
	cd cosim    && $(MAKE) clean

realclean:
	cd scan     && $(MAKE) realclean
	cd rtl_edit && $(MAKE) realclean
	cd cosim    && $(MAKE) realclean
