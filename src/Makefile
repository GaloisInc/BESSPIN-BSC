#

PWD:=$(shell pwd)
TOP:=$(PWD)/..
INSTALL?= $(TOP)/util/install

RM = rm -f
LN = ln -sf

PREFIX?=$(TOP)/inst

.PHONY: all install clean realclean

include $(TOP)/platform.mk

.PHONY: all
all: install

install: 
	cd comp   && $(MAKE) PREFIX=$(PREFIX) install
	cd lib    && $(MAKE) PREFIX=$(PREFIX) install
	cd sim    && $(MAKE) PREFIX=$(PREFIX) install
	cd hdl    && $(MAKE) PREFIX=$(PREFIX) install

clean:
	-cd comp   && $(MAKE) clean
	-cd lib    && $(MAKE) clean
	-cd sim    && $(MAKE) clean
	-cd hdl    && $(MAKE) clean

realclean:
	-cd comp   && $(MAKE) realclean
	-cd lib    && $(MAKE) realclean
	-cd sim    && $(MAKE) realclean
	-cd hdl    && $(MAKE) realclean

