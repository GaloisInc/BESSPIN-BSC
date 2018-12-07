# BSVSource/Contrib/Makefile


PWD:=$(shell pwd)
PREFIX?=$(abspath $(PWD)/../../../../inst)
LIBDIR=$(PREFIX)/lib
include ../common.mk

# All sub directories and the build order (TLM Early)
DIRS = \
	TLM \
	AHB \
	Axi \
	NonPipelinedMath

.PHONY: clean sanitize realclean local

default: build

build:
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true

install: build
	$(MAKE) -C .. install

realclean clean:
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@;)
	@rm -f *~ *.bo *.ba


sanitize:
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@;)
	@rm -rf Makefile .svn *~ Contrib.bsv *.bo

