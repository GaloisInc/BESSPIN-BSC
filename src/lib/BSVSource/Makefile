# BSVSource/Makefile


PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

PREFIX?=$(abspath $(PWD)/../../../inst)
LIBDIR=$(abspath $(PREFIX)/lib)

# These flags are passed to makes in the BSVSource area.
include common.mk


DIRS = \
	Altera \
	Ahb \
	Apb \
	Axi \
	Axi4 \
	BlueNoC \
	Bus \
	Contexts \
	Contrib \
	DDR2 \
	Dini \
	Intel \
	Math \
	Misc \
	Readback \
	SceMi \
	TLM3 \
	Xactors \
	Xilinx

# Removed this directory from the BSVSource tree release
NUKE_DIR = \
	Readback

# Build order for the BSV libraries
BUILD_ORDER= \
	Misc \
	Altera \
	Contexts \
	Math \
	DDR2 \
	Bus \
	TLM3 \
	Ahb \
	Apb \
	Axi \
	Axi4 \
	Contrib \
	BlueNoC \
	Xilinx \
	Dini \
	Intel \
	SceMi \
	Xactors \
	Readback

# Find files to build tags -- everything below except Contrib
ALLSRCS = $(shell find . -name "*.bsv")
CTRIBSRCS = $(shell find ./Contrib -name "*.bsv")
SRCS = $(filter-out $(CTRIBSRCS), $(ALLSRCS))
SRCS+= $(wildcard ../Libraries/*.bsv)
SRCS+= $(wildcard ../Prelude/*.bsv)

.PHONY: clean sanitize realclean build

all: install TAGS

build:
	$(foreach dir, $(BUILD_ORDER), $(MAKE) -C $(dir) $@ &&) true

install:
	$(MAKE) -C ../Libraries install

TAGS: $(SRCS)
	../../../util/scripts/btags $+

realclean clean:
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@;)
	@rm -f *~ TAGS

sanitize:
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@;)
	@rm -rf Makefile common.mk .svn *~ $(NUKE_DIR)
