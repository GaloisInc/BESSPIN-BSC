PWD:=$(shell pwd)
TOP=$(PWD)/../../../../..

include $(TOP)/platform.mk

DIRLIST = \
	verilog

RELEASEDIRS = $(DIRLIST)

.PHONY: install
install:
	for dir in $(RELEASEDIRS) ; do \
		${MAKE} -C $$dir install; \
	done

.PHONY: clean realclean
clean realclean:
	for dir in $(DIRLIST) ; do \
		${MAKE} -C $$dir $@ ; \
	done

