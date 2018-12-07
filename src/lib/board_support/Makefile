PWD:=$(shell pwd)
TOP:=$(PWD)/../../..

include $(TOP)/platform.mk

DIRLIST = \
	BlueNoC \
	Classic \
	scripts \
	workstation \


RELEASEDIRS = $(DIRLIST)

.PHONY: install
install:
	for dir in $(RELEASEDIRS) ; do \
		${MAKE} -C $$dir install; \
	done

.PHONY: clean realclean
realclean clean:
	for dir in $(DIRLIST) ; do \
		${MAKE} -C $$dir $@ ; \
	done
