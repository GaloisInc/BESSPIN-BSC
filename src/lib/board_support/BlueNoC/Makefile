PWD:=$(shell pwd)
TOP:=$(PWD)/../../../../

include $(TOP)/platform.mk

# The "cadence" directory is needed for "dini"
DIRLIST = \
	prodesign \
	hypersilicon \
	xilinx \
	altera \
	intel \
	cadence \
	dini \
	bridges \
	drivers \
	bluenoc

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
