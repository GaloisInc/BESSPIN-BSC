# -*- makefile -*-

RELEASE_NUM ?= latest
RELEASE_DATE ?= $(shell date +%Y%m%d)
DEBIAN_REVISION ?= 1

UPSTREAM_VERSION = $(RELEASE_DATE)
SOURCE_VERSION = $(UPSTREAM_VERSION)-$(DEBIAN_REVISION)

TEMPLATE_FILES = $(wildcard *.template)
CONTROL_FILES = $(patsubst bluespec-RELEASE_NUM%.template,bluespec-$(RELEASE_NUM)%, $(filter bluespec-RELEASE_NUM%.template,$(TEMPLATE_FILES)))
CONTROL_FILES += $(patsubst %.template,%,$(filter-out bluespec-RELEASE_NUM%.template,$(TEMPLATE_FILES)))
CONTROL_FILES += changelog

SRC_COMPONENTS =
SRC_TAR_LIST = bluespec-$(RELEASE_NUM)_$(UPSTREAM_VERSION).orig.tar.xz
SRC_TAR_LIST += $(patsubst %,bluespec-$(RELEASE_NUM)_$(UPSTREAM_VERSION).orig-%.tar.xz, $(SRC_COMPONENTS))

default:
	@echo "No default.  Try 'make help'."

help:
	@echo
	@echo "Usage:  make <vars> <target>"
	@echo
	@echo "Where <vars> may be zero or more of the following:"
	@echo "  RELEASE_NUM=<num>        - The release number"
	@echo "  RELEASE_DATE=<date>      - The release version"
	@echo "  DEBIAN_REVISION=<rev>    - The Debian packave revision"
	@echo
	@echo "And <target> is one or more of the following:"
	@echo "  all      - Create Debian packaging configuration"
	@echo "  orig     - Create source tar archives"
	@echo "  clean    - Remove automatically generated package configuration"
	@echo
	@echo "Multiple targets may be combined.  The <vars>, if specified, must be"
	@echo "consistent between calls to all targets."
	@echo
	@echo
	@false

#orig: $(SRC_TAR_LIST)

#$(SRC_TAR_LIST):

orig:
	./export $(RELEASE_NUM) $(UPSTREAM_VERSION)

deb:
	cd .. && dpkg-buildpackage -i -us -uc -b

changelog:
	cd .. && dch --create --package bluespec-$(RELEASE_NUM) --newversion $(SOURCE_VERSION) --distribution stable "Bluespec release." --controlmaint

expand-template = \
cat $<  \
 | sed "s/__RELEASE_NUM__/$(RELEASE_NUM)/g"  \
 | sed "s/__RELEASE_DATE__/$(RELEASE_DATE)/g"  \
 > $@

rules: rules.template
	$(expand-template)
	chmod +x rules

%: %.template
	$(expand-template)

bluespec-$(RELEASE_NUM)%: bluespec-RELEASE_NUM%.template
	$(expand-template)

all: $(CONTROL_FILES)

clean:
	$(RM) $(CONTROL_FILES)

.PHONY: default orig deb clean
