.DEFAULT_GOAL := tarballs
#do not run tests by default because they are currently
#broken because of asking for chroot password in parallel
#make

#none of the sub-makes should be affected by 
#what this make is called with
unexport MAKEFLAGS

#make -f sargebuild.mk -j -k -r --warn-undefined all
#recommended run with -r and (-d for debugging?)
# --debug=b

.PHONY: beta
beta: normal-tarball testsuite-tarball source-tarball

.PHONY: tarballs-without-checking
tarballs-without-checking: normal-tarball testsuite-tarball variant-tarballs \
 source-tarball

ifndef VERSION
#Colons in directory names confuse make,
# especially if a directory
#appears as a target, as it does in bsc/src/Makefile.
$(error need a VERSION)
endif

BSC_BUILD = NORMAL
SVNTAG = HEAD
SVNURL = https://svn.bluespec.com:8080/bs/bsc/trunk
SVNFLAGS = -q --non-interactive
SVN = svn

BUILD_TMP = /tmp
BUILD_WORK_DIR = $(BUILD_TMP)/build-bsc-$(VERSION)
CHROOT_BASE = /home/2013-sarge/
CHROOT32_DIR = $(CHROOT_BASE)32
CHROOT64_DIR = $(CHROOT_BASE)64

# this is used both in the build chroot and 
# the testing chroot
CHROOT_USERNAME = tinderbox
CHROOT_UID = 1061

TARBALL_OUTPUT_DIR = $(BUILD_WORK_DIR)

BUILD_BUILD_DIR = $(BUILD_WORK_DIR)/build
BUILD_LOG_DIR = $(BUILD_WORK_DIR)/logs

PERL = perl

RELEASE_SCRIPTS = $(BUILD_BUILD_DIR)/unpack/bsc/util/release

COMPARE_RELEASE_TARBALLS = $(RELEASE_SCRIPTS)/compare-release-tarballs.sh
$(BUILD_BUILD_DIR)/unpack/bsc/util/release/compare-release-tarballs.sh: unpack-tarball-natively

DKMS_GENERATE = $(RELEASE_SCRIPTS)/dkms-generate.pl
$(BUILD_BUILD_DIR)/unpack/bsc/util/release/dkms-generate.pl: unpack-tarball-natively

SPEC_GENERATE_BLUENOC = $(RELEASE_SCRIPTS)/spec-generate-bluenoc.pl
$(BUILD_BUILD_DIR)/unpack/bsc/util/release/spec-generate-bluenoc.pl: unpack-tarball-natively

SPEC_GENERATE_DEPS = $(RELEASE_SCRIPTS)/spec-generate-deps.pl
$(BUILD_BUILD_DIR)/unpack/bsc/util/release/spec-generate-deps.pl: unpack-tarball-natively

SPEC_GENERATE_EXTERNAL = $(RELEASE_SCRIPTS)/spec-generate-external.pl
$(BUILD_BUILD_DIR)/unpack/bsc/util/release/spec-generate-external.pl: unpack-tarball-natively

.PHONY: tarballs
tarballs: tarballs-without-checking $(COMPARE_RELEASE_TARBALLS)
	$(COMPARE_RELEASE_TARBALLS) $(BUILD_WORK_DIR)

.PHONY: create-build-work-dir
create-build-work-dir: $(BUILD_WORK_DIR) $(BUILD_LOG_DIR) $(BUILD_BUILD_DIR)

$(BUILD_WORK_DIR):
	mkdir $(BUILD_WORK_DIR)

$(BUILD_LOG_DIR): $(BUILD_WORK_DIR)
	mkdir $(BUILD_LOG_DIR)

$(BUILD_BUILD_DIR): $(BUILD_WORK_DIR)
	mkdir $(BUILD_BUILD_DIR)

BUILD_CHROOT_DIR = /tmp/build-bsc-$(VERSION)
DIR32=$(CHROOT32_DIR)$(BUILD_CHROOT_DIR)
DIR64=$(CHROOT64_DIR)$(BUILD_CHROOT_DIR)

SVNCHECKOUTDIR=$(DIR32)/svn-checkout

.PHONY: create-checkout-dir
create-checkout-dir: create-build-work-dir $(DIR32)
	$(AS_TINDER) mkdir $(SVNCHECKOUTDIR)
	mkdir $(BUILD_BUILD_DIR)/svn-checkout

#manually set BSCTARBALL if you wish to avoid checkout
ifdef BSCTARBALL
GET_TARBALL=
else
GET_TARBALL=checkout
endif

BSCTARBALL = $(FINISHED_SOURCE_TARBALL)

#This will do a checkout using the credentials of
#/home/sarge32/home/tinderbox/.subversion
#which (as of 2009-oct) is user "ken".
.PHONY: checkout
checkout: create-checkout-dir
	$(CHROOT32) $(CHROOT32_DIR) \
	 sudo -u $(CHROOT_USERNAME) -H \
	 $(SVN) $(SVNFLAGS) co -r $(SVNTAG) \
	 $(SVNURL) $(BUILD_CHROOT_DIR)/svn-checkout/bsc
	tar czf $(BSCTARBALL) -C $(SVNCHECKOUTDIR) bsc
	chmod a+r $(BSCTARBALL)

FINISHED_SOURCE_TARBALL = $(TARBALL_OUTPUT_DIR)/source-$(VERSION).tar.gz
.PHONY: source-tarball
source-tarball: $(FINISHED_SOURCE_TARBALL)

$(FINISHED_SOURCE_TARBALL): $(GET_TARBALL) $(TARBALL_OUTPUT_DIR)
	if [ $(FINISHED_SOURCE_TARBALL) != $(BSCTARBALL) ] ; then cp $(BSCTARBALL) $@ ; fi

BUILD_LICENSE_DIR = /raid/tinderbox/License
BUILD_LICENSE_NAME = tinderbox.lic
BUILD_LICENSE_FILE = $(BUILD_LICENSE_DIR)/$(BUILD_LICENSE_NAME)

#Override this to 27000@license if you wish.  Currently not done
#because networking seems flaky inside the chroot.
BUILD_LICENSE = $(BUILD_CHROOT_DIR)/$(BUILD_LICENSE_NAME)

AS_TINDER=$(SUDO) -u $(CHROOT_USERNAME)

$(DIR32):
	$(AS_TINDER) mkdir $@

$(DIR64):
	$(AS_TINDER) mkdir $@

.PHONY: license32
license32: $(DIR32)/$(BUILD_LICENSE_NAME)

.PHONY: license64
license64: $(DIR64)/$(BUILD_LICENSE_NAME)

$(DIR32)/$(BUILD_LICENSE_NAME): $(DIR32)
	$(AS_TINDER) cp $(BUILD_LICENSE_FILE) $(DIR32)

$(DIR64)/$(BUILD_LICENSE_NAME): $(DIR64)
	$(AS_TINDER) cp $(BUILD_LICENSE_FILE) $(DIR64)

#note we rely on the the fact that the
# UID of user tinderbox is the
#same inside and outside the chroot (1061)

#some weirdness about the sticky
# bit (maybe?) requires chroot
CHOWN_R=$(SUDO) chown -R $(CHROOT_USERNAME)

#non-C locales have not been installed in the chroots,
#so programs like Perl with have warnings unless run in the
#C locale.  This does slightly bias testing...
SUDO=sudo
PRODUCT = Bluespec-$(VERSION)
BUILD_PRODUCT_DIR = $(BUILD_BUILD_DIR)/$(PRODUCT)
CHROOT_TEMPLATE = LANG=C sudo $(1) chroot
CHROOT32 = $(call CHROOT_TEMPLATE,linux32)
CHROOT64 = $(call CHROOT_TEMPLATE,linux64)


#set STRIP=true to disable strip
STRIP = strip


BUILD_PARALLEL=1

#This is the only one where we build the libraries,
#hence "primary"
BUILD_PRIMARY=$(BUILD_CHROOT_DIR)/primary/$(PRODUCT)
BUILD_FLAGS=
GHC_VERSION=7.6.3
#Platform-dependent Libraries
define LIBRARIES_TEMPLATE
	make -j1 -C src/sim realclean \
	 > ../40clean-sim$(2).log 2>&1 ;\
	make -j1 -C src/sim BSC_BUILD=$(1) \
	 $(BUILD_FLAGS) PREFIX=$(3) \
	 install > ../41sim$(2).log 2>&1;\
	make -j1 -C src/lib/VPI realclean \
	 > ../50clean-vpi$(2).log 2>&1 ;\
	make -j1 -C src/lib/VPI BSC_BUILD=$(1) \
	 $(BUILD_FLAGS) PREFIX=$(3)\
	 install > ../51vpi$(2).log 2>&1;\
	make -j1 -C src/lib/Readback realclean > ../55clean-readback$(2).log 2>&1 ;\
	make -j1 -C src/lib/Readback BSC_BUILD=$(1) $(BUILD_FLAGS) PREFIX=$(3)\
	 install > ../56readback$(2).log 2>&1;\
	make -j1 -C src/lib/BlueNoC realclean > ../60clean-bluenoc$(2).log 2>&1 ;\
	make -j1 -C src/lib/BlueNoC BSC_BUILD=$(1) $(BUILD_FLAGS) PREFIX=$(3)\
	 install > ../61bluenoc$(2).log 2>&1;\
	make -j1 -C src/lib/SceMi realclean > ../70clean-scemi$(2).log 2>&1 ;\
	make -j1 -C src/lib/SceMi BSC_BUILD=$(1) $(BUILD_FLAGS) PREFIX=$(3)\
	 install > ../71scemi$(2).log 2>&1;\
	make -j1 -C vendor/stp realclean > ../80clean-stp$(2).log 2>&1 ;\
	make -j1 -C vendor/stp BSC_BUILD=$(1) $(BUILD_FLAGS) PREFIX=$(3)\
	 install > ../81stp$(2).log 2>&1;\
	make -j1 -C vendor/yices realclean > ../82clean-yices$(2).log 2>&1 ;\
	make -j1 -C vendor/yices BSC_BUILD=$(1) $(BUILD_FLAGS) PREFIX=$(3)\
	 install > ../83yices$(2).log 2>&1;\

endef

.PHONY: build-bsc-32bit
build-bsc-32bit: $(GET_TARBALL) $(DIR32) license32
	$(AS_TINDER) mkdir $(DIR32)/primary
	$(AS_TINDER) tar xzf $(BSCTARBALL) -C $(DIR32)/primary
	mkdir -p $(BUILD_LOG_DIR)/1primary
	ln -s $(DIR32)/primary/20install.log \
	 $(BUILD_LOG_DIR)/1primary
	$(CHROOT32) $(CHROOT32_DIR) \
	sudo -u $(CHROOT_USERNAME) -H bash -c "\
	set -x;\
	set -e;\
	export PATH=/tmp/$(GHC_VERSION)/bin:\"\$$PATH\";\
	export BLUESPEC_LICENSE_FILE=$(BUILD_LICENSE) ;\
	cd $(BUILD_CHROOT_DIR)/primary/bsc;\
	make -j1 BSC_BUILD=$(BSC_BUILD) $(BUILD_FLAGS) \
	 NOSYSTEMC=1 PREFIX=$(BUILD_PRIMARY) \
	 BLUESPEC_VERSION=$(VERSION) \
	 install-bin > ../20install.log 2>&1;\
	find $(BUILD_PRIMARY)/lib/bin/linux32 -type f \
	 | xargs -t $(STRIP) ;\
	"

define BUILD_TEMPLATE
.PHONY: build-bsc-$(2)bit-$(1)-untar
build-bsc-$(2)bit-$(1)-untar: $(GET_TARBALL) $(DIR$(2))
	$(AS_TINDER) mkdir \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)
	$(AS_TINDER) tar xzf $(BSCTARBALL) \
	 -C $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)
	mkdir -p $(BUILD_LOG_DIR)/$(1)-$(2)

.PHONY: build-bsc-$(2)bit-$(1)-compiler
build-bsc-$(2)bit-$(1)-compiler: build-bsc-$(2)bit-$(1)-untar license$(2)
	ln -s \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/10util.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/20vendor.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/30makebscinstall.log \
	 $(BUILD_LOG_DIR)/$(1)-$(2)
	$(call CHROOT_TEMPLATE,linux$(2)) $$(CHROOT$(2)_DIR) \
	 sudo -u $(CHROOT_USERNAME) -H bash -c "\
	set -x;\
	set -e;\
	export PATH=/tmp/$(GHC_VERSION)/bin:\"\$$$$PATH\";\
	export BLUESPEC_LICENSE_FILE=$(BUILD_LICENSE) ;\
	cd $(BUILD_CHROOT_DIR)/$(1)/bsc;\
	make -j $(3) -C util NOSYSTEMC=1 \
	 PREFIX=$(BUILD_CHROOT_DIR)/$(1)/$(PRODUCT) \
	 BLUESPEC_VERSION=$(VERSION) \
	 install \
	 > ../10util.log 2>&1;\
	make -j $(3) -C vendor NOSYSTEMC=1 \
	 PREFIX=$(BUILD_CHROOT_DIR)/$(1)/$(PRODUCT) \
	 > ../20vendor.log 2>&1;\
	make -j1 BSC_BUILD=$(1) $(BUILD_FLAGS) \
	 PREFIX=$(BUILD_CHROOT_DIR)/$(1)/$(PRODUCT) \
	 -C src/comp install \
	 > ../30makebscinstall.log 2>&1;\
	find $(BUILD_CHROOT_DIR)/$(1)/$(PRODUCT)/lib/bin/linux$(2) \
	 -type f | xargs -t $(STRIP) ;\
	"
#Note we have no mechanism for rebuilding
#the platform INDEPENDENT libraries, 
#for those just use build-bsc-32bit

#for libraries which are not shared (i.e., *.so), this is
#a bit inefficient, building for both gcc 3.3 and 3.4,
#but discarding (hopefully, assuming "make realclean" works)
#the first and only keeping the second.

.PHONY: build-bsc-$(2)bit-$(1)-platform-libraries
build-bsc-$(2)bit-$(1)-platform-libraries: \
 build-bsc-$(2)bit-$(1)-compiler license$(2)
	ln -s \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/40clean-sim34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/41sim34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/46hdl.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/50clean-vpi34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/51vpi34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/55clean-readback34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/56readback34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/60clean-bluenoc34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/61bluenoc34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/70clean-scemi34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/71scemi34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/80clean-stp34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/81stp34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/82clean-yices34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/83yices34.log \
	 $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/90tcllib.log \
	 $(BUILD_LOG_DIR)/$(1)-$(2)
	$(call CHROOT_TEMPLATE,linux$(2)) $$(CHROOT$(2)_DIR) \
	 sudo -u $(CHROOT_USERNAME) -H bash -c "\
	set -x;\
	set -e;\
	export BLUESPEC_LICENSE_FILE=$(BUILD_LICENSE);\
	cd $(BUILD_CHROOT_DIR)/$(1)/bsc;\
	make -j1 -C src/hdl BSC_BUILD=$(1) \
	 $(BUILD_FLAGS) \
	 PREFIX=$(BUILD_CHROOT_DIR)/$(1)/$(PRODUCT) \
	 install > ../46hdl.log 2>&1;\
	$(call LIBRARIES_TEMPLATE,$(1),34,$(BUILD_CHROOT_DIR)/$(1)/$(PRODUCT))\
	make -j1 -C src/lib/tcllib $(BUILD_FLAGS) \
	 PREFIX=$(BUILD_CHROOT_DIR)/$(1)/$(PRODUCT) \
	 > ../90tcllib.log 2>&1; \
	"

.PHONY: build-bsc-$(2)bit-$(1)-bluenoc
build-bsc-$(2)bit-$(1)-bluenoc: \
 build-bsc-$(2)bit-$(1)-untar
	ln -s $$(CHROOT$(2)_DIR)$(BUILD_CHROOT_DIR)/$(1)/80bluenoc.log \
	 $(BUILD_LOG_DIR)/$(1)-$(2)
	$(call CHROOT_TEMPLATE,linux$(2)) $$(CHROOT$(2)_DIR) \
	 sudo -u $(CHROOT_USERNAME) -H bash -c "\
	set -x;\
	set -e;\
	cd $(BUILD_CHROOT_DIR)/$(1)/bsc;\
	make -C src/lib/board_support/BlueNoC/bluenoc \
	 PREFIX=$(BUILD_CHROOT_DIR)/$(1)/$(PRODUCT) \
	 BLUESPEC_VERSION=$(VERSION) \
	 install > ../80bluenoc.log ;\
	"

.PHONY: everything-except-platform-libraries-$(2)bit-$(1)
everything-except-platform-libraries-$(2)bit-$(1): \
 build-bsc-$(2)bit-$(1)-compiler \
 build-bsc-$(2)bit-$(1)-bluenoc

.PHONY: build-variant-bsc-$(2)bit-$(1)
build-variant-bsc-$(2)bit-$(1): \
 everything-except-platform-libraries-$(2)bit-$(1) \
 build-bsc-$(2)bit-$(1)-platform-libraries

endef

BUILD_PREFIX = $(BUILD_CHROOT_DIR)/$(BSC_BUILD)/$(PRODUCT)
NATIVE_PREFIX = $(BUILD_BUILD_DIR)/unpack/$(PRODUCT)

$(eval $(call BUILD_TEMPLATE,$(BSC_BUILD),64,$(BUILD_PARALLEL)))

#parallel builds not supported for debug and prof
#only supported for bsc_ghc6_parallel_make

$(eval $(call BUILD_TEMPLATE,DEBUG,64,1))
$(eval $(call BUILD_TEMPLATE,PROF,64,1))
$(eval $(call BUILD_TEMPLATE,DEBUG,32,1))
$(eval $(call BUILD_TEMPLATE,PROF,32,1))

.PHONY: variants
variants: \
  build-bsc-32bit-DEBUG-compiler \
  build-bsc-64bit-DEBUG-compiler \
  build-bsc-32bit-PROF-compiler \
  build-bsc-64bit-PROF-compiler

.PHONY: variants-all
variants-all: \
  build-bsc-32bit-DEBUG \
  build-bsc-64bit-DEBUG \
  build-bsc-32bit-PROF \
  build-bsc-64bit-PROF


#we build the doc natively, outside the
# chroot, in the hopes that
#the doc-building tools are newer and better.

.PHONY: install-full
install-full: unpack-tarball-natively
	$(MAKE) -j1 -C $(BUILD_BUILD_DIR)/unpack/bsc \
	 PREFIX=$(NATIVE_PREFIX) \
	 BLUESPEC_VERSION=$(VERSION) \
	 install-full > $(BUILD_LOG_DIR)/install-full.log 2>&1

.PHONY: unpack-tarball-natively
unpack-tarball-natively: create-checkout-dir $(GET_TARBALL)
	mkdir $(BUILD_BUILD_DIR)/unpack
	tar xzf $(BSCTARBALL) -C $(BUILD_BUILD_DIR)/unpack

#hardcoding GCC_FAMILY avoids the call to 
#c++family which is not installed yet

#This should be replaced by svn export. XXX
.PHONY: testsuite-tarball
testsuite-tarball: unpack-tarball-natively
	$(MAKE) -j1 GCC_FAMILY=g++4 \
	 -C $(BUILD_BUILD_DIR)/unpack/bsc/testsuite/bsc.customers \
	 all \
	 > $(BUILD_LOG_DIR)/co-testsuite-customers.log 2>&1
	find $(BUILD_BUILD_DIR)/unpack/bsc/testsuite \
	 -name .svn -type d \
	 -print0 | xargs -0 rm -fr
	cp $(BUILD_BUILD_DIR)/unpack/bsc/platform.mk \
	 $(BUILD_BUILD_DIR)/unpack/bsc/testsuite/
	tar czf $(TARBALL_OUTPUT_DIR)/testsuite-$(VERSION).tar.gz \
	 -C $(BUILD_BUILD_DIR)/unpack/bsc testsuite

.PHONY: copy-32
copy-32: build-bsc-32bit create-checkout-dir
	cp -r $(CHROOT32_DIR)$(BUILD_PRIMARY) $(BUILD_BUILD_DIR)

define COPY_64_TEMPLATE
	cp -r $(CHROOT64_DIR)$(BUILD_PREFIX)/lib/$(1)/g++4_64 \
	 $(BUILD_PRODUCT_DIR)/lib/$(1)/
endef

.PHONY: copy-64
copy-64: build-variant-bsc-64bit-$(BSC_BUILD) copy-32
	cp -r $(CHROOT64_DIR)$(BUILD_PREFIX)/lib/bin/linux64 \
	 $(BUILD_PRODUCT_DIR)/lib/bin/
	$(call COPY_64_TEMPLATE,Bluesim)
	$(call COPY_64_TEMPLATE,VPI)
	$(call COPY_64_TEMPLATE,Readback)
	$(call COPY_64_TEMPLATE,BlueNoC)
	$(call COPY_64_TEMPLATE,SceMi/BlueNoC)
	$(call COPY_64_TEMPLATE,SceMi/Classic)
	$(call COPY_64_TEMPLATE,SAT)
	cp -r $(CHROOT64_DIR)$(BUILD_PREFIX)/lib/tcllib/lib.linux64 \
	 $(BUILD_PRODUCT_DIR)/lib/tcllib

REWRITE_README = sed -i -e "s/yyyy\\.mm/$(VERSION)/g"

.PHONY: merge-directories
merge-directories: copy-32 copy-64 install-full
	rsync -v -v -a $(BUILD_BUILD_DIR)/unpack/$(PRODUCT) \
	 $(BUILD_BUILD_DIR) > $(BUILD_LOG_DIR)/rsync.log 2>&1
	 $(REWRITE_README) $(BUILD_PRODUCT_DIR)/README.txt

.PHONY: normal-tarball
normal-tarball: add-rpm
	tar czf $(TARBALL_OUTPUT_DIR)/$(PRODUCT).tar.gz \
	 -C $(BUILD_BUILD_DIR) $(PRODUCT)

RPM_NAME = bluespec-bluenoc
DEPS_NAME = bluespec-redhat-dependencies
EXTERNAL_NAME = bluespec-external-dependencies
REDHAT_PACKAGING_DIR = $(BUILD_BUILD_DIR)/rpmbuild
BLUENOC_RPMBUILD = $(REDHAT_PACKAGING_DIR)/bluenoc
DEPS_RPMBUILD = $(REDHAT_PACKAGING_DIR)/deps
EXTERNAL_RPMBUILD = $(REDHAT_PACKAGING_DIR)/external-deps
FINISHED_BLUENOC_BASENAME = $(RPM_NAME)-$(VERSION)-2dkms.noarch.rpm
FINISHED_BLUENOC_RPM = $(BLUENOC_RPMBUILD)/RPMS/noarch/$(FINISHED_BLUENOC_BASENAME)
FINISHED_DEPS_RPM = $(DEPS_RPMBUILD)/RPMS/noarch/$(DEPS_NAME)-$(VERSION)-el6.noarch.rpm
FINISHED_EXTERNAL_RPM = $(EXTERNAL_RPMBUILD)/RPMS/noarch/$(EXTERNAL_NAME)-$(VERSION)-el6.noarch.rpm
DRIVER_OUTPUT_DIR = lib/board_support/bluenoc/drivers

define COPY_64_VARIANT_TEMPLATE
	cp -r $(DIR64)/$(1)/$(PRODUCT)/lib/$(2)/g++4_64 \
	 $(BUILD_BUILD_DIR)/$(1)/$(PRODUCT)/lib/$(2)/
endef

RPMS_INSTALL_DIR=install/bluenoc/rpms

define ASSEMBLE_VARIANT
ASSEMBLE_$(1) = \
 $(BUILD_BUILD_DIR)/$(1)/$(PRODUCT)

# It may be possible to use
# everything-except-platform-libraries-32bit-$(1)
# instead of build-variant-bsc-32bit-$(1)
# as the dependency, but do a little bit of overkill just in case.
.PHONY: $(1)-tarball
$(1)-tarball: build-bsc-32bit create-checkout-dir\
            build-variant-bsc-32bit-$(1) \
            build-variant-bsc-64bit-$(1) \
            install-full rpm
	mkdir $(BUILD_BUILD_DIR)/$(1)
	cp -r $(CHROOT32_DIR)$(BUILD_PRIMARY) $$(ASSEMBLE_$(1))
	rm -fr $$(ASSEMBLE_$(1))/lib/bin/linux32
	if [ -e $$(ASSEMBLE_$(1))/lib/bin/linux64 ] ; \
	 then false ; fi
	cp -r \
	 $(DIR32)/$(1)/$(PRODUCT)/lib/bin/linux32 \
	 $(DIR64)/$(1)/$(PRODUCT)/lib/bin/linux64 \
	 $$(ASSEMBLE_$(1))/lib/bin
	$(call COPY_64_VARIANT_TEMPLATE,$(1),Bluesim)
	$(call COPY_64_VARIANT_TEMPLATE,$(1),VPI)
	$(call COPY_64_VARIANT_TEMPLATE,$(1),Readback)
	$(call COPY_64_VARIANT_TEMPLATE,$(1),BlueNoC)
	$(call COPY_64_VARIANT_TEMPLATE,$(1),SceMi/BlueNoC)
	$(call COPY_64_VARIANT_TEMPLATE,$(1),SceMi/Classic)
	$(call COPY_64_VARIANT_TEMPLATE,$(1),SAT)
	cp -r $(DIR64)/$(1)/$(PRODUCT)/lib/tcllib/lib.linux64 $(BUILD_BUILD_DIR)/$(1)/$(PRODUCT)/lib/tcllib/
	rsync -v -v -a $(BUILD_BUILD_DIR)/unpack/$(PRODUCT) \
	 $$(ASSEMBLE_$(1))/.. \
	 > $(BUILD_LOG_DIR)/rsync-$(1).log 2>&1
	$(REWRITE_README) $$(ASSEMBLE_$(1))/README.txt
	cp $(FINISHED_BLUENOC_RPM) $$(ASSEMBLE_$(1))/$(DRIVER_OUTPUT_DIR)
	mkdir -p $$(ASSEMBLE_$(1))/$(RPMS_INSTALL_DIR)
	cp $(FINISHED_DEPS_RPM) $$(ASSEMBLE_$(1))/$(RPMS_INSTALL_DIR)
	cp $(FINISHED_EXTERNAL_RPM) $$(ASSEMBLE_$(1))/$(RPMS_INSTALL_DIR)
	ln -s ../../$(DRIVER_OUTPUT_DIR)/$(FINISHED_BLUENOC_BASENAME) $$(ASSEMBLE_$(1))/$(RPMS_INSTALL_DIR)
	mv $$(ASSEMBLE_$(1)) $$(ASSEMBLE_$(1))-$(1)
	tar czf $(TARBALL_OUTPUT_DIR)/$(PRODUCT)-$(1).tar.gz \
	 -C $$(ASSEMBLE_$(1))-$(1)/.. $(PRODUCT)-$(1)
endef


$(eval $(call ASSEMBLE_VARIANT,DEBUG))
$(eval $(call ASSEMBLE_VARIANT,PROF))

.PHONY: variant-tarballs
variant-tarballs: DEBUG-tarball PROF-tarball

ifdef SYSTEMC_TARBALL
GET_SYSTEMC_TARBALL =
else
GET_SYSTEMC_TARBALL = unpack-tarball-natively
endif

SYSTEMC_TARBALL = \
 $(BUILD_BUILD_DIR)/unpack/bsc/vendor/systemc/systemc-2.2.0.tgz

ifdef TESTSUITE_TARBALL
GET_TESTSUITE_TARBALL =
else
GET_TESTSUITE_TARBALL = testsuite-tarball
endif
TESTSUITE_TARBALL = \
 $(TARBALL_OUTPUT_DIR)/testsuite-$(VERSION).tar.gz

ifdef NORMAL_TARBALL
GET_NORMAL_TARBALL =
else
GET_NORMAL_TARBALL = normal-tarball
endif
NORMAL_TARBALL = $(TARBALL_OUTPUT_DIR)/$(PRODUCT).tar.gz


## Bluenoc driver RPM

$(REDHAT_PACKAGING_DIR): $(BUILD_BUILD_DIR)
	mkdir $@

$(REDHAT_PACKAGING_DIR)/$(RPM_NAME)-$(VERSION)/dkms.conf: \
 $(REDHAT_PACKAGING_DIR)/$(RPM_NAME)-$(VERSION) $(DKMS_GENERATE)
	$(PERL) $(DKMS_GENERATE) $(RPM_NAME) $(VERSION) > $@

$(BLUENOC_RPMBUILD)/SPECS/bluenoc.spec: bluenoc-rpmbuild-dirs $(SPEC_GENERATE_BLUENOC)
	$(PERL) $(SPEC_GENERATE_BLUENOC) $(RPM_NAME) $(VERSION) > $@

$(REDHAT_PACKAGING_DIR)/$(RPM_NAME)-$(VERSION): copy-32
	mkdir -p $@
	cp $(BUILD_PRODUCT_DIR)/lib/board_support/bluenoc/drivers/* $@

#short cut was to depend on unpack-tarball-natively
#	$(SVN) export $(BUILD_BUILD_DIR)/unpack/bsc/src/lib/board_support/BlueNoC/drivers $@
# but this does not write the MODULE_VERSION into bluenoc.c

.PHONY:bluenoc-rpmbuild-dirs
bluenoc-rpmbuild-dirs: $(REDHAT_PACKAGING_DIR)
	mkdir $(BLUENOC_RPMBUILD)
	mkdir $(BLUENOC_RPMBUILD)/BUILD
	mkdir $(BLUENOC_RPMBUILD)/RPMS
	mkdir $(BLUENOC_RPMBUILD)/SPECS
	mkdir $(BLUENOC_RPMBUILD)/SOURCES
	mkdir $(BLUENOC_RPMBUILD)/SRPMS

.PHONY:deps-rpmbuild-dirs
deps-rpmbuild-dirs: $(REDHAT_PACKAGING_DIR)
	mkdir $(DEPS_RPMBUILD)
	mkdir $(DEPS_RPMBUILD)/BUILD
	mkdir $(DEPS_RPMBUILD)/RPMS
	mkdir $(DEPS_RPMBUILD)/SPECS
	mkdir $(DEPS_RPMBUILD)/SOURCES
	mkdir $(DEPS_RPMBUILD)/SRPMS

.PHONY:external-rpmbuild-dirs
external-rpmbuild-dirs: $(REDHAT_PACKAGING_DIR)
	mkdir $(EXTERNAL_RPMBUILD)
	mkdir $(EXTERNAL_RPMBUILD)/BUILD
	mkdir $(EXTERNAL_RPMBUILD)/RPMS
	mkdir $(EXTERNAL_RPMBUILD)/SPECS
	mkdir $(EXTERNAL_RPMBUILD)/SOURCES
	mkdir $(EXTERNAL_RPMBUILD)/SRPMS

$(BLUENOC_RPMBUILD)/SOURCES/$(RPM_NAME)-$(VERSION).tar.gz: bluenoc-rpmbuild-dirs \
 $(REDHAT_PACKAGING_DIR)/$(RPM_NAME)-$(VERSION)/dkms.conf
	tar czf $@ -C $(REDHAT_PACKAGING_DIR) $(RPM_NAME)-$(VERSION)

# rpmbuild needs at least one Source
$(DEPS_RPMBUILD)/SOURCES/empty.tar.gz: deps-rpmbuild-dirs
	tar cvfzT $@ /dev/null
$(EXTERNAL_RPMBUILD)/SOURCES/empty.tar.gz: external-rpmbuild-dirs
	tar cvfzT $@ /dev/null

$(DEPS_RPMBUILD)/SPECS/deps-rh.spec: deps-rpmbuild-dirs $(SPEC_GENERATE_DEPS)
	$(PERL) $(SPEC_GENERATE_DEPS) $(DEPS_NAME) $(VERSION) > $@
$(EXTERNAL_RPMBUILD)/SPECS/external-rh.spec: external-rpmbuild-dirs $(SPEC_GENERATE_EXTERNAL)
	$(PERL) $(SPEC_GENERATE_EXTERNAL) $(EXTERNAL_NAME) $(VERSION) > $@

RPMBUILD = rpmbuild

$(FINISHED_BLUENOC_RPM): $(BLUENOC_RPMBUILD)/SPECS/bluenoc.spec $(BLUENOC_RPMBUILD)/SOURCES/$(RPM_NAME)-$(VERSION).tar.gz
	$(RPMBUILD) --define '_topdir '$(BLUENOC_RPMBUILD) -v -ba $< > $(BUILD_LOG_DIR)/rpmbuild.log 2>&1

$(FINISHED_DEPS_RPM): $(DEPS_RPMBUILD)/SPECS/deps-rh.spec $(DEPS_RPMBUILD)/SOURCES/empty.tar.gz
	$(RPMBUILD) --define '_topdir '$(DEPS_RPMBUILD) -v -ba $< > $(BUILD_LOG_DIR)/rpmbuild-deps.log 2>&1
$(FINISHED_EXTERNAL_RPM): $(EXTERNAL_RPMBUILD)/SPECS/external-rh.spec $(EXTERNAL_RPMBUILD)/SOURCES/empty.tar.gz
	$(RPMBUILD) --define '_topdir '$(EXTERNAL_RPMBUILD) -v -ba $< > $(BUILD_LOG_DIR)/rpmbuild-external.log 2>&1

.PHONY: rpm
rpm: $(FINISHED_BLUENOC_RPM) $(FINISHED_DEPS_RPM)  $(FINISHED_EXTERNAL_RPM)

.PHONY: add-rpm
add-rpm: merge-directories rpm
	cp $(FINISHED_BLUENOC_RPM) $(BUILD_PRODUCT_DIR)/$(DRIVER_OUTPUT_DIR)
	mkdir -p $(BUILD_PRODUCT_DIR)/$(RPMS_INSTALL_DIR)
	ln -s ../../../$(DRIVER_OUTPUT_DIR)/$(FINISHED_BLUENOC_BASENAME) $(BUILD_PRODUCT_DIR)/$(RPMS_INSTALL_DIR)
	cp $(FINISHED_DEPS_RPM) $(FINISHED_EXTERNAL_RPM) $(BUILD_PRODUCT_DIR)/$(RPMS_INSTALL_DIR)


######################### TESTING

#VERIFY THAT THE BAD RHEL4 timer-create
# BEHAVIOR HAPPENS IN THE CHROOT!
#It does not. It is a kernel issue.

SUDO_RPM = $(SUDO) rpm

#remember to set the PATH also to find bsc (for "bluespec")
#	BLUESPECDIR=$(1)/lib \

TEST_ARGS = TEST_RELEASE=$(1) \
	NO_CUSTOMERS_CHECKOUT=1

TEST_ENV = SYSTEMCTEST=1 CTEST=1 VTEST=1

TEST_TARGET = fullparallel
TEST_DIR = testsuite

define TEST_CHROOT_TEMPLATE
$(1)_CHROOT = $(BUILD_BUILD_DIR)/chroot-$(1)

.PHONY: $(1)-setup-chroot
$(1)-setup-chroot: $(1)-create-chroot
	$(call CHROOT_TEMPLATE,linux$(2)) $$($(1)_CHROOT) \
	 /usr/sbin/useradd -u $(CHROOT_UID) $(CHROOT_USERNAME)
	tar xjf /raid/tools/ghc/tarballs/ghc-6.12.3-etch$(2)-tmp-install-with-cabal-prof.tar.bz2 -C $$($(1)_CHROOT)

old_old-$(1):
	cp $(BUILD_LICENSE_FILE) $$($(1)_CHROOT)/tmp/
	$(SUDO) mkdir $$($(1)_CHROOT)/tmp/.X11-unix
	$(SUDO) mount --bind /tmp/.X11-unix \
	 $$($(1)_CHROOT)/tmp/.X11-unix
	tar xzf \
	 /raid/tools/iverilog/current-0.9/iverilog-$(1).tar.gz \
	 -C $$($(1)_CHROOT)/tmp
	tar xjf \
	 /raid/tools/python/release-script-tarballs/current/python-$(1).tar.bz2 \
	 -C $$($(1)_CHROOT)/tmp


#creating a subdirectory systemc 
#makes "find" work, and chown work.

.PHONY: $(1)-systemc
$(1)-systemc: $(1)-setup-chroot $(GET_SYSTEMC_TARBALL)
	$(AS_TINDER) mkdir $$($(1)_CHROOT)/tmp/systemc
	$(AS_TINDER) tar xzf $(SYSTEMC_TARBALL) \
	 -C $$($(1)_CHROOT)/tmp/systemc
	mkdir -p $(BUILD_LOG_DIR)/test-$(1)
	ln -s \
	 $$($(1)_CHROOT)/tmp/systemc/current-systemc/systemc-configure.log \
	 $$($(1)_CHROOT)/tmp/systemc/current-systemc/systemc-make.log \
	 $(BUILD_LOG_DIR)/test-$(1)
	$(call CHROOT_TEMPLATE,linux$(2)) $$($(1)_CHROOT) \
	sudo -u $(CHROOT_USERNAME) -H bash -c "\
	set -x;\
	set -e;\
	cd /tmp/systemc;\
	SYSTEMC=\`find . -maxdepth 1 -mindepth 1 -type d\`;\
	ln -s \$$$$SYSTEMC current-systemc;\
	cd current-systemc;\
	./configure > systemc-configure.log 2>&1;\
	make -i install > systemc-make.log 2>&1;\
	"

.PHONY: $(1)-testsuite
$(1)-testsuite: $(1)-setup-chroot $(GET_TESTSUITE_TARBALL)
	$(AS_TINDER) tar xzf $(TESTSUITE_TARBALL) \
	 -C $$($(1)_CHROOT)/tmp

.PHONY: $(1)-compiler
$(1)-compiler: $(GET_NORMAL_TARBALL) $(1)-setup-chroot
	$(AS_TINDER) mkdir $$($(1)_CHROOT)/tmp/bluespec
	$(AS_TINDER) tar xzf $(NORMAL_TARBALL) \
	 -C $$($(1)_CHROOT)/tmp/bluespec

#This can also be used just to create a chroot for
#interactive debugging.
.PHONY: $(1)-unpack
$(1)-unpack: $(1)-testsuite $(1)-compiler

#NOTE that cannot do parallel testsuite 
#because the version of make in
#these redhat chroots is (probably) too old/has a bug.

#fullparallel because old versions 
#of dejagnu do not recurse far enough.
#The PATH is set because "bluespec" 
#looks for bsc in the path.

#WHICH_CHROOT exists only to be able to 
#see in "ps" which one is doing what.

.PHONY: $(1)-test-prepare
$(1)-test-prepare: $(1)-unpack $(1)-systemc

.PHONY: $(1)-test
$(1)-test: $(1)-test-prepare
	mkdir -p $(BUILD_LOG_DIR)/test-$(1)
	ln -s $$($(1)_CHROOT)/tmp/testsuite.log \
	 $$($(1)_CHROOT)/tmp/testsuite-summary.log \
	 $$($(1)_CHROOT)/tmp/bsc.labs--direct-c.log \
	 $$($(1)_CHROOT)/tmp/bsc.codegen--foreign.log \
	 $(BUILD_LOG_DIR)/test-$(1)
	$(call CHROOT_TEMPLATE,linux$(2)) $$($(1)_CHROOT) \
	sudo -u $(CHROOT_USERNAME) -H bash -c "\
	set -x;\
	WHICH_CHROOT=$(1);\
	ROOTDIR=\`find /tmp/bluespec -maxdepth 1 \
	 -mindepth 1 -type d\`;\
	SYSTEMC=/tmp/systemc/current-systemc;\
	export SYSTEMC;\
	export BLUESPEC_LICENSE_FILE=/tmp/$(BUILD_LICENSE_NAME);\
	unset LS_COLORS;\
	export PATH=/tmp/python/bin:\$$$$PATH;\
	PATH=/tmp/iverilog/bin:\$$$$PATH $(TEST_ENV) make \
	 $(call TEST_ARGS,\$$$$ROOTDIR) \
	 PLATFORM_MAKE=/tmp/testsuite/platform.mk \
	 -C /tmp/testsuite/bsc.labs/direct-c localcheck \
	 > /tmp/bsc.labs--direct-c.log 2>&1;\
	DIRECTC_EXIT=\$$$$?;\
	PATH=/tmp/iverilog/bin:\$$$$PATH $(TEST_ENV) make \
	 $(call TEST_ARGS,\$$$$ROOTDIR) \
	 PLATFORM_MAKE=/tmp/testsuite/platform.mk \
	 -C /tmp/testsuite/bsc.codegen/foreign localcheck \
	 > /tmp/bsc.codegen--foreign.log 2>&1;\
	FOREIGN_EXIT=\$$$$?;\
	export PATH=\$$$$ROOTDIR/bin:\$$$$PATH;\
	export DISPLAY=:88;\
	$(TEST_ENV) make \
	 $(call TEST_ARGS,\$$$$ROOTDIR) \
	 PLATFORM_MAKE=/tmp/testsuite/platform.mk \
	 -C /tmp/$(TEST_DIR) $(TEST_TARGET) \
	 > /tmp/testsuite.log 2>&1;\
	TESTSUITE_EXIT_STATUS=\$$$$?;\
	make GCC_FAMILY=g++4 \
	 PLATFORM_MAKE=/tmp/testsuite/platform.mk \
	 -C /tmp/testsuite generate-stats \
	 > /tmp/testsuite-summary.log 2>&1 ;\
	COMBINED_EXIT=\`expr \$$$$DIRECTC_EXIT \| \
	 \$$$$FOREIGN_EXIT \| \
	 \$$$$TESTSUITE_EXIT_STATUS\`; \
	exit \$$$$COMBINED_EXIT \
	"
endef

$(eval $(call TEST_CHROOT_TEMPLATE,RHEL3_32,32))
$(eval $(call TEST_CHROOT_TEMPLATE,RHEL4_32,32))
$(eval $(call TEST_CHROOT_TEMPLATE,RHEL3_64,64))
$(eval $(call TEST_CHROOT_TEMPLATE,RHEL4_64,64))
$(eval $(call TEST_CHROOT_TEMPLATE,RHEL5_32,32))

# The following two targets, all-chroots and all-unpack
# are useful for manually creating a chroot and investigating

.PHONY: all-chroots
all-chroots: RHEL3_32-setup-chroot RHEL3_64-setup-chroot \
             RHEL4_32-setup-chroot RHEL4_64-setup-chroot

.PHONY: all-unpack
all-unpack: RHEL3_32-unpack RHEL3_64-unpack \
            RHEL4_32-unpack RHEL4_64-unpack

REDHAT_DIR = /raid/redhat

define UNPACK_RPMS_TEMPLATE
	rpm --root $($(1)_$(2)_CHROOT) \
	 --import $(REDHAT_DIR)/new/$(1)_$(2)/RPM-GPG-KEY
	set -e ; \
	 set -x ; \
	 cd $(REDHAT_DIR)/new/$(1)_$(2)/RPMS ; \
	 for dir in * ; \
	 do if [ -e ../flags/$$dir ] ; \
	 then FLAGS=`cat ../flags/$$dir` ; \
	 else FLAGS= ; \
	 fi ; \
	 sudo linux$(2) rpm -Uvh --root \
	 $($(1)_$(2)_CHROOT) $$FLAGS $$dir/*rpm ; \
	 done > $(BUILD_LOG_DIR)/$(1)_$(2)-create-chroot.log 2>&1
endef
.PHONY: RHEL3_32-create-chroot
RHEL3_32-create-chroot: create-build-work-dir
	mkdir $(RHEL3_32_CHROOT)
	$(call UNPACK_RPMS_TEMPLATE,RHEL3,32)

.PHONY: RHEL3_64-create-chroot
RHEL3_64-create-chroot: create-build-work-dir
	mkdir $(RHEL3_64_CHROOT)
	$(call UNPACK_RPMS_TEMPLATE,RHEL3,64)

MOUNT=sudo mount
#be careful NOT to delete /dev when cleaning up

.PHONY: RHEL4_32-create-chroot
RHEL4_32-create-chroot: create-build-work-dir
	mkdir $(RHEL4_32_CHROOT)
	mkdir $(RHEL4_32_CHROOT)/dev
	$(MOUNT) --bind /dev $(RHEL4_32_CHROOT)/dev
	$(call UNPACK_RPMS_TEMPLATE,RHEL4,32)

.PHONY: RHEL4_64-create-chroot
RHEL4_64-create-chroot: create-build-work-dir
	mkdir $(RHEL4_64_CHROOT)
	mkdir $(RHEL4_64_CHROOT)/dev
	$(MOUNT) --bind /dev $(RHEL4_64_CHROOT)/dev
	$(call UNPACK_RPMS_TEMPLATE,RHEL4,64)

RHEL5_32-create-chroot: create-build-work-dir
	mkdir $(RHEL5_32_CHROOT)
	mkdir $(RHEL5_32_CHROOT)/dev
	$(MOUNT) --bind /dev $(RHEL5_32_CHROOT)/dev
	$(call UNPACK_RPMS_TEMPLATE,RHEL5,32)

#run the testsuite natively, i.e., not in a chroot
NATIVE_TEST_DIR = $(BUILD_BUILD_DIR)/native-test

.PHONY: native-create-dir
native-create-dir: create-build-work-dir
	mkdir $(NATIVE_TEST_DIR)

.PHONY: native-systemc
native-systemc: native-create-dir $(GET_SYSTEMC_TARBALL)
	mkdir $(NATIVE_TEST_DIR)/systemc
	tar xzf $(SYSTEMC_TARBALL) -C $(NATIVE_TEST_DIR)/systemc
	ln -s `find $(NATIVE_TEST_DIR)/systemc \
	 -maxdepth 1 -mindepth 1 -type d` \
	 $(NATIVE_TEST_DIR)/current-systemc
	cd $(NATIVE_TEST_DIR)/current-systemc &&\
	  ./configure > test-configure.log 2>&1 &&\
	  $(MAKE) -i install > make.log 2>&1

.PHONY: native-testsuite
native-testsuite: native-create-dir $(GET_TESTSUITE_TARBALL)
	tar xzf $(TESTSUITE_TARBALL) -C $(NATIVE_TEST_DIR)

.PHONY: native-compiler
native-compiler: native-create-dir $(GET_NORMAL_TARBALL)
	mkdir $(NATIVE_TEST_DIR)/bluespec
	tar xzf $(NORMAL_TARBALL) -C $(NATIVE_TEST_DIR)/bluespec
	ln -s `find $(NATIVE_TEST_DIR)/bluespec \
	 -maxdepth 1 -mindepth 1 -type d` \
	 $(NATIVE_TEST_DIR)/current-bluespec


NATIVE_PARALLEL = 1

NATIVE_GENERATE_STATS = $(MAKE) -j 1 GCC_FAMILY=g++4 \
 PLATFORM_MAKE=$(NATIVE_TEST_DIR)/testsuite/platform.mk \
 -C $(NATIVE_TEST_DIR)/testsuite generate-stats \
 > $(BUILD_LOG_DIR)/testsuite-summary-native.log

#It is up to the user to supply a license, and 
#set the DISPLAY.
#BDPI not tested unless a good verilog simulator supplied

.PHONY: native-test
native-test: native-systemc native-testsuite native-compiler
	if SYSTEMC=$(NATIVE_TEST_DIR)/current-systemc \
	 PATH=$(NATIVE_TEST_DIR)/current-bluespec/bin:$$PATH \
	 $(TEST_ENV) \
	 $(MAKE) \
	 $(call TEST_ARGS,$(NATIVE_TEST_DIR)/current-bluespec) \
	 PLATFORM_MAKE=$(NATIVE_TEST_DIR)/testsuite/platform.mk \
	 -C $(NATIVE_TEST_DIR)/$(TEST_DIR) \
	 -j $(NATIVE_PARALLEL) $(TEST_TARGET) \
	 > $(BUILD_LOG_DIR)/testsuite-native.log 2>&1 ; \
	 then $(NATIVE_GENERATE_STATS) ; \
	 else $(NATIVE_GENERATE_STATS) ; false ; fi

.PHONY: all-tests
all-tests: RHEL4_32-test RHEL4_64-test

#omit rhel3 tests because they are doomed to fail
#because we no longer support gcc 3.3.

.PHONY: all
all: all-tests variant-tarballs source-tarball
	ls $(TARBALL_OUTPUT_DIR)/*.tar.gz

#we do not mount /dev on RHEL3, but we umount just in case.
.PHONY: umount
umount:
	-sudo umount $(RHEL3_32_CHROOT)/proc
	-sudo umount $(RHEL3_64_CHROOT)/proc
	-sudo umount $(RHEL4_32_CHROOT)/proc
	-sudo umount $(RHEL4_64_CHROOT)/proc
	-sudo umount $(RHEL3_32_CHROOT)/dev
	-sudo umount $(RHEL3_64_CHROOT)/dev
	-sudo umount $(RHEL4_32_CHROOT)/dev
	-sudo umount $(RHEL4_64_CHROOT)/dev
	-sudo umount $(RHEL3_32_CHROOT)/tmp/.X11-unix
	-sudo umount $(RHEL3_64_CHROOT)/tmp/.X11-unix
	-sudo umount $(RHEL4_32_CHROOT)/tmp/.X11-unix
	-sudo umount $(RHEL4_64_CHROOT)/tmp/.X11-unix

.PHONY: umount-sleep
umount-sleep: umount
	@echo ""
	@echo \
	 "********************************************************************"
	@echo \
	 "*** Hit Control-C to abort if anything BELOW still looks mounted ***"
	-mount | grep build
	@echo \
	 "*** Hit Control-C to abort if anything ABOVE still looks mounted ***"
	@echo \
	 "********************************************************************"
	sleep 20

#you probably want to set VERSION for this.

.PHONY: clean
clean: umount-sleep
	$(SUDO) rm -fr $(DIR32)
	$(SUDO) rm -fr $(DIR64)
	$(SUDO) rm -fr $(BUILD_BUILD_DIR)/chroot-RHEL[34]_*
	rm -fr $(BUILD_WORK_DIR)

define TEST_TEMPLATE
.PHONY: test
test:
	echo Hello world
endef
$(eval $(call TEST_TEMPLATE,999))

