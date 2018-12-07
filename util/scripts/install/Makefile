PREFIX?=../../../inst
TARGET=$(PREFIX)/install/bluenoc
PERL=perl

# BLUESPEC_VERSION gets set in sargebuild.mk
.PHONY: install
install:
	install -d $(TARGET)
	perl utils/re-number.pl | bash
	install -m 755 [0-9][0-9]-*.sh $(TARGET)
	perl utils/un-number.pl | bash
	install -m 644 dkms-check.pl $(TARGET)
	install -m 644 check-functions.sh $(TARGET)
	install -d $(TARGET)/repository-rpms
	install -d $(TARGET)/repository-rpms/5
	install -d $(TARGET)/repository-rpms/6
	install -m 644 repository-rpms/5/*.rpm $(TARGET)/repository-rpms/5/
	install -m 644 repository-rpms/6/*.rpm $(TARGET)/repository-rpms/6/
	install -d $(TARGET)/data
	install -m 644 data/libusb-driverrc $(TARGET)/data
	echo $(BLUESPEC_VERSION) > $(TARGET)/version.dat
	$(MAKE) -C bootstrap install

.PHONY: clean-install
clean-install:
	rm -fr $(TARGET)

.PHONY: renumber
renumber:
	perl utils/un-number.pl | bash
	perl utils/re-number.pl | bash

.PHONY: doc
doc: ../../../doc/bluenoc-install/scripts-doc.tex

#a hack, making a real target phony to always regenerate
.PHONY: ../../../doc/bluenoc-install/scripts-doc.tex
../../../doc/bluenoc-install/scripts-doc.tex:
	perl utils/re-number.pl | bash
	perl utils/extract-doc.pl [0-9][0-9]*.sh > $@
	perl utils/un-number.pl | bash
