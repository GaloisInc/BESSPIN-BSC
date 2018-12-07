PREFIX?=../../../../inst
TARGET=$(PREFIX)/install/bluenoc/bootstrap

.PHONY: install
install:
	install -d $(TARGET)
	set -x ; for file in `find . -maxdepth 1 -type d -not -name '.*'` ; do install -d $(TARGET)/$$file ; done
	set -x ; for file in `find . -maxdepth 1 -type d -not -name '.*'` ; do install -m 644 $$file/* $(TARGET)/$$file ; done

.PHONY: clean-install
clean-install:
	rm -fr $(TARGET)
