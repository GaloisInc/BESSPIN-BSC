.PHONY: clean default

default:
	svn co --non-interactive https://svn.bluespec.com:8080/bs/marketing/trunk/DAC2005Demos/UTMI
	cd UTMI && $(MAKE) mkUTMI.v

clean:
	rm -rf UTMI
