# BSCSource/SceMi/Makefile

DIRS= \
	Classic \
	BlueNoC

# Files to release publically
SHOW_FILE = \
	SceMiAldecMacros.bsv \
	SceMiEveMacros.bsv \
	SceMiMacros.bsv \
	SceMiMemoryXactor.bsv \
	SceMiProxies.bsv \
	SimTbProxies.bsv \
	SceMiScan.bsv \
	SceMiSharedMemory.bsv \
	SceMiXactors.bsv


SRC_FILE = $(wildcard *.bsv)
RM_SRC = $(filter-out $(SHOW_FILE),$(SRC_FILE))


.PHONY: clean sanitize realclean build


build:
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true

install:
	$(MAKE) -C .. install

clean:
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true
	@rm -f *.bi *.bo *.ba *.v vpi_wrapper* *.info *~

sanitize: clean
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true
	@rm -rf Makefile .svn   $(RM_SRC)

realclean: clean
	$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@ &&) true
