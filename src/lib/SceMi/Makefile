# Makefile for SCE-MI library
# Copyright 2008 Bluespec, Inc. All Rights Reserved
.PHONY: all BlueNoC Classic clean realclean
all:    BlueNoC Classic 

BlueNoC:
	@$(MAKE) -C BlueNoC

Classic:
	@$(MAKE) -C Classic

install: all
	@$(MAKE) -C BlueNoC  install
	@$(MAKE) -C Classic  install
	@$(MAKE) -C lumina   install
	@$(MAKE) -C semu     install
	@$(MAKE) -C capi     install
	@$(MAKE) -C simtb    install

clean:
	@$(MAKE) -C BlueNoC     clean
	@$(MAKE) -C Classic     clean
	@$(MAKE) -C bsvxactors  clean
	@$(MAKE) -C tlmxactors  clean
	@$(MAKE) -C lumina      clean
	@$(MAKE) -C simtb       clean
	@$(MAKE) -C capi        clean

realclean: clean
