DIRS = \
	board_support \
	bridges \
	src_BSV \
	tcllib \

.PHONY: install clean realclean
install clean realclean:
	@$(foreach dir, $(DIRS), $(MAKE) -C $(dir) $@;)
