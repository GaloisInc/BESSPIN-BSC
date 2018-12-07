\
.PHONY: clean sanitize realclean

install:
	$(MAKE) -C .. install

clean:
	@$(MAKE) -C AXI    clean
	@rm -f *~


sanitize: clean
	$(MAKE) -C AXI    sanitize
	@rm -rf Makefile .svn *~

realclean: clean
