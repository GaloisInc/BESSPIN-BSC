PREFIX?=$(PWD)/../../../../inst
LIBDIR = $(PREFIX)/lib

FILES = \
	gvim_config.vim


.PHONY: clean all install

all: 

install: 
	install -d -m755 $(LIBDIR)/tcllib/gvim
	install -m644 $(FILES) $(LIBDIR)/tcllib/gvim

.PHONY: clean realclean	
realclean: clean

clean:
