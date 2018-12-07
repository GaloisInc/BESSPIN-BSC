PREFIX?=$(PWD)/../../../../inst
LIBDIR = $(PREFIX)/lib

FILES = \
	emacs_init.el \
	server-20-21.el \
	easy-mmode-20.el \
	workstation.el \


.PHONY: clean all install

all: 

install: 
	install -d -m755 $(LIBDIR)/tcllib/emacs
	install -d -m755 $(LIBDIR)/tcllib/emacs/linux32
	install -d -m755 $(LIBDIR)/tcllib/emacs/linux64
	install -m644 $(FILES) $(LIBDIR)/tcllib/emacs
	install -m755 emacsclient    $(LIBDIR)/tcllib/emacs
	install -m755 emacsclient-24 $(LIBDIR)/tcllib/emacs
	install -m755 linux32/emacsclient-22.1 $(LIBDIR)/tcllib/emacs/linux32
	install -m755 linux32/emacsclient-24.2 $(LIBDIR)/tcllib/emacs/linux32
	install -m755 linux64/emacsclient-22.1 $(LIBDIR)/tcllib/emacs/linux64
	install -m755 linux64/emacsclient-24.2 $(LIBDIR)/tcllib/emacs/linux64

.PHONY: clean realclean	
realclean: clean

clean:
