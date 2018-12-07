
RM = rm -f 

INSTALL?=install -c

PWD:=$(shell pwd)
PREFIX?=$(PWD)/../../../../../inst

LIBDIR=$(PREFIX)/lib
BWIDGETDIR = $(LIBDIR)/Blueview/lib/BWidget-1.7.0/


## List of files to install 
BWIDGET_SOURCES = \
	arrow.tcl \
	bitmap.tcl \
	button.tcl \
	buttonbox.tcl \
	color.tcl \
	combobox.tcl \
	dialog.tcl \
	dragsite.tcl \
	dropsite.tcl \
	dynhelp.tcl \
	entry.tcl \
	font.tcl \
	init.tcl \
	label.tcl \
	labelentry.tcl \
	labelframe.tcl \
	listbox.tcl \
	mainframe.tcl \
	messagedlg.tcl \
	notebook.tcl \
	pagesmgr.tcl \
	panedw.tcl \
	passwddlg.tcl \
	progressbar.tcl \
	progressdlg.tcl \
	scrollframe.tcl \
	scrollview.tcl \
	scrollw.tcl \
	separator.tcl \
	spinbox.tcl \
	titleframe.tcl \
	tree.tcl \
	utils.tcl \
	widget.tcl \
	wizard.tcl \
	xpm2image.tcl


.PHONY: all install install-tar clean realclean


all:

install:
	$(INSTALL) -m 755 -d $(LIBDIR)/Blueview
	$(INSTALL) -m 755 -d $(LIBDIR)/Blueview/lib
	$(INSTALL) -m 755 -d $(LIBDIR)/Blueview/lib/BWidget-1.7.0/
	$(INSTALL) -m 755 -d $(LIBDIR)/Blueview/lib/BWidget-1.7.0/BWman
	$(INSTALL) -m 755 -d $(LIBDIR)/Blueview/lib/BWidget-1.7.0/demo
	$(INSTALL) -m 755 -d $(LIBDIR)/Blueview/lib/BWidget-1.7.0/images
	$(INSTALL) -m 755 -d $(LIBDIR)/Blueview/lib/BWidget-1.7.0/lang

	@for i in $(BWIDGET_SOURCES) ; do \
	    $(INSTALL) -m 444 $$i $(BWIDGETDIR) ; \
	done;
	$(INSTALL)  -m 444 pkgIndex.tcl   $(BWIDGETDIR) 
	$(INSTALL)  -m 444 README.txt     $(BWIDGETDIR) 
	$(INSTALL)  -m 444 LICENSE.txt    $(BWIDGETDIR) 

	@echo "Installing language files in $(DESTDIR)$(pkglibdir)/lang"
	@for i in lang/*.rc ; do \
	    $(INSTALL) -m 444 $$i $(BWIDGETDIR)/lang ; \
	done;

	@for i in images/*.gif images/*.xbm ; do \
	    $(INSTALL) -m 444 $$i $(BWIDGETDIR)/images ; \
	done;
	@for i in BWman/*.htm* ; do \
	    $(INSTALL) -m 444  $$i $(BWIDGETDIR)/BWman ; \
	done;

	@for i in demo/*.tcl demo/*.xbm ; do \
	    $(INSTALL) -m 444  $$i $(BWIDGETDIR)/demo ; \
	done;



clean:
	$(RM) *.~

realclean: clean
