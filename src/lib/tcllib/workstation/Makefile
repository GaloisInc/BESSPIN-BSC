PREFIX ?= $(PWD)/../../../../inst
LIBDIR ?= $(PREFIX)/lib/

PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

BLUETCL_EXEC?=$(LIBDIR)/bin/bluetcl
BDIR=$(LIBDIR)
export BLUESPECDIR = $(LIBDIR)

EXEFILES = open_graph.tcl \


TCLFILES = \
	analysis_commands.tcl \
	bindings.tcl \
	build_commands.tcl \
	build_skeleton_dialog.tcl \
	command_line_handlers.tcl \
	dialog.tcl \
	file_commands.tcl \
	file_dialogs.tcl \
	finddialog.tcl \
	fonts.tcl \
	graph_export_dialog.tcl \
	show_text_dialog.tcl \
	graph_window.tcl \
	help.tcl \
	hierarchy.tcl \
	import_bvi_wizard_analysis.tcl \
	import_bvi_wizard_graphic.tcl \
	main_window.tcl \
	menu_actions.tcl \
	menubar.tcl \
	messagebox.tcl \
	module_browser_window.tcl \
	package_window.tcl \
	paned_window.tcl \
	project_back_up_dialog.tcl \
	project_commands.tcl \
	project_options_dialog.tcl \
	project_top_file_dialog.tcl \
	project_window.tcl \
	schedule_analysis.tcl \
	scrolledcheckbox.tcl \
	select_file_dialog.tcl \
	selection_dialog.tcl \
	semu_input.tcl \
	semu_waves.tcl \
	semu_util.tcl \
	status_command_window.tcl \
	tabnotebook.tcl \
	tclIndex.sh \
	tclIndex \
	toolbar.tcl \
	type_browser_window.tcl \
	workstation.tcl \
	status_window.tcl \
	EmulGui.tcl \


PACKAGES = \
	slave_namespace.tcl \
	ReadBackGui.tcl \
	SemuGui.tcl \
	semu_config.tcl \
	SemuInit.tcl \
	semu_build.tcl \
	semu_testbench.tcl \
	sim_testbench.tcl \
	semu_ctb.tcl \
	semu_stb.tcl \
	semu_advanced.tcl \
	semu_pin.tcl \
	emu_control.tcl \
	emulite_control.tcl \
	Instlite.tcl \
        Instrument.tcl \
	read_hdl.tcl \


FILES = $(TCLFILES) $(PACKAGES) pkgIndex.tcl

.PHONY: all
all: install

run: *.tcl
	@bluewish workstation.tcl

tclIndex: *.tcl
	TCLSH=$(BLUETCL_EXEC) BLUESPECDIR=$(LIBDIR) ./tclIndex.sh "$(TCLFILES)" "$(PACKAGES)"

install: tclIndex 
	install -d -m755 $(LIBDIR)/tcllib/workstation
	install -m644 $(FILES) $(LIBDIR)/tcllib/workstation
	install -m755 $(EXEFILES) $(LIBDIR)/tcllib/workstation
	install -d -m755 $(LIBDIR)/bin
	install -m755 bluespec $(LIBDIR)/bin
	install -m755 semu $(LIBDIR)/bin
	rm -rf $(LIBDIR)/tcllib/workstation/help
	#cp -pr help $(LIBDIR)/tcllib/workstation
	$(MAKE) -C images LIBDIR=$(LIBDIR) $@


TAGS: *.tcl
	etags --lang=none \
                --regex='/proc[ \t]+\([^ \t]+\)/\1/' \
                --regex='/itcl::body[ \t]+\([^ \t]+\)/\1/' \
                --regex='/itcl::body[ \t]+[^: \t]+::\([^ \t]+\)/\1/'  $+

tags: *.tcl
	ctags $+

.PHONY: clean realclean clean-tix
clean realclean: clean-tix
	@rm -rf tclIndex logs help TAGS tags

clean-tix:
	@rm -rf *.tix

docs: 
	@doxygen Doxyfile

pdf: docs
	@cd ../../doc/latex; make &> /dev/null

help_content: 
	@mkdir -p help
	@doxygen help_src/Doxyfile
help: 
	@echo "Following targets are available: "
	@echo "\"tclIndex\" - creates Tcl index file"
	@echo "\"run\" - runs entire project"
	@echo "\"docs\" - creates doxygen documentation"
	@echo "\"clean\" - cleans build results" 
	@echo "\"help\" - prints this message"
