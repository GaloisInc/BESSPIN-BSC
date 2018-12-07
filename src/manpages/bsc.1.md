% bsc(1) bsc Manual
% Bluespec, Inc. <support@bluespec.com>

# NAME

bsc - Bluespec Compiler

# SYNOPSIS

**bsc** [*options*]

# OPTIONS

Most flags may be preceded by a **no-** to reverse the effect.
Flags later on the command line override earlier ones.

## Compiler flags:

  **-D** *macro*

    Define a macro for the BSV or Verilog preprocessor

  **-E**

    Run just the preprocessor, dumping result to stdout

  **-I** *path*

    Include path for compiling foreign C/C++ source

  **-L** *path*

    Library path for linking foreign C/C++ objects

  **-O**

    Turn on various optimizations

  **-Werror**

    Make warnings to errors

  **-Xc** *arg*

    Pass argument to the C compiler

  **-Xc++** *arg*

    Pass argument to the C++ compiler

  **-Xcpp** *arg*

    Pass argument to the C preprocessor

  **-Xl** *arg*

    Pass argument to the C/C++ linker

  **-Xv** *arg*

    Pass argument to the Verilog link process

  **-aggressive-conditions**

    Construct implicit conditions aggressively

  **-bdir** *dir*

    Output directory for .bo and .ba files

  **-check-assert**

    Test assertions with the Assert library

  **-continue-after-errors**

    Aggressively continue compilation after an error has been detected

  **-cpp**

    Preprocess the source with the C preprocessor

  **-cross-info**

    Generate .info cross reference file for emacs

  **-e** *module*

    Top-level module for simulation

  **-fdir** *dir*

    Working directory for relative file paths during elaboration

  **-g** *module*

    Generate code for *module* (requires -sim or -verilog)

  **-help**

    Generate help message

  **-i** *dir*

    Override $BLUESPECDIR

  **-info-dir** *dir*

    Output directory for cross-info files

  **-keep-fires**

    Preserve CAN_FIRE and WILL_FIRE signals

  **-keep-inlined-boundaries**

    Preserve inlined register and wire boundaries

  **-l** *library*

    Library to use when linking foreign C/C++ objects

  **-license-type** *type*

    Sets the type of license (Seat or Floating or BlueSimOnly or Any) for bsc

  **-licenseWarning** *days*

    Sets the number of days before a license expires to issue a warning

  **-lift**

    Lift method calls in "if" actions

  **-o** *name*

    Name of generated executable

  **-opt-undetermined-vals**

    Aggressive optimization of undetermined values

  **-p** *path*

    Directory path (**:** sep.) for source and intermediate files

  **-parallel-sim-link** *jobs*

    Specify the # of simultaneous jobs when linking Bluesim

  **-print-expiration**

    Print the expiration date and exit

  **-print-flags**

    Print flag values after command-line parsing

  **-remove-dollar**

    Remove dollar signs from Verilog identifiers

  **-remove-empty-rules**

    Remove rules whose bodies have no side effects

  **-remove-false-rules**

    Remove rules whose condition is provably false

  **-remove-starved-rules**

    Remove rules that are never fired by the generated schedule

  **-remove-unused-modules**

    Remove unconnected modules from the Verilog

  **-reset-prefix** *name*

    Reset name or prefix for generated modules

  **-resource-off**

    Fail on insufficient resources

  **-resource-simple**

    Reschedule on insufficient resources

  **-runtime-license**

    Control use of run-time license vs. compile-time license

  **-sat-cudd**

    Use CUDD BDD for disjoint testing and SAT

  **-sat-stp**

    Use STP SMT for disjoint testing and SAT

  **-sat-yices**

    Use Yices SMT for disjoint testing and SAT

  **-scemi**

    Build a model using Sce-Mi link

  **-scemi-classic**

    Build a model or a testbench model using the Classic Sce-Mi infrastructure

  **-scemiTB**

    Build a testbench model using Sce-Mi link

  **-sched-dot**

    Generate .dot files with schedule information

  **-scheduler-effort** *limit*

    Set effort for disjoint testing during scheduling

  **-show-all-warnings**

    Clear the list of warnings to ignore

  **-show-compiles**

    Show recompilations

  **-show-elab-progress**

    Display trace as modules, rules, methods are elaborated

  **-show-license-detail**

    Show more details regarding license acquisition

  **-show-method-conf**

    Show method conflict information in the generated code

  **-show-module-use**

    Output instantiated Verilog modules names

  **-show-range-conflict**

    Show predicates when reporting a parallel-composability error

  **-show-rule-rel** *r1* *r2*

    Display scheduling information about rules r1 and r2

  **-show-schedule**

    Show generated schedule

  **-show-stats**

    Show package statistics

  **-sim**

    Compile BSV generating Bluesim object

  **-simdir** *dir*

    Output directory for Bluesim intermediate files

  **-split-if**

    Split "if" in actions

  **-steps** *n*

    Terminate elaboration after this many function unfolding steps

  **-steps-max-intervals** *n*

    Terminate elaboration after this number of unfolding messages

  **-steps-warn-interval** *n*

    Issue a warning each time this many unfolding steps are executed

  **-suppress-warnings** *list*

    Ignore a list of warnings (**:** sep list of tags)

  **-systemc**

    Generate a SystemC model

  **-u**

    Check and recompile packages that are not up to date

  **-unspecified-to** *val*

    Remaining unspecified values are set to: 'X', '0', '1', 'Z', or 'A'

  **-v**

    Same as -verbose

  **-v95**

    Generate strict Verilog 95 code

  **-vdir** *dir*

    Output directory for .v files

  **-verbose**

    Be talkative

  **-verilog**

    Compile BSV generating Verilog file

  **-vsearch** *path*

    Search path (**:** sep.) for Verilog files

  **-vsim** *simulator*

    Specify which Verilog simulator to use

  **-wait-for-license**

    Wait for license to free rather than exit

  **-warn-action-shadowing**

    Warn when a rule's action is overwritten by a later rule

  **-warn-method-urgency**

    Warn when a method's urgency is arbitrarily chosen

  **-warn-scheduler-effort**

    Displays warnings when the scheduler limit is reached

The import path and Verilog search path can contain the character
**%** representing the current Bluespec directory, as well as
**+** representing the current path.

# EXAMPLES

  To get help:

    bsc -help

  To partially compile a Bluespec file:

    bsc [flags] file.bsv

  To compile a module to Verilog:

    bsc [flags] -verilog -g mod file.bsv

  To recursively compile modules to Verilog:

    bsc [flags] -verilog -g mod -u file.bsv

  To link Verilog into a simulation model:

    bsc [flags] -verilog -e topmodule

  To compile to a Bluesim object:

    bsc [flags] -sim -g mod file.bsv

  To recursively compile to Bluesim objects:

    bsc [flags] -sim -g mod -u file.bsv

  To link objects into a Bluesim binary:

    bsc [flags] -sim -e topmodule

  To link objects into a SystemC model:

    bsc [flags] -systemc -e topmodule
