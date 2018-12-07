% build(1) build Manual
% Bluespec, Inc. <support@bluespec.com>

# NAME

build - Bluespec build utility

# SYNOPSIS

**build** [*options*]

# DESCRIPTION

  The 'build' utility codifies the rules for building Bluespec
  projects in a single place and eliminates most of the boilerplate
  involved in setting up a project, with the goal that sensible
  defaults are used and deviations from the defaults must only be
  specified once per project.

  It also allows for a repeatable build procedure with safe defaults,
  erring on the side of rebuilding too much rather than using a stale
  build product.

# USAGE

  The 'build' utility reads a project description from a project
  configuration file, specified with the -p (or --project) option.  If
  none is given, it reads from 'project.bld' by default.

  The project description defines a number of build targets and
  provides directives that describe what the target is and how it
  should be rebuilt.  All arguments to the build utility are
  interpreted as targets which should be rebuilt.  If no targets are
  supplied, the build utility will try to determine a sensible default
  action based on the project description file.

  Each build target comprises a number of stages that are executed in
  sequence to rebuild that target.

  To see which targets are available in a project, and which stages
  are involved in rebuilding each target, use the -l (or --list)
  option.

  By default, all of the stages in a selected target are executed, so
  that the target is fully rebuilt from scratch.  The user can select
  a subset of stages to execute using the -t (or --to) and -f (or
  --from) options.

  The --dry-run option allows the commands executed in each stage to
  be displayed without actually executing any of them.

  As the build utility executes, it logs the output of commands into
  log files using the naming scheme <target>_<stage>.log.  If the -v
  (or --verbose) option is given, the command output is also displayed
  on the screen.

# PROJECT FILES

  Project files use a simple configuration file syntax similar to a
  Windows .ini file syntax.  The file is divided into sections by
  target names enclosed in square brackets:

    [target1]
    ...

    [target2]
    ...

  Beneath each section header there can be a number of directives of
  the form:

    var: value

  or:

    var=value

  Values are typically strings, strings separated by spaces, or
  boolean values encoded as 'y'/'n', 'yes'/'no', '1'/'0',
  'true'/'false' or 'on'/'off'.  Boolean values can also use the
  alternate single-token syntax 'var' instead of 'var: true' and
  '!var' instead of 'var: false'.

  Values can contain environment variable references of the form
  ${VAR}.  The content of the named environment variable is
  substituted into the value string before it is processed.  It is an
  error if the named variable is not defined in the environment.

  The special variable ${PROJECT_ROOT} holds the name of the
  directory containing the project file.

  The special variable ${BUILD_TARGET} holds the top-level target
  used to run the given directive.

  The special variable ${BUILD_SECTION} holds the name of the
  section containing the current directive.

  The special variable ${CXX_FAMILY} holds the bsenv response to
  the cxx_family command -- A special path for library linkage.

  Comments begin with '#' and continue until the end of the line.

  The section headers define the various build targets and the
  directives within the sections describe the target and its build
  procedure.  The build utility constructs the required command
  sequence based on the supplied directives for each target.

  Sections can be hidden using the 'hide-target' directive.  Hidden
  sections cannot be used as targets directly.  However, they can be
  used to define other sections using the 'extends-target' mechanism.
  Any section can name another section in an
  'extends-target: <section>' directive, causing the directives of
  that section to be incorporated into the current section.  When a
  directive in the current section also appears in the extended
  section, the values are reconciled in a directive-dependent manner.

  The user can define defaults in a special section (using the special
  header [DEFAULT]), whose directives apply to all targets.  The
  DEFAULT section is treated as a hidden section which is extended by
  all other sections.

  It is possible to define targets which simply sequence the stages of
  other targets, using the 'sub-targets: <list of targets>' directive.
  A section which contains this directive will incorporate the
  sequence of stages for the listed targets, in order, and will not
  define any stages locally.  Thus, the 'sub-targets' directive allows
  the creation of "meta-targets".

  It is also possible to define targets which will run before and after
  the stages defined locally in a section using the 'pre-targets' and
  'post-targets' directives.

  Targets can be made conditional using the 'skip-target-when'
  directive.  This directive provides a command to be executed in a
  shell prior to building the target.  If the command returns an exit
  status of 0, the stages for the target will be skipped.  There is
  also a 'skip-target-unless' directive which will skip the target
  stages when the exit status is not 0.

  Some targets attempt to clean directories before they run.  If
  multiple targets are run and a later target wishes to clean a
  directory or file that an earlier target has already cleaned, the
  second cleaning will be suppressed.

  The backslash character is used in four escape sequences:
    \# is interpreted as a single '#' character that does not
       start a comment.
    \$ is interpreted as a single '$' character that does not
       introduce an environment variable name.
    \\ is interpreted as a single '\' character that does not
       escape the following character.
    \  at the end of a line is a continuation marker for values
       that span multiple lines.

# DIRECTIVES

## Build targets

  default-targets: <list of targets>

    This directive can be used only within the DEFAULT section to
    define which targets should be rebuilt if no target is specified
    on the command line.

  include-files: <list of files>

    This directive permits the inclusion of other project description
    files.

  skip-target-unless: <command>

    When present, the given command will be run in a shell and if the
    exit status of the command is not 0, the target stages will be
    skipped.

  skip-target-when: <command>

    When present, the given command will be run in a shell and if the
    exit status of the command is 0, the target stages will be
    skipped.

## Controlling bsc

  bsc: <executable name>

    This is the name of the command to use for invoking the Bluespec
    compiler.  Default value is 'bsc' or uses $BSC if defined.

  bsc-compile-options: <list of options>

    This directive can be used to supply options to the bsc
    compilation stage.

  bsc-link-options: <list of options>

    This directive can be used to supply options to the bsc link
    stage.

  bsc-rts-options: <list of options>

    This directive can be used to supply RTS options to the bsc
    compilation stage.

  bsv-define: <list of definitions>

    Specifies macro definitions (of the form <VAR> or <VAR>=<VAL>) to
    be passed to the bsv compile stage using the -D option.

  build-for: <platform>

    Specifies whether to build for Bluesim, Verilog, FPGAs, etc.
    Valid values are:
      bluesim    - build and link for Bluesim
      verilog    - build and link for Verilog simulation
      rtl        - build Verilog RTL but do not link for simulation
      rtllink    - link for Verilog simulation, but do not build Verilog RTL
      dn7002     - build Verilog RTL and synthesize for a Dini 7002 board
      dn7006     - build Verilog RTL and synthesize for a Dini 7006 board
      dn7406     - build Verilog RTL and synthesize for a Dini 7406 board
      dn10ghxtll - build Verilog RTL and synthesize for a Dini PCIe_10G_HXT_LL board
      ml507      - build Verilog RTL and synthesize for a Xilinx ML507 board
      ml605      - build Verilog RTL and synthesize for a Xilinx ML605 board
      kc705      - build Verilog RTL and synthesize for a Xilinx KC705 board
      vc707      - build Verilog RTL and synthesize for a Xilinx VC707 board
      xupv5      - build Verilog RTL and synthesize for a Xilinx XUPV5 board
      c++        - build software from C++ source code
      systemc    - build software from SystemC source code
    Default value is 'bluesim'.

  cosim-scd-file: <cosim simulator input file path>

    Specifies the filename used as input to the CoSimulation executable.
    Default value is 'dump.scd' or uses $[] if defined.

  cosim-verilog-simulator: <cosim simulator name>

    This directive can be used to specify which simulator is used for
    CoSim simulation.  Default value is 'cvc' or uses $[] if defined.

  imported-c-files: <list of files>

    Specifies a list of C source or object files to be included in the
    linking of the Verilog or Bluesim simulation executable.

  imported-verilog-files: <list of files>

    Specifies a list of verilog files to be included in the linking of the
    Verilog simulation executable or synthesis.

  verilog-define: <list of definitions>

    Specifies macro definitions (of the form <VAR> or <VAR>=<VAL>) to
    be passed to the Verilog simulator using the -D option, to the bsc
    link stage and to the synthesis tools.

  verilog-simulator: <simulator name>

    This directive can be used to specify which simulator is used for
    Verilog simulation.  Default value is 'modelsim' or uses
    $BSC_VERILOG_SIM if defined.

## Describing the project layout

  binary-directory: <directory>

    The name of output directory for Bluespec binary data files, which
    becomes the -bdir option.  Default value is '.'.

  board-top-module: <module name>

    The alternate name of the top-level verilog which will override the top-module directive
    when synthesizing or linking a targeted fpga verilog that may contain the design.
    Default value is ''.

  bsv-source-directories: <list of directories>

    Alternative form of bsv-source-directories which allows multiple
    source directories to be added to the -p path during bsc
    compilations.

  bsv-source-directory: <directory>

    The directory containing the BSV source code.  This directory will
    be added to the -p path during bsc compilations.  Default value is
    '.'.

  exe-file: <executable name>

    The name of the executable file to generate, for Bluesim, Verilog
    simulation or from a C++ compilation.  This is used with the -o
    option.  Default value is 'a.out'.

  info-directory: <directory>

    The name of output directory for Bluespec compiler information
    files, which becomes the -info-dir option.  Default value is '.'.

  link-lib-directories: <list of directories>

    A list of paths to search through for verilog files during bsc
    linking and verilog simulation.

  link-top-module: <module name>

    The alternate name of the top-level verilog which will override the top-module directive
    when linking .

  log-directory: <directory>

    The name of output directory for build logs.  Default value is '.'.

  simulation-directory: <directory>

    The name of output directory for Bluesim code files, which becomes
    the -simdir option.  Default value is '.'.

  target-root-directory: <directory>

    The name of root directory for a particular target where all the build input and output
    data will reside.  Default value is '.'.

  top-file: <file name>

    The path to the top-level BSV file.  Default value is 'Top.bsv'.

  top-module: <module name>

    The name of the top-level BSV module.  Default value is 'mk-'
    prefix used with the name derived from the top-level BSV file name.

  verilog-bootstrap-directory: <directory>

    The name of input Verilog directory for verilog linkage flow
    option.  Default value is 'vlog_bootstrap'.

  verilog-directory: <directory>

    The name of output Verilog directory, which becomes the -vdir
    option.  Default value is '.'.

  verilog-inc-directories: <list of directories>

    A list of paths to search through for verilog include files for `include statement during
    edithdl compilation, and fpga synthesis.

  verilog-lib-directories: <list of directories>

    A list of paths to search through for verilog files during bsc
    compilation, verilog simulation, and fpga synthesis.

## Explicit command targets

  post-stage-build_c++_tb: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  post-stage-build_systemc_tb: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  post-stage-compile_for_bluesim: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  post-stage-compile_for_verilog: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  post-stage-edithdl_modify_verilog: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  post-stage-generate_scemi_parameters: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  post-stage-link_for_bluesim: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  post-stage-link_for_cosim: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  post-stage-link_for_verilog: <command>

    Each post-stage-<STAGE> directive specifies a command to be
    executed after the stage command(s).

  pre-stage-build_c++_tb: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  pre-stage-build_systemc_tb: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  pre-stage-compile_for_bluesim: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  pre-stage-compile_for_verilog: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  pre-stage-edithdl_modify_verilog: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  pre-stage-generate_scemi_parameters: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  pre-stage-link_for_bluesim: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  pre-stage-link_for_cosim: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  pre-stage-link_for_verilog: <command>

    Each pre-stage-<STAGE> directive specifies a command to be
    executed before the stage command(s).

  run-shell-<STAGE>-<N>: <command>

    Each run-shell-<STAGE>-<N> directive specifies a command to be
    executed using the shell.  Commands are grouped by the <STAGE>
    part of the name, and within each stage commands are executed in
    ascending order of their numeric -<N> suffix.

  stage-order: <list of stages>

    When run-shell-<STAGE>-<N> directives are given for multiple
    stages, this directive specifies the order in which stages are
    to be executed.  If not given, the order is undefined.

## FPGA Synthesis

  altera-directory: <directory>

    The directory in which to perform Quartus synthesis for Altera designs.
    Default value is '.'.

  board-support-directory: <directory>

    The directory from which to retrieve board support files.  Default
    value is '$BLUESPECDIR/board_support'.

  board-top-fpga-file: <file name>

    This directive can be used to specify the top level verilog file that will be used
    for Quartus fpga synthesis.  Default value is 'fpga_a.v'.

  board-top-fpga-file-a: <file name>

    This directive can be used to specify the top level verilog file that will be used
    for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-file-a' directive
    specifies the top level verilog file for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-fpga-file directive for
    specifying top verilog file.

  board-top-fpga-file-b: <file name>

    This directive can be used to specify the top level verilog file that will be used
    for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-file-a' directive
    specifies the top level verilog file for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-fpga-file directive for
    specifying top verilog file.

  board-top-fpga-file-d: <file name>

    This directive can be used to specify the top level verilog file that will be used
    for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-file-a' directive
    specifies the top level verilog file for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-fpga-file directive for
    specifying top verilog file.

  board-top-fpga-file-e: <file name>

    This directive can be used to specify the top level verilog file that will be used
    for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-file-a' directive
    specifies the top level verilog file for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-fpga-file directive for
    specifying top verilog file.

  board-top-fpga-module: <module name>

    This directive can be used to specify the top level verilog module that will be used
    for Quartus fpga synthesis.  Default value is 'fpga_a'.

  board-top-fpga-module-a: <module name>

    This directive can be used to specify the top level fpga verilog module that will be
    used for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-module-a' directive
    specifies the top level fpga verilog module for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-fpga-module directive for
    specifying top fpga verilog module.  Default value is 'fpga_a'.

  board-top-fpga-module-b: <module name>

    This directive can be used to specify the top level fpga verilog module that will be
    used for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-module-a' directive
    specifies the top level fpga verilog module for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-fpga-module directive for
    specifying top fpga verilog module.  Default value is 'fpga_b'.

  board-top-fpga-module-d: <module name>

    This directive can be used to specify the top level fpga verilog module that will be
    used for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-module-a' directive
    specifies the top level fpga verilog module for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-fpga-module directive for
    specifying top fpga verilog module.  Default value is 'fpga_d'.

  board-top-fpga-module-e: <module name>

    This directive can be used to specify the top level fpga verilog module that will be
    used for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-module-a' directive
    specifies the top level fpga verilog module for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-fpga-module directive for
    specifying top fpga verilog module.  Default value is 'fpga_a'.

  board-top-module-a: <module name>

    This directive can be used to specify the top level verilog module that will be used
    for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-module-a' directive
    specifies the top level verilog module for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-module directive for
    specifying top verilog module.

  board-top-module-b: <module name>

    This directive can be used to specify the top level verilog module that will be used
    for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-module-a' directive
    specifies the top level verilog module for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-module directive for
    specifying top verilog module.

  board-top-module-d: <module name>

    This directive can be used to specify the top level verilog module that will be used
    for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-module-a' directive
    specifies the top level verilog module for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-module directive for
    specifying top verilog module.

  board-top-module-e: <module name>

    This directive can be used to specify the top level verilog module that will be used
    for Quartus fpga synthesis for each specific fpga on a board with multiple chips.
    The 'X' can have different values that match the designations of the chip to be
    synthesized (ie. 'a', 'b', etc...).  For example, 'board-top-fpga-module-a' directive
    specifies the top level verilog module for running Quartus synthesis on the fpga_a.
    When this directive is set, it overrides the board-top-module directive for
    specifying top verilog module.

  design-clock-frequency: <float>

    If set, sets the target clock frequency (mhz) for the design.
    This value will be used to determine the synthesis constraints and scemi clock.
    Default value is '0.0'.

  generate-qxp-file: <bool>

    Generate a Quartus post-synthesis netlist file.  Default value is
    false.

  generate-vo-file: <bool>

    Generate a Quartus post-synthesis verilog simulation netlist file.
    Default value is false.

  memory-clock-period: <float>

    If set, sets the reference clock period that is used to drive
    the memory interface, if one exists.  This value propagates into
    the synthesis constraints.  Default value is '0.0'.

  on-fpga: <list of fpgas>

    The designator of the FPGA on which the design will be loaded.
    Default list is: A.

  post-program-command: <command>

    A command to be run after programming the FPGA.  This is useful,
    for example, to initialize or re-enable a PCIE device.

  pre-program-command: <command>

    A command to be run before programming the FPGA.  This is useful,
    for example, to shutdown a PCIE slot safely.

  program-fpga: <bool>

    If set, after FPGA synthesis an attempt will be made to program the
    board.  If this directive is not used, then a file will be generated
    which the user must manually load onto the FPGA.  Default value is
    false.

  quartus-asm-options: <list of options>

    Specifies options to be passed to the Quartus asm tool.

  quartus-drc-options: <list of options>

    Specifies options to be passed to the Quartus drc tool.

  quartus-fit-options: <list of options>

    Specifies options to be passed to the Quartus fit tool.

  quartus-map-options: <list of options>

    Specifies options to be passed to the Quartus map tool.

  quartus-qsf-file: <path to alternate QSF template file>

    Specifies the path to an alternate QSF file.  If provided, this
    QSF file will be used instead of the default QSF file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-qsf-file-a: <path to alternate QSF template file>

    Specifies the path to an alternate QSF file.  If provided, this
    QSF file will be used instead of the default QSF file template for fpga_a.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-qsf-file-b: <path to alternate QSF template file>

    Specifies the path to an alternate QSF file.  If provided, this
    QSF file will be used instead of the default QSF file template for fpga_b.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-qsf-file-d: <path to alternate QSF template file>

    Specifies the path to an alternate QSF file.  If provided, this
    QSF file will be used instead of the default QSF file template for fpga_d.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-qsf-file-e: <path to alternate QSF template file>

    Specifies the path to an alternate QSF file.  If provided, this
    QSF file will be used instead of the default QSF file template for fpga_e.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-qsf-supplement-file: <path to supplemental QSF file>

    Specifies the path to a supplemental QSF file.  If provided, this
    QSF file will be appended to the end of the default QSF file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-sdc-file: <path to alternate SDC template file>

    Specifies the path to an alternate SDC file.  If provided, this
    SDC file will be used instead of the default SDC file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-sdc-file-a: <path to alternate SDC template file>

    Specifies the path to an alternate SDC file.  If provided, this
    SDC file will be used instead of the default SDC file template for fpga_a.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-sdc-file-b: <path to alternate SDC template file>

    Specifies the path to an alternate SDC file.  If provided, this
    SDC file will be used instead of the default SDC file template for fpga_b.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-sdc-file-d: <path to alternate SDC template file>

    Specifies the path to an alternate SDC file.  If provided, this
    SDC file will be used instead of the default SDC file template for fpga_d.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-sdc-file-e: <path to alternate SDC template file>

    Specifies the path to an alternate SDC file.  If provided, this
    SDC file will be used instead of the default SDC file template for fpga_e.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-sdc-supplement-file: <path to supplemental SDC file>

    Specifies the path to a supplemental SDC file.  If provided, this
    SDC file will be appended to the end of the default SDC file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  quartus-sta-options: <list of options>

    Specifies options to be passed to the Quartus sta tool.

  quartus-tan-options: <list of options>

    Specifies options to be passed to the Quartus tan tool.

  scemi-clock-period: <float>

    If set, sets the reference clock period that SceMi uses to
    create the uncontrolled and controlled clock domains.  This value
    propagates into the synthesis constraints.  Default value is '0.0'.

  sodimm-style: <SODIMM style>

    This directive controls the synthesis directives associated with a SODIMM
    connector on this FPGA.  Valid values are:
      NONE
      DDR2
      DDR3
      SRAM
    Default value is 'NONE'.

  use-quartus-sta: <bool>

    Use the new Quartus timing analyzer (quartus_sta) instead of the
    classic timing analyzer (quartus_tan).  Default value is false.

  xilinx-bitgen-options: <list of options>

    Specifies options to be passed to the Xilinx bitgen tool.  Default
    list is: -w.

  xilinx-directory: <directory>

    The directory in which to perform XST synthesis for Xilinx designs.
    Default value is '.'.

  xilinx-impact-options: <list of options>

    Specifies options to be passed to the Xilinx impact tool.

  xilinx-impl-strategy: <strategy to employ for Xilinx implementation>

    Specifies the overall strategy to use for Xilinx 
    implementation with PlanAhead/Vivado  Default value is
    'MapLogicOptParHighExtra'.

  xilinx-map-options: <list of options>

    Specifies options to be passed to the Xilinx map tool.  Default
    list is: -w -pr b -ol high -timing -global_opt speed -logic_opt on -register_duplication -retiming on -equivalent_register_removal on -xe n -lc auto.

  xilinx-ngdbuild-options: <list of options>

    Specifies options to be passed to the Xilinx ngdbuild tool.
    Default list is: -intstyle ise -dd ./_ngo -nt timestamp -aut.

  xilinx-par-options: <list of options>

    Specifies options to be passed to the Xilinx par tool.  Default
    list is: -w -ol high.

  xilinx-patcl-file: <path to alternate PlanAhead TCL template file>

    Specifies the path to an alternate PlanAhead TCL script.  If provided, this
    TCL fiel will be used instead of the default TCL script template.  The special
    identifiers '__BLUESPECDIR__', '__BOARD__', '__VDIRS__', '__TOP_MODULE__'
    and '__SODIMM_STYLE__' will be replaced everywhere they occur in the given
    file with appropriate substitutions.

  xilinx-patcl-supplement-file: <path to supplemental PlanAhead TCL template file>

    Specifies the path to an supplemental PlanAhead TCL script.  If provided, this
    TCL fiel will be appended to the end of the default TCL script template.  The special
    identifiers '__BLUESPECDIR__', '__BOARD__', '__VDIRS__', '__TOP_MODULE__'
    and '__SODIMM_STYLE__' will be replaced everywhere they occur in the given
    file with appropriate substitutions.

  xilinx-preserve-signals: <bool>

    Set PlanAhead options so as to preserve signals for readback debugging.
    Default value is false.

  xilinx-scr-file: <path to alternate SCR template file>

    Specifies the path to an alternate SCR file.  If provided, this
    SCR file will be used instead of the default SCR file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  xilinx-synth-strategy: <strategy to employ for Xilinx synthesis>

    Specifies the overall strategy to use for Xilinx 
    synthesis with PlanAhead/Vivado  Default value is
    'TimingWithIOBPacking'.

  xilinx-trce-options: <list of options>

    Specifies options to be passed to the Xilinx trce tool.  Default
    list is: -e 3 -u 3.

  xilinx-ucf-file: <path to alternate UCF template file>

    Specifies the path to an alternate UCF file.  If provided, this
    UCF file will be used instead of the default UCF file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  xilinx-ucf-supplement-file: <path to supplemental UCF file>

    Specifies the path to a supplemental UCF file.  If provided, this
    UCF file will be appended to the end of the default UCF file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  xilinx-use-planahead: <bool>

    Use the new Xilinx tool, PlanAhead, to synthesize the design.
    Default value is false.

  xilinx-use-precompiled: <bool>

    While using PlanAhead, use pre-synthesized ngc files for boilerplate blocks.
    Default value is false.

  xilinx-xcf-file: <path to alternate XCF template file>

    Specifies the path to an alternate XCF file.  If provided, this
    XCF file will be used instead of the default XCF file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  xilinx-xcf-supplement-file: <path to supplemental XCF file>

    Specifies the path to a supplemental XCF file.  If provided, this
    XCF file will be appended to the end of the default XCF file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  xilinx-xdc-file: <path to alternate XDC template file>

    Specifies the path to an alternate XDC file.  If provided, this
    XDC file will be used instead of the default XDC file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  xilinx-xdc-supplement-file: <path to supplemental XDC file>

    Specifies the path to a supplemental XDC file.  If provided, this
    XDC file will be appended to the end of the default XDC file template.
    The special identifiers '__BLUESPECDIR__', '__BOARD__',
    '__VDIRS__', '__TOP_MODULE__' and '__SODIMM_STYLE__' will be
    replaced everywhere they occur in the given file with appropriate
    substitutions.

  xilinx-xst-options: <list of options>

    Specifies options to be passed to the Xilinx xst tool.  Default
    list is: -intstyle ise.

## RTL editing

  design-editor-edit-params: <bool>

    This directive is usd to indicate that the SceMi params file is to be
    updated in the verilog source files editing flow.  Default value
    is false.

  design-editor-options: <list of options>

    This directive can be used to supply options to the edithdl
    utility.  Note that default options start the graphical interface.
    Set this option to -batch to run the tool in batch mode.

  design-editor-output-directory: <directory>

    The name of output directory for the modified verilog source files.
    The verilog files will have unique names and will not conflict with
    the user's source files.  Default value is '.'.

  design-editor-output-params: <file name>

    The name to use when generating the modified SCE-MI parameters file after completing the
    edithdl flow.  Default value is the value of top-module with a
    _EDITED.params suffix.

  design-editor-partition-for: <platform>

    Specifies the FPGA system to build for.
    Valid values are: (only dn7002 for now)
      none       - partitioning not active
      dn7002     - partitioning Verilog RTL for a Dini 7002 board
      dn7006     - partitioning Verilog RTL for a Dini 7006 board
      dn7406     - partitioning Verilog RTL for a Dini 7406 board
      dn10ghxtll - partitioning Verilog RTL for a Dini PCIe_10G_HXT_LL board
      ml507      - partitioning Verilog RTL for a Xilinx ML507 board
      ml605      - partitioning Verilog RTL for a Xilinx ML605 board
      kc705      - partitioning Verilog RTL for a Xilinx KC705 board
      vc707      - partitioning Verilog RTL for a Xilinx VC707 board
      xupv5      - partitioning Verilog RTL for a Xilinx XUPV5 board
    Currently only dn7002 is supported.

  design-editor-script: <file name>

    This directive can be used to specify the script for use with edithdl in modifying
    the verilog source files.  The script is used when --batch option is supplied in
    the design-editor-options.  Default value is 'replay_edits.script'.

  run-design-editor: <bool>

    This directive is usd to indicate that the edithdl will be used to
    modified the verilog netlist.  Default value is false.

  run-verilog-scemi-linkage: <bool>

    This directive is usd to indicate that the scemi linkage generation will be used to
    modified the verilog netlist.  Default value is false.

  scemi-clk-signals: <list of clk signals>

    This directive is usd to indicate all the signals that will be controlled by scemi.
    This value will be used to properly process and edit verilog files.

  scemi-reset-signals: <list of reset signals>

    This directive is usd to indicate all the signals that will be controlled by scemi.
    This value will be used to properly process and edit verilog files.

## SCE-MI

  c++-compiler: <executable name>

    Specifies the name of the C++ compiler to use.  Default value is
    'g++' or uses $CXX if defined.

  c++-define: <list of macro definitions>

    Specifies macro definitions (of the form <VAR> or <VAR>=<VAL>) to
    be passed to the C++ compiler using the -D option.

  c++-files: <list of source files>

    Lists the C++ source files for the testbench.  If no c++-files
    directive is given, all files in the c++-source-directory with
    a .c, .cc, .cpp, or .cxx extension will be used.

  c++-header-aliases: <bool>

    This directive can be used to generate header files for any
    relevant aliases for types that are being generated.  Default
    value is false.

  c++-header-directory: <directory>

    The directory in which to write generated SceMi Header .h files.
    Default value is the c++-source-directory.

  c++-header-enum-prefix: <prefix string>

    The prefix used for enums in SceMi Message Header files.  Default
    value is 'e_'.

  c++-header-member-prefix: <prefix string>

    The prefix used for class members in SceMi Message Header files.
    Default value is 'm_'.

  c++-header-probe-code: <bool>

    This directive can be used to generate an additional include file
    that will add all probes.  Default value is false.

  c++-header-targets: <list of targets>

    When generating SceMi header files, this directive defines the groups
    of types that will be generated.  Valid values are:
      inputs
      outputs
      probes
      all
      none
    Default list is: all.

  c++-header-types-package: <package name>

    The name of the top level package which defines the types needed
    for probes.  The BSV source used for the probe types is found by
    consulting the bsv-source-directory and bsv-source-directories
    directives.  Default value is the package of top-file.

  c++-libraries: <list of c++ libraries>

    This directive can be used to specify additional link libraries during
    c++ linking step.

  c++-options: <list of options>

    Specifies options to be passed to the C++ compiler.

  c++-source-directory: <directory>

    Specifies a directory from which to retrieve C++ source files.
    Default value is '.'.

  emu-testbench-type: <list of testbench types for build>

    Specifies the semu testbench types to be built.  Possible values are;
    manual, c++.

  probe-vcd-file: <file name>

    This directive can be used to specify the VCD file that contains
    probe waveforms.  Default value is 'scemi_test.vcd'.

  scemi-infrastructure-style: <style>

    When building a SCE-MI DUT, this directive defines the internal SCE-MI
    architecture that is used.  Valid values are:
      CLASSIC
      BLUENOC
    Bsc compilation will be passed a -D SCEMI_INFRA_<style> definition
    based on this directive.  Default value is 'BLUENOC'.

  scemi-parameters-file: <file name>

    The name to use when generating the SCE-MI parameters file.  This
    becomes the --params option to scemilink.  Default value is the
    value of top-module with a .params suffix.

  scemi-tb: <bool>

    This directive is used to designate a target as being a SCE-MI
    testbench.  Default value is false.

  scemi-tcp-port: <number>

    The TCP port number for the TCP link type.  This becomes the
    --port option to scemilink, and is used only when scemi-type=TCP.
    Default value is 7381.

  scemi-type: <link type>

    When building a SCE-MI DUT, this directive defines the type of
    SCE-MI link to use.  Valid values are:
      TCP
      PCIE_DINI
      PCIE_VIRTEX5
      PCIE_VIRTEX6
      PCIE_KINTEX7
      PCIE_VIRTEX7
      EVE
      SCEMI
    Bsc compilation will be passed a -D SCEMI_<scemi-type> definition
    based on this directive.  Without this directive, a non-SCE-MI
    DUT build target will be assumed, or an appropriate SCE-MI target
    will be selected based on the build-for directive.

  scemilink-options: <list of options>

    This directive can be used to supply options to the scemilink
    utility.

  shared-lib: <shared libarary name>

    This directive causes a shared library to be produced instead
    of an executable file.

  sim-testbench-type: <list of testbench types for build>

    Specifies the semu testbench types to be built.  Possible values are;
    manual, c++.

  systemc-home: <directory>

    The root of the SystemC installation, used for -I and -L options when
    a target built for SystemC uses the scemi-tb directive.  Default
    value is $SYSTEMC.

  tcl-home: <directory>

    The root of the TCL area, used when the uses-tcl directive
    is supplied.  Default value is '$BLUESPECDIR/tcllib' or uses
    $TCL_HOME if defined.

  uses-tcl: <bool>

    This directive is used to indicate that a C++ testbench target
    uses the TCL library.  Default value is false.

## Section composition

  extends-target: <section name>

    Incorporates the directives of the named section into this section.
    If extension duplicates a directive, the conflict is resolved in a
    directive-specific manner: either by using the local directive or by
    merging the values of the directives.

  hide-target: <bool>

    Do not allow targets based on this section to be named on the
    command line or used as a default target.  This directive is not
    inherited when the section is extended.  Default value is false.

  post-targets: <list of targets>

    This directive lists targets which should be run after the stages
    for the current section.

  pre-targets: <list of targets>

    This directive lists targets which should be run before the stages
    for the current section.

  sub-targets: <list of targets>

    This directive lists targets which should be run in sequence as the
    action of the current section.  A section which uses the 'sub-targets'
    directive will not be considered to build any direct products of its
    own.  This directive can be used with an otherwise empty section to
    create a meta-target which names a sequence of build targets.

## VCD player

  input-vcd-dut-path: <file name>

    This directive can be used to specify the hierchical path to the dut in the input VCD file 
    for the VCD player.  Default value is ''.

  input-vcd-file: <file name>

    This directive can be used to specify the input VCD file for the VCD player.
    Default value is ''.

## Workstation

  create-workstation-project: <bool>

    This directive is used to indicate that the Bluespec Workstation
    will be used to assist in debug.  A default project file will be
    generated for use in the Workstation.  Default value is false.

  workstation-project-file: <file name>

    This directive can be used to specify the project file for use with
    the Bluespec Workstation.  Default value is 'project.bspec'.

# EXAMPLES

  To completely rebuild the default targets using the project.bld
  file:

    build

  To completely rebuild the bsim_dut target using the demo.bld project
  file:

    build -p demo.bld bsim_dut

  To see what targets and stages are supported by the demo.bld project:

    build -p demo.bld --list

  To stop after generating RTL for the vlog_dut target of the demo.bld
  project:

    build -p demo.bld vlog_dut --to compile_for_verilog

  To continue the Dini 7002 build process starting with the synthesis step
  in the 7002 target of the demo.bld project:

    build -p demo.bld 7002 --from quartus_map

  To see what commands would be executed to rebuild the sw_tb target
  of the demo.bld project:

    build --dry-run -p demo.bld sw_tb

  The simplest possible projects file contains just a single target name:

    [dut]

  It defines a target named dut that builds a Bluesim executable for
  the mkTop module from a file called Top.bsv.

  A typical project file will define additional targets and provide
  more directives to specialize the build process.  For example, this
  project file defines a SCE-MI dut and two versions of a testbench
  for it, one in BSV and one in SystemC:

    [DEFAULT]
    default-targets:      bsim_dut bsim_tb
    bsc-compile-options:  -aggressive-conditions -keep-fires
    bsc-link-options:     -keep-fires

    # description of DUT

    [dut]
    hide-target
    top-file:             gcd/Bridge.bsv
    bsv-source-directory: gcd
    scemi-type:           TCP
    scemi-tcp-port:       3375
    verilog-directory:    vlog_dut
    binary-directory:     bdir_dut
    simulation-directory: simdir_dut
    info-directory:       info_dut
    exe-file:             gcd_dut

    [bsim_dut]
    hide-target
    extends-target: dut
    build-for:      bluesim

    [vlog_dut]
    extends-target:    dut
    build-for:         verilog
    verilog-simulator: cvc
    run-design-editor: False

    [edit_hdl]
    design-editor-partition-for:    dn7002
    design-editor-options:          --batch
    design-editor-output-directory: vlog_edited
    design-editor-script:           probe1.script
    design-editor-edit-params:      True

    # description of TB in BSV

    [bsv_tb]
    hide-target
    scemi-tb
    top-file:             gcd/Tb.bsv
    top-module:           mkTestBench
    verilog-directory:    vlog_tb
    binary-directory:     bdir_tb
    simulation-directory: simdir_tb
    info-directory:       info_tb
    exe-file:             gcd_tb

    [bsim_tb]
    extends-target: bsv_tb
    build-for:      bluesim

    [vlog_tb]
    extends-target:    bsv_tb
    build-for:         verilog
    verilog-simulator: cvc

    # description of TB in SystemC

    [sysc_tb]
    scemi-tb
    build-for: systemc
    c++-files: gcd/Tb.cpp
    exe-file:  gcd_tb

