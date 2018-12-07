% scemilink(1) scemilink Manual
% Bluespec, Inc. <support@bluespec.com>

# NAME

scemilink - Bluespec Compiler linker for Sce-Mi

# SYNOPSIS

**scemilink** [*OPTIONS*] <*MODULE*>

# OPTIONS

  **\-v**, **\-\-verbose**

    Write verbose status messages

  **\-\-debug**[*=FILE*]

    Generate very detailed debug traces

  **\-V**, **\-\-version**

    Show version information and exit

  **\-h**, **\-?**, **\-\-help**

    Print usage information and exit

  **\-\-designfeatures=***FILE*

    Specify the design features file location for use with EVE

  **\-p***PATH*,**\-\-path=***PATH*

    .ba file search path (% = $BLUESPECDIR and + = original path)

  **\-\-params=***FILE*

    Override the name of the generated parameters file

  **\-\-port=***PORT*

    Set the port number for the TCP link

  **\-\-scemi\-classic**

    Using the Classic SceMi infrastructure mode

  **\-\-sim**

    Generate Sce\-Mi linkage files for Bluesim

  **\-\-simdir=***DIR*

    Specify the output directory for Bluesim linkage files

  **\-\-verilog**

    Generate Sce\-Mi linkage files for Verilog

  **\-\-vdir=***DIR*

    Specify the output directory for Verilog linkage files

  **\-\-work=***DIR*

    Specify the work directory for EVE ZTide execution

  **\-\-zebu**

    Generate Sce\-Mi parameters for Zebu emulation

  **\-\-show\-license\-detail**

    Shows information regarding the license checkout
