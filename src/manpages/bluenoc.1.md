% bluenoc(1) bluenoc Manual
% Bluespec, Inc. <support@bluespec.com>

# NAME

bluenoc - BlueNoC device utility

# SYNOPSIS

**bluenoc** **help**

**bluenoc** **info** [ <*file1*> [ .. <*fileN*> ] ]

**bluenoc** **reset** [ <*file1*> [ .. <*fileN*> ] ]

**bluenoc** **down** [ <*file1*> [ .. <*fileN*> ] ]

**bluenoc** **up** [ <*file1*> [ .. <*fileN*> ] ]

**bluenoc** **debug**   ([*+-*]<*debug_mode*>) ... [ <*file1*> [ .. <*fileN*> ] ]

**bluenoc** **profile** <*profile_command*> [ <*file1*> [ .. <*fileN*> ] ]

# OPTIONS

If no mode is specified, it defaults to 'info', so the command
'bluenoc' by itself is equivalent to 'bluenoc info' (it will search out
and describe all BlueNoC targets).

## Modes

  help    - Print usage information and exit.

  info    - Describe the BlueNoC target(s).

  reset   - Reset the BlueNoC target(s).

  down    - Deactivate the BlueNoC target(s).

  up      - Reactivate the BlueNoC target(s).

  debug   - Control debugging output written to the kernel log.

  profile - Start and stop profiling BlueNoC driver activity.

## File arguments
  
  \<file\> - Operate only on the named file.

  Multiple files can be supplied as arguments.  If no file argument is
  supplied, the tool will search for BlueNoC targets and operate on
  all of them it finds.

## Debug modes

  calls   - Log driver function calls for reading and writing.

  data    - Log all data read and written across the BlueNoC link.

  dma     - Log DMA command activity.

  intrs   - Log interrupts.

  off     - Turn off all debugging.

  on      - Turn on all debugging.

  Multiple non-off modes can be specified on the same command line.  A
  mode can be prefixed with '-' to turn it off. If it is prefixed with
  a '+' or has no prefix, it will be turned on. Prefixes should not be
  used with the 'off' and 'on' modes.

## Profiling commands:
  start   - Begin collecting profiling data.

  stop    - Stop collecting profiling data and write accumulated data
            to the kernel log.

