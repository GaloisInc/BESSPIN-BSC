#  Requires that LIBDIR is set

export BSCFLAGS_EXT= -stdlib-names +RTS -K32M -RTS -vsearch $(LIBDIR)
export BLUESPECDIR = $(LIBDIR)
export BSC ?= $(LIBDIR)/bin/bsc
