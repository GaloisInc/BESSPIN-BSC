## --------------------
## Setup system variables

# Note well these variables override the default values set by Bash shell of
# OSTYPE and MACHTYPE under linux:
# MACHTYPE = x86_64-pc-linux-gnu becomes x86_64
# OSTYPE = linux-gnu becomes Linux

MACHTYPE = $(shell $(TOP)/util/scripts/bsenv machtype)
export MACHTYPE

OSTYPE = $(shell $(TOP)/util/scripts/bsenv ostype)
export OSTYPE

W32or64 = $(shell $(TOP)/util/scripts/bsenv w32or64)
export W32or64

KERNELNAME = $(shell $(TOP)/util/scripts/bsenv kernel)
export KERNELNAME

## --------------------
## Setup common C/C++ flags

ifeq ($(MACHTYPE), $(findstring $(MACHTYPE), i386 i486 i586 i686))
# Set -m32 to be sure that CC is generating 32-bit
# Set -march=pentiumpro to generate for the least common denominator
CFLAGS ?= -march=pentiumpro -m32
CXXFLAGS ?= -march=pentiumpro -m32
else
ifeq ($(MACHTYPE), $(findstring $(MACHTYPE), x86_64))
# Set -m64 to be sure that CC is generating 64-bit
CFLAGS ?= -m64
CXXFLAGS ?= -m64
else
$(error MACHTYPE environment not recognized: $(MACHTYPE))
endif
endif
export CFLAGS
export CXXFLAGS

## --------------------
## Setup the default flex lm architecture 

# This chooses the FLEXLMVER ("v10.1" etc)
include $(TOP)/vendor/flexlm/flexdefines.mk

ifeq ($(FLEXLMVER),v10.1)

nodename = $(shell uname -n | cut -d. -f1)

r6archlist = bluejay
r6arch = $(findstring $(nodename), $(r6archlist) )

re3archlist = bluekey
re3arch = $(findstring $(nodename), $(re3archlist) )

amd64archlist = blue64
amd64arch = $(findstring $(nodename), $(amd64archlist) )

ifeq ($(OSTYPE), Darwin)
	FLEXLMARCH ?= i86_darwin
else
ifeq ($(MACHTYPE), x86_64)
	FLEXLMARCH ?= amd64_re3
else
ifeq ($(r6arch),$(nodename))
	FLEXLMARCH ?= i86_r6
else 
ifeq ($(re3arch),$(nodename))
	FLEXLMARCH ?= i86_re3
else
ifeq ($(amd64arch),$(nodename))
	FLEXLMARCH ?= amd64_re3
else
	FLEXLMARCH ?= i86_r9
endif
endif
endif
endif
endif

else

ifeq ($(MACHTYPE), x86_64)
	FLEXLMARCH ?= x64_lsb
else
	FLEXLMARCH ?= i86_lsb
endif

endif

export FLEXLMARCH

## --------------------

