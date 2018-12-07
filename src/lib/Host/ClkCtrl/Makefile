PWD := $(shell pwd)
TOP := $(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX ?= $(TOP)/inst

RM ?= rm -f
INSTALL ?= install

# -------------------------

# Install location for the header files
HDR_DIR = $(PREFIX)/lib/Host

# Install location for executable
BIN_DIR = $(PREFIX)/lib/bin

# Executable name
PROGNAME = clkctrl_prompt

# -------------------------

# Headers to be installed
HEADERS = \
	ClkCtrl.hpp \


# -------------------------

# Dependencies
SRC_INC_DIRS = \
	. \

INC_DEPS = $(foreach dir,$(SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

SRC_LIB_DIRS = \

# XXX be more specific about the lib dependencies?
LIB_DEPS = $(foreach dir,$(SRC_LIB_DIRS),$(wildcard $(dir)/*.so) $(wildcard $(dir)/*.a))

# -------------------------

# Compiler FLAGS
#CXXFLAGS += -g
CXXFLAGS += -O3
CXXFLAGS += -Wall
CXXFLAGS += -fPIC
CXXFLAGS += $(foreach dir,$(SRC_INC_DIRS),-I$(dir))
CXXFLAGS += $(foreach dir,$(SRC_LIB_DIRS),-L$(dir))

ifeq ($(OSTYPE),$(findstring $(OSTYPE), Darwin darwin))
# for Mac OS X, add additional directories to the path
CXXFLAGS += -I/opt/local/include
CXXFLAGS += -L/opt/local/lib
endif

# Compiler library arguments
CXXLIBS += \
	-lpthread \

SRC_CFILES = $(PROGNAME).cpp ClkCtrlSocket.cpp

# -------------------------

.PHONY: all
all: $(PROGNAME)

$(PROGNAME): $(SRC_CFILES) $(INC_DEPS) $(LIB_DEPS)
	$(CXX) $(CXXFLAGS) -o $@ $(SRC_CFILES) $(CXXLIBS)

.PHONY: install
install:
	$(INSTALL) -m 755 -d $(HDR_DIR)
	$(INSTALL) -m 644 $(HEADERS) $(HDR_DIR)
	install -d -m755 $(BIN_DIR)
	install -m755 $(PROGNAME) $(BIN_DIR)

.PHONY: clean
clean:
	$(RM) $(PROGNAME)

.PHONY: realclean
realclean: clean

# -------------------------
