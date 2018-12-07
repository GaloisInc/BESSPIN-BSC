
PWD:=$(shell pwd)
TOP:=$(PWD)/../../../..

# definitions for platform dependent variables
include $(TOP)/platform.mk

# Install directory
PREFIX?=$(TOP)/inst

# -------------------------

# To build for JTAG instead of socket, set LUMINA_CONTROL=JTAG

ifeq ($(LUMINA_CONTROL), JTAG)
CXXFLAGS += -DJTAG_CONTROL
endif

# -------------------------

# Install location for executable
INSTALLDIR = $(PREFIX)/lib/bin

# Executable name
PROGNAME = lumina_prompt

# -------------------------

VENDOR = $(TOP)/vendor

# Local directory for the compiled libraries
OBJDIR = obj-$(KERNELNAME)$(W32or64)

# Location for Readback core source include and lib files
RDBACK_ROOT = $(TOP)/src/lib/Readback

# Location for FTDI source include and lib files
FTDI_ROOT = $(VENDOR)/libftdi/inst

# Source location for JTAG files
JTAG_DIR = $(RDBACK_ROOT)/jtag

# Source location for Socket files
SOCK_DIR = $(RDBACK_ROOT)/lumina_socket

# -------------------------

# JTAG files to be compiled into the library
JTAG_CFILES = \
	$(JTAG_DIR)/log.cpp \
	$(JTAG_DIR)/jtag.cpp \
	$(JTAG_DIR)/cable.cpp \
	$(JTAG_DIR)/sim.cpp \
	$(JTAG_DIR)/control.cpp \
	$(JTAG_DIR)/fpga.cpp \
	$(JTAG_DIR)/xpc.cpp \
	$(abspath $(wildcard $(JTAG_DIR)/xc*.cpp)) \
	$(abspath $(wildcard $(JTAG_DIR)/ft*.cpp)) \

SOCK_CFILES = \
	$(SOCK_DIR)/SocketLuminaControl.cpp \

ifeq ($(LUMINA_CONTROL), JTAG)
CTRL_CFILES = $(JTAG_CFILES)
else
CTRL_CFILES = $(SOCK_CFILES)
endif

# -------------------------

# Dependencies
SRC_INC_DIRS = \
	. \
	$(RDBACK_ROOT)/core \
	$(RDBACK_ROOT)/lumina \

ifeq ($(LUMINA_CONTROL), JTAG)
SRC_INC_DIRS += \
	$(FTDI_ROOT)/include \
	$(JTAG_DIR) \

else
SRC_INC_DIRS += \
	$(SOCK_DIR) \

endif

INC_DEPS = $(foreach dir,$(SRC_INC_DIRS),$(wildcard $(dir)/*.h*))

SRC_LIB_DIRS = \
	$(RDBACK_ROOT)/core/$(OBJDIR) \

ifeq ($(LUMINA_CONTROL), JTAG)
SRC_LIB_DIRS += \
	$(FTDI_ROOT)/lib \

endif

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
	-lreadback \
	-lz \

ifeq ($(LUMINA_CONTROL), JTAG)
CXXLIBS += \
	-lusb \
	-lftdi \

endif

SRC_CFILES = $(PROGNAME).cxx Lumina.cxx

# -------------------------

.PHONY: all
all: $(PROGNAME)

$(PROGNAME): $(SRC_CFILES) $(CTRL_CFILES) $(INC_DEPS) $(LIB_DEPS)
	$(CXX) $(CXXFLAGS) -o $@ $(SRC_CFILES) $(CTRL_CFILES) $(CXXLIBS)

# XXX Don't release this without a way for the user to rebuild,
# XXX because of the license on gzstream
.PHONY: install
install: all
#	install -d -m755 $(INSTALLDIR)
#	install -m755 $(PROGNAME) $(INSTALLDIR)

.PHONY: realclean
realclean: clean

.PHONY: clean
clean:
	rm -rf $(PROGNAME)

# -------------------------
