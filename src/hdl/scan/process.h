
#include "V.h"
#include "ModuleDesc.h"

#include <boost/regex.hpp>

enum ScanMode {SET_INPUTS, GET_INPUTS, \
	       SET_STATE, GET_STATE,   \
	       SET_BRAMS, GET_BRAMS,   \
	       SET_OUTPUTS, GET_OUTPUTS};

ModuleDesc* ProcessModule(Verific::VeriModule* modulex, boost::regex filter, const char *parent, const char *name, unsigned level, unsigned copy, ScanMode mode, int width);

ModuleDesc* FindAndProcessModule(Verific::VeriModule* module, const char *parent, const char *name, unsigned level);
