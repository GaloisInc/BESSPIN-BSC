#include <string>
#include <stdlib.h>

#include "Module.hpp"
#include "Utils.hpp"

class Export  
{

public:

  Export();

  ~Export();

  static unsigned int exportDesign(Family family, Module* mod_rtl, Module* mod_synth, std::string file, bool include_hidden=false);


};
