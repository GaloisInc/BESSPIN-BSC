#pragma once
#include <string>
#include "vpi_user.h"

// Class for managing t_vpi_value
class VValue {

 public:
  static bool populateValue (t_vpi_value *pval, const char *valstr);
  static std::string showVal (const t_vpi_value *pval);

};
