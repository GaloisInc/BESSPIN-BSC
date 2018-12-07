#include "stdio.h"
#include <string>

#include"VValue.h"

using namespace std;


static PLI_INT32 getFromFormat (const char format)
{
  switch (format)
    {
    case 'b': case 'B': return vpiBinStrVal;
    case 'o': case 'O': return vpiOctStrVal;
    case 'd': case 'D': return vpiDecStrVal;
    case 'h': case 'H': return vpiHexStrVal;
    return vpiUndefined;
    }
  return vpiUndefined;
}

// Populates pval based on format string for valstr
//Only works for [n]'[bohd]n
bool VValue::populateValue( t_vpi_value *pval, const char *valstr)
{
  bool ret = false;
  int size;
  char format;
  static char buf0[1024];
  if (3 == (sscanf (valstr, "%d'%c%s", &size, &format, (char *) &buf0))) {
      ret = true;
      pval->format = getFromFormat (format);
      pval->value.str = (PLI_BYTE8*) &buf0;
  } else if (2 == (sscanf (valstr, "'%c%s", &format, (char *) &buf0))) {
      ret = true;
      pval->format = getFromFormat (format);
      pval->value.str = (PLI_BYTE8*) &buf0;
  } else if (1 == (sscanf (valstr, "%s", (char *) &buf0))) {
      ret = true;
      pval->format = vpiDecStrVal;
      pval->value.str = (PLI_BYTE8*) &buf0;
  } 
  return ret;
}

string VValue::showVal(const t_vpi_value *pval) 
{
  string res("XXX");
  switch (pval->format) {
  case vpiBinStrVal:
    res = string("'b") + pval->value.str;
    break;
  case vpiOctStrVal:
    res = string("'o") + pval->value.str;
    break;
  case vpiDecStrVal:
    res = string("'d") + pval->value.str;
    break;
  case vpiHexStrVal:
    res = string ("'h") + pval->value.str;
    break;
  case vpiScalarVal:
    switch (pval->value.scalar) {
    case vpi0: res = "0"; break;
    case vpi1: res = "0"; break;
    case vpiZ: res = "Z"; break;
    case vpiX: res = "X"; break;
    case vpiH: res = "H"; break;
    case vpiL: res = "L"; break;
    case vpiDontCare: res = "x"; break;
    default: res = "XX" ;
    }
    break;
  case vpiIntVal:
  case vpiRealVal:
  case vpiStringVal:
  case vpiVectorVal:
  case vpiStrengthVal:
  case vpiTimeVal:
  case vpiObjTypeVal:
  case vpiSuppressVal:
  default:
    res = "VValue::showVal Unimplemented" ;
    break;
  }
  return res;
};
