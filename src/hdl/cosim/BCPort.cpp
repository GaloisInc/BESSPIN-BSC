#include "VHandle.h"
#include "BCPort.h"
#include "VValue.h"
#include "vpi_user.h"

BCPort BCPort::nullPort = BCPort( VHandle(0), VHandle(0) );

const char * BCPort::getDirection () const
{
  switch (getDir())
    {
    case vpiInput: return "Input" ;
    case vpiOutput: return "Output" ;
    case vpiInout: return "Inout" ;
    case vpiMixedIO: return "MixedIO";
    case vpiNoDirection: return "NoDirection";
    }
  return "ERROR-Direction";
}

bool BCPort::setValue (char valueStr[])
{
  if (m_net.get() == 0) return false;

  static s_vpi_value val;

  if ( ! (VValue::populateValue (&val, valueStr)))  return false;

  printf ("Set: %s to %s\n", getName(), valueStr);
  vpi_put_value (m_net.get(), &val, NULL, vpiNoDelay);
  return true;
}

