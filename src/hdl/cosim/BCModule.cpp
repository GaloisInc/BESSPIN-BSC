#include <string.h>

#include "VHandle.h"
#include "BCPort.h"
#include "BCModule.h"

// Static data member
BCModule BCModule::nullModule = BCModule(VHandle(0));

BCPort & BCModule::addPort (class VHandle &port, class VHandle &net)
{
  m_ports.push_back (BCPort(port, net));
  BCPort & p = m_ports.back();
  p.setIdx (m_ports.size() - 1);
//  printf("ADDING %s\n", p.getName());
  return p;
}

unsigned int  BCModule::getIdx () const
{
  return m_idx;
}
void  BCModule::setIdx (unsigned int i)
{
  m_idx = i;
}

BCPort &  BCModule::getPort (unsigned int idx)
{
  if (idx >= m_ports.size())  return BCPort::nullPort;
  return m_ports[idx];
}
// Linear search O(n)
// relace with hash map if performance is issue
BCPort &  BCModule::lookupPort (const char *pn)
{
  for (unsigned int i = 0 ; i < m_ports.size(); i++ ) {
    PLI_BYTE8 *n = m_ports[i].getName();

    if (0 == (strcmp (n, pn))){
      printf("FOUND: %s\n", pn);
      return m_ports[i];
    }
  }
  return BCPort::nullPort;
}

VHandle & BCModule::getModule()
{
  return m_module;
}
