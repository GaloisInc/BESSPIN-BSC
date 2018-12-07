#pragma once

#include <stdio.h>
#include "vpi_user.h"

enum IterateStatus { CONTINUE, CONT_ERROR, ERROR };

class GenFunctionClass {
public:
  virtual IterateStatus iterate (class VHandle &h ) = 0;
  virtual ~GenFunctionClass () {};
};


// C++ Wrapper around the vpiHandle type.
class VHandle {
private:
  vpiHandle m_handle;

public:
  VHandle(vpiHandle h=0)
    : m_handle(h)
  {};

  vpiHandle get() {return m_handle;}
  const vpiHandle get() const {return m_handle;}

  PLI_INT32 getProp ( const PLI_INT32 prop ) const
  {
    return vpi_get(prop, m_handle);
  }
  PLI_BYTE8 * getStr ( const PLI_INT32 prop ) const
  {
    return vpi_get_str(prop, m_handle);
  }
  VHandle getHandle ( const PLI_INT32 prop ) const
  {
    return vpi_handle( prop, m_handle);
  }

  // Scan over all children of childtype, calling "fc.iterate" on each object.
  IterateStatus map_children ( PLI_INT32 childtype, GenFunctionClass &fc )
  {
     IterateStatus ret = CONTINUE;
     IterateStatus thisret = CONTINUE;
     vpiHandle toph, iterh;
     toph = vpi_iterate (childtype, get());
     while (0 != (iterh = vpi_scan ( toph ) ) ) {
       VHandle t(iterh);
       thisret = fc.iterate ( t );

       if (thisret > ret) { ret = thisret; }
       if (thisret == ERROR) {
         vpi_free_object (toph);
         break;
       }
     }
     return ret ;
  }

};

