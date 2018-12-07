#pragma once

#include "CktEdits.h"


class CrossReference {
 public:
  // Top entry
  // review the list of edits to collect and disperse cross reference information
  static int applyCrossReferences ( struct Tcl_Interp *interp,  CktEdits::ModList_t & list );

 private:
  typedef std::map<BString, unsigned int> StringKeyMap_t;

  unsigned int       m_errCount;
  StringKeyMap_t     m_captureMap;
  BStringSet         m_captures;
  BStringSet         m_cosims;

  struct Tcl_Interp *m_interp;

 private:
  CrossReference (  struct Tcl_Interp *interp, CktEdits::ModList_t & list );

 public:
  void addErrorMsg (const BString &);

  int addCapture (unsigned int key, const BString &name);
  int addCosim   (unsigned int key, const BString &name);
  unsigned int lookupCapture (const BString &name) ;
 private:
  int getErrorCount() const {return m_errCount;}
};
