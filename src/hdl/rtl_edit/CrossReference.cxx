

#include "CrossReference.h"
#include "TclUtils.h"
#include "tcl.h"


// Collects trigger
class CrossReferenceCollect : public CktModVisitor {
private:
  CrossReference & m_cr;
public:
  CrossReferenceCollect ( CrossReference & cr)
    : m_cr(cr)
  {}
  int novisitor (CktMod *cm) {
    return 0;
  }
  int visit (AddCapture *cm) {
    int stat = m_cr.addCapture( cm->getKey(), cm->getName() );
    if (stat != 0) {
      BString err("Duplicate capture name found: `");
      err += cm->getName() + "'" ;
      m_cr.addErrorMsg(err);
    }
    return stat;
  }
  int visit (AddCosim *cm) {
    int stat = m_cr.addCosim( cm->getKey(), cm->getName() );
    if (stat != 0) {
      BString err("Duplicate capture name found: `");
      err += cm->getName() + "'" ;
      m_cr.addErrorMsg(err);
    }
    return stat;
  }


};

///////////////////////////////////////////////////////////////////////////
class CrossReferenceDisperse : public CktModVisitor {
private:
  CrossReference & m_cr;
public:
  CrossReferenceDisperse ( CrossReference & cr)
    : m_cr(cr)
  {}
  int novisitor (CktMod *cm) {
    return 0;
  }
  int visit (AddTrigger *cm) {
    cm->clearKeys();            // empty previous data

    int stat = 0;
    const BStringList & caps = cm->getCaptures();
    for (BStringList::const_iterator iter = caps.begin(); iter != caps.end(); ++ iter ) {
      unsigned int key = m_cr.lookupCapture( *iter );
      if (key == 0) {
        BString err ("Could not find a capture probe named `");
        err += *iter + "' which was referenced in trigger `" + cm->getName() + "'";
        m_cr.addErrorMsg( err );
        stat = 1;
      } else {
        cm->addKey(key);
      }
    }
    return stat;
  }

};

///////////////////////////////////////////////////////////////////////////
int CrossReference::applyCrossReferences ( struct Tcl_Interp *interp,  CktEdits::ModList_t & list )
{
  CrossReference cr(interp, list);
  int stat = cr.getErrorCount() == 0 ? TCL_OK : TCL_ERROR ;
  return stat;
}

CrossReference::CrossReference (struct Tcl_Interp *interp, CktEdits::ModList_t & editlist )
  : m_errCount(0)
  , m_interp(interp)
{
  CktEdits::ModListIter_t iter;

  CrossReferenceCollect collector(*this);
  for (iter = editlist.begin(); iter != editlist.end(); ++iter) {
    (*iter)->accept (&collector);
  }

  CrossReferenceDisperse distributor(*this);
  for (iter = editlist.begin(); iter != editlist.end(); ++iter) {
    (*iter)->accept (&distributor);
  }

  // Check that all captures have triggers.
  if (! m_captures.empty() ) {
    BString errMsg = "The following capture probes do not have any triggers:";
    for (BStringSet::iterator iter = m_captures.begin(); iter != m_captures.end(); ++ iter) {
      errMsg += " " + *iter;
    }
    addErrorMsg (errMsg);
  }
}

void CrossReference::addErrorMsg ( const BString &err)
{
  ++m_errCount;
  toTclResult( m_interp, err);
}

int CrossReference::addCapture (unsigned int key, const BString &name)
{
  int stat;
  if (m_captureMap.find (name) != m_captureMap.end()) {
    stat = 1;
  }
  else {
    StringKeyMap_t::value_type vdata(name, key);
    m_captureMap.insert(vdata);
    stat = 0;
  }

  m_captures.insert(name);
  return stat;
}
int CrossReference::addCosim (unsigned int key, const BString &name)
{
  int stat;
  if (m_captureMap.find (name) != m_captureMap.end()) {
    stat = 1;
  }
  else {
    StringKeyMap_t::value_type vdata(name, key);
    m_captureMap.insert(vdata);
    stat = 0;
  }

  m_cosims.insert(name);
  return stat;
}
unsigned int CrossReference::lookupCapture (const BString &name)
{
  // 0 indicates an error
  StringKeyMap_t::const_iterator iter = m_captureMap.find(name);
  if (iter == m_captureMap.end()) {
    return 0;
  }
  // We erase it from the set to mark it as used.
  m_captures.erase(name);
  return (*iter).second ;
}

