// Copyright 2009, Bluespec Inc, All Rights Reserved

#include "BSVType.h"
#include <sstream>

using namespace std;

BSVType::MapType * BSVType::s_pPutToOverRides = 0;

BSVType::PutTo * BSVType::lookupPutToOverride ( const char * type)
{
  if (s_pPutToOverRides == 0) return 0;

  MapType::iterator found = s_pPutToOverRides->find (type);
  if ( found == s_pPutToOverRides->end() ) {
    return 0;
  }
  return found->second;
}

// Static function
void    BSVType::setPutToOverRide ( const char * t, PutTo *f)
{
  if (s_pPutToOverRides == 0) {
    s_pPutToOverRides = new MapType();
  }

  pair<BSVType::MapType::iterator, bool> ret ;
  ret = s_pPutToOverRides->insert (MapType::value_type(t,f));
}

void BSVType::setPutToOverRide (PutTo *func) const
{
  const char * cn = getClassName();
  BSVType::setPutToOverRide (cn, func );
}

bool BSVType::operator== ( const BSVType &that) const {
  std::ostringstream as, bs;
  this->getBitString(as);
  as <<  this->getClassName();

  that.getBitString(bs);
  bs << that.getClassName();
  return as.str() == bs.str();
}

bool BSVType::operator!= ( const BSVType &that) const {
  return !(*this == that);
}



