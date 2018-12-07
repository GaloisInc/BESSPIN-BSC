/// Copyright (c) 2014-2016, Bluespec Inc.  ALL RIGHTS RESERVED

#include <string>

#include "Design.hpp"
#include "LuminaPlusControl.hpp"

class LuminaPlus
{
  static LuminaPlus               * m_luminaplus;

 protected:

  bool                              m_initialized;
  LuminaPlusControl               * m_pControl;
  Design                          * m_pDesign;
  RdBack::VCDWriter               * m_pVCDWriter;

  // Protected cannot be called by user
  LuminaPlus();
  ~LuminaPlus();

 public:

  static LuminaPlus *getOrCreate();
  static void destroy();

  // Operations
  void init(const unsigned int port);
  std::string query(const std::string &signal);

  // Accessors
  LuminaPlusControl* getLuminaPlusControl() { return m_pControl; }
  Design* getDesign() { return m_pDesign; }

};

