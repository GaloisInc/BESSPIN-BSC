
#include "Utils.hpp"
#include <cstdlib>
#include <iostream>
#include <map>
#include <set>
#include <stdio.h>
#include <string>

typedef std::map<std::string,  std::string>  tInstanceMap;
typedef std::map<std::string,  std::string>::value_type tInstanceMapPair;
typedef std::map<std::string,  std::string>::iterator   tInstanceMapPtr;

typedef std::map<std::string,  tInstanceMap*> tCellMap;
typedef std::map<std::string,  tInstanceMap*>::value_type tCellMapPair;
typedef std::map<std::string,  tInstanceMap*>::iterator   tCellMapPtr;

class EdifData {
protected:
  tCellMap*     CellMap;
  tInstanceMap* CurrentInstanceMap;
  tCIMap*       CIMap;
  std::string   Name;

public:
  // Constructor
  EdifData();
  
  // Destructor
  ~EdifData() 
    {
    }

  void setName(const char* name);

  const char* getName();

  void addCell(const char* cell_def);

  void addInstance(const char* cell_inst, const char* cell_def);

  void updateCIMap ();
  void createInstanceMap(char* map_file);
  tCIMap* getCIMap();
  
};



