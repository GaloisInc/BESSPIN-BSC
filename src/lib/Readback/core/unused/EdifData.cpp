
#include "EdifData.hpp"

EdifData::EdifData() {
  CellMap = new tCellMap;
  CIMap   = new tCIMap;
}

void EdifData::setName(const char* name) {
  Name = name;
  //    printf("NAME: %s\n", Name.c_str());
  tCellMapPtr cell_it = CellMap->find(Name);
  if (cell_it != CellMap->end()) { // it exists
    tInstanceMap* instance_map = cell_it->second;
    instance_map->insert(tInstanceMapPair(std::string(""), Name));
  }
}

const char* EdifData::getName() {
  return Name.c_str();
}

void EdifData::addCell(const char* cell_def)
{
  std::string * cell_name = new std::string(cell_def);
  CurrentInstanceMap = new tInstanceMap;
  if (cell_name->compare(Name) == 0) {
    printf("CELL_NAME: %s\n", cell_name->c_str());
    CurrentInstanceMap->insert(tInstanceMapPair(std::string(""), *cell_name));
  }
  CellMap->insert(tCellMapPair(*cell_name, CurrentInstanceMap));
}

void EdifData::addInstance(const char* cell_inst, const char* cell_def) {
  std::string* cell_name    = new std::string(cell_inst);
  std::string* cell_defname = new std::string(cell_def);
  tCellMapPtr cell_it = CellMap->find(*cell_defname);
  if (cell_it == CellMap->end()) { // a leaf
    //      printf("ADDING: %s %s\n", cell_name.c_str(), cell_defname.c_str());
    //      CurrentInstanceMap->insert(tInstanceMapPair(cell_name, cell_defname));
  } else {
    CurrentInstanceMap->insert(tInstanceMapPair(*cell_name, *cell_defname));
    printf("ADDING 0: %s %s\n", cell_name->c_str(), cell_defname->c_str());
    tInstanceMap* instance_map = cell_it->second;
    tInstanceMapPtr it;
    for (it=instance_map->begin() ; it!=instance_map->end() ; it++ )
      {
	std::string* full_name = new std::string(*cell_name);
	full_name->append("/");
	std::string path = it->first;
	std::string def  = it->second;
	full_name->append(path);
	printf("ADDING 1: %s %s\n", full_name->c_str(), def.c_str());
	CurrentInstanceMap->insert(tInstanceMapPair(*full_name, def));
      }
  }
}

void EdifData::updateCIMap () {
  tCellMapPtr cell_it = CellMap->find(Name);
  if (cell_it != CellMap->end()) { // it exists
    std::string cell = cell_it->first;
    tInstanceMap* instance_map = cell_it->second;
    tInstanceMapPtr it;
    for (it=instance_map->begin() ; it!=instance_map->end() ; it++ ) {
      std::string path = Name;
      if (it->first.length() != 0) {
	path.append("/");
      }
      path.append(it->first);
      std::string def  = it->second;
      tCIMapPtr ci_it = CIMap->find(def);
      if (ci_it != CIMap->end()) { // it exists
	std::set<std::string>* ns = ci_it->second;
	ns->insert(path);
      } else {
	std::set<std::string>* ns = new std::set<std::string>();
	ns->insert(path);
	CIMap->insert(tCIMapPair(def, ns));
      }
    }

    // add to map local instance name -> full path
    // (only if it is a unique map and doesn't clash with existing defs)
    tCIMap * zow = new tCIMap;
    for (it=instance_map->begin() ; it!=instance_map->end() ; it++ ) {
      std::string path = Name;
      std::string name;
      if (it->first.length() != 0) {
	path.append("/");
      }
      path.append(it->first);

      size_t pos;
      pos = path.find_last_of('/');
	
      if (pos != std::string::npos) {
	name = path.substr(pos+1);
	tCIMapPtr ci_it = CIMap->find(name);
	if (ci_it != CIMap->end()) { // it exists
	  //	    fprintf(stderr, "CLASH! %s\n", name.c_str());
	  continue;
	}
	ci_it = zow->find(name);
	if (ci_it != zow->end()) { // it exists
	  std::set<std::string>* ns = ci_it->second;
	  ns->insert(path);
	} else {
	  std::set<std::string>* ns = new std::set<std::string>();
	  ns->insert(path);
	  zow->insert(tCIMapPair(name, ns));
	}
      }
    }

    tCIMapPtr jt;
    for (jt=zow->begin() ; jt!=zow->end() ; jt++ ) {
      std::string cell = jt->first;
      std::set<std::string>* ns = jt->second;
      if(ns->size() == 1) {
	std::set<std::string>::iterator it;
	for (it=ns->begin() ; it!=ns->end() ; it++ ) {
	  std::string xx =  (*it);
	  std::string * pcell = new std::string(cell);
	  CIMap->insert(tCIMapPair(*pcell, ns));
	}
      } else {
	delete(ns);
      }
    }
  }
}

void EdifData::createInstanceMap(char* map_file) {

  FILE* FileMap = stderr;

  // if( (FileMap = fopen(map_file, "wt" )) == NULL ) {
  //   fprintf(stderr, "Unable to open : %s\n", map_file);
  //   exit(1);
  // }

  fprintf(stderr, "Creating map file: %s ...\n", map_file);

  fprintf(FileMap, "\n;; Edif Instance Map file created at: %s\n\n", utils::currentDateTime().c_str());

  fprintf(FileMap, "TOP: %s\n", Name.c_str());
  tCellMapPtr cell_it = CellMap->find(Name);
  if (cell_it != CellMap->end()) { // it exists
    std::string cell = cell_it->first;
    //      fprintf(stderr, "CELL %s\n", cell.c_str());
    tInstanceMap* instance_map = cell_it->second;
    tInstanceMapPtr it;
    for (it=instance_map->begin() ; it!=instance_map->end() ; it++ ) {
      std::string def = it->second;
      std::string path = "/";
      path.append(Name);
      if (def.compare(Name) != 0) {
	path.append("/");
      }
      path.append(it->first);
      fprintf(FileMap, "PATH: %s DEF: %s\n", path.c_str(), def.c_str());
    }
  }

  fprintf(stderr, "Creating map file: %s complete.\n", map_file);

  //    fclose(FileMap);
}

tCIMap* EdifData::getCIMap() {
  return CIMap;
}
