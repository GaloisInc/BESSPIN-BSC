#pragma once

#include <fstream>
#include <sstream>
#include <cstdlib>
#include <stdint.h>
#include <map>
#include <algorithm>

class Vvp {
private:
  std::map<std::string,  unsigned int> m_num_map;
  unsigned int                         m_num_next;

public:
  Vvp()
  {
    m_num_next = 1 << 28;
  }

  ~Vvp();

  unsigned int getNetNumFromString(const std::string &l) {

    //    printf("STRING: %s\n", l.c_str());
    if (m_num_map.count(l) != 0) {
      unsigned int n = m_num_map.find(l)-> second;
      return n;
    } else {
      std::string * p = new std::string(l);
      m_num_map.insert(std::pair<std::string, unsigned int>((*p), m_num_next));
      return m_num_next++;	  
    }
  }

 unsigned int getNetNum(char* label, const char* line) {
    unsigned int matches, suffix;
    unsigned int ignore;
    std::string l = label;

    matches = sscanf (label, "v0x%x_%d", &ignore, &suffix);
    if (matches == 2) {
      if (suffix == 0) {
	return getNetNumFromString(l.substr(3, l.length() - 5));
      } else {
	return getNetNumFromString(l);
      }
    }

    matches = sscanf (label, "p0x%x", &ignore);
    if (matches == 1) {
      return getNetNumFromString(l.substr(3));
    }

    matches = sscanf (label, "L_0x%x", &ignore);
    if (matches == 1) {
      return getNetNumFromString(l.substr(4));
    }

    matches = sscanf (label, "RS_0x%x", &ignore);
    if (matches == 1) {
      return getNetNumFromString(l.substr(5));
    }

    matches = sscanf (label, "LS_0x%x", &ignore);
    if (matches == 1) { 
      return getNetNumFromString(l);
    }

    std::cerr << "ERROR: Unhandled label " << label << " " << line << " ." << std::endl;
    exit(1);
  }

  unsigned int getNewNum() {
    return m_num_next++;	  
  }

  const char* getConstString(char* label, unsigned int offset, unsigned int size) {
    int matches;
    std::string l = label;
    std::replace(l.begin(), l.end(), '<', ' ');
    std::replace(l.begin(), l.end(), '>', ' ');

    char binary_num[8192];
    matches = sscanf (l.c_str(), " C4 %s", (char *) binary_num);
    if (matches != 0) {
      //      binary_num[0] = '1';
      std::string temp = (char *) binary_num;
      unsigned int l = temp.length();
      temp = temp.substr(l - (size + offset), size);
      std::string * perm = new std::string(temp);
      return perm->c_str();
    }
    return NULL;
  }

  const char * createName(unsigned int num)
  {
    char label[16];
    sprintf(label, "AUTO_%8x", num);
    std::string * perm = new std::string((char*) label);
    return perm->c_str();
  }

  const char * createConstName(unsigned int num)
  {
    char label[16];
    sprintf(label, "CONST_%8x", num);
    std::string * perm = new std::string((char*) label);
    return perm->c_str();
  }
};
