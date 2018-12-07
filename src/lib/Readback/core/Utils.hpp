#pragma once

#include <string>
#include <set>
#include <map>

enum Family {NOTSET, VIRTEX6, KINTEX7};
enum Board  {NoBoard, _10GHXTLL, _10GK7LL, B2000T, DH2000TQ, DNV7F2A, KC705, ML507, ML605, PDV72KR2, VC707, VC709, XUPV5};
enum Part {NoPart, XC7K325T, XC7K410T, XC7V2000T, XC7VX485T, XC7VX690T, XC7Z020};
enum EdifView {RTL, SYNTH};

typedef std::map<std::string,  std::set<std::string>*> tCIMap;
typedef std::map<std::string,  std::set<std::string>*>::value_type tCIMapPair;
typedef std::map<std::string,  std::set<std::string>*>::iterator   tCIMapPtr;

namespace utils
{
  std::string createVcdId(unsigned int num);
  std::string hexToBinary(std::string binstr);
  std::string binaryToHex(std::string binstr);
  std::string getBinaryString(char hex);
  char getHexCharacter(std::string str);
  const std::string currentDateTime();
  unsigned int compressAddress(unsigned int address, Family family);
  unsigned int decompressAddress(unsigned int address, Family family);
  unsigned int compressOffset(unsigned int offset, Family family);
  unsigned int decompressOffset(unsigned int offset, Family family);
  bool isAddedXilinxNetName(Family family, const char* name);
  Part getPart(const char* part_name);
  unsigned int getSLRSize(Part part);
  unsigned int getVersion();
}
