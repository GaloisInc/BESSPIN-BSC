

#include "Utils.hpp"
#include <iostream>
#include <map>
#include <bitset>
#include <stdint.h>
#include <fstream>
#include <cstdlib>
#include <sys/types.h>
#include <regex.h>

namespace utils
{

std::string createVcdId(unsigned int num)
{
  std::string symbol;
  char buf[6];
  char* cptr = buf + 6;
  unsigned int len = 0;
  *(--cptr) = '\0';
  do {
    *(--cptr) = '!' + (num % 94);
    num = num / 94;
    ++len;
  } while (num > 0);
  symbol = cptr;
  return symbol;
}


std::string hexToBinary(std::string hexstr)
{
  std::string endresult = "";
  for(unsigned int i = 0; i < hexstr.length(); i++)
    {
      endresult += getBinaryString(hexstr[i]);
    }
  return endresult;
}

std::string getBinaryString(char hex)
{

  std::map<char, std::string> binstrmap;

  binstrmap.insert(std::pair<char, std::string>('F', "1111"));
  binstrmap.insert(std::pair<char, std::string>('E', "1110"));
  binstrmap.insert(std::pair<char, std::string>('D', "1101"));
  binstrmap.insert(std::pair<char, std::string>('C', "1100"));
  binstrmap.insert(std::pair<char, std::string>('B', "1011"));
  binstrmap.insert(std::pair<char, std::string>('A', "1010"));
  binstrmap.insert(std::pair<char, std::string>('f', "1111"));
  binstrmap.insert(std::pair<char, std::string>('e', "1110"));
  binstrmap.insert(std::pair<char, std::string>('d', "1101"));
  binstrmap.insert(std::pair<char, std::string>('c', "1100"));
  binstrmap.insert(std::pair<char, std::string>('b', "1011"));
  binstrmap.insert(std::pair<char, std::string>('a', "1010"));
  binstrmap.insert(std::pair<char, std::string>('9', "1001"));
  binstrmap.insert(std::pair<char, std::string>('8', "1000"));
  binstrmap.insert(std::pair<char, std::string>('7', "0111"));
  binstrmap.insert(std::pair<char, std::string>('6', "0110"));
  binstrmap.insert(std::pair<char, std::string>('5', "0101"));
  binstrmap.insert(std::pair<char, std::string>('4', "0100"));
  binstrmap.insert(std::pair<char, std::string>('3', "0011"));
  binstrmap.insert(std::pair<char, std::string>('2', "0010"));
  binstrmap.insert(std::pair<char, std::string>('1', "0001"));
  binstrmap.insert(std::pair<char, std::string>('0', "0000"));
  binstrmap.insert(std::pair<char, std::string>('X', ""));
  binstrmap.insert(std::pair<char, std::string>('x', ""));

  if (binstrmap.count(hex) == 0) {
    fprintf(stderr, "ERROR: Illegal hex character '%c'\n", hex);
    return "";

  }
  return binstrmap.find(hex)->second;

}

std::string binaryToHex(std::string binstr)
{
	std::string endresult = "";
	for(unsigned int i = 0; i < binstr.length(); i = i+4)
	{
		endresult += getHexCharacter(binstr.substr(i,4));
	}
	return endresult;
}

char getHexCharacter(std::string str)
{

  std::map<std::string, char> hexcharmap;

  hexcharmap.insert(std::pair<std::string, char>("1111", 'F'));
  hexcharmap.insert(std::pair<std::string, char>("1110", 'E'));
  hexcharmap.insert(std::pair<std::string, char>("1101", 'D'));
  hexcharmap.insert(std::pair<std::string, char>("1100", 'C'));   
  hexcharmap.insert(std::pair<std::string, char>("1011", 'B'));
  hexcharmap.insert(std::pair<std::string, char>("1010", 'A'));  
  hexcharmap.insert(std::pair<std::string, char>("1001", '9'));
  hexcharmap.insert(std::pair<std::string, char>("1000", '8'));
  hexcharmap.insert(std::pair<std::string, char>("0111", '7'));
  hexcharmap.insert(std::pair<std::string, char>("0110", '6'));
  hexcharmap.insert(std::pair<std::string, char>("0101", '5'));
  hexcharmap.insert(std::pair<std::string, char>("0100", '4'));
  hexcharmap.insert(std::pair<std::string, char>("0011", '3'));
  hexcharmap.insert(std::pair<std::string, char>("0010", '2'));
  hexcharmap.insert(std::pair<std::string, char>("0001", '1'));
  hexcharmap.insert(std::pair<std::string, char>("0000", '0'));
  hexcharmap.insert(std::pair<std::string, char>("111", '7'));
  hexcharmap.insert(std::pair<std::string, char>("110", '6'));
  hexcharmap.insert(std::pair<std::string, char>("101", '5'));
  hexcharmap.insert(std::pair<std::string, char>("100", '4'));
  hexcharmap.insert(std::pair<std::string, char>("011", '3'));
  hexcharmap.insert(std::pair<std::string, char>("010", '2'));
  hexcharmap.insert(std::pair<std::string, char>("001", '1'));
  hexcharmap.insert(std::pair<std::string, char>("000", '0'));
  hexcharmap.insert(std::pair<std::string, char>("11", '3'));
  hexcharmap.insert(std::pair<std::string, char>("10", '2'));
  hexcharmap.insert(std::pair<std::string, char>("01", '1'));
  hexcharmap.insert(std::pair<std::string, char>("00", '0'));
  hexcharmap.insert(std::pair<std::string, char>("1", '1'));
  hexcharmap.insert(std::pair<std::string, char>("0", '0'));

  return hexcharmap.find(str)->second;
}

const std::string currentDateTime() {
    time_t     now = time(0);
    struct tm  tstruct;
    char       buf[80];
    tstruct = *localtime(&now);
    // Visit http://www.cplusplus.com/reference/clibrary/ctime/strftime/
    // for more information about date/time format
    strftime(buf, sizeof(buf), "%Y-%m-%d.%X", &tstruct);

    return buf;
}

unsigned int compressAddress6(unsigned int address) {


  std::bitset<32> addr   = std::bitset<32>(address);
  std::bitset<8>  tmp0   = std::bitset<8>(0);
  std::bitset<4>  tmp1   = std::bitset<4>(0);
  std::bitset<4>  tmp2   = std::bitset<4>(0);
  std::bitset<4>  tmp3   = std::bitset<4>(0);
  std::bitset<11> result = std::bitset<11>(0);

  for(unsigned int i = 0; i < 8; i = i+1) {
    tmp0[i] = addr[i];
  }

  if (tmp0.to_ulong() == 30) { // 1e
    result[0] = 0;
    result[1] = 0;
  } else if (tmp0.to_ulong() ==  31) { // 1f
    result[0] = 1;
    result[1] = 0;
  } else  if (tmp0.to_ulong() == 158) { // 9e
    result[0] = 0;
    result[1] = 1;
  } else  if (tmp0.to_ulong() == 159) { // 9f
    result[0] = 1;
    result[1] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address A %x %x\n", address, (unsigned int) tmp0.to_ulong());
    exit(0);
  }

  for(unsigned int i = 8; i < 12; i = i+1) {
    result[i-6] = addr[i];
  }

  for(unsigned int i = 12; i < 16; i = i+1) {
    tmp1[i-12] = addr[i];
  }
 
  if (tmp1.to_ulong() == 0) {
    result[6] = 0;
    result[7] = 0;
    result[8] = 0;
  } else if (tmp1.to_ulong() == 1) {
    result[6] = 1;
    result[7] = 0;
    result[8] = 0;
  } else if (tmp1.to_ulong() == 2) {
    result[6] = 0;
    result[7] = 1;
    result[8] = 0;
  } else if (tmp1.to_ulong() == 3) {
    result[6] = 1;
    result[7] = 1;
    result[8] = 0;
  } else if (tmp1.to_ulong() == 8) {
    result[6] = 0;
    result[7] = 0;
    result[8] = 1;
  } else if (tmp1.to_ulong() == 9) {
    result[6] = 1;
    result[7] = 0;
    result[8] = 1;
  } else if (tmp1.to_ulong() == 10) {
    result[6] = 0;
    result[7] = 1;
    result[8] = 1;
  } else if (tmp1.to_ulong() == 11) {
    result[6] = 1;
    result[7] = 1;
    result[8] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address B %x %x\n", address, (unsigned int) tmp1.to_ulong());
    exit(0);
  }

  for(unsigned int i = 16; i < 20; i = i+1) {
    tmp2[i-16] = addr[i];
  }

  if (tmp2.to_ulong() == 0) {
    result[9] = 0;
  } else if (tmp2.to_ulong() == 1) {
    result[9] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address C %x %x\n", address, (unsigned int) tmp2.to_ulong());
    exit(0);
  }

  for(unsigned int i = 20; i < 24; i = i+1) {
    tmp3[i-20] = addr[i];
  }

  if (tmp3.to_ulong() == 0) {
    result[10] = 0;
  } else if (tmp3.to_ulong() == 1) {
    result[10] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address D %x %x\n", address, (unsigned int) tmp3.to_ulong());
    exit(0);
  }

  return result.to_ulong();

}

unsigned int compressAddress7(unsigned int address) {


  std::bitset<32> addr   = std::bitset<32>(address);
  std::bitset<8>  tmp0   = std::bitset<8>(0);
  std::bitset<4>  tmp1   = std::bitset<4>(0);
  std::bitset<4>  tmp2   = std::bitset<4>(0);
  std::bitset<4>  tmp3   = std::bitset<4>(0);
  std::bitset<12> result = std::bitset<12>(0);

  for(unsigned int i = 0; i < 8; i = i+1) {
    tmp0[i] = addr[i];
  }

  if (tmp0.to_ulong() == 30) { // 1e
    result[0] = 0;
    result[1] = 0;
  } else if (tmp0.to_ulong() ==  31) { // 1f
    result[0] = 1;
    result[1] = 0;
  } else  if (tmp0.to_ulong() == 158) { // 9e
    result[0] = 0;
    result[1] = 1;
  } else  if (tmp0.to_ulong() == 159) { // 9f
    result[0] = 1;
    result[1] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address 0 %x %x\n", address, (unsigned int) tmp0.to_ulong());
    exit(0);
  }

  for(unsigned int i = 8; i < 12; i = i+1) {
    result[i-6] = addr[i];
  }

  for(unsigned int i = 12; i < 16; i = i+1) {
    tmp1[i-12] = addr[i];
  }
 
  if (tmp1.to_ulong() == 0) {
    result[6] = 0;
    result[7] = 0;
    result[8] = 0;
  } else if (tmp1.to_ulong() == 1) {
    result[6] = 1;
    result[7] = 0;
    result[8] = 0;
  } else if (tmp1.to_ulong() == 2) {
    result[6] = 0;
    result[7] = 1;
    result[8] = 0;
  } else if (tmp1.to_ulong() == 3) {
    result[6] = 1;
    result[7] = 1;
    result[8] = 0;
  } else if (tmp1.to_ulong() == 4) {
    result[6] = 0;
    result[7] = 0;
    result[8] = 1;
  } else if (tmp1.to_ulong() == 6) {
    result[6] = 0;
    result[7] = 1;
    result[8] = 1;
  } else if (tmp1.to_ulong() == 7) {
    result[6] = 1;
    result[7] = 1;
    result[8] = 1;
  } else if (tmp1.to_ulong() == 8) {
    result[6] = 1;
    result[7] = 0;
    result[8] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address 1 %x %x\n", address, (unsigned int) tmp1.to_ulong());
    exit(0);
  }

  for(unsigned int i = 16; i < 20; i = i+1) {
    tmp2[i-16] = addr[i];
  }

  if (tmp2.to_ulong() == 0) {
    result[9] = 0;
    result[10] = 0;
  } else if (tmp2.to_ulong() == 2) {
    result[9] = 1;
    result[10] = 0;
  } else if (tmp2.to_ulong() == 4) {
    result[9] = 0;
    result[10] = 1;
  } else if (tmp2.to_ulong() == 8) {
    result[9] = 1;
    result[10] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address 2 %x %x\n", address, (unsigned int) tmp2.to_ulong());
    exit(0);
  }

  for(unsigned int i = 20; i < 24; i = i+1) {
    tmp3[i-20] = addr[i];
  }

  if (tmp3.to_ulong() == 0) {
    result[11] = 0;
  } else if (tmp3.to_ulong() == 4) {
    result[11] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address 3 %x %x\n", address, (unsigned int) tmp3.to_ulong());
    exit(0);
  }

  return result.to_ulong();

}
unsigned int compressAddress(unsigned int address, Family family) {
  return address;
}

// unsigned int compressAddress(unsigned int address, Family family) {
//   if (family == VIRTEX6) {
//     return compressAddress6(address);
//   }
//   if (family == KINTEX7) {
//     return compressAddress7(address);
//   }
//   fprintf(stderr, "ERROR: Unsupported family\n");
//   exit(0);
// }

unsigned int decompressAddress6(unsigned int address) {

  std::bitset<32> addr   = std::bitset<32>(address);
  std::bitset<2>  tmp0   = std::bitset<2>(0);
  std::bitset<3>  tmp1   = std::bitset<3>(0);
  std::bitset<24> result = std::bitset<24>(0);

  for(unsigned int i = 0; i < 2; i = i+1) {
    tmp0[i] = addr[i];
  }

  if (tmp0.to_ulong() == 0) { // 1e
    result[0] = 0;
    result[1] = 1;
    result[2] = 1;
    result[3] = 1;
    result[4] = 1;
    result[5] = 0;
    result[6] = 0;
    result[7] = 0;
  } else if (tmp0.to_ulong() ==  1) { // 1f
    result[0] = 1;
    result[1] = 1;
    result[2] = 1;
    result[3] = 1;
    result[4] = 1;
    result[5] = 0;
    result[6] = 0;
    result[7] = 0;
  } else  if (tmp0.to_ulong() == 2) { // 9e
    result[0] = 0;
    result[1] = 1;
    result[2] = 1;
    result[3] = 1;
    result[4] = 1;
    result[5] = 0;
    result[6] = 0;
    result[7] = 1;
  } else  if (tmp0.to_ulong() == 3) { // 9f
    result[0] = 1;
    result[1] = 1;
    result[2] = 1;
    result[3] = 1;
    result[4] = 1;
    result[5] = 0;
    result[6] = 0;
    result[7] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address %x\n", address);
    exit(0);
  }

  for(unsigned int i = 8; i < 12; i = i+1) {
    result[i] = addr[i-6];
  }

  for(unsigned int i = 0; i < 3; i = i+1) {
    tmp1[i] = addr[i+6];
  }

  if (tmp1.to_ulong() == 0) {
    result[12] = 0;
    result[13] = 0;
    result[14] = 0;
    result[15] = 0;
  } else if (tmp1.to_ulong() ==  1) {
    result[12] = 1;
    result[13] = 0;
    result[14] = 0;
    result[15] = 0;
  } else if (tmp1.to_ulong() ==  2) {
    result[12] = 0;
    result[13] = 1;
    result[14] = 0;
    result[15] = 0;
  } else if (tmp1.to_ulong() ==  3) {
    result[12] = 1;
    result[13] = 1;
    result[14] = 0;
    result[15] = 0;
  } else if (tmp1.to_ulong() ==  4) {
    result[12] = 0;
    result[13] = 0;
    result[14] = 0;
    result[15] = 1;
  } else if (tmp1.to_ulong() ==  5) {
    result[12] = 1;
    result[13] = 0;
    result[14] = 0;
    result[15] = 1;
  } else if (tmp1.to_ulong() ==  6) {
    result[12] = 0;
    result[13] = 1;
    result[14] = 0;
    result[15] = 1;
  } else if (tmp1.to_ulong() ==  7) {
    result[12] = 1;
    result[13] = 1;
    result[14] = 0;
    result[15] = 1;
  }

  result[16] = addr[9];
  result[17] = 0;
  result[18] = 0;
  result[19] = 0;

  result[20] = addr[10];
  result[21] = 0;
  result[22] = 0;
  result[23] = 0;

  return result.to_ulong();

}

unsigned int decompressAddress7(unsigned int address) {

  std::bitset<32> addr   = std::bitset<32>(address);
  std::bitset<2>  tmp0   = std::bitset<2>(0);
  std::bitset<2>  tmp1   = std::bitset<2>(0);
  std::bitset<24> result = std::bitset<24>(0);

  for(unsigned int i = 0; i < 2; i = i+1) {
    tmp0[i] = addr[i];
  }

  if (tmp0.to_ulong() == 0) { // 1e
    result[0] = 0;
    result[1] = 1;
    result[2] = 1;
    result[3] = 1;
    result[4] = 1;
    result[5] = 0;
    result[6] = 0;
    result[7] = 0;
  } else if (tmp0.to_ulong() ==  1) { // 1f
    result[0] = 1;
    result[1] = 1;
    result[2] = 1;
    result[3] = 1;
    result[4] = 1;
    result[5] = 0;
    result[6] = 0;
    result[7] = 0;
  } else  if (tmp0.to_ulong() == 2) { // 9e
    result[0] = 0;
    result[1] = 1;
    result[2] = 1;
    result[3] = 1;
    result[4] = 1;
    result[5] = 0;
    result[6] = 0;
    result[7] = 1;
  } else  if (tmp0.to_ulong() == 3) { // 9f
    result[0] = 1;
    result[1] = 1;
    result[2] = 1;
    result[3] = 1;
    result[4] = 1;
    result[5] = 0;
    result[6] = 0;
    result[7] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address %x\n", address);
    exit(0);
  }

  for(unsigned int i = 8; i < 12; i = i+1) {
    result[i] = addr[i-6];
  }

  for(unsigned int i = 12; i < 15; i = i+1) {
    result[i] = addr[i-6];
  }

  if (result[12] == 1 && result[13] == 0 && result[14] == 1) {
    result[12] = 0;
    result[13] = 0;
    result[14] = 0;
    result[15] = 1;
  } 

  for(unsigned int i = 0; i < 2; i = i+1) {
    tmp1[i] = addr[i+9];
  }

  if (tmp1.to_ulong() == 0) { // 0
    result[16] = 0;
    result[17] = 0;
    result[18] = 0;
    result[19] = 0;
  } else if (tmp1.to_ulong() ==  1) { // 2
    result[16] = 0;
    result[17] = 1;
    result[18] = 0;
    result[19] = 0;
  } else  if (tmp1.to_ulong() == 2) { // 4
    result[16] = 0;
    result[17] = 0;
    result[18] = 1;
    result[19] = 0;
  } else  if (tmp1.to_ulong() == 3) { // 8
    result[16] = 0;
    result[17] = 0;
    result[18] = 0;
    result[19] = 1;
  } else {
    fprintf(stderr, "ERROR: unexpected address %x\n", address);
    exit(0);
  }

  if (addr[11] == 1) {
    result[22] = 1;
  }

  return result.to_ulong();

}

unsigned int decompressAddress(unsigned int address, Family family) {
  return address;
}

// unsigned int decompressAddress(unsigned int address, Family family) {
//   if (family == VIRTEX6) {
//     return decompressAddress6(address);
//   }
//   if (family == KINTEX7) {
//     return decompressAddress7(address);
//   }
//   fprintf(stderr, "ERROR: Unsupported family\n");
//   exit(0);
// }

unsigned int compressOffset(unsigned int offset, Family family)
{

  static std::map<unsigned int, unsigned int> offsetmap;

  if (family == VIRTEX6) {
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 0, 0));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 2, 1));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 4, 2));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 6, 3));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 8, 4));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(12, 5));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(19, 6));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(20, 7));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(22, 8));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(23, 9));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(25, 10));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(26, 11));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(28, 12));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(29, 13));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(31, 14));
  }
  if (family == KINTEX7) {
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 1, 0));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 2, 1));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 3, 2));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 4, 3));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 5, 4));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 6, 5));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 9, 6));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(10, 7));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(19, 8));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(20, 9));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(22,10));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(23,11));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(26,12));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(27,13));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(28,14));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(29,15));
  }

  uint32_t cnt;
  uint32_t local_offset;
  uint32_t offset_mod = 0;
    
  local_offset = offset % 32;
  cnt          = offset / 32;

  if (offsetmap.count(local_offset) == 0) {
    std::cerr << "ERROR: unexpected offset " << offset << std::endl;
  } else {
    offset_mod = offsetmap.find(local_offset)->second;
  }

  offset_mod = 16 * cnt + offset_mod;

  return offset_mod;


}

unsigned int decompressOffset(unsigned int offset, Family family)
{

  static std::map<unsigned int, unsigned int> offsetmap;

  if (family == VIRTEX6) {
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 0, 0));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 1, 2));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 2, 4));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 3, 6));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 4, 8));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 5, 12));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 6, 19));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 7, 20));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 8, 22));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 9, 23));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(10, 25));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(11, 26));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(12, 28));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(13, 29));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(14, 31));
  }
  if (family == KINTEX7) {
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 0, 1));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 1, 2));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 2, 3));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 3, 4));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 4, 5));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 5, 6));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 6, 9));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 7, 10));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 8, 19));
    offsetmap.insert(std::pair<unsigned int, unsigned int>( 9, 20));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(10, 22));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(11, 23));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(12, 26));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(13, 27));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(14, 28));
    offsetmap.insert(std::pair<unsigned int, unsigned int>(15, 29));
  }

  uint32_t cnt;
  uint32_t local_offset;
  uint32_t offset_mod = 0;
    
  local_offset = offset % 16;
  cnt          = offset / 16;

  if (offsetmap.count(local_offset) == 0) {
    std::cerr << "ERROR: unexpected offset " << offset << std::endl;
  } else {
    offset_mod = offsetmap.find(local_offset)->second;
  }

  offset_mod = 32 * cnt + offset_mod;

  return offset_mod;
}

static bool regex_match(const char *string, const char *regex)
{
  regex_t preg;
  regcomp(&preg, regex, 0);
  int match = regexec(&preg, string, 0, NULL, 0);
  regfree(&preg);
  return match == 0;
}

static const char *kintex7_expr_0 = "^n_[0-9]+_.+_i__[0-9]+$";
static const char *kintex7_expr_1 = "^n_[0-9]+_.+_i$";

bool isAddedXilinxNetName(Family family, const char* name)
{
  if (family == KINTEX7) {
    if (name[0] == 'n' && name[1] == '_') {
      if (regex_match(name, kintex7_expr_0)) {
	return true;
      } 
      if (regex_match(name, kintex7_expr_1)) {
	return true;
      }
      //      fprintf(stderr, "SKIP! %s\n", name);
    }
  }
  return false;
}

static const char *xc7k325t_expr = "^xc7k325t.*$";
static const char *xc7k410t_expr = "^xc7k410t.*$";
static const char *xc7v2000t_expr = "^xc7v2000t.*$";
static const char *xc7vx485t_expr = "^xc7vx485t.*$";
static const char *xc7vx690t_expr = "^xc7vx690t.*$";
static const char *xc7z020_expr = "^xc7z020.*$";

Part getPart(const char* part_name)
{
  if (regex_match(part_name, xc7k325t_expr)) {
    return XC7K325T;
  }
  if (regex_match(part_name, xc7k410t_expr)) {
    return XC7K410T;
  }
  if (regex_match(part_name, xc7v2000t_expr)) {
    return XC7V2000T;
  }
  if (regex_match(part_name, xc7vx485t_expr)) {
    return XC7VX485T;
  }
  if (regex_match(part_name, xc7vx690t_expr)) {
    return XC7VX690T;
  }
  if (regex_match(part_name, xc7z020_expr)) {
    return XC7Z020;
  }
  fprintf(stderr, "ERROR: unsupported part: %s\n", part_name);
  exit(0);
}


unsigned int getSLRSize(Part part)
{
  switch (part) {
  case XC7K325T:
    return 0;
  case XC7K410T:
    return 0;
  case XC7V2000T:
    return 150;
  case XC7VX485T:
    return 0;
  case XC7VX690T:
    return 0;
  case XC7Z020:
    return 0;
  default:
    return 0;
  }
}

unsigned int getVersion()
{
  return 1;
}

}
