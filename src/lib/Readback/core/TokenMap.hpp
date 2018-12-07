#pragma once

#include <string>
#include <map>
#include <fstream>
#include <cstdlib>

class TokenMap
{
private:
  std::map<unsigned int, std::string> m_code_to_token;
  std::map<std::string, unsigned int> m_token_to_code;
  unsigned int                        m_code_next;

public:
  TokenMap();
  ~TokenMap();
  unsigned int  getCode(std::string & token);
  unsigned int  getCode(char* token);
  std::string & getToken(unsigned int code);
  unsigned int  clear();
  unsigned int  seed(std::string token, unsigned int code);
};
