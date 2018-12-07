
#include "TokenMap.hpp"

TokenMap::TokenMap()
{
  clear();
}

unsigned int TokenMap::getCode(std::string & token)
{

  unsigned int code;
  std::map<std::string, unsigned int>::iterator it = m_token_to_code.find(token);
  if (it == m_token_to_code.end()) {
    m_token_to_code.insert(std::pair<std::string, unsigned int>(token, m_code_next));
    m_code_to_token.insert(std::pair<unsigned int, std::string>(m_code_next, token));
    code = m_code_next;
    m_code_next++;
  } else {
    code = it->second;
  }
  return code;

}

unsigned int TokenMap::getCode(char* token)
{

  unsigned int code;
  std::map<std::string, unsigned int>::iterator it = m_token_to_code.find(token);
  if (it == m_token_to_code.end()) {
    m_token_to_code.insert(std::pair<std::string, unsigned int>(token, m_code_next));
    m_code_to_token.insert(std::pair<unsigned int, std::string>(m_code_next, token));
    code = m_code_next;
    m_code_next++;
  } else {
    code = it->second;
  }
  return code;

}

std::string & TokenMap::getToken(unsigned int code)
{

  std::string token;
  std::map<unsigned int, std::string>::iterator it = m_code_to_token.find(code);
  if (it == m_code_to_token.end()) {
    fprintf(stderr, "ERROR: no token for code: %d\n", code);
    exit(1);
  }
  return it->second;
}

unsigned int TokenMap::clear()
{

  m_code_to_token.clear();
  m_token_to_code.clear();
  m_code_next = 0;
  return 0;
}

unsigned int TokenMap::seed(std::string token, unsigned int code)
{
  m_token_to_code.insert(std::pair<std::string, unsigned int>(token, code));
  m_code_to_token.insert(std::pair<unsigned int, std::string>(code, token));

  m_code_next = std::max(code + 1, m_code_next);
  return 0;

}
