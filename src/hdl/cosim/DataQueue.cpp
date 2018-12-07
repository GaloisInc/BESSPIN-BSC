
#include "DataQueue.h"
#include <stdlib.h>
#include <string>
#include <bitset>

/* -------------------------------------------------------------------------- */
/*                                                                            */
/* -------------------------------------------------------------------------- */
  
DataQueue::DataQueue()
{
  _data    = "";
  _current = "";

}

DataQueue::~DataQueue()
{

}

bool DataQueue::HasN(unsigned num)
{
  return (_data.size() >= num);
}

void  DataQueue::PushN(unsigned value, unsigned size)
{

  if (size > 32) {

  }

  std::bitset<32> bits = value;
  unsigned pos = 32 - size;
  std::string sbits = bits.to_string<char, std::char_traits<char>, std::allocator<char> > ().substr(pos);

  _data += sbits;

}

void  DataQueue::PopN(unsigned size)
{
  _data.erase(0,size);
}

const char* DataQueue::FirstN(unsigned num)
{
  _current =  _data.substr(0,num);
  return _current.c_str();
}

void DataQueue::Clear()
{
  _data = "";
}
