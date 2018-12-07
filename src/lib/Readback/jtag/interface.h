#pragma once

#include <map>
#include <string>
#include <set>
#include <stdexcept>

template <typename interface_type>
class interface_registry
{
private:
  std::map< std::string, interface_type* (*)(void) > m_creator_map;
  interface_registry(){}

public:
  static interface_registry& shared_instance();

  bool register_class( const std::string &name, interface_type * (*creator)(void) );
  std::set<std::string> get_entries();

  interface_type *create_object_of(std::string name);
};

#define REGISTER( CLASS, INTERFACE )			\
  bool dummy_##CLASS = interface_registry<INTERFACE>::shared_instance().register_class(std::string(#CLASS), CLASS::create);


template <typename interface_type>
interface_registry<interface_type>& interface_registry<interface_type>::shared_instance()
{
  static interface_registry<interface_type> registry;
  return registry;
}

template <typename interface_type>
bool interface_registry<interface_type>::register_class( const std::string &name, interface_type* (*creator)(void) )
{
  m_creator_map[name] = creator;
  return true;
}

template <typename interface_type>
std::set<std::string> interface_registry<interface_type>::get_entries()
{
  std::set<std::string> keys;
  typename std::map< std::string, interface_type* (*)(void)>::iterator pair;
  
  for (pair = m_creator_map.begin(); pair != m_creator_map.end(); pair++) {
    keys.insert(pair->first);
  }
  
  return keys;
}

template <typename interface_type>
interface_type* interface_registry<interface_type>::create_object_of(std::string name)
{
  interface_type* (*creator)(void) = m_creator_map[name];

  if (creator)
    return creator();
  else
    throw runtime_error(name + string(" is not a registered type"));
}
