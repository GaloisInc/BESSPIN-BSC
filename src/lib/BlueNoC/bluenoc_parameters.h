// Copyright (c) 2014 Bluespec, Inc.  ALL RIGHTS RESERVED

#ifndef __BLUENOC_PARAMETERS_H__
#define __BLUENOC_PARAMETERS_H__

#include <map>
#include <vector>
#include <list>
#include <string>
#include <exception>

#include <stdint.h>

struct bn_attribute_t;

typedef std::map<std::string, bn_attribute_t> bn_attribute_map_t;

typedef enum { 
  BN_BAD_OBJECT = -1,
  BN_IN_PIPE  = 0,
  BN_OUT_PIPE,
  BN_CLOCK,
  BN_LINK
} bn_object_kind_t;

const bn_object_kind_t minObjectKind = BN_IN_PIPE;
const bn_object_kind_t maxObjectKind = BN_LINK;

// This type describes the types of attributes supported
typedef enum {
  BN_NO_ATTR = -1,
  BN_INTEGER_ATTR = 0,
  BN_STRING_ATTR
} bn_attr_type_t;

// The bn_parameters class handles parsing of the params file.
class bluenoc_parameters
{
private:
  // Whether the parameters file successfully loaded
  bool              m_loaded;

  // A vector of attribute associations for each kind of object
  std::vector<bn_attribute_map_t> params[maxObjectKind + 1];

public:
  // CREATORS
  //
  // This constructor initializes some parameters from the parameters
  // file in the config directory, and some other parameters directly
  // from the config file
  bluenoc_parameters(const char *paramsfile);

  // This constructor is used to make an internal copy of the parameters
  // structure.
  bluenoc_parameters(const bluenoc_parameters &parameters);

  ~bluenoc_parameters();

  // ACCESSORS
  //
  // This accessor returns the number of instances of objects of the
  // specified objectKind name.
  uint32_t NumberOfObjects(const char *objectKind) const;

    //
    // These accessors return an integer or string attribute values of the
    // given object kind. It is considered an error if the index > number
    // returned by ::NumberOfObjects() or the objectKind and attributeName
    // arguments are unrecognized.
    //

  int32_t AttributeIntegerValue(const char *objectKind,     // Input: Object kind name.
				 uint32_t    index,          // Input: Index of object instance
				 const char *attributeName   // Input: Name of attribute being read
				 ) const; 

  const char *AttributeStringValue(const char *objectKind,   // Input: Object kind name.
				   uint32_t    index,        // Input: Index of object instance.
				   const char *attributeName // Input: Name of attribute being read
				   ) const;

  // MANIPULATORS
  //
  // These manipulators override an integer or string attribute values of the
  // given object kind.  It is considered an error if the index > number returned
  // by ::NumberOfObjects(). or the objectKind and attributeName arguments are 
  // unrecognized.
  void OverrideAttributeIntegerValue(const char *objectKind,    // Input: Object kind name.
				     uint32_t    index,         // Input: Index of object instance.
				     const char *attributeName, // Input: Name of attribute being read
				     int32_t     value);        // Input: New integer value of attribute.

  void OverrideAttributeStringValue(const char *objectKind,     // Input: Object kind name.
				    uint32_t    index,          // Input: Index of object instance.
				    const char *attributeName,  // Input: Name of attribute being read
				    const char *value);         // Input: New string value of attribute

  // Additional routine to test for the existence and type of an attribute
  bn_attr_type_t GetAttributeType(const char *objectKind,     // Input: Object kind name.
				  uint32_t    index,          // Input: Index of object instance.
				  const char *attributeName); // Input: Name of attribute to inspect.

  // Additional routine to allow run-time creation of parameter objects.
  // The objectKind must be valid.  Integer and String attributes will
  // be created with the value 0 and NULL, respectively.  The return value
  // is the index of the newly created object.
  uint32_t AddObject(const char* objectKind,                  // Input: Object kind name.
		     std::list<const char*> intAttrNames,     // Input: List of Integer attributes
		     std::list<const char*> stringAttrNames); // Input: List of String attributes

  //
  // Whether the parameters file has successfully loaded
  //
  bool loaded() const { return m_loaded; }
  
  //
  // Dump the content of all the parameters
  //
  void dump() const;
};

#endif // __BLUENOC_PARAMETERS_H__
