// Copyright (c) 2014 Bluespec, Inc.  ALL RIGHTS RESERVED

#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <cstdlib>
#include <stdexcept>

#include "bluenoc_parameters.h"

#define MAX_LINE_SIZE (1023)

struct bn_attribute_t {
  bn_attr_type_t tag;
  union {
    char    *strVal;
    int32_t  intVal;
  } data;
};

static bn_object_kind_t parseObjectKind(const char *objKind)
{
  if (objKind == NULL)
    return BN_BAD_OBJECT;

  if      (!strcmp(objKind, "InPipe"))    return BN_IN_PIPE;
  else if (!strcmp(objKind, "OutPipe"))   return BN_OUT_PIPE;
  else if (!strcmp(objKind, "Clock"))     return BN_CLOCK;
  else if (!strcmp(objKind, "Link"))      return BN_LINK;
  else return BN_BAD_OBJECT;
}

static const char *objectKindToString(bn_object_kind_t kind)
{
  switch (kind) {
    case BN_IN_PIPE:  return "InPipe";
    case BN_OUT_PIPE: return "OutPipe";
    case BN_CLOCK:    return "Clock";
    case BN_LINK:     return "Link";
    default:          return "?";
  }
}

bluenoc_parameters::bluenoc_parameters(const char *paramsfile)
  : m_loaded(false)
{
  std::ifstream file;
  const char *delimiter = " \n\t,";
  char lineBuf[MAX_LINE_SIZE+1];
  char *objectKind, *indexString, *attributeName, *valueString, *s;
  int32_t ivalue, index;
  char *svalue;
  uint32_t lineno = 0;

  file.open(paramsfile);
  if (file.fail()) {
    throw std::string(std::string("bluenoc_parameters::bluenoc_parameters() Unable to open parameters file ") + std::string(paramsfile) + std::string("."));
  }

  m_loaded = true;

  // Read in each line.  Right now assuming each line consists of 4 tokens:
  // ObjectKind, Index, AttributeName, Value
  while (!file.eof()) {
    file.getline(lineBuf, MAX_LINE_SIZE);
    lineno++;
    objectKind = strtok(lineBuf, delimiter);
    if (objectKind && (strncmp(objectKind, "//", 2) != 0)) { // if the line is not commented out
      indexString = strtok(0, delimiter);
      attributeName = strtok(0, delimiter);
      valueString = strtok(0, "\n\t"); // allow commas and spaces in string attribute values

      bn_object_kind_t kind = parseObjectKind(objectKind);
      if (kind == BN_BAD_OBJECT) {
	file.close();
	throw std::string(std::string("bluenoc_parameters::bluenoc_parameters() Invalid object kind."));
      }

      index = atoi(indexString);
      if (index >= (int32_t)params[kind].size()) {
	params[kind].resize(index+1);
      }

      while (valueString[0] == ' ') valueString++;

      if (valueString[0] == '"') {
	svalue = valueString+1;
	s = strchr(svalue, '"');
	if (s == NULL) {
	  int32_t len = snprintf(NULL, 0, 
				  "Invalid token in file %s on line %d at %s, closing \" is needed",
				  paramsfile, lineno, valueString);
	  char str[len+1];
	  snprintf(str, len+1, "Invalid token in file %s on line %d at %s, closing \" is needed",
				  paramsfile, lineno, valueString);

	  file.close();
	  throw std::string(std::string("bluenoc_parameters::bluenoc_parameters() ") + std::string(str));
	}
	*s = '\0';
	params[kind][index][attributeName].tag = BN_STRING_ATTR;
	params[kind][index][attributeName].data.strVal = strdup(svalue);
      }
      else if (isdigit(valueString[0])) {
	ivalue = atoi(valueString);
	params[kind][index][attributeName].tag = BN_INTEGER_ATTR;
	params[kind][index][attributeName].data.intVal = ivalue;
      }
      else {
	  int32_t len = snprintf(NULL, 0, 
				  "Invalid bluenoc parameter specification in file %s on line %d (%s)",
				 paramsfile, lineno, valueString);
	  char str[len+1];
	  snprintf(str, len+1, "Invalid bluenoc parameter specification in file %s on line %d (%s)",
		   paramsfile, lineno, valueString);

	  file.close();
	  throw std::string(std::string("bluenoc_parameters::bluenoc_parameters() ") + std::string(str));	
      }
    }
  }
  file.close();
}

// This constructor is used in the implementation to make an internal
// copy of the parameters structure.
bluenoc_parameters::bluenoc_parameters(const bluenoc_parameters &parameters)
{
  m_loaded = parameters.m_loaded;

  for(bn_object_kind_t kind = minObjectKind;
      kind <= maxObjectKind;
      kind = (bn_object_kind_t)(kind + 1)) {
    params[kind].resize(parameters.params[kind].size());
    for (uint32_t index = 0; index < parameters.params[kind].size(); ++index) {
      const bn_attribute_map_t &attr_map = parameters.params[kind][index];

      for(bn_attribute_map_t::const_iterator i = attr_map.begin();
	  i != attr_map.end();
	  ++i) {
	const std::string &name = i->first;
	const bn_attribute_t &attr = i->second;

	params[kind][index][name].tag = attr.tag;
	switch(attr.tag) {
	  case BN_STRING_ATTR:
	    params[kind][index][name].data.strVal = strdup(attr.data.strVal);
	    break;
	  case BN_INTEGER_ATTR:
	    params[kind][index][name].data.intVal = attr.data.intVal;
	    break;
	  default:
	    break;
	}
      }
    }
  }
}

// Destructor
bluenoc_parameters::~bluenoc_parameters()
{
  for (bn_object_kind_t kind = minObjectKind;
       kind <= maxObjectKind;
       kind = (bn_object_kind_t)(kind + 1)) {
    for (uint32_t index = 0; index < params[kind].size(); ++index) {

      bn_attribute_map_t &attr_map = params[kind][index];

      for(bn_attribute_map_t::iterator i = attr_map.begin();
	  i != attr_map.end();
	  ++i) {
	bn_attribute_t &attr = i->second;

	if ((attr.tag == BN_STRING_ATTR) && (attr.data.strVal != NULL)) {
	  free(attr.data.strVal);
	}
      }
    }
  }
}

uint32_t bluenoc_parameters::NumberOfObjects(const char *objectKind) const
{
  bn_object_kind_t kind = parseObjectKind(objectKind);
  if (kind == BN_BAD_OBJECT) {
    throw std::string("bluenoc_parameters::NumberOfObjects() Invalid object kind.");
  }
  return static_cast<uint32_t>(params[kind].size());
}

int32_t bluenoc_parameters::AttributeIntegerValue(const char *objectKind, uint32_t index, const char *attributeName) const 
{
  bn_object_kind_t kind = parseObjectKind(objectKind);
  if (kind == BN_BAD_OBJECT) {
    throw std::string("bluenoc_parameters::AttributeIntegerValue() Invalid object kind.");
  }

  if (index >= params[kind].size()) {
    throw std::string("bluenoc_parameters::AttributeIntegerValue() Object index is out of range.");
  }

  const bn_attribute_map_t &attr_map = params[kind][index];
  bn_attribute_map_t::const_iterator i = attr_map.find(attributeName);
  if (i == attr_map.end()) {
    throw std::string("bluenoc_parameters::AttributeIntegerValue() Invalid attribute name.");
  }

  const bn_attribute_t &attr = i->second;
  if (attr.tag != BN_INTEGER_ATTR) {
    throw std::string("bluenoc_parameters::AttributeIntegerValue() Attribute is not of integer type.");
  }

  return attr.data.intVal;
}

const char *bluenoc_parameters::AttributeStringValue(const char *objectKind, uint32_t index, const char *attributeName) const 
{
  bn_object_kind_t kind = parseObjectKind(objectKind);
  if (kind == BN_BAD_OBJECT) {
    throw std::string("bluenoc_parameters::AttributeStringValue() Invalid object kind.");
  }

  if (index >= params[kind].size()) {
    throw std::string("bluenoc_parameters::AttributeStringValue() Object index is out of range.");
  }

  const bn_attribute_map_t &attr_map = params[kind][index];
  bn_attribute_map_t::const_iterator i = attr_map.find(attributeName);
  if (i == attr_map.end()) {
    throw std::string("bluenoc_parameters::AttributeStringValue() Invalid attribute name.");
  }

  const bn_attribute_t &attr = i->second;
  if (attr.tag != BN_STRING_ATTR) {
    throw std::string("bluenoc_parameters::AttributeStringValue() Attribute is not of string type.");
  }

  return attr.data.strVal;
}

void bluenoc_parameters::OverrideAttributeIntegerValue(const char *objectKind,    // Input: Object kind name.
						       uint32_t    index,         // Input: Index of object instance.
						       const char *attributeName, // Input: Name of attribute being read
						       int32_t     value)         // Input: New integer value of attribute.
{
  bn_object_kind_t kind = parseObjectKind(objectKind);
  if (kind == BN_BAD_OBJECT) {
    throw std::string("bluenoc_parameters::OverrideAttributeIntegerValue() Invalid object kind.");
  }

  if (index >= params[kind].size()) {
    throw std::string("bluenoc_parameters::OverrideAttributeIntegerValue() Object index is out of range.");
  }

  bn_attribute_map_t &attr_map = params[kind][index];
  bn_attribute_map_t::iterator i = attr_map.find(attributeName);
  if (i == attr_map.end()) {
    throw std::string("bluenoc_parameters::OverrideAttributeIntegerValue() Invalid attribute name.");
  }

  bn_attribute_t &attr = i->second;
  if (attr.tag != BN_INTEGER_ATTR) {
    throw std::string("bluenoc_parameters::OverrideAttributeIntegerValue() Attribute is not of integer type.");
  }

  attr.data.intVal = value;
}

void bluenoc_parameters::OverrideAttributeStringValue(const char *objectKind,     // Input: Object kind name.
						      uint32_t    index,          // Input: Index of object instance.
						      const char *attributeName,  // Input: Name of attribute being read
						      const char *value)          // Input: New string value of attribute
{
  bn_object_kind_t kind = parseObjectKind(objectKind);
  if (kind == BN_BAD_OBJECT) {
    throw std::string("bluenoc_parameters::OverrideAttributeStringValue() Invalid object kind.");
  }

  if (index >= params[kind].size()) {
    throw std::string("bluenoc_parameters::OverrideAttributeStringValue() Object index is out of range.");
  }

  bn_attribute_map_t &attr_map = params[kind][index];
  bn_attribute_map_t::iterator i = attr_map.find(attributeName);
  if (i == attr_map.end()) {
    throw std::string("bluenoc_parameters::OverrideAttributeStringValue() Invalid attribute name.");
  }

  bn_attribute_t &attr = i->second;
  if (attr.tag != BN_STRING_ATTR) {
    throw std::string("bluenoc_parameters::OverrideAttributeStringValue() Attribute is not of string type.");
  }

  if (attr.data.strVal != NULL)
    free(attr.data.strVal);
  attr.data.strVal = strdup(value);
}

bn_attr_type_t bluenoc_parameters::GetAttributeType(const char *objectKind,     // Input: Object kind name.
						    uint32_t    index,          // Input: Index of object instance.
						    const char *attributeName)  // Input: Name of attribute to inspect.
{
  bn_object_kind_t kind = parseObjectKind(objectKind);
  if (kind == BN_BAD_OBJECT)
    return BN_NO_ATTR;

  if (index >= params[kind].size())
    return BN_NO_ATTR;

  const bn_attribute_map_t &attr_map = params[kind][index];
  bn_attribute_map_t::const_iterator i = attr_map.find(attributeName);
  if (i == attr_map.end())
    return BN_NO_ATTR;

  const bn_attribute_t &attr = i->second;
  return attr.tag;
}

// Additional routine to allow run-time creation of parameter objects.
// The objectKind must be valid.  Integer and String attributes will
// be created with the value 0 and NULL, respectively.  The return value
// is the index of the newly created object.
uint32_t bluenoc_parameters::AddObject(const char* objectKind,                  // Input: Object kind name.
				       std::list<const char*> intAttrNames,     // Input: List of Integer attributes
				       std::list<const char*> stringAttrNames)  // Input: List of String attributes
{
  bn_object_kind_t kind = parseObjectKind(objectKind);
  if (kind == BN_BAD_OBJECT) {
    throw std::string("bluenoc_parameters::AddObject() Invalid object kind.");
  }

  uint32_t index = static_cast<uint32_t>(params[kind].size());
  bn_attribute_map_t &attr_map = params[kind][index];

  for(std::list<const char*>::const_iterator p = intAttrNames.begin();
      p != intAttrNames.end();
      ++p) {
    const char *attributeName = *p;
    bn_attribute_t &attr = attr_map[attributeName];
    attr.tag = BN_INTEGER_ATTR;
    attr.data.intVal = 0;
  }

  for(std::list<const char*>::const_iterator p = stringAttrNames.begin();
      p != stringAttrNames.end();
      ++p) {
    const char *attributeName = *p;
    bn_attribute_t &attr = attr_map[attributeName];
    attr.tag = BN_STRING_ATTR;
    attr.data.strVal = NULL;
  }

  return index;
}

//
// Dump the content of all the parameters
//
void bluenoc_parameters::dump() const
{
  std::cout << "Parameters" << std::endl;
  std::cout << "==========" << std::endl;
  std::cout << std::endl;
  std::cout << "ObjectKind      " << "Index        " << "AttributeName    "
            << "Value" << std::endl;
  std::cout << "----------      " << "-----        " << "-------------    "
            << "------------- " << std::endl;
  std::cout << std::endl;

  for (bn_object_kind_t kind = minObjectKind;
       kind <= maxObjectKind;
       kind = (bn_object_kind_t) (kind + 1))
  {
    const char* objKind = objectKindToString(kind);

    for (uint32_t index = 0; index < params[kind].size(); ++index)
    {
      const bn_attribute_map_t& attr_map = params[kind][index];

      for (bn_attribute_map_t::const_iterator i = attr_map.begin();
           i != attr_map.end();
           ++i)
      {
        const std::string& name = i->first;
        const bn_attribute_t& attr = i->second;

        printf("%-15s ", objKind);
        printf("%-12d ", index);
        printf("%-16s ", name.c_str());
        switch (attr.tag)
        {
          case BN_STRING_ATTR:
            printf("%-14s", attr.data.strVal);
            break;
          case BN_INTEGER_ATTR:
            printf("%-14d ", attr.data.intVal);
            break;
          default:
            printf("%-14s", "?");
            break;
        }
        printf("\n");
      }
    }
  }
}
