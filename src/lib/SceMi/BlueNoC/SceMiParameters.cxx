// ****************************************************************
// This is an implementation of the SCE-MI API, as defined in the
// Accellera document:
//     Standard Co-Emulation Modeling Interface (SCE-MI)
//     Reference Manual
//     Version 2.0 Release
//     March 22nd, 2007
// and embodied in the file 'scemi.h' in Appendix E.1 of that document

// Copyright (c) 2008 Bluespec, Inc; all rights reserved
// ****************************************************************

#include <iostream>
#include <fstream>
#include <cstring>
#include <cstdlib>

#include "SceMiParameters.h"

#define MAX_LINE_SIZE 1023

// Sce-Mi parameters can have either integer-valued or string-valued attributes

struct tSceMiAttribute
{
  tSceMiAttrType tag;
  union {
    char* strVal;
    int   intVal;
  } data;
};

static tSceMiObjectKind parseObjectKind(const char* objKind)
{
  if (objKind == NULL) return SCEMI_BAD_OBJECT;

  if      (!strcmp(objKind, "MessageInPort"))  return SCEMI_IN_PORT;
  else if (!strcmp(objKind, "MessageOutPort")) return SCEMI_OUT_PORT;
  else if (!strcmp(objKind, "InputPipe"))      return SCEMI_IN_PIPE;
  else if (!strcmp(objKind, "OutputPipe"))     return SCEMI_OUT_PIPE;
  else if (!strcmp(objKind, "Clock"))          return SCEMI_CLOCK;
  else if (!strcmp(objKind, "ClockBinding"))   return SCEMI_CLOCK_BINDING;
  else if (!strcmp(objKind, "System"))         return SCEMI_SYSTEM;
  else if (!strcmp(objKind, "Link"))           return SCEMI_LINK;
  else if (!strcmp(objKind, "Serial"))         return SCEMI_SERIAL;
  else return SCEMI_BAD_OBJECT;
}

static const char* objectKindToString(tSceMiObjectKind kind)
{
  switch (kind)
  {
    case SCEMI_IN_PORT:       return "MessageInPort";
    case SCEMI_OUT_PORT:      return "MessageOutPort";
    case SCEMI_IN_PIPE:       return "InputPipe";
    case SCEMI_OUT_PIPE:      return "OutputPipe";
    case SCEMI_CLOCK:         return "Clock";
    case SCEMI_CLOCK_BINDING: return "ClockBinding";
    case SCEMI_SYSTEM:        return "System";
    case SCEMI_LINK:          return "Link";
    case SCEMI_SERIAL:        return "Serial";
    default:                  return "?";
  }
}

// ****************************************************************
// class SceMiParameters members

SceMiParameters::SceMiParameters(const char* paramsfile, SceMiEC* ec)
{
  std::ifstream file;
  const char *delimiter = " \n\t,";
  char lineBuf[MAX_LINE_SIZE+1];
  char *objectKind, *indexString, *attributeName, *valueString, *s;
  int ivalue, index;
  char *svalue;
  unsigned int lineno = 0;

  m_loaded = false;

  file.open(paramsfile);

  if (file.fail()) {
    std::cerr << "ERROR: unable to open parameters file " << paramsfile
              << "." << std::endl;
    raiseError ("SceMiParameters::SceMiParameters()",
                "Unable to open parameters file.", SceMiError, ec);
    return;
  }

  m_loaded = true;

  // Read in each line.  Right now assuming each line consists of 4 tokens:
  // ObjectKind, Index, AttributeName, Value
  while (!file.eof()){

    file.getline(lineBuf, MAX_LINE_SIZE);
    lineno++;
    objectKind = strtok(lineBuf, delimiter);
    if (objectKind && (strncmp(objectKind, "//", 2) != 0)) {
      indexString = strtok(0, delimiter);
      attributeName = strtok(0, delimiter);
      valueString = strtok(0, "\n\t"); // allow commas in string attribute values, allow space too.

      tSceMiObjectKind kind = parseObjectKind(objectKind);
      if (kind == SCEMI_BAD_OBJECT)
      {
        raiseError ("SceMiParameters::SceMiParameters()",
                    "Invalid object kind.", SceMiError, ec);
        file.close();
        return;
      }

      index = atoi(indexString);
      if (index >= (int) params[kind].size())
        params[kind].resize(index+1);


      while (valueString[0] == ' ')
        valueString++;
      if (valueString[0] == '"') {
        svalue = valueString+1;
        s = strchr(svalue, '"');
        if (s == NULL) {
          int len = snprintf(NULL, 0,
                             "Invalid token in file %s on line %d at %s, closing \" is needed",
                             paramsfile, lineno, valueString);

          char *str = new char[len+1];
          snprintf(str, len+1, "Invalid token in file %s on line %d at %s, closing \" is needed",
                   paramsfile, lineno, valueString);

          raiseError ("SceMiParameters::SceMiParameters()",
                      str, SceMiError, ec);
          file.close();
          return;
        }
        *s = '\0';
        params[kind][index][attributeName].tag = SCEMI_STRING_ATTR;
        params[kind][index][attributeName].data.strVal = strdup(svalue);
      }
      else if (isdigit(valueString[0])) {
        ivalue = atoi(valueString);
        params[kind][index][attributeName].tag = SCEMI_INTEGER_ATTR;
        params[kind][index][attributeName].data.intVal = ivalue;
      }
      else {
        int len = snprintf(NULL, 0,
                           "Invalid SceMi Parameter specification in file %s on line %d",
                           paramsfile, lineno);

        char *str = new char[len+1];
        snprintf(str, len+1, "Invalid SceMi Parameter specification in file %s on line %d",
                 paramsfile, lineno);

        raiseError ("SceMiParameters::SceMiParameters()",
                    str, SceMiError, ec);
        file.close();
        return;
      }
    }
  }
  file.close();
}

// This constructor is not part of the SCE-MI API, but is used
// in the implementation to make an internal copy of the
// parameters structure.
SceMiParameters::SceMiParameters(const SceMiParameters& parameters)
{
  m_loaded = parameters.m_loaded;

  for (tSceMiObjectKind kind = minObjectKind;
       kind <= maxObjectKind;
       kind = (tSceMiObjectKind) (kind + 1))
  {
    params[kind].resize(parameters.params[kind].size());

    for (unsigned int index = 0; index < parameters.params[kind].size(); ++index)
    {
      const tAttributeMap& attr_map = parameters.params[kind][index];

      for (tAttributeMap::const_iterator i = attr_map.begin();
           i != attr_map.end();
           ++i)
      {
        const std::string& name = i->first;
        const tSceMiAttribute& attr = i->second;

        params[kind][index][name].tag = attr.tag;
        switch (attr.tag)
        {
          case SCEMI_STRING_ATTR:
            params[kind][index][name].data.strVal = strdup(attr.data.strVal);
            break;
          case SCEMI_INTEGER_ATTR:
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
SceMiParameters::~SceMiParameters()
{
  // free all strings
  for (tSceMiObjectKind kind = minObjectKind;
       kind <= maxObjectKind;
       kind = (tSceMiObjectKind) (kind + 1))
  {
    for (unsigned int index = 0; index < params[kind].size(); ++index)
    {
      tAttributeMap& attr_map = params[kind][index];

      for (tAttributeMap::iterator i = attr_map.begin();
           i != attr_map.end();
           ++i)
      {
        tSceMiAttribute& attr = i->second;

        if ((attr.tag == SCEMI_STRING_ATTR) && (attr.data.strVal != NULL))
          free(attr.data.strVal);
      }
    }
  }
}

unsigned int SceMiParameters::NumberOfObjects(const char* objectKind,  // Input: Object kind name.
                                              SceMiEC* ec) const       // Input/Output: Error status.
{
  tSceMiObjectKind kind = parseObjectKind(objectKind);
  if (kind == SCEMI_BAD_OBJECT)
  {
    raiseError ("SceMiParameters::NumberOfObjects()",
                "Invalid object kind.", SceMiError, ec);
    return 0;
  }
  return static_cast<unsigned int>(params[kind].size());
}

int SceMiParameters::AttributeIntegerValue(const char* objectKind,     // Input: Object kind name.
                                           unsigned int index,         // Input: Index of object instance.
                                           const char* attributeName,  // Input: Name of attribute being read.
                                           SceMiEC* ec) const          // Input/Output: Error status.
{
  tSceMiObjectKind kind = parseObjectKind(objectKind);
  if (kind == SCEMI_BAD_OBJECT)
  {
    raiseError ("SceMiParameters::AttributeIntegerValue()",
                "Invalid object kind.", SceMiError, ec);
    return 0;
  }

  if (index >= params[kind].size())
  {
    raiseError ("SceMiParameters::AttributeIntegerValue()",
                "Object index is out of range.", SceMiError, ec);
    return 0;
  }

  const tAttributeMap& attr_map = params[kind][index];
  tAttributeMap::const_iterator i = attr_map.find(attributeName);
  if (i == attr_map.end())
  {
    raiseError ("SceMiParameters::AttributeIntegerValue()",
                "Invalid attribute name.", SceMiError, ec);
    return 0;
  }

  const tSceMiAttribute& attr = i->second;
  if (attr.tag != SCEMI_INTEGER_ATTR)
  {
    raiseError ("SceMiParameters::AttributeIntegerValue()",
                "Attribute is not of integer type.", SceMiError, ec);
    return 0;
  }

  return attr.data.intVal;
}

const char* SceMiParameters::AttributeStringValue(const char* objectKind,     // Input: Object kind name.
                                                  unsigned int index,         // Input: Index of object instance.
                                                  const char* attributeName,  // Input: Name of attribute being read.
                                                  SceMiEC* ec) const          // Input/Output: Error status.
{
  tSceMiObjectKind kind = parseObjectKind(objectKind);
  if (kind == SCEMI_BAD_OBJECT)
  {
    raiseError ("SceMiParameters::AttributeStringValue()",
                "Invalid object kind.", SceMiError, ec);
    return 0;
  }

  if (index >= params[kind].size())
  {
    raiseError ("SceMiParameters::AttributeStringValue()",
                "Object index is out of range.", SceMiError, ec);
    return 0;
  }

  const tAttributeMap& attr_map = params[kind][index];
  tAttributeMap::const_iterator i = attr_map.find(attributeName);
  if (i == attr_map.end())
  {
    raiseError ("SceMiParameters::AttributeStringValue()",
                "Invalid attribute name.", SceMiError, ec);
    return 0;
  }

  const tSceMiAttribute& attr = i->second;
  if (attr.tag != SCEMI_STRING_ATTR)
  {
    raiseError ("SceMiParameters::AttributeStringValue()",
                "Attribute is not of string type.", SceMiError, ec);
    return 0;
  }

  return attr.data.strVal;
}

void SceMiParameters::OverrideAttributeIntegerValue(const char* objectKind,    // Input: Object kind name.
                                                    unsigned int index,        // Input: Index of object instance.
                                                    const char* attributeName, // Input: Name of attribute being read.
                                                    int value,                 // Input: New integer value of attribute.
                                                    SceMiEC* ec)               // Input/Output: Error status.
{
  tSceMiObjectKind kind = parseObjectKind(objectKind);
  if (kind == SCEMI_BAD_OBJECT)
  {
    raiseError ("SceMiParameters::OverrideAttributeIntegerValue()",
                "Invalid object kind.", SceMiError, ec);
    return;
  }

  if (index >= params[kind].size())
  {
    raiseError ("SceMiParameters::OverrideAttributeIntegerValue()",
                "Object index is out of range.", SceMiError, ec);
    return;
  }

  tAttributeMap& attr_map = params[kind][index];
  tAttributeMap::iterator i = attr_map.find(attributeName);
  if (i == attr_map.end())
  {
    raiseError ("SceMiParameters::OverrideAttributeIntegerValue()",
                "Invalid attribute name.", SceMiError, ec);
    return;
  }

  tSceMiAttribute& attr = i->second;
  if (attr.tag != SCEMI_INTEGER_ATTR)
  {
    raiseError ("SceMiParameters::OverrideAttributeIntegerValue()",
                "Attribute is not of integer type.", SceMiError, ec);
    return;
  }

  attr.data.intVal = value;
}

void SceMiParameters::OverrideAttributeStringValue(const char* objectKind,     // Input: Object kind name.
                                                   unsigned int index,         // Input: Index of object instance.
                                                   const char* attributeName,  // Input: Name of attribute being read.
                                                   const char* value,          // Input: New string value of attribute.
                                                   SceMiEC* ec)                // Input/Output: Error status.
{
  tSceMiObjectKind kind = parseObjectKind(objectKind);
  if (kind == SCEMI_BAD_OBJECT)
  {
    raiseError ("SceMiParameters::OverrideAttributeStringValue()",
                "Invalid object kind.", SceMiError, ec);
    return;
  }

  if (index >= params[kind].size())
  {
    raiseError ("SceMiParameters::OverrideAttributeStringValue()",
                "Object index is out of range.", SceMiError, ec);
    return;
  }

  tAttributeMap& attr_map = params[kind][index];
  tAttributeMap::iterator i = attr_map.find(attributeName);
  if (i == attr_map.end())
  {
    raiseError ("SceMiParameters::OverrideAttributeStringValue()",
                "Invalid attribute name.", SceMiError, ec);
    return;
  }

  tSceMiAttribute& attr = i->second;
  if (attr.tag != SCEMI_STRING_ATTR)
  {
    raiseError ("SceMiParameters::OverrideAttributeStringValue()",
                "Attribute is not of string type.", SceMiError, ec);
    return;
  }

  if (attr.data.strVal != NULL)
    free(attr.data.strVal);
  attr.data.strVal = strdup(value);
}

tSceMiAttrType SceMiParameters::GetAttributeType(const char* objectKind,    // Input: Object kind name.
                                                 unsigned int index,        // Input: Index of object instance.
                                                 const char* attributeName) // Input: Name of attribute to inspect.
{
  tSceMiObjectKind kind = parseObjectKind(objectKind);
  if (kind == SCEMI_BAD_OBJECT)
    return SCEMI_NO_ATTR;

  if (index >= params[kind].size())
    return SCEMI_NO_ATTR;

  const tAttributeMap& attr_map = params[kind][index];
  tAttributeMap::const_iterator i = attr_map.find(attributeName);
  if (i == attr_map.end())
    return SCEMI_NO_ATTR;

  const tSceMiAttribute& attr = i->second;
  return attr.tag;
}

unsigned int SceMiParameters::AddObject(const char* objectKind,                 // Input: Object kind name.
                                        std::list<const char*> intAttrNames,    // Input: List of Integer attributes
                                        std::list<const char*> stringAttrNames, // Input: List of String attributes
                                        SceMiEC* ec)                            // Input/Output: Error status.
{
  tSceMiObjectKind kind = parseObjectKind(objectKind);
  if (kind == SCEMI_BAD_OBJECT)
  {
    raiseError ("SceMiParameters::AddObject()",
                "Invalid object kind.", SceMiError, ec);
    return 0;
  }

  unsigned int index = static_cast<unsigned int>(params[kind].size());

  tAttributeMap& attr_map = params[kind][index];

  for (std::list<const char*>::const_iterator p = intAttrNames.begin();
       p != intAttrNames.end();
       ++p)
  {
    const char* attributeName = *p;
    tSceMiAttribute& attr = attr_map[attributeName];
    attr.tag = SCEMI_INTEGER_ATTR;
    attr.data.intVal = 0;
  }

  for (std::list<const char*>::const_iterator p = stringAttrNames.begin();
       p != stringAttrNames.end();
       ++p)
  {
    const char* attributeName = *p;
    tSceMiAttribute& attr = attr_map[attributeName];
    attr.tag = SCEMI_STRING_ATTR;
    attr.data.strVal = NULL;
  }

  return index;
}


void SceMiParameters::dump() const
{
  std::cout << "Parameters" << std::endl;
  std::cout << "==========" << std::endl;
  std::cout << std::endl;
  std::cout << "ObjectKind      " << "Index        " << "AttributeName    "
            << "Value" << std::endl;
  std::cout << "----------      " << "-----        " << "-------------    "
            << "------------- " << std::endl;
  std::cout << std::endl;

  for (tSceMiObjectKind kind = minObjectKind;
       kind <= maxObjectKind;
       kind = (tSceMiObjectKind) (kind + 1))
  {
    const char* objKind = objectKindToString(kind);

    for (unsigned int index = 0; index < params[kind].size(); ++index)
    {
      const tAttributeMap& attr_map = params[kind][index];

      for (tAttributeMap::const_iterator i = attr_map.begin();
           i != attr_map.end();
           ++i)
      {
        const std::string& name = i->first;
        const tSceMiAttribute& attr = i->second;

        printf("%-15s ", objKind);
        printf("%-12d ", index);
        printf("%-16s ", name.c_str());
        switch (attr.tag)
        {
          case SCEMI_STRING_ATTR:
            printf("%-14s", attr.data.strVal);
            break;
          case SCEMI_INTEGER_ATTR:
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
