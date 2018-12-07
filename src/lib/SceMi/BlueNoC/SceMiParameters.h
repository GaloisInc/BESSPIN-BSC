/* Copyright 2004--2009 Bluespec, Inc.  All rights reserved. */

#ifndef __SCEMI_PARAMETERS_H__
#define __SCEMI_PARAMETERS_H__

#include <map>
#include <vector>
#include <list>
#include <string>

#include "SceMiEC.h"

struct tSceMiAttribute;

typedef std::map<std::string,tSceMiAttribute> tAttributeMap;

// Sce-Mi objects come in a pre-defined number of kinds

typedef enum { SCEMI_BAD_OBJECT = -1
             , SCEMI_IN_PORT=0
             , SCEMI_OUT_PORT
             , SCEMI_IN_PIPE
             , SCEMI_OUT_PIPE
             , SCEMI_CLOCK
             , SCEMI_CLOCK_BINDING
             , SCEMI_SYSTEM
             , SCEMI_LINK
             , SCEMI_SERIAL
             } tSceMiObjectKind;

const tSceMiObjectKind minObjectKind = SCEMI_IN_PORT;
const tSceMiObjectKind maxObjectKind = SCEMI_SERIAL;

// This enumerated type describes the types of attributes supported
typedef enum { SCEMI_NO_ATTR = -1
             , SCEMI_INTEGER_ATTR = 0
             , SCEMI_STRING_ATTR
             } tSceMiAttrType;

// The SceMiParameters class handles parsing of the
class SceMiParameters {

 private:
    // Whether the parameters file successfully loaded
    bool m_loaded;

    // A vector of attribute associations for each kind of object
    std::vector<tAttributeMap> params[maxObjectKind + 1];

 public:
    // CREATORS
    //
    // This constructor initializes some parameters from the
    // parameters file in the config directory, and some other
    // parameters directly from the config file.
    //
    SceMiParameters(const char* paramsfile, SceMiEC* ec = 0);

    // This constructor is not part of the SCE-MI API, but is used
    // in the implementation to make an internal copy of the
    // parameters structure.
    SceMiParameters(const SceMiParameters& parameters);

    ~SceMiParameters();

    // ACCESSORS
    //
    // This accessor returns the number of instances of objects of
    // the specified objectKind name.
    //
    unsigned int NumberOfObjects(const char* objectKind, // Input: Object kind name.
                                 SceMiEC* ec = 0) const; // Input/Output: Error status.

    //
    // These accessors return an integer or string attribute values of the
    // given object kind. It is considered an error if the index > number
    // returned by ::NumberOfObjects() or the objectKind and attributeName
    // arguments are unrecognized.
    //

    int AttributeIntegerValue(const char* objectKind,    // Input: Object kind name.
                              unsigned int index,        // Input: Index of object instance.
                              const char* attributeName, // Input: Name of attribute being read.
                              SceMiEC* ec = 0) const;    // Input/Output: Error status.

    const char* AttributeStringValue(const char* objectKind,    // Input: Object kind name.
                                     unsigned int index,        // Input: Index of object instance.
                                     const char* attributeName, // Input: Name of attribute being read.
                                     SceMiEC* ec = 0) const;    // Input/Output: Error status.

    // MANIPULATORS
    //
    // These manipulators override an integer or string attribute values of the
    // given object kind. It is considered an error if the index > number
    // returned by ::NumberOfObjects(). or the objectKind and attribute¬Name
    // arguments are unrecognized.
    //
    void OverrideAttributeIntegerValue(const char* objectKind,    // Input: Object kind name.
                                       unsigned int index,        // Input: Index of object instance.
                                       const char* attributeName, // Input: Name of attribute being read.
                                       int value,                 // Input: New integer value of attribute.
                                       SceMiEC* ec = 0);          // Input/Output: Error status.

    void OverrideAttributeStringValue(const char* objectKind,    // Input: Object kind name.
                                      unsigned int index,        // Input: Index of object instance.
                                      const char* attributeName, // Input: Name of attribute being read.
                                      const char* value,         // Input: New string value of attribute.
                                      SceMiEC* ec = 0);          // Input/Output: Error status.

    // Additional routine to test for the existence and type of an attribute
    tSceMiAttrType GetAttributeType(const char* objectKind,     // Input: Object kind name.
                                    unsigned int index,         // Input: Index of object instance.
                                    const char* attributeName); // Input: Name of attribute to inspect.

    // Additional routine to allow run-time creation of parameter objects.
    // The objectKind must be valid.  Integer and String attributes will
    // be created with the value 0 and NULL, respectively.  The return value
    // is the index of the newly created object.
    unsigned int AddObject(const char* objectKind,                 // Input: Object kind name.
                           std::list<const char*> intAttrNames,    // Input: List of Integer attributes
                           std::list<const char*> stringAttrNames, // Input: List of String attributes
                           SceMiEC* ec = 0);                       // Input/Output: Error status.

    //
    // Whether the parameters file has successfully loaded
    //
    bool loaded() const { return m_loaded; }

    //
    // Dump the content of all the parameters
    //
    void dump() const;
};


#endif /* __SCEMI_PARAMETERS_H__ */
