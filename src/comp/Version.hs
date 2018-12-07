module Version(bluespec, version, release, versionnum, versiondate, copyright,
               versionMajor, versionMinor, build, buildnum
              ) where

import BuildVersion(buildVersion, buildVersionNum)

{-# NOINLINE bluespec #-}
{-# NOINLINE version #-}
{-# NOINLINE release #-}
{-# NOINLINE versionnum #-}
{-# NOINLINE versiondate #-}
{-# NOINLINE copyright #-}
{-# NOINLINE versionMajor #-}
{-# NOINLINE versionMinor #-}

bluespec = "Bluespec"
release = "Tarcoola"

-- Major and minor version numbers are used by FlexLM
versionMajor :: String
versionMajor = "3"
versionMinor :: String
versionMinor = "8"

-- Version number format is YEAR.MONTH or YEAR.MONTH.ANNOTATION
-- Where the annotation is a description for an unusual release,
-- such as a customer name, a letter or a day.
-- (eg. 2007.10.beta2 or 2007.02.b or 2007.04.17)
versionnum = "2017.07.A"
versiondate = "2017-07-21"
-- Please consider changing the date in doc/common_dates.tex as well


build = if null buildVersion then "" else "build " ++ buildVersion ++ ", "
buildnum = buildVersionNum
version = bluespec ++ " Compiler, version " ++ versionnum ++
          " (" ++ build ++ versiondate ++ ")"

copyright :: String
copyright = unlines copyrights

copyrights :: [String]
copyrights = ["Copyright 2000-2014 Bluespec, Inc.",
              "Parts copyright 2002, The University Court of the University of Glasgow.",
              "Parts copyright 1982-1999 Lennart Augustsson, Thomas Johnsson,",
              "    Chalmers University of Technology.",
              "Parts copyright 1999-2000, Daan Leijen.",
              "Parts copyright 1991, 1999 Free Software Foundation, Inc.",
              "Parts copyright 1995-2014, Regents of the University of Colorado.",
              "Parts copyright 2010, Don Stewart.",
              "All rights reserved.",
              "See documentation for license details."]
