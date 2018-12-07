#!/bin/sh
# Copyright 2007--2009 Bluespec, Inc.  All rights reserved.
# $Id: expandPorts.tcl 14076 2008-05-20 20:39:33Z czeck $
# \
exec $BLUESPECDIR/bin/bluetcl "$0" "$@"

package require SceMiMsg 

proc usage {} {
    puts "" 
    puts "usage: $::argv0 <options> -package package types \[type2\]*"
    puts "Options:"
    puts "  -p <path>          Bluespec search path"
    puts "  -bdir <dir>        Bluespec bdir path"
    puts "  -outdir <dir>      Directory for generated header files"
    puts "  -memberPrefix str  Prefix for class member names; defaults: \"m_\""
    puts "  -enumPrefix str    Prefix for enum type; default: \"e_\""
    puts "  -tagPrefix str     Prefix for taggged union type; default: \"tag_\""
    puts "" 
    puts " e.g: -p bdir:+ -package Foo Bar Foo"
}

set boolOptions [list -- -scemi-classic]
set valOptions [list -p -package -bdir -outdir -memberPrefix -enumPrefix -tagPrefix]

set OPT(-outdir) ""
if { [catch [list ::utils::scanOptions $boolOptions $valOptions  true OPT "$argv"] opts] } {
    puts stderr $opts
    usage
    exit 1
}

if { [info exists OPT(-p)] } {
    Bluetcl::flags set -p $OPT(-p)
}
if { [info exists OPT(-bdir)] } {
    Bluetcl::flags set -bdir $OPT(-bdir)
}
if { [info exists OPT(-scemi-classic)] } {
    Bluetcl::flags set -scemi-classic
}

if { ! [info exist OPT(-package)] } {
    puts stderr "A -package argument is required"
    usage
    exit 1
} else {
    if { [catch "SceMiMsg::generateSceMiHeaders  OPT  [list $opts]" err] } {
        puts stderr $err
        exit 1
    }
}


exit



