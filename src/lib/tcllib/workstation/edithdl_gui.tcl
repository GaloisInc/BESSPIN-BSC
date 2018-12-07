#!/bin/sh
##
## Copyright 2009--2010 Bluespec, Inc.  All rights reserved.
##
## \
exec $BLUESPECDIR/bin/bluewish "$0" "$@"

# top-level user script to invoke hdl editing commands via graphical environment
lappend auto_path $env(BLUESPECDIR)/tcllib/workstation

package require Iwidgets 4.0
package require Itcl
package require Bluetcl
package require RtlEdit


# standard Bluespec colours and fonts
fonts::set_colours
fonts::initialize

text .splash  -relief flat -height 5 -width 40
.splash image create 0.0 -image [fonts::getBSCIncImage]
.splash insert end "\n\nStarting Bluespec HDL Editor ..."
pack .splash -expand 1 -fill both
wm title . "Bluespec HDL Editor & Probe Insertion Tool"
update


eval RtlEdit::guiEntry $argv
set BSPEC(MAIN_WINDOW) .editor
wm withdraw .
rtl_editor::create .editor



