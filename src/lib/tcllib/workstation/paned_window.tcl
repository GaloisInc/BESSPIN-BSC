
##
# @file paned_window.tcl
#
# @brief Definition of the Panedwindow.
#
# @author Instigate CJSC.
#
# Copyright 2007--2009 Bluespec Inc. All rights reserved
#

package require Iwidgets
namespace eval base {

itk::usual panedwindow {
    keep -background -cursor -sashcursor
}

##
# @brief Definition of class paned_window
# 
itcl::class panedwindow {
    inherit ::iwidgets::IPanedwindow
    constructor {args} {
            lappend args -sashindent 30 
            eval itk_initialize $args        
        }
}
}
