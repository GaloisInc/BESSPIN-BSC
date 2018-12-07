#!/bin/sh
# the next line restarts using wish\
    exec wish "$0" -- "$@" 

################################################################################
# This file is Copyright (c) 2004 - 2008 Bluespec, Inc. 
#
# BWidget1.7.0 library redistrubuted as public library for your convinience
#   license terms of the bwidget library available at 
#   $(BLUESPECDIR)/bwidget1.7.0/LICENSE.txt
#   this is an *exact* copy of 1.7.0 available on sourceforge.net as of 
#   today 11/29/2004
#
################################################################################

# get this from ~/bsc/src/comp/Version.hs
set gExpDate [list 6 5]
set gVersion 3.8.51

set gBuild 6551

# will become -output-cross-info soon!
set gInfoSwitch {-cross-info}
set gDebugLevel 1
set gLogDir .blueview

# default search switches

################################################################################
################################################################################
#
# Example 39-1
# Procedures to help build dialogs.
# taken from "Practical Programming in Tcl and Tk" an excellent book that
#   I recommend you get
#
proc DialogUtilsCreate {top title args} {
    global dialog
    if {[winfo exists $top]} {
	switch -- [wm state $top] {
	    normal {
		raise $top  ;# Raise a buried window
	    }
	    withdrawn -
	    iconic {
		# Open and restore geometry
		wm deiconify $top
		catch {wm geometry $top $dialog(geo,$top)}
	    }
	}
	return 0
    } else {
	eval {toplevel $top} $args
	wm title $top $title
	return 1
    }
}
proc DialogUtilsWait {top varName {focus {}}} {
    upvar $varName var
    
    # Poke the variable if the user nukes the window
    bind $top <Destroy> [list set $varName cancel]
    
    # Grab focus for the dialog
    if {[string length $focus] == 0} {
	set focus $top
    }
    set old [focus -displayof $top]
    focus $focus
    catch {tkwait visibility $top}
    catch {grab $top}
    
    # Wait for the dialog to complete
    tkwait variable $varName
    catch {grab release $top}
    focus $old
}
proc DialogUtilsDismiss {top} {
    global dialog
    # Save current size and position
    catch {
	# window may have been deleted
	set dialog(geo,$top) [wm geometry $top]
	wm withdraw $top
    }
}

################################################################################
###
################################################################################

proc initializeColors {} {

    global gColorList

    array unset gColorList

    set gColorList(treearea)   {-background grey90 -highlightcolor blue -selectforeground red}
    set gColorList(textarea)   {-background grey90 -highlightcolor blue -foreground black -insertbackground black}
    set gColorList(statusarea) {-background turquoise -insertbackground black -foreground black}

    ### Joydeep : settings for file name label.
    set gColorList(filenamearea) {-background grey90 -foreground black -relief ridge -bd 4}
    
    set gColorList(tagkeyword)    {-foreground brown}       ;# keyword in bsv
    set gColorList(tagdatatype)   {-foreground sienna}      ;# Reg, etc
    set gColorList(taglibrary)    {-foreground blue}        ;# library type
    set gColorList(tagcomment)    {-foreground darkgreen}   ;# comment colors
    set gColorList(tagmodselect)  {-background dodgerblue}         ;#
    set gColorList(tagmodselectfg) {-foreground dodgerblue}        ;#
    set gColorList(tagselection) {-foreground blue -font  {-family courier -underline 1 -weight bold}}  ;# when selected
    set gColorList(tagselectionset) {-foreground blue}   ;# when selected

#    set gColorList(tagwavable)    {-background lightsalmon}
    set gColorList(tagwavable)    {-background orange}
    set gColorList(tagxref)       {-background yellow}

    return 1

}

proc initializeAcceleratorKeys {} {

    global gKeyList

    set gKeyList(help)                      "<Control-h>"
    set gKeyList(quit)                      "<Control-q>"
    set gKeyList(opensources)               "<Control-o>"      ;# open/update source files
    set gKeyList(textcut)                   "<Control-w>"      ;# cut text
    set gKeyList(textcopy)                  "<Meta-w>"         ;# copy text
    set gKeyList(textpaste)                 "<Control-y>"      ;# paste text
    set gKeyList(search)                    "<Control-s>"      ;# simple text search
    set gKeyList(rotateselections)          "<F3>"             ;# rotate through current selections
    set gKeyList(clearselections)           "<Control-g>"      ;# clear through current selections
    set gKeyList(findrelatedxrefs)          "<F6>"             ;# find related locations/signals in verilog or bsv file
    set gKeyList(sendwaves)                 "<F10>"            ;# send signals to wave view
    set gKeyList(treeJumpToBSVDef)          "B"                ;# send signals to wave view
    set gKeyList(treeJumpToBSVInst)         "b"                ;# send signals to wave view
    set gKeyList(treeJumpToVerilogDef)      "V"                ;# jump to verilog definition
    set gKeyList(treeJumpToVerilogInst)     "v"                ;# jump to verilog instantiation

    ### Joydeep : keys to send can/will fire signals to nWave
    set gKeyList(sendcanfiresignals2nWave)  "<F11>"      ;# send can fire signals to nWave
    set gKeyList(sendwillfiresignals2nWave) "<F12>"      ;# send will fire signals to nWave

    return 1

}


################################################################################
################################################################################
if {![info exists gVTcl(sourcing)]} {
    # package require tcldebugger_attach
    # use tcldebugger_eval {lots of code}
    
    package require Tk 8.3
    
#    global gAndroidPresent
#    set    gAndroidPresent [catch {package require android}]
    
    switch $tcl_platform(platform) {
	windows {
	    option add *Button.padY 0
	}
	default {
	    option add *Scrollbar.width 10
	    option add *Scrollbar.highlightThickness 0
	    option add *Scrollbar.elementBorderWidth 2
	    option add *Scrollbar.borderWidth 2
	}
    }
    
    global env gBLUESPECDIR
    set gBLUESPECDIR $env(BLUESPECDIR)
    lappend auto_path gBLUESPECDIR

    # this comes from ActiveState or 'apt-get install bwidget'
    if {[catch {package require BWidget}]==1} {
	puts "ERROR: you have the tk package 'bwidget1.7.0' or better installed"
	puts "       'apt-get install bwidget' will work on debian"
	exit
    }
    
    # defaults

    initializeColors
    initializeAcceleratorKeys
    
    proc userSignalMatchFunction {sig} { return 1 }
    set gnWaveTimeout 30
    set gTabStops 8
    set gIgnoreLibraryModules 1
    
    # look for preference file
    if {[info exists env(BLUEVIEW_PREF_FILE)]} {
	set prefs $env(BLUEVIEW_PREF_FILE)

    } else {
	# find out where we were called from, and get preference file from there.
	set bv [info script]
	set dr [file dirname $bv]
	set prefs $dr/blueview-preferences.tcl
    }
    
    if {[file exists $prefs] == 1} {
	puts "Loading local preference file $prefs"
	source $prefs
    }
    
}

################################################################################
###
################################################################################

proc debug {pri mess} {
    global gDebug gdbIndent gDebugLevel
    if {$pri > $gDebugLevel} return
    if {[info exists gDebug]} {
	puts $gDebug "$gdbIndent$mess"
	flush $gDebug
    }
}

# this is extremely low tech, but good enough for now
proc checkLicense {} {
    global gExpDate
    set date [exec date +%D]
    set f [split $date /]
    # puts "DBG: $gExpDate vs $date"
    
    if {([lindex $f 2] <= [lindex $gExpDate 1]) &&
	([lindex $f 0] <= [lindex $gExpDate 0])} {
	return 
    } else {
	puts " ERROR: bsc/blueview licenses has expired";
	puts "      : contact support@bluespec.com for an updated license";
	exit
    }
}


if {![info exists gVTcl(sourcing)]} {
    #############################################################################
    ## Procedure:  gVTcl:rename
    
    proc gVTcl:rename {name} {
	## This procedure may be used free of restrictions.
	##    Exception added by Christian Gavin on 08/08/02.
	## Other packages and gWidget toolkits have different licensing requirements.
	##    Please read their license agreements for details.
	
	regsub -all "\\." $name "_" ret
	regsub -all "\\-" $ret "_" ret
	regsub -all " " $ret "_" ret
	regsub -all "/" $ret "__" ret
	regsub -all "::" $ret "__" ret
	
	return [string tolower $ret]
    }
}




#############################################################################
## gVTcl Code to Load User Images


#################################
# VTCL LIBRARY PROCEDURES
#

#############################################################################
## Library Procedure:  Window

if {![info exists gVTcl(sourcing)]} {
    
    proc Window {args} {
	## This procedure may be used free of restrictions.
	##    Exception added by Christian Gavin on 08/08/02.
	## Other packages and gWidget toolkits have different licensing requirements.
	##    Please read their license agreements for details.
	
	global gVTcl
	foreach {cmd name newname} [lrange $args 0 2] {}
	set rest    [lrange $args 3 end]
	if {$name == "" || $cmd == ""} { return }
	if {$newname == ""} { set newname $name }
	if {$name == "."} { wm withdraw $name; return }
	set exists [winfo exists $newname]
	switch $cmd {
	    show {
		if {$exists} {
		    wm deiconify $newname
		} elseif {[info procs vTclWindow$name] != ""} {
		    eval "vTclWindow$name $newname $rest"
		}
		if {[winfo exists $newname] && [wm state $newname] == "normal"} {
		    gVTcl:FireEvent $newname <<Show>>
		}
	    }
	    hide    {
		if {$exists} {
		    wm withdraw $newname
		    gVTcl:FireEvent $newname <<Hide>>
		    return}
	    }
	    iconify { if {$exists} {wm iconify $newname; return} }
	    destroy { if {$exists} {destroy $newname; return} }
	}
    }

    #############################################################################
    ## Library Procedure:  gVTcl:DefineAlias
    
    proc gVTcl:DefineAlias {target alias widgetProc top_or_alias cmdalias} {
	## This procedure may be used free of restrictions.
	##    Exception added by Christian Gavin on 08/08/02.
	## Other packages and gWidget toolkits have different licensing requirements.
	##    Please read their license agreements for details.
	
	global gWidget
	set gWidget($alias) $target
	set gWidget(rev,$target) $alias
	if {$cmdalias} {
	    interp alias {} $alias {} $widgetProc $target
	}
	if {$top_or_alias != ""} {
	    set gWidget($top_or_alias,$alias) $target
	    if {$cmdalias} {
		interp alias {} $top_or_alias.$alias {} $widgetProc $target
	    }
	}
    }
    #############################################################################
    ## Library Procedure:  gVTcl:DoCmdOption
    
    proc gVTcl:DoCmdOption {target cmd} {
	## This procedure may be used free of restrictions.
	##    Exception added by Christian Gavin on 08/08/02.
	## Other packages and gWidget toolkits have different licensing requirements.
	##    Please read their license agreements for details.
	
	## menus are considered toplevel windows
	set parent $target
	while {[winfo class $parent] == "Menu"} {
	    set parent [winfo parent $parent]
	}
	
	regsub -all {\%gWidget} $cmd $target cmd
	regsub -all {\%top} $cmd [winfo toplevel $parent] cmd
	
	uplevel #0 [list eval $cmd]
    }
    #############################################################################
    ## Library Procedure:  gVTcl:FireEvent
    
    proc gVTcl:FireEvent {target evnt {params {}}} {
	## This procedure may be used free of restrictions.
	##    Exception added by Christian Gavin on 08/08/02.
	## Other packages and gWidget toolkits have different licensing requirements.
	##    Please read their license agreements for details.
	
	## The window may have disappeared
	if {![winfo exists $target]} return
	## Process each binding tag, looking for the event
	foreach bindtag [bindtags $target] {
	    set tag_events [bind $bindtag]
	    set stop_processing 0
	    foreach tag_event $tag_events {
		if {$tag_event == $evnt} {
		    set bind_code [bind $bindtag $tag_event]
		    foreach rep "\{%W $target\} $params" {
		        regsub -all [lindex $rep 0] $bind_code [lindex $rep 1] bind_code
		    }
		    set result [catch {uplevel #0 $bind_code} errortext]
		        if {$result == 3} {
		            ## break exception, stop processing
		            set stop_processing 1
		        } elseif {$result != 0} {
		            bgerror $errortext
		        }
		        break
		    }
		            }
		    if {$stop_processing} {break}
		}
	    }
	    #############################################################################
	    ## Library Procedure:  gVTcl:Toplevel:WidgetProc
	    
	    proc gVTcl:Toplevel:WidgetProc {w args} {
		## This procedure may be used free of restrictions.
		##    Exception added by Christian Gavin on 08/08/02.
		## Other packages and gWidget toolkits have different licensing requirements.
		##    Please read their license agreements for details.
		
		if {[llength $args] == 0} {
		    ## If no arguments, returns the path the alias points to
		    return $w
		}
		set command [lindex $args 0]
		set args [lrange $args 1 end]
		switch -- [string tolower $command] {
		    "setvar" {
		        foreach {varname value} $args {}
		        if {$value == ""} {
		            return [set ::${w}::${varname}]
		        } else {
		            return [set ::${w}::${varname} $value]
		        }
		    }
		    "hide" - "show" {
		        Window [string tolower $command] $w
		    }
		    "showmodal" {
		        ## modal dialog ends when window is destroyed
		        Window show $w; raise $w
		        grab $w; tkwait window $w; grab release $w
		    }
		    "startmodal" {
		        ## ends when endmodal called
		        Window show $w; raise $w
		        set ::${w}::_modal 1
		        grab $w; tkwait variable ::${w}::_modal; grab release $w
		    }
		    "endmodal" {
		        ## ends modal dialog started with startmodal, argument is var name
		        set ::${w}::_modal 0
		        Window hide $w
		    }
		    default {
		        uplevel $w $command $args
		    }
		}
	    }
	    #############################################################################
	    ## Library Procedure:  gVTcl:WidgetProc
	    
	    proc gVTcl:WidgetProc {w args} {
		## This procedure may be used free of restrictions.
		##    Exception added by Christian Gavin on 08/08/02.
		## Other packages and gWidget toolkits have different licensing requirements.
		##    Please read their license agreements for details.
		
		if {[llength $args] == 0} {
		    ## If no arguments, returns the path the alias points to
		    return $w
		}
		
		set command [lindex $args 0]
		set args [lrange $args 1 end]
		uplevel $w $command $args
	    }
	    #############################################################################
	    ## Library Procedure:  gVTcl:toplevel
	    
	    proc gVTcl:toplevel {args} {
		## This procedure may be used free of restrictions.
		##    Exception added by Christian Gavin on 08/08/02.
		## Other packages and gWidget toolkits have different licensing requirements.
		##    Please read their license agreements for details.
		
		uplevel #0 eval toplevel $args
		set target [lindex $args 0]
		namespace eval ::$target {set _modal 0}
	    }
	}
	    

	if {[info exists gVTcl(sourcing)]} {
	    
	    proc gVTcl:project:info {} {
		set base .top
		namespace eval ::widgets::$base {
		    set set,origin 1
		    set set,size 1
		    set runvisible 1
		}
		namespace eval ::widgets::$base.fra48 {
		    array set save {-borderwidth 1 -relief 1}
		}
		set site_3_0 $base.fra48
		namespace eval ::widgets::$site_3_0.pan50 {
		    array set save {}
		}
		namespace eval ::widgets::$site_3_0.pan50.f1 {
		    array set save {-highlightcolor 1}
		}
		set site_5_0 $site_3_0.pan50.f1
		namespace eval ::widgets::$site_5_0.tr.lis51 {
		    array set save {-background 1 -listvariable 1 -xscrollcommand 1 -yscrollcommand 1}
		}
		namespace eval ::widgets::$site_3_0.pan50.f2 {
		    array set save {-highlightcolor 1}
		}
		set site_5_0 $site_3_0.pan50.f2
		namespace eval ::widgets::$site_5_1.txt.tex54 {
		    array set save {-background 1 -insertbackground 1}
		}
		namespace eval ::widgets::$base.fra49 {
		    array set save {-borderwidth 1 -height 1 -relief 1 -width 1}
		}
		set site_3_0 $base.fra49
		namespace eval ::widgets::$site_3_0.lab80 {
		    array set save {-image 1 -takefocus 1 -text 1}
		}
		namespace eval ::widgets::$site_3_0.tex81 {
		    array set save {-background 1 -insertbackground 1}
		}
		namespace eval ::widgets::$site_3_0.scr82 {
		    array set save {}
		}
		namespace eval ::widgets::$base.m74 {
		    array set save {-tearoff 1}
		    namespace eval subOptions {
		        array set save {-command 1 -label 1 -menu 1}
		    }
		}
		set site_3_0 $base.m74
		namespace eval ::widgets::$site_3_0.men75 {
		    array set save {-tearoff 1}
		    namespace eval subOptions {
		        array set save {-command 1 -label 1 -menu 1}
		    }
		}
		set site_3_0 $base.m74
		namespace eval ::widgets::$site_3_0.men76 {
		    array set save {-tearoff 1}
		    namespace eval subOptions {
		        array set save {-command 1 -label 1}
		    }
		}
		set site_3_0 $base.m74
		namespace eval ::widgets::$site_3_0.men77 {
		    array set save {-tearoff 1}
		    namespace eval subOptions {
		        array set save {-command 1 -label 1}
		    }
		}
		set site_3_0 $base.m74
		namespace eval ::widgets::$site_3_0.men78 {
		    array set save {-tearoff 1}
		    namespace eval subOptions {
		        array set save {-command 1 -label 1}
		    }
		}
		set site_3_0 $base.m74
		namespace eval ::widgets::$site_3_0.men79 {
		    array set save {-tearoff 1}
		    namespace eval subOptions {
		        array set save {-command 1 -label 1}
		    }
		}
		namespace eval ::widgets_bindings {
		    set tagslist _TopLevel
		}
		namespace eval ::gVTcl::modules::main {
		    set procs {
		        init
		        main
		        clickQuit
		        clickRotateSelections
		        clickSearch
		        clickShowSignals
		        clickSwitchToRelatedView
		        clickTreeSelectAction
		        debCallback
		        debShowSignal
		        menuAbout
		        menuHelp
		        menuOpenSources
		        menuQuit
		        menuSaveBsv
		        menuToggleWritable
		        userClose
		        userComment
		        userFindAndOpen
		        userFindMatchingIndex
		        userGetLogo
		        userHilightHitlist
		        userLoadColorInfoBsv
		        userLoadColorInfoV
		        userLoadTreeView
		        userMessageBox
		        userOpenSources
		        userOpenSource
		        userOpenWaves
		        userLoadInfoFile
		        userReadInfoFile
		        userLoadView
		        userStatus
		        userTag
		        userUnHilightHitlist

			clickDelSignals
			sendCanFireSignals
			sendWillFireSignals
		    }
		    set compounds {
		    }
		    set projectType single
		}
	    }
	}


#################################
# USER DEFINED PROCEDURES
#
################################################################################
proc modifyTag {tag value} {
    global gPreferences gPrompt
    # use color selection font to change color
    set color [tk_chooseColor -initialcolor gray -title "Choose Color"]
    
    # replace color with new color
    # puts "color selected = $color"
    # set gPreferences($tag) "-foreground $color"
}

proc menuHelp {} {
    global gKeyList
    
    userMessageBox Info \
	[list \
	     [format "%-20s - quit blueview" $gKeyList(quit)] \
	     [format "%-20s - open/update sources" $gKeyList(opensources)] \
	     [format "%-20s - search in text area" $gKeyList(search)] \
	     [format "%-20s - show available commands in tree area" "<Right Mouse>"] \
	     [format "%-20s - show available commands in text area" "<Right Mouse>"] \
	     "" \
	     "Left  Mouse Button - select things" \
	     "Right Mouse Button - popup menu" \
	     "" \
	     "Plus all the basic emacs text navigation keys work." \
	     "" \
	     "If you toggle the read-only mode in the File menu"\
	     "then you can edit and write BSV files." \
	    ]
}

proc opendebugger {} {
    puts "Openning RamDebugger"

    global gRamDebuggerLoaded
    if {[info exists gRamDebuggerLoaded] == 1} return
    if {[file exists /usr/local/RamDebugger/RamDebugger.tcl] == 0} return

    global auto_path
    # set auto_path /usr/lib/tcl8.4 /usr/lib /usr/local/RamDebugger
    lappend auto_path /usr/local/RamDebugger
    puts $auto_path
    package require RamDebugger
    set gRamDebuggerLoaded 1
}

proc menuAbout {} {
    global gVersion gPrompt gBuild gExpDate
    
    set expDate [format "%02d/%04d" [lindex $gExpDate 0] [expr [lindex $gExpDate 1] + 2000]]
    
    # create a fancy dialog
    set t .top.m
    if {[DialogUtilsCreate $t "About Blueview" -borderwidth 10]} {
	set f $t.f
	frame $f
	
	button $f.l0  -image [image create photo -data [userGetLogo]] 
	label  $f.l1 -text "Blueview version $gVersion, build $gBuild"
	label  $f.l2 -text "Expires the last day of $expDate"
	label  $f.l3 -text "Copyright 2008, Bluespec Incorporated"
	label  $f.l4 -text "nWave Copyright by Novas"
	label  $f.l5 -text "Tcl/Tk/wish/bwidget are public domain"
	button $f.ok -text Ok -command { set gPrompt 1 }
	
	grid columnconf $f.l1 0 -weight 1
	grid rowconf    $f.l1 0 -weight 1
	grid $f.l0  -in $f -column 0 -row 0 -columnspan 1 -rowspan 1 -stick ns
	grid $f.l1  -in $f -column 0 -row 1 -columnspan 1 -rowspan 1 -stick ns
	grid $f.l2  -in $f -column 0 -row 2 -columnspan 1 -rowspan 1 -stick ns
	grid $f.l3  -in $f -column 0 -row 3 -columnspan 1 -rowspan 1 -stick ns
	grid $f.l4  -in $f -column 0 -row 4 -columnspan 1 -rowspan 1 -stick ns
	grid $f.l5  -in $f -column 0 -row 5 -columnspan 1 -rowspan 1 -stick ns
	grid $f.ok  -in $f -column 0 -row 6 -columnspan 1 -rowspan 1 -stick ns
	grid $f     -in $t -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
    }
    set gPrompt 0
    DialogUtilsWait $t gPrompt $t.f.ok
    DialogUtilsDismiss $t
    destroy $t
}

proc menuQuit {} {
    global gWidget
    global gTkAppList
    clickQuit
}

proc menuToggleWritable {} {
    global gReadOnly
    if {$gReadOnly == 1} {
	set gReadOnly 0
	userStatus "BSV files are now read only!"
    } else {
	set gReadOnly 1
	userStatus "BSV files are now writable!"
    }
}


# text edit doesn't exist in 8.3 but does in 8.4
proc textEdit { cmd {flag false} } {
    global gTextArea gTextModified
    if {$cmd == "reset"} {
        set gTextModified 0
        return 0
    } elseif {$cmd == "modified"} {
        return $gTextModified
    }

    return 0
}

proc menuColorizeTextArea {} {

    global gCurrViewIsBsv gCurrTreeIndex gTextArea gTreeInfo gInfoDefof

    if {$gCurrViewIsBsv==0} {

    } else {

	set i   $gCurrTreeIndex
	set mod $gTreeInfo($i:mod)
	set file $gInfoDefof($mod:bfile)

	userLoadColorInfoBsv $file

    }
}


    
proc menuSaveBsv {} {
    global gCurrViewIsBsv gTreeInfo gCurrTreeIndex gTextArea
    global gInfoDefof
    
    # TODO save the current BSV file
    if {$gCurrViewIsBsv==0} {
	userMessageBox Info [list "You shouldn't be able to edit .v files with blueview"]
	return
    }
    
    set i   $gCurrTreeIndex
    set mod $gTreeInfo($i:mod)
    set bfile $gInfoDefof($mod:bfile)
    
    # get text, backup file, open file, write file
    if {[file exists $bfile]} {
	exec cp -p $bfile $bfile~   ;# back up original
    }
    
    set fp [open $bfile w]
    if {$fp != 0} {
	puts $fp [$gTextArea get "1.0" end]
	::userStatus "Saving $bfile"
        
        global gTextModified
        set gTextModified 0
    } else {
	::userStatus "unable to save $bfile"
    }
    close $fp
}

proc menuSaveAsBsv {} {
    global gCurrViewIsBsv gTextArea
    
    # TODO save the current BSV file
    if {$gCurrViewIsBsv==0} {
	userMessageBox Info [list "You really don't want to edit generated verilog files, do you?"]
	return
    }
    
    set bfile [tk_getOpenFile -filetypes {{{BSV Files} .bsv} {{All Files} *}}]
    if {$bfile=={}} return
    
    # get text, backup file, open file, write file
    if {[file exists $bfile]} {
	exec cp -p $bfile $bfile~   ;# back up original
    }
    
    set fp [open $bfile w]
    if {$fp != 0} {
	puts $fp [$gTextArea get "1.0" end]
	::userStatus "Saving $bfile"
    } else {
	::userStatus "unable to save $bfile"
    }
    close $fp
}

# create popup and return file as needed (or null if no result)
proc getFile {opts} {
    set fl {}

    eval set fl \"[tk_getOpenFile -filetypes $opts]\"
    if {$fl=={}} { return {} }
    
    global gPwd
    regsub "$gPwd/?" $fl {} fl   ;# remove all hierarchy to *this* point
    return $fl
}

proc menuOpenSources {} {
    
    # create a dialog to get everything we need.
    global gPrompt 
    global gInfoFile gVcdFile gInstance 
    global tmpInfoFile tmpVcdFile tmpInstance
    
    set t .top.m

    if {[DialogUtilsCreate $t "Open/Update Sources" -borderwidth 10]} {

	set tmpInfoFile $gInfoFile
	set tmpVcdFile $gVcdFile
	set tmpInstance $gInstance

	set f $t.f
	frame $f
	frame $f.f1
	frame $f.f2
	frame $f.f3
	frame $f.f4
	set f1 $f.f1
	set f2 $f.f2
	set f3 $f.f3
	set f4 $f.f4
	
	label $f2.l -text {.info File}  -width 12
	entry $f2.m -textvariable tmpInfoFile -width 50
	button $f2.r -text {...} -command {set tmpInfoFile [getFile {{Info .info} {{All Files} *}}]}
	grid columnconf $f2 1 -weight 1
	grid $f2.l -in $f2  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f2.m -in $f2  -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f2.r -in $f2  -column 2 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	

	label $f1.l -text {.vcd/.fsdb File} -width 12
	entry $f1.m -textvariable tmpVcdFile -width 50
	button $f1.r -text {...} -command {set tmpVcdFile [getFile \
		                                              {{Vcd .vcd} \
		                                                   {Fsdb .fsdb} \
		                                                   {{All Files} *}}]}
	
	grid columnconf $f1 1 -weight 1
	grid $f1.l -in $f1  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f1.m -in $f1  -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f1.r -in $f1  -column 2 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	
	label $f3.l -text {Top Instance} -width 12
	entry $f3.m -textvariable tmpInstance -width 50
	grid columnconf $f3 1 -weight 1
	grid $f3.l -in $f3  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f3.m -in $f3  -column 1 -row 0 -columnspan 2 -rowspan 1 -sticky nesw
	
	button $f4.ok      -text Ok     -command { set gPrompt 1 }
	button $f4.cancel  -text Cancel -command { set gPrompt 0 }
	grid $f4.ok     -in $f4  -column 0 -row 0 -columnspan 1 -rowspan 1
	grid $f4.cancel -in $f4  -column 2 -row 0 -columnspan 1 -rowspan 1
	
	grid $f1 -in $f  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f2 -in $f  -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f3 -in $f  -column 0 -row 2 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f4 -in $f  -column 0 -row 3 -columnspan 1 -rowspan 1 -sticky nesw
	
	grid $f      -in $t  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	
    }
    
    
    set gPrompt 0                           ;# wait for answer
    DialogUtilsWait $t gPrompt $t.f.f4.ok
    set res $gPrompt
    DialogUtilsDismiss $t
    destroy $t

    if {$res == 1} {
	# ARG0 top level .info matches to /$ARGV2/$ARG3
	# ARG1 vcd file name
	# ARG2 path to top level .info file
	
	set fail 0

	if {$tmpInfoFile == ""} {

	    set fail 1

	    bell
	    userMessageBox Notice [list \
				      "Blueview cannot operate without a .info file." \
				      "Please specify a valid .info file." \
				     ]
	    
	}

	if {!($tmpInfoFile == "") && ![file exists $tmpInfoFile]} {

	    set fail 1

	    bell
	    userMessageBox Notice [list \
				      "Cannot find the file $tmpInfoFile."\
				      "Blueview cannot operate without a .info file!" \
				      "Please specify a valid .info file." \
				     ]
	    
	}

	if {![string equal $gVcdFile $tmpVcdFile] && ![file exists $tmpVcdFile]} {

	    set fail 1

	    bell
	    userMessageBox Notice [list \
				      "Cannot find the file $tmpVcdFile." \
				      "Please specify a valid .vcd file." \
				     ]
	    
	}

	if !$fail {

	    set gInfoFile $tmpInfoFile
	    set gVcdFile $tmpVcdFile
	    set gInstance $tmpInstance

	    if {$gInstance=={}} {

		if {[regexp {\.fsdb$} $gVcdFile]} {


		    bell
		    userStatus "No path given, Can't automatically determine path to top from a .fsdb file yet"

		} else {

		    set gInstance [update_path_value $gInstance $gVcdFile]

		}
	    }

	    userStatus "Source files being updated."
	    userStatus "Current Values: .info file: $gInfoFile .vcd file: $gVcdFile"
#	    userStatus "From tcsh: blueview $gInfoFile $gVcdFile $gInstance"
	    set gCurrPosBsv [list "1.0" "1.0"]
	    set gCurrPosV   [list "1.0" "1.0"]
	
	    debug 0 "userOpenSources $gInfoFile $gInstance $gVcdFile "
	    userOpenSources $gInfoFile $gInstance $gVcdFile 

	} else {

	    userStatus "Source files not being updated."
	    userStatus "Current Values: .info file: $gInfoFile .vcd file: $gVcdFile"

	}
    }
}


################################################################################
###
################################################################################

proc userFindFileExists {file_name} {

    global gSearchPath
    if {[file exists $file_name]} {

	return 1
    }
    
    foreach dir $gSearchPath {
	
	set full [file join $dir $file_name]
	
	if {[file exists $full]} {
	    return 1
	}
    }

    return 0
}


################################################################################
###
################################################################################


proc userFindAndOpen {fl} {
    global gSearchPath
    if {[file exists $fl]} {
	return [open $fl]
    }
    
    foreach dir $gSearchPath {
	set full [file join $dir $fl]
	if {[file exists $full]} {
	    debug 10 "userFindAndOpen $full"
	    return [open $full]
	}
    }

    userMessageBox Error [list \
		              "File '$fl' not found in search path" \
		              "Path is $gSearchPath" \
		             ]
    return 0
}


################################################################################
###
################################################################################

proc userFindFile {file_name} {

    global gSearchPath
    if {[file exists $file_name]} {

	return $file_name
    }
    
    foreach dir $gSearchPath {
	
	set full [file join $dir $file_name]
	
	if {[file exists $full]} {
	    return $full
	}
    }

    userMessageBox Error [list \
		              "File '$file_name' not found in search path" \
		              "Path is $gSearchPath" \
		             ]
    return ""
}


################################################################################
###
################################################################################


################################################################################
###
################################################################################

proc send_to_debussy {command args} {

    global gDebussyProcName gHackMode gLogFileName

    catch {send $gDebussyProcName $command $args} value

    if {0} {

	set channel [open $gLogFileName a]

	puts -nonewline $channel "Send to debussy: "
	puts -nonewline $channel [create_string command]
	puts -nonewline $channel " "
	puts $channel [create_string args]
	puts $channel "mode = $gHackMode"
	puts $channel "return value = [create_string value]"
	puts $channel ""

	close $channel

    }

    return $value

}
################################################################################
###
################################################################################

proc userClose {} {

    if {[isDebussyRunning]} {

	catch {wvCloseFile -win $gDebussyProcName}
	catch {wvGetSignalClose -win $gDebussyProcName}
	userStatus "Exiting nWave and blueview - this may take a few seconds"
	update
	    
	catch {send_to_debussy wvExit}

    }
}

proc isDebussyRunning {} {

    global gDebussyProcName

    set app_list [winfo interp]
    if {[lsearch $app_list $gDebussyProcName] >= 0} { return 1 }

    return 0

}

proc userStatus {mess} {
    global gStatusArea gTextArea
    debug 0 $mess
    $gStatusArea insert end $mess
    $gStatusArea insert end "\n"
    $gStatusArea see end
}

proc userMessageBox {type mess} {
    global gPrompt 
    # find needed width first
    set width 40
    debug 0 "::userMessageBox $type"
    foreach l $mess {
	debug 0 "  -> $l"
	if {$width < [string length $l]} {
	    set width [string length $l]
	}
    }
    set height [llength $mess]
    
    global gSnaplog
    if {[info exists gSnaplog]} {
        puts $gSnaplog "!!!!! userMessageBox $type !!!!!"
        puts $gSnaplog [join $mess "\n"]

        # can I get a stack trace?
        puts $gSnaplog [dumpStack]

    } else {
        set t .top.m
        if {[DialogUtilsCreate $t $type -borderwidth 10]} {
            set f $t.f
            frame $f
            text $f.txt -height $height -width $width \
                -background grey90 -insertbackground black -wrap none \
                -yscrollcommand { .top.m.f.scrY set } \
                -xscrollcommand { .top.m.f.scrX set }
            scrollbar $f.scrX -command { .top.m.f.txt xview } -orient h
            scrollbar $f.scrY -command { .top.m.f.txt yview } -orient v
            button $f.ok  -text Ok -command { set gPrompt 1 }
            
            grid columnconf $f.txt 0 -weight 1
            grid rowconf $f.txt 0 -weight 1
            grid $f.txt  -in $f -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
            grid $f.scrY -in $f -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky ns
            grid $f.scrX -in $f -column 0 -row 1 -columnspan 2 -rowspan 1 -sticky ew
            grid $f.ok   -in $f -column 0 -row 2 -columnspan 2 -rowspan 1 
            
            grid columnconf $f 0 -weight 1
            grid rowconf $f 0 -weight 1
            grid $f      -in $t  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
            $f.txt insert end [join $mess "\n"]
            bind $f.txt <Any-Key> break
            bind $t     <Return>  {set gPrompt 1}
        }
        set gPrompt 0
        DialogUtilsWait $t gPrompt $t.f.ok
        DialogUtilsDismiss $t
        destroy $t
    }

}

#############################################################################
# misc utils

# split /foo/bar/goo into two fields "/foo/bar" and "goo"
proc splitOffInstance {path} {
    set fields [split $path /]
    set n    [expr [llength $fields] - 1]
    set hier [join [lrange $fields 0 [expr $n-1]] /]
    set inst [lindex $fields $n]
    return [list $hier $inst]
}


#############################################################################
proc getInfoMod {i} {
    global gInfoDefof gTreeArea gTreeInfo
    while {$i != "root"} {
        set mod $gTreeInfo($i:mod)
        if {[info exists gInfoDefof($mod:vfile)]} {
            return $mod
        }
        set i [$gTreeArea parent $i]
        debug 1 "getInfoMod: move up to node $i"
    }
    return {}
}

#############################################################################

proc clickShowSignals { {sendto nWave} } {
    
    global gCurrViewIsBsv gCurrPosBsv gCurrPosV
    global gCurrentSelection
    global gTreeInfo gCurrTreeIndex
    global gInfoNet  gTextArea
    global gMenuDone gTextArea gInfoDefof
    global gHackMode gWaveState

    if {$gWaveState == "nowaves"} { 
	
	bell
	userStatus "No .vcd/.fsbd file has been loaded yet."
	set gMenuDone 1
	return
	
    }
    
    if {![isDebussyRunning]} { 
	
	bell
	userStatus "No waveform viewer is running."
	set gMenuDone 1
	return
	
    }
    
    # just get what we grabbed
    set range [getSelection tagWaveable]
    if {[llength $range] == 0} {
	set range [getSelection tagXRef]
	
	if {[llength $range] == 0} {
	    
	} else {
	    
	    bell  
	    userStatus "The selected region is cross-referenced but has no associated signal net(s)."
	    set gMenuDone 1
	    return
	    
	}
    }
    
    if {[llength $range] == 0} {
	
	bell  
	userStatus "Notice: The cursor is not inside a tagged region."
	set gMenuDone 1
	return
	
    }
    
    set inx   [lindex $range 0]
    set enx   [lindex $range 1]
    
    if {$gCurrViewIsBsv==1} { 
	set gCurrPosBsv $gCurrentSelection
    } else {
	set gCurrPosV $gCurrentSelection
    }
    
    set infomod  [getInfoMod $gCurrTreeIndex]
    set mod      $infomod
    set vfile    $gInfoDefof($mod:vfile)
    set vpath    [makepath $gTreeInfo($gCurrTreeIndex:vpath)]
    
    # if no .info file, then go to parent until we find an info file
    while {[array names gInfoNet $infomod]=={}} {
	userMessageBox Error [list "$infomod.info file not found!"]
	set gMenuDone 1
	return
    }
    
    # else we are looking at a verilog file, parse it to find out what signals are valid
    # and what their sizes are if it hasn't been done already
    global gVerilogRef
    if {[info exists gVerilogRef] == 0} {
	::readVerilogDefines $vfile
    }
    
    if {$gCurrViewIsBsv==1} { 
	######################################################################
	# look for useful signals if we are looking at a BSV
	# this is a list of lists
	# puts "DBG check for match at $inx"
	debug 3 ":signal check for things in range of ($inx -> $enx) in $infomod"
	
	foreach f $gInfoNet($infomod) {
	    array set tmp $f
	    set signal $tmp(signal)
	    
	    # we should really match this to the exact index
	    if {$gCurrViewIsBsv==1} {
		set pnt $tmp(bpos)
	    } else {
		set pnt $tmp(vpos)
	    }
	    
	    # if in range, then add signal (word or drag or otherwise :)
	    # debug ":signal check $inx <= $pnt <= $enx"
	    if {[$gTextArea compare $inx <= $pnt] && [$gTextArea compare $pnt <= $enx]} {
		debug 4 "  match at $pnt for $signal"
		
		if {[info exists gVerilogRef($signal)]} {
		    set szH [lindex $gVerilogRef($signal) 0]
		    set szL [lindex $gVerilogRef($signal) 1]
		    set sig [create_debussy_signal 0 $vpath $signal $szH $szL]
		    set sig_1 [create_debussy_signal 1 $vpath $signal $szH $szL]
		    if {[::userSignalMatchFunction $sig] == 1} {
		        if {[info exists driven($sig)]==0} {
		            lappend slist $sig
		            lappend slist_1 $sig_1
		            set driven($sig) 1
		        }
		    }
		}
	    }
	}
	
    } else {
	######################################################################
	
	# now select all the strings in the verilog file and send them to
	# nWave if they are valid signals
	if {$inx != "0.0"} {
	    while 1 {
		set hit [$gTextArea search -count cnt -forward -regexp {[\w\d_\$]+} $inx $enx]
		if {$hit == ""} break
		
		set eom [$gTextArea index "$hit +$cnt chars"]
		set signal [$gTextArea get $hit $eom]
		
		if {[info exists gVerilogRef($signal)]} {
		    set szH [lindex $gVerilogRef($signal) 0]
		    set szL [lindex $gVerilogRef($signal) 1]
		    set sig [create_debussy_signal 0 $vpath $signal $szH $szL]
		    set sig_1 [create_debussy_signal 1 $vpath $signal $szH $szL]
		    if {[::userSignalMatchFunction $sig] == 1} {
		        if {[info exists driven($sig)]==0} {
		            lappend slist $sig
		            lappend slist_1 $sig_1
		            set driven($sig) 1
		        }
		    }
		}
		
		set inx $eom
	    }
	}
    }

    if {[info exists slist] == 0} {
	if {$gCurrViewIsBsv==1} { 
	    bell  
	    userStatus "Notice: The selected region does not include any tagged regions."

	} else {

	    userMessageBox Warning [list "No valid selection" \
		                        "Please select a signal or range including valid signals" \
		                        "(as defined by input, output, wire and reg definitions" ]
	}
	
	set gMenuDone 1
	return
    }
    
    # create a group name and add as group?
    # name is probably "/foo/foo/name$xxxx"
    # set firstsig [lindex $siglist 0]
    # regsub "{ " $firstsig "" firstsig
    # regsub " }" $firstsig "" firstsig
    
    # set grp "BSVgrp_$gWaveGroup"
    
    # puts "wvAddGroup -win abc $grp"
    # puts "wvSetPosition -win abc {{\"$grp\" 1}}"
    # puts "wvAddSignal -win abc -group { \"$grp\" {$lst} }"
    # puts "wvSelectSignal -win abc -delim / $lst"
    
    set gAddNewGroup 0
    updateHackMode $slist $slist_1

    if {$gHackMode > 0} {
	
	set ls [join $slist_1 " "]

    } else {

	set ls [join $slist " "]

    }

    set mess "wvAddSignal $ls" 
    debug 2 $mess

    global gSnaplog
    if {[info exists gSnaplog]} {
        puts $gSnaplog $mess
    }

    if {[::userOpenWaves] == "loaded"} {

	if {$gAddNewGroup == 1} {
	    send_to_debussy "wvAddGroup  $grp"
	    send_to_debussy "wvSetPosition  {{\"$grp\" 1}}"
	    send_to_debussy "wvAddSignal  -group { \"$grp\" {$ls} }" 
	} else {

	    set count [llength $ls]

	    if { $count > 1 } {

		userStatus "Sending $count signals to the wave viewer."

	    } elseif { $count == 1 } {

		userStatus "Sending $count signal to the wave viewer."

	    }

	    # add the signal after current selection
#            send abc "wvAddSignal -win abc $ls -scope $vpath"
            set value [send_to_debussy "wvAddSignal $ls"]

	}
    }
    
    set gMenuDone 1

}

################################################################################
###
################################################################################

proc create_debussy_signal {mode vpath signal szH szL} {

    if {$szH != -1} {

	if {$mode == 1 } {

	    set sig $signal\[$szH:$szL\]
	    set sig $vpath/\\$sig

	} else {

	    set sig $signal\[$szH:$szL\]
	    set sig $vpath/$sig
	}

    } else {

	set sig $signal
	set sig $vpath/$sig

    }

    regsub {[ \t]+$} $sig {} sig
    regsub -all "\\." $sig "_" sig

    set sig \{$sig\}
    
    return $sig

}

################################################################################
###
################################################################################

proc updateHackMode {slist slist_1} {

    global gHackMode gVcdFile

    # Mode already set.
    if {$gHackMode > -1 } { return }

    set index [lsearch -glob $slist "*:*"]

    if {$index == -1} {

	set sig [lindex $slist 0]

	set value [send_to_debussy "wvAddSignal $sig"]
	
	if {$value == 1} {

	    send_to_debussy "wvUndo"

	} else {

	    bell

	    if { $gVcdFile == {} } {

		userStatus "Warning: No .vcd/.fsbd file has been loaded yet."

	    } else {

		userMessageBox Info [list "Warning: The signal: $sig does not seem to exist in the .vcd file. (Is the hierarchy prefix correct?)"]

	    }

	}

	return

    }

    set sig [lindex $slist $index]
    set sig_1 [lindex $slist_1 $index]

    set value [send_to_debussy "wvAddSignal $sig"]

    if {$value == 0 } {

	set value_1 [send_to_debussy "wvAddSignal $sig_1"]

	if {$value_1 == 1} {

	    send_to_debussy "wvUndo"
	    set gHackMode 1
	    bell
	    userStatus "Notice: Adjusting debussy filter mode. Please ignore the unrecognized signals notice."
#	    userMessageBox Info [list "Notice: Adjusting tcl-to-debussy filter mode for current environment. You can ignore this unrecognized signals notice."]


	} else {

	    # Neither worked
	    # set gHackMode 0
	    bell
	    userMessageBox Info [list "Warning: The signal: $sig does not seem to exist in the .vcd file. (Is the hierarchy prefix correct?)"]


	}


    } else {
	
	send_to_debussy "wvUndo"
	set gHackMode 0

    }

}


################################################################################
###
################################################################################



################################################################################
# return item n
proc llast {lst} {
    return [lindex $lst end]
}

# return items 0..n-1
proc lfront {lst} {
    set len [llength $lst]
    if {$len == 1} { return {} }
    return [lrange $lst 0 [expr $len - 2]]
}

# return a nWave style path (like a file path)
proc makepath {lst} {
    return /[join $lst /]
}

#############################################################################
# this is the very heart of this data structure.
# rules:
# 1) EVERY node has following info
#    a) list of .info file(s) containing bsv info and v info for this module
#         .info file is at $mod.info (if not, then go to parent to find this info)
#    b) bsv path to this node
#    c) verilog path to this node
#
# 2) if 

proc createNodeInfoHier {mod type parentmod bparent_index vparent_index bpath vpath hierlist {level 0}} {
    global gInfoDefof gInfoNet gInfoInstof
    global gTreeMaxIndex gLibraryModule gTreeInfo gInst2TreeIndex

    if {[info exists gLibraryModule($mod)]} {
        debug 10 "skipping library module $mod"
        return
    }

    # create info for this module
    set  i $gTreeMaxIndex
    debug 1 "Create gTreeInfo($i) {$mod} {$bpath} {$vpath} $level"

    set  gInst2TreeIndex([makepath $bpath]) $i
    set  gTreeInfo($i:mod)   $mod
    set  gTreeInfo($i:info)  $parentmod
    set  gTreeInfo($i:type)  $type
    set  gTreeInfo($i:bpath) $bpath
    set  gTreeInfo($i:vpath) $vpath
    set  gTreeInfo($i:level) $level
    set  gTreeInfo($i:bparent_index) $bparent_index
    set  gTreeInfo($i:vparent_index) $vparent_index

    if { $type == "v" } {
	
	set next_bparent_index $i
	set next_vparent_index $i

    } else {

	set next_bparent_index $i
	set next_vparent_index $vparent_index

    }

    incr gTreeMaxIndex
    set  i $gTreeMaxIndex

    # check for info file to continue to recurse
    if {$hierlist == "_"} {

        return
    }

    # get instof info for this mod

    # loop/recurse children
    foreach item $hierlist {
        set chtype      [lindex $item 0]
        set chbinst     [lindex $item 1]
        set chinst      [lindex $item 2]
        set chhierlist  [lindex $item 4]

        if {[info exists gInfoInstof($mod:$chinst)] == 0} {
            # find parent module until we hit one with a .info file that is present
            # and use the verilog instance name since we are in a flat hierachy
            if {[info exists gInfoInstof($parentmod:$chinst)]} {
                set mod $parentmod
            } else {
#		breakf
                debug 1 "Warning: gInfoInstof($mod:$chinst) info file does not exist, but listed in hierarchy!"
                continue
            }
        }

        array set infoInstof $gInfoInstof($mod:$chinst)

        # for each instance name, find related mod defined in gDefof (flat or not)
        debug 1 "check for gInfoInstof( $mod : $chinst : info) \[{$chtype}{$chinst}\]"

#        set chmod $infoInstof(info)
        set chmod [lindex $item 3]
        set chvpath $vpath

        if {$chtype == "v" } {
	    #        if {[file exists "$chmod.info"]} { }
            set chvpath [concat $vpath $chinst]
        } else {
            # same path as what we already have if there is no .info file
            set chvpath $vpath
        }
        
        set     chbpath [concat $bpath $chinst]
        debug 1 "found $chinst $chhierlist ($chmod)"
        
        # if there is a gHierarchy for thios module then go doit now!
        global gHierarchy
        if {[info exists gHierarchy($chmod)] && ($chhierlist == "_")} {
            set chhierlist $gHierarchy($chmod)
        }

        createNodeInfoHier $chmod $chtype $mod $next_bparent_index $next_vparent_index $chbpath $chvpath $chhierlist [expr $level + 1]
    }

}

################################################################################
###
################################################################################

proc userLoadTreeView {} {

    global gTreeArea gTreeInfo gCurrTreeIndex gInst2TreeIndex
    global gTreeMaxIndex

    replace_tree_area
    $gTreeArea delete root
    set parent root
    set lastmod ""
    set parent root

    for {set i 0} {$i < $gTreeMaxIndex} {incr i} {
        set mod    $gTreeInfo($i:mod)
        set bpath  $gTreeInfo($i:bpath)
        set type  $gTreeInfo($i:type)

        if {$i == 0} {
            set inst [makepath $bpath]
            set parent root
        } else {
            set inst [llast $bpath]
            set ppath [lfront $bpath]
            while {[info exists gInst2TreeIndex([makepath $ppath])] == 0} {
                # yikes - we have hierachy problems! - go to nearest defined parent!
                set ppath [lfront $ppath]
            }
            set parent $gInst2TreeIndex([makepath $ppath])
        }

        debug 0 "LoadTreeView {$bpath} {$mod} parent=$parent inx=$i"

        if {$lastmod == $mod} {
            set attr ""
        } else {
            set attr " ($mod)"
        }
        set lastmod $mod

	regsub  {.*\.} $inst "" suffix

	if { $type == "v" } {

	    $gTreeArea insert end $parent $i  -fill black -text "$suffix$attr"

	} else {

	    $gTreeArea insert end $parent $i -fill gray50 -text "$suffix$attr"

	}
    }
    
    set gCurrTreeIndex 0
    $gTreeArea selection set $gCurrTreeIndex
    
    $gTreeArea opentree $gCurrTreeIndex 0     ;# open at least one level
    $gTreeArea see      $gCurrTreeIndex       ;# look at index 0 (top of tree)
    $gTreeArea xview    moveto 0              ;# look at far left

    clickTreeSelectAction defof bsv 0

}

################################################################################
### This is a hack because I can't seem to get the treearea to reload
### without replacing it!
################################################################################

proc replace_tree_area {} {

    global gTreeArea gColorList gKeyList

    set parent [winfo parent $gTreeArea]

    set name $gTreeArea

    append name "X"

    set gTreeArea $name

    eval Tree $name $gColorList(treearea) 

    $parent setwidget $name
    
    gVTcl:DefineAlias $name "TreeArea" gVTcl:WidgetProc "Toplevel1" 1

    bind $name.c <Button-3> { 
	global gTextArea
	global gMenuDone
	
	set m .top.popup2
	set gMenuDone 0
	tk_popup $m %X %Y
	tkwait variable gMenuDone
    }

    foreach key [split $gKeyList(treeJumpToBSVDef) ':'] {
        bind $name.c $key { ::clickTreeSelectAction defof bsv }
    }

    foreach key [split $gKeyList(treeJumpToBSVInst) ':'] {
        bind $name.c $key { ::clickTreeSelectAction instof bsv }
    }

    foreach key [split $gKeyList(treeJumpToVerilogDef) ':'] {
	bind $name.c $key { ::clickTreeSelectAction defof verilog }
    }

    foreach key [split $gKeyList(treeJumpToVerilogInst) ':'] {
        bind $name.c $key { ::clickTreeSelectAction instof verilog }
    }
    
}

#############################################################################
proc userOpenSources { infofile path dumpfile } {

    global gInfoFile gInstance gPwd

    set gInfoFile $infofile
    set gInstance $path

    # global gSnaplog
    # set gSnaplog $dumpfile
    
    openSplashScreen "Initializing Data Structures"

    global gWidget gTkAppList gDebApp
    global gStatusArea gTextArea
    global gTreeInfo gInfoSwitch
    global gVcdFile gFsdbFile gInfoFile gWaveState
    global gTopPath gInfoDefof gTreeMaxIndex gVerilogDefs gVerilogLibDefs
    global gHackMode

    #initialize things
    set gVerilogDefs [list]
    set gVerilogLibDefs [list]
    
    # regsub {/[A-Za-z0-9_]+$} $path  {} gTopPath
    regsub {^/*} $path {} gTopPath
    
    # get file name

    set gWaveState nowaves

    if {($gWaveState == "nowaves") && !($dumpfile=={})} {

	set gWaveState "notstarted"

    }

    if {!($dumpfile=={})} {

        if {[regexp {.vcd$} $dumpfile]} {
            set gVcdFile $dumpfile        
            regsub {.vcd$} $gVcdFile ".fsdb" gFsdbFile
	    set gHackMode [deduce_hack_from_vcd $dumpfile]
        } elseif {[regexp {.fsdb$} $dumpfile]} {
            set gVcdFile ""
            set gFsdbFile $dumpfile
        } else {
            # neither, so error for now
            set mess [list "dumpfile '$dumpfile' is neither .fsdb nor .vcd"]
            puts "ERROR: $mess"
            debug 1 "ERROR: $mess"
            exit
            return
        }

	catch {set gVcdFile [file_normalize $gVcdFile]}
	catch {set gFsdbFile [file_normalize $gFsdbFile]}
	set gWaveState "nofsdbyet"
    }

    if {$gWaveState == "nofsdbyet"} {

	if {![isDebussyRunning]} {

	    global gProgressText
	    set gProgressText "starting nWave...."
	    update idletasks
	    puts "Initializing nWave"
	    userOpenWaves

	} else {

	    userOpenWaves

	}
    }

    ############################################################
    # assume main is +
    # assume top  is this.info

    if {[userFindFileExists $infofile]==0} {
	userStatus "Could not find file '$infofile'. Starting with no specified .info file."
	closeSplashScreen
        return
    }
    
    # parse hierarcy from .info files!


    if ![string equal $gInfoFile ""] {

	set info_files [get_all_info_files $gInfoFile]

	lappend info_files $gInfoFile

 	foreach info $info_files {
	    
 	    if {[userReadInfoFile $info] == 0 } {

		bell
		closeSplashScreen
		userMessageBox Error [list "In directory '$gPwd':" \
					 "The .info file '$info' is incompatible with this version of blueview." \
					  "Please generate new .info files by re-running the bsc compiler with version 3.8.59 or later." \
					 "Aborting sourcing of .info file(s)." \
					]
		return

	    }
 	}
    }
    
    # now stitch it all together - the fun part!
    ########################################
    # top module is from defof of top info file
#    regsub {\.info$} $infofile {} topMod
    regsub {^/}      $gTopPath {} topPath

    set topMod [get_module_for_info_file $infofile]
    
    # blow out hierarchy to it's full glory so we can parse it!
    # set hier [userExpandHierarchy $topPath $topMod 

    array unset gTreeInfo
    set gTreeMaxIndex 0

    global gHierarchy gVerilogDefs
    createNodeInfoHier $topMod v {} {} {} [list $topPath] [list $topPath] $gHierarchy($topMod)

    # cleanup tree
    # hierachy may have .v files that have no information in them
    #   so let's clean up and reassign all the index values in the tree
    # cleanupUnusedTreeEntries 
    
    # gotta click something!
    userLoadTreeView
    
    # find vfile -> bfile in gTreeInfo
    # open top level by default!
    set mod    $gTreeInfo(0:mod)
    set bfile  $gInfoDefof($mod:bfile)
    
    set gCurrViewIsBsv 1
    set gCurrPosBsv [list "1.0" "1.0"]
    set gCurrPosV   [list "1.0" "1.0"]

    closeSplashScreen
    ::userOpenSource $bfile $mod

}

#############################################################################
proc generalIndex {inx add} {
    set f [split $inx .]
    set i [lindex $f 1]
    set n [expr $i + $add]
    return "1.0 +$n chars"
}

proc userClearHitList {} {
    global gHitList gHitListIndex
    if {[info exists gHitList]} { unset gHitList }
    set gHitListIndex -1
}

proc userAddToHitList {inx ntype siglen {fname ""}} {

    global gCurrViewIsBsv gHitList gHitListIndex
    # for verilog we can figure this out by length of the signal
    set st $inx
    set en "$inx +$siglen chars"
    lappend gHitList [list $ntype [list $st $en] $fname]
    debug 5 "  hit ($st,$en) $fname"
    incr gHitListIndex
}

proc userFindFirstHitListAssign { {fl ""} } {
    global gHitListIndex gHitList
    
    if {[info exists gHitList]} {
        # return 0 index by default
        set gHitListIndex 0
        set thisHit [lindex $gHitList $gHitListIndex]
        set retInx  [lindex $thisHit 1]
        for {set i 0} {$i < [llength $gHitList]} {incr i} {
            set thisHit [lindex $gHitList $i]
            if {[lindex $thisHit 0] == "ASSIGN"} {
                # return first ASSIGN index if found
                set gHitListIndex $i
                set retInx [lindex $thisHit 1]
            }
        }
    } else {
        set gHitListIndex -1
        set retInx [list "1.0" "1.0"]
    }
    
    debug 6 "::userFindFirstHitListAssign returns $retInx"
    return $retInx
}


# return startinx endinx treeindex
proc userFindMatchingIndex {fullinx inx} { 
    global gInfoNet gTreeInfo gInfoDefof
    global gTextArea gHitList gHitListIndex gCurrViewIsBsv
    
    # set inx  [lindex [split $fullinx {\.}] 0]
    set mod    $gTreeInfo($inx:mod)
    set bpath  $gTreeInfo($inx:bpath)
    
    # get current module name for both bsv and v
    set currinx [lindex $fullinx 0]
    
    ::userClearHitList
    
    if {$currinx == "1.0"} { 
        userStatus "No Related signals were found in the cross reference" 
        return [list "1.0" "1.0"]
    }
    
    set infomod [getInfoMod $inx]
    # set obfile $gInfoDefof($mod:bfile)
    # set ovfile $gInfoDefof($infomod:vfile)
    # debug 1 "info mod = $infomod {$ovfile} {$obfile}"

    foreach f $gInfoNet($infomod) {
        array set tmp $f
        set signal $tmp(signal)
        set vfile  $tmp(vfile)
        set vpos   $tmp(vpos)
        set bfile  $tmp(bfile)
        set bpos   $tmp(bpos)
        set ntype  $tmp(type)
        
        if {$gCurrViewIsBsv == 1} {
            set matchInx  $bpos
            set targetInx $vpos
            if {$signal == "UNNAMED"} {
                set strlen 0  ;# go figure this out
#		set strlen    1
            } else {
                set strlen    [string length $signal]
            }
            
            # might match to several files of opposite type?  
            #  what to do if they do? - tabbed windows?

            debug 3 ":Index check $infomod (tInx=$targetInx, mInx=$matchInx) $vfile"
            if {$currinx == $matchInx} {
                lappend hits($vfile) [list $targetInx $ntype $strlen]
                # ::userAddToHitList $targetInx $ntype $strlen $vfile
            }
            
        } else {
            set matchInx  $vpos
            set targetInx $bpos
            set strlen    0
            
            debug 3 ":Index check $infomod (tInx=$targetInx, mInx=$matchInx) $bfile"
            if {$currinx == $matchInx} {
                lappend hits($bfile) [list $targetInx $ntype $strlen]
                # ::userAddToHitList $targetInx $ntype $strlen $bfile
            }
        }
	
        # currinx is the index of the selection in .verilog
        # matchinx is verilog inx from .info file
        # $targetinx is BSV inx
    }

    if {[info exists hits]} {
        # make sure we got at least one file hit
        set hitfiles [array names hits]

        # warn, but take the first file right now
        if {[llength $hitfiles] > 1} {
            userMessageBox Warning [list "hit more than one file" \
                                         "using first only for now" \
                                         " files are:" \
                                         $hitfiles]
        }

        # get list of hits for this file only for now
        # TODO: tabbed pages for multihits?
        set fl [lindex $hitfiles 0]
        foreach item $hits($fl) {
            ::userAddToHitList [lindex $item 0] [lindex $item 1] [lindex $item 2] $fl
        }

        set res [userFindFirstHitListAssign $fl]
        if {$gCurrViewIsBsv == 0} {   ;# move mouse to the right place
            set gCurrPosV $res
        } else {
            set gCurrPosBsv $res
        }
    } else {
        set res [list "1.0" "1.0"]
    }

    if {[lindex $res 1] == "1.0"} {
#        userStatus "No Related signals were found in the cross reference" 
#        userStatus "    Toggling view, but selecting top of the file!"
	lappend res $inx
    } else {
    
	# third returned element is treeindex
	lappend res $inx
	lappend res $fl
    }
    
    return $res
}


proc clickClearSelections {} {
    
    userUnHilightHitlist
    userStatus "Clearing current selection set." 

}

proc clickRotateSelections {} {
    
    global gTextArea
    global gCurrViewIsBsv gCurrPosV gCurrPosBsv
    global gHitList gHitListIndex
    
    # move to next selection in the hit list, if there is one
    if {[info exists gHitList]} {
        set len [llength $gHitList]
	if {$len < 1 } {

	    bell
	    userStatus "There are no selected points for cycling." 
	    return
	}
	if {$len < 2 } {

	    bell
	    userStatus "There are no additional selected points for cycling." 
	    return
	}
        set gHitListIndex [expr ($gHitListIndex + 1) % $len]
        set f [lindex $gHitList $gHitListIndex]
        set ntype [lindex $f 0]
        set finx  [lindex $f 1]
        if {[lindex $finx 0] == "-1.-1"} return
	
        if {$gCurrViewIsBsv} {
            set gCurrPosBsv $finx
	    $gTextArea tag remove tagSelection 1.0 end
	    $gTextArea tag remove tagSelectionSet 1.0 end
	    userHilightHitlist
            userMoveCursor [lindex $gCurrPosBsv 0]
        } else {
            set gCurrPosV   $finx
	    $gTextArea tag remove tagSelection 1.0 end
	    $gTextArea tag remove tagSelectionSet 1.0 end
	    userHilightHitlist
            userMoveCursor [lindex $gCurrPosV 0]
        }
    } else {

	bell
	userStatus "There are no selected points for cycling." 
 
    }
}

############################################################
proc readLines {fl} {
    set fp [userFindAndOpen $fl]
    if {$fp == 0} {
        return {}
    }
    set lines [split [read $fp] \n]
    close $fp
    return $lines
}


############################################################
# read input/outputs/wires from verilog file
# so we can determine what the size of things is
proc readVerilogDefines { vfile } {
    global gVerilogRef
    
    set lines [readLines $vfile]
    if {[llength $lines] == 0} return
    
    # prepare lines, get rid of useless stuff
    set targetlines {}
    set lmax [llength $lines]
    for {set i 0} {$i < $lmax} {incr i} {
        set l [lindex $lines $i]
        if {[regsub {^ +(input|output|wire|reg) +} $l {} l]} {
            # collapse line up to semi colon
            while {[regexp {;} $l] == 0} {
                incr i
                set nl [lindex $lines $i]
                set l [concat $l $nl]
            }
            
            regsub {^ +} $l {}  l     ;# remove leading space
            regsub { +}  $l { } l     ;# remove duplicate space
            regsub { : } $l {:} l     ;# remove unneeded space

            debug 100 "found line: $l"
            lappend targetlines $l
        }
    }


    # collapse lines that start with input, output, wire, reg
    foreach l $targetlines {
        set f [split $l { ,;}]

        # collect fields
        set high -1
        set low  -1
        set f1 [lindex $f 0]
        
        if {[regexp {^\[} $f1]} {
            set f1 [split $f1 {[]:}]   ;# get fields
            set high [lindex $f1 1]    ;# get size information
            set low  [lindex $f1 2]
            set f [lrange $f 1 end]    ;# pop size off list
        }

        foreach sig $f {
            if {$sig == {}} continue
            debug 100 "  parse verilog signal = $sig $high $low"
            set gVerilogRef($sig) [list $high $low]
        }
    }
}

proc userLoadView { inx file } {
    global gTextArea gCurrViewIsBsv gCurrTreeIndex
    global gInfoDefof gTreeInfo
    
#    set mod     $gTreeInfo($inx:mod)
    set infomod [getInfoMod $inx]

    if {$gCurrViewIsBsv==1} {
#        set bfile $gInfoDefof($mod:bfile)
#        ::userOpenSource $bfile $mod $bpath
	set bfile $file
        ::userOpenSource $bfile $infomod

    } else {
#        set vfile $gInfoDefof($infomod:vfile)
	set vfile $file
        ::userOpenSource $vfile $infomod
    }
    
    global gVerilogRef
    if {[info exists gVerilogRef]} { unset gVerilogRef }
    set vfile $gInfoDefof($infomod:vfile)
    ::readVerilogDefines $vfile
}

#############################################################################
## Procedure:  clickQuit
proc userMoveCursor {pnt} {
    global gTextArea
    focus $gTextArea
    set inx [$gTextArea index  $pnt]

    ## Don't know why the extra offset line is needed, but it it seems to be.
    $gTextArea mark set insert $inx
    $gTextArea see "$inx + 1 line"
}


proc clickQuit {} {

    debug 0 "clickQuit"
    
    ::askToSaveFile
    ::userClose
    
    catch { exit }
}

proc clickSearch {} {
    
    global gSearch_dir gSearch_exact gSearch_casesen 
    global gSearch_frombegin gSearch_string gSearch_regexp
    global gCurrentSelection gTextArea
    global gPrompt
    global gHitListIndex gHitList gKeyList
    
    # if no selection then that's okay also
    if {[info exists gCurrentSelection] == 0} {
        # nothing

    # if there is a selection, start with that value
    } elseif {[lindex $gCurrentSelection 0] != [lindex $gCurrentSelection 1]} {
        set p1 [lindex $gCurrentSelection 0]
        set p2 [lindex $gCurrentSelection 1]
        set gSearch_string [$gTextArea get $p1 $p2]
    }
    
    set t .top.s
    if {[DialogUtilsCreate $t Search -borderwidth 10]} {
        set f $t.f
        frame $f
	
        frame $f.t
        label $f.t.l -text Search 
        entry $f.t.e -textvariable gSearch_string -relief sunken
        grid columnconf $f.t 1 -weight 1
        grid rowconf    $f.t 1 -weight 1
        grid $f.t.l -in $f.t -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w
        grid $f.t.e -in $f.t -column 1 -row 0 -columnspan 1 -rowspan 3 -sticky news
	

	label $f.opt1 -text {Direction}
#        labelframe $f.opt1 -text {Direction}
        radiobutton $f.opt1.1 -text Forward  -variable gSearch_dir -value {-forward}
        radiobutton $f.opt1.2 -text Backward -variable gSearch_dir -value {-backward}
        grid $f.opt1.1 -in $f.opt1 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w
        grid $f.opt1.2 -in $f.opt1 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky w
	
	label $f.opt2 -text {Type}
#        labelframe $f.opt2 -text {Type}
        checkbutton $f.opt2.1 -text Exact  -variable gSearch_exact
        checkbutton $f.opt2.2 -text Regexp -variable gSearch_regexp
        grid $f.opt2.1 -in $f.opt2 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w
        grid $f.opt2.2 -in $f.opt2 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky w
	

	label $f.opt3 -text {Misc}
#        labelframe $f.opt3 -text {Misc}
        checkbutton $f.opt3.1 -text {Case Sensitive} -variable gSearch_casesen
        checkbutton $f.opt3.2 -text {From Beginning} -variable gSearch_frombegin
        grid $f.opt3.1 -in $f.opt3 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w
        grid $f.opt3.2 -in $f.opt3 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky w
	
        frame $f.b
        button $f.b.cancel -text Cancel -command { set gPrompt cancel }
        button $f.b.ok     -text Ok     -command { set gPrompt ok }
        grid $f.b.cancel   -in $f.b -column 1 -row 0 -columnspan 1 -rowspan 1
        grid $f.b.ok       -in $f.b -column 0 -row 0 -columnspan 1 -rowspan 1
	
        grid $f.t        -in $f -column 0 -row 0 -columnspan 3 -rowspan 1 -sticky news
        grid $f.opt1     -in $f -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew
        grid $f.opt2     -in $f -column 1 -row 1 -columnspan 1 -rowspan 1 -sticky ew
        grid $f.opt3     -in $f -column 2 -row 1 -columnspan 1 -rowspan 1 -sticky ew
        grid $f.b        -in $f -column 0 -row 2 -columnspan 3 -rowspan 1 -sticky news
	
        grid columnconf $f 0 -weight 1
        grid rowconf    $f 0 -weight 1
        grid $f -in $t  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	
        bind $f.t.e <Return>    { set gPrompt ok }
        bind $t     <Return>    { set gPrompt ok }
        bind $f.t.e <Control-c> { set gPrompt cancel }
        bind $t     <Control-c> { set gPrompt cancel }
    }
    set gPrompt ok
    DialogUtilsWait $t gPrompt $t.f.b.ok
    set res $gPrompt ;# for some reason this changes after wait is destroyed
    DialogUtilsDismiss $t
    destroy $t

    # debug "gPrompt = $res"
    if {($res == "cancel") || ($gSearch_string == {})} {

        return
    }
    
    # remove anything that is highlited now
    ::userUnHilightHitlist
    
    # if from beginning start at 1.0
    if {$gSearch_frombegin == 1} { 
        set inx "1.0"
    } else {
        set inx [$gTextArea index insert]
    }
    
    ########################################
    # go to the beginning of the file and highlight everything we find
    if {[info exists gHitList]} { unset gHitList }
    set gHitListIndex -1
    
    lappend slist $gSearch_dir
    if {$gSearch_casesen == 0} { lappend slist "-nocase" }
    if {$gSearch_exact   == 1} { lappend slist "-exact" }
    if {$gSearch_regexp  == 1} { lappend slist "-regexp" }
    
    set switches [join $slist " "]
    
    # first entry relative to where we start is a different color
    set hilight tagSelection
    set ln [string length "$gSearch_string"]
    set gHitListIndex -1
    
    while 1 {
        set cmd "$gTextArea search $switches {$gSearch_string} \"$inx\" end"
        debug 6 ":search command = {$cmd}"
        set hitpos [eval $cmd]
	
        if {$hitpos == ""} break
        set gHitListIndex 0
	
        # $gTextArea tag add $hilight $hitpos "$hitpos + $ln chars"
        set eos [$gTextArea index "$hitpos + $ln chars"]
        ::userAddToHitList $hitpos search $ln
	
        # next potential hit
        if {$gSearch_dir == {-forward}} {
            set inx "$hitpos + 1 char"
        } else {
            set inx "$hitpos - 1 char"
        }
    }
    
    # move cursor to first match, set insert cursor there
    if {[info exists gHitList]} {
	set gHitListIndex 0
        set count [userHilightHitlist]          ;# color everything that's relevant
        set pnts [lindex [lindex $gHitList 0] 1]
        set p1   [lindex $pnts 0]
        ::userMoveCursor $p1


	if {$count > 1 } {

	    userStatus "More than one match found. Hit $gKeyList(rotateselections) to cycle through the $count options."

	}
    }
}


#############################################################################
## Procedure:  userOpenWaves
#############################################################################
# open nWave if possible
################################################################################
#  Parameters:
#    Arg = wvCursorTimeChange
#    Arg = 1425
#    Arg = Tb.TOP.WILL_FIRE_RL_connect_Get_Put_0
#    Arg = /home/sallen/bsc/testsuite/demos/mesa1/mesa.fsdb
proc debCallback {args} {
    global gCurrTime gCurrSignal
    
    catch {
        set reason [lindex $args 0]
        foreach i $args { debug 2 "  Arg = $i" }
	
        if {$reason == "wvCloseWindow"} {
            clickQuit
	    
        } elseif {$reason == "wvCursorTimeChange"} {
            set gCurrTime   [lindex $args 1]
            set gCurrSignal [lindex $args 2]
	    
        } elseif {$reason == "wvDblClkWaveForm"} {
            set gCurrSignal [lindex $args 1]
	    
        } elseif {$reason == "UDR_GOTO_BSV"} {
            ::debShowSignal 1
	    
        } elseif {$reason == "UDR_GOTO_V"} {
            ::debShowSignal 0
	    
        } else {
            # drop it on the floor?
            # TODO: if we saved a session, then reload it next time?
        }
    }
}

proc menuOpenWaves {} {

    global gWaveState

    if {![isDebussyRunning]} {

	if {$gWaveState == "nowaves"} { 

	    set gWaveState "allowwaves"

	}

	global gProgressText
	set gProgressText "starting nWave...."
	update idletasks
	puts "Initializing nWave"
	userOpenWaves

    } else {

	bell
	userStatus "The waveform viewer is already running."
	set gMenuDone 1

    }
}

proc userOpenWaves {} {
    global gWidget gTkAppList
    global gWaveState gLicVerdi gDebApp
    global gFsdbFile gVcdFile gDebussyProcName 
    global gLoadedFsdbFile
    
    if {$gWaveState == "nowaves"} { return $gWaveState }
#    if {$gWaveState == "loaded"}  { return $gWaveState }
    
    
    # catch {[exec ps -C snslmgrd]} err
    # if {[regexp lmgrd $err] == 0} {
    # not running lmgrd right now - try to start it up!
    # puts "Status: lmgrd doesn't appear to be running"
    # puts "       : Novas nWave will not be able to run!"
    # puts "       : Please verify that it is running"
    # exit
    # }
    
    if {![isDebussyRunning]} {

        # make sure it isn't 
        set gTkAppList [winfo interp]
	
        # set deb  [exec which debussy]
        if {[catch {exec which nWave}]==0} {
            set nWave  [exec which nWave]
        } else {
            set nWave  {}
        }
        set gDebApp $nWave
	
        if {[file exists $gDebApp]} {
            # TODO - can I verify nWave is not alreay running?

	    if {$gLicVerdi == 1} {
		
		exec $gDebApp -licverdi -tkName $gDebussyProcName &

	    } else {

		exec $gDebApp -tkName $gDebussyProcName &

	    }
	    
            # dialog box to cancel?
            global gnWaveTimeout
            set timeout [expr $gnWaveTimeout * 4]
            for {set i 0} {$i < $timeout} {incr i} {
                after 250
                set gTkAppList [winfo interp]
                if {[lsearch $gTkAppList $gDebussyProcName] >= 0} break
            }
	    
            # if we timed out, then we are done!
            if {[lsearch $gTkAppList $gDebussyProcName] < 0} {
                puts " ERROR: it doesn't seem like nWave started running"
                puts "      : aborting blueview"
                exit
            }
	    
            debug 2 ":userOpenWaves launch $gDebApp!"
            set gWaveState nofsdbyet
	    set gLoadedFsdbFile ""
	    
            # add callbacks needed
            set name [winfo name .]
            #send abc "AddEventCallback $name ::debCallback wvCloseWindow 1"
            #send abc "AddEventCallback $name ::debCallback wvCursorTimeChange 1"
            #send abc "AddEventCallback $name ::debCallback wvDblClkWaveForm   1"

	    #I have no idea why this fails different ways different times.
            catch {send_to_debussy "AddEventCallback $name ::debCallback UDR_GOTO_BSV 1"}
            catch {send_to_debussy "AddEventCallback $name UDR_GOTO_BSV 1"}

	    catch {send_to_debussy "AddEventCallback $name ::debCallback UDR_GOTO_V 1"}
	    catch {send_to_debussy "AddEventCallback $name UDR_GOTO_V 1"}

            debug 0  "$gDebApp loaded - EventCall backs registered for $name"
	    
            ########################################
            # import verilog design based on top level of design
            # global gTreeInfo
            # send abc {debImport -verilog -top "testbench.v" "mkDesign.v"}
	    
            # send abc {debImport \"/home/sallen/bsc/testsuite/bsv.sallen/bug647/mkDesign.v\" 
            #                     \"/home/sallen/bsc/testsuite/bsv.sallen/bug647/testbench.v\" -path 
            #                    {/home/sallen/bsc/testsuite/bsv.sallen/bug647} }
	    
            # send abc {srcHBSelect testbench -win abc}
            # send abc {srcSetScope -win abc \"testbench\" -delim /}
	    
            # send abc \"wvCreateWindow -win abw\"
	    
        } else {
            ::userStatus "$gDebApp not found - disabling wave viewing"
            debug 2 ":userOpenWaves $gDebApp not found - turning off waves"
            set gWaveState nowaves
        }
    }
    
    
    # if we have a gVcdFile and state is "half started" then
    # open it up and make it "loaded"
    
    if {($gWaveState == "nofsdbyet") && ($gVcdFile != "")} {
        # create window waves window
#        debug 1 ":userOpenWaves convert /$gVcdFile/$gFsdbFile"
        if {$gVcdFile != ""} {
            send_to_debussy "wvConvertFile  -o $gFsdbFile $gVcdFile"
        }

	if {$gLoadedFsdbFile == $gFsdbFile} {

	    send_to_debussy "wvReloadFile"

	} else {

	    set gLoadedFsdbFile $gFsdbFile
	    send_to_debussy "wvOpenFile     $gFsdbFile"
	    send_to_debussy "wvZoomAll     "
	}

        set gWaveState "loaded"
    }
    
    return $gWaveState
}

################################################################################
proc extractPathInfo {inst} {
    # ram_inst.ifc.ram_inst_ifc_theififo
    # hier  = [list ram_inst ifc theififo]    ;# bsc path
    # vinst = ram_inst_ifc_theififo
    if {[regexp {\.} $inst]} {
        set fullinst [split $inst {\.}]
        set hier     [lfront $fullinst]       ;# flattened hierachy
        set vinst    [llast  $fullinst]
        # regsub {\.}  $inst {_} vinst
        if {[llength $fullinst] > 1} {
            set sugar [join $hier _]
            regsub ^$sugar\_ $vinst {} binst  ;# as it appears in bsv hierarchy
        } else {
            set binst $vinst
        }
    } else {
        set hier  {}
        set vinst $inst
        set binst $vinst
    }

    return [list $hier $vinst $binst]
}

################################################################################
###
################################################################################

proc set-hierarchy {string} {

    global gHierTree

    regsub -all "\n" $string "" string

    set gHierTree [uplevel $string]

    set gHierTree [lindex $gHierTree 4]

    set gHierTree [fixHierTree $gHierTree]

    return $gHierTree

}

proc bsv-inst     { inst def {lst _} } {

    regsub  {.*\.} $def "" suffix

    return [list b $inst $inst $suffix $lst] 

}

proc verilog-inst { inst def {lst _} } { 

    regsub  {.*\.} $def "" suffix
 
    return [list v $inst $inst $suffix $lst] 

}

proc fixHierTree {h {prefix _}} {

    set res {}

    foreach i $h {
        set tp [lindex $i 0]
        set bi [lindex $i 1]
        set vi [lindex $i 2]
        set def [lindex $i 3]
        set ls [lindex $i 4]
        
        if {$prefix != "_"} {
            set vi "$prefix$vi"
        }
        
        if {$ls != "_"} {
            set ls [fixHierTree $ls "$vi\."]
        }

        lappend res [list $tp $bi $vi $def $ls]
    }
    return $res
}

################################################################################
###
################################################################################

proc add-bsv-id-info {lmod inst vfile vpos_row vpos_column bfile bpos_row bpos_column typ {extra ""}} {

    global gProgess
    global gInfoNet
    global gHierarchy gVerilogDefs gVerilogLibDefs
    global buildVersionOk gHierTree gMod gFirst
    global gInfoDefof
    global gInfoInstof gInfoInstofList
    global gLibraryModule

    if {$vpos_row == -1} {

	set vpos "-1.-1"

    } else {

	set vpos [join [list $vpos_row [expr $vpos_column - 1]] .]

    }

    if {$bpos_row == -1} {

	set bpos "-1.-1"

    } else {

	set bpos [join [list $bpos_row [expr $bpos_column - 1]] .]

    }

    if {([regexp {\.bsv$} $bfile] == 0) && ($typ != "FLATTENED_DEFOF")} return

    if {$gMod != $lmod} {

	set gMod $lmod

    }

    if {$typ == "DEFOF"} {
	
	############################################################
	global gInfoDefof
	set gInfoDefof($lmod:vfile)     $vfile   ;# assoicated verilog file
	set gInfoDefof($lmod:vpos)      $vpos    ;# assoicated verilog position
	set gInfoDefof($lmod:bfile)     $bfile   ;# assoicated bsv file
	set gInfoDefof($lmod:bpos)      $bpos    ;# assoicated bsv position
	set gInfoDefof($lmod:type)      $typ
	set gInfoDefof($lmod:info)      $gMod     ;# info file for this module
	
	lappend gVerilogDefs $inst
	
	set tmp(signal)  ""
	set tmp(vfile)  $vfile
	set tmp(vpos)   $vpos
	set tmp(bfile)  $bfile
	set tmp(bpos)   $bpos
	set tmp(type)   $typ
	
	
	if { $vpos == "-1.-1" } {return}
	
	
	if { $gFirst } {
	    
	    #clear out old data (if reloading)
	    set gFirst 0
	    set gInfoNet($lmod) {}
	    
	}
	
	lappend gInfoNet($lmod) [array get tmp]
	
    } elseif {$typ == "FLATTENED_DEFOF"} {


		if {[isLibraryFile $bfile]} {

		    lappend gVerilogDefs $inst
		    lappend gVerilogLibDefs $inst

		} else {

		    set infomod $extra
		    set fmod    $infomod

		    global gInfoDefof

		    # there is no verilog file, it is flattened into the parent!
		    # set gInfoDefof($fmod:vfile)   $lmod.v
		    # set gInfoDefof($fmod:vpos)    1.1
		    set gInfoDefof($fmod:bfile)     $bfile
		    set gInfoDefof($fmod:bpos)      $bpos
		    set gInfoDefof($fmod:type)      $typ
		    set gInfoDefof($fmod:info)      $lmod
		}

	    } elseif {$typ == "BINSTOF"} {

                global gProgress
                incr gProgress

                set infoinst $extra

                regsub {/} $inst {.} key

                # examples are "ram_inst"
                # examples are "ram_inst.theififo"
                set i [extractPathInfo $inst]
                set hier     [lindex $i 0]
                set vinst    [lindex $i 1]
                set binst    [lindex $i 2]


#                regsub {\.} $inst {_} key

                set tmp(signal)  ""
                set tmp(vfile)  $vfile
                set tmp(vpos)   $vpos
                set tmp(bfile)  $bfile
                set tmp(bpos)   $bpos
                set tmp(hier)   $hier
                set tmp(binst)  $binst
                set tmp(vinst)  $vinst
                set tmp(info)   $infoinst
		set tmp(type)   $typ
		set tmp(key)    $key

                global gInfoInstof gInfoInstofList
                set gInfoInstof($lmod:$key) [array get tmp]
                lappend gInfoInstofList($lmod) $key

                #printf "\n\n"
                #printf "Add BINSTOF: $lmod:$key => $infoinst $vfile $vpos $bfile $bpos {$hier}{$binst}"

#		breakf

		if { $vpos == "-1.-1" } {return }


		if { $gFirst } {

		    #clear out old data (if reloading)
		    set gFirst 0
		    set gInfoNet($lmod) {}
			
		}

		lappend gInfoNet($lmod) [array get tmp]
		
            } elseif {$typ == "INSTOF"} {
                ############################################################
                global gProgress
                incr gProgress

                set infoinst $extra

                # examples are "ram_inst"
                # examples are "ram_inst.theififo"
                set i [extractPathInfo $inst]
                set hier     [lindex $i 0]
                set vinst    [lindex $i 1]
                set binst    [lindex $i 2]

#                regsub {/} $vinst {.} key
#                regsub {\.} $inst {_} key
                regsub -all {\.} $inst {_} key

                # if module is a library element, then skip it
                #  all others should appear in hierachy ?
                #  even if there is no .info file?
                global gLibraryModule
                if {[info exists gLibraryModule($lmod)]} {
                    debug 1 "Drop INSTOF: $lmod:$inst $info (library element)"
                    return
                }

                # if no hierachy, module name is unique enough
                # if hierachy then watch out 
                # vinst is unique by definition
                if {[info exists infoInstof($key)]} {
                    set mess "ERROR: overlapping data structure: infoInstof($key) in $mod"
                    debug 1 $mess
                    puts    $mess
		    return
                }

                set tmp(signal)  ""
                set tmp(vfile)  $vfile
                set tmp(vpos)   $vpos
                set tmp(bfile)  $bfile
                set tmp(bpos)   $bpos
                set tmp(hier)   $hier
                set tmp(binst)  $binst
                set tmp(vinst)  $vinst
                set tmp(info)   $infoinst
		set tmp(type)   $typ

                global gInfoInstof gInfoInstofList
                set gInfoInstof($lmod:$key) [array get tmp]
                set gInfoInstof($bfile:$bpos) [array get tmp]
                set gInfoInstof($lmod:$key:$bfile:$bpos) [array get tmp]
                lappend gInfoInstofList($lmod) $key

#                printf "Add INSTOF: $lmod:$key => $infoinst $vfile $vpos $bfile $bpos {$hier}{$binst}\n"

		if { $vpos == "-1.-1" } {return }


		if { $gFirst } {

		    #clear out old data (if reloading)
		    set gFirst 0
		    set gInfoNet($lmod) {}
			
		}

		lappend gInfoNet($lmod) [array get tmp]
		
            } elseif {($typ == "NET") || ($typ == "ASSIGN")} {
                ############################################################
#                if {($inst != "UNNAMED") && ([regexp {\.bsv$} $bfile])} 
                if {([regexp {\.bsv$} $bfile])} {
                    set f [split $inst {\.}]
                    set tmp(signal) [llast $f]
                    set tmp(hier)   [lfront $f]
                    set tmp(vfile)  $vfile
                    set tmp(vpos)   $vpos
                    set tmp(bfile)  $bfile
                    set tmp(bpos)   $bpos
                    set tmp(type)   $typ

		    if {$tmp(signal) == "UNNAMED"} {

			set tmp(type)   "UNNET"

		    }

		    if {[regexp {^\$} $tmp(signal)]} {

			set tmp(type)   "UNNET"

		    }

                    # don't redo this for every instance - too much data
                    #   and it's always the same anyway?

		    if { $gFirst } {

			#clear out old data (if reloading)
			set gFirst 0
			set gInfoNet($lmod) {}
			
		    }

		    lappend gInfoNet($lmod) [array get tmp]

                }
		
            } elseif {$typ != "IGNORE"} {
                debug 6 "unknown type of add-bsv-id-info called with \"$typ\""
            }
}

################################################################################
###
################################################################################

proc userReadInfoFile {infofile} {

    global gProgess
    global gInfoNet
    global gHierarchy gVerilogDefs gVerilogLibDefs
    global buildVersionOk gHierTree gMod gFirst
    global gInfoDefof
    global gInfoInstof gInfoInstofList
    global gLibraryModule

    set buildVersionOk 0

    regsub {\.info$} $infofile {} gMod

    set gFirst 1

    set found [userFindFile $infofile]

    if {$found != ""} {

	set type [getInfoFileType $found]

	if { $type == "EMACS" } {

	    return 0

	}
	
	source $found

    }

    catch {set gHierarchy($gMod) [refineHierTree $gHierTree]}

    return 1

}

################################################################################
###
################################################################################

proc getInfoFileType {infofile} {

    if { ![file exists $infofile] } {

	return "NONE"

    }

    set emacs 0

    catch {[set emacs [exec grep -l -E {Emacs\-Lisp} $infofile]]}

    if {$emacs != 0} {

	return "EMACS"

    } else {

	return "TCL"

    }
}

################################################################################
###
################################################################################

proc userReadInfoFileOrig {infofile} {
    global gProgess
    global gInfoNet
    global gHierarchy gVerilogDefs gVerilogLibDefs

    set buildVersionOk 0

    # suck all the information into internal structures so we can do stuff with it
    set ts [clock clicks]
    set fl [userFindAndOpen $infofile]
    set lines [split [read $fl] \n]
    close $fl
    set tm [expr [clock clicks] - $ts]; 
    debug 0 "  file read time = $tm"

    set first 1
    
    regsub {\.info$} $infofile {} mod
    
    ################################################################################
    # collapse each line into a matching parenthesis line
    foreach line $lines {
        if {[info exists nline]} {
            set nline [concat $nline $line]
            set lp [regexp -all {\(} $nline]
            set rp [regexp -all {\)} $nline]
            if {$lp == $rp} {
                lappend newlines $nline
                unset nline
            }
            
        } elseif  {[regexp {\(bsv-inst} $line]} {
            set nline $line
            set lp [regexp -all {\(} $nline]
            set rp [regexp -all {\)} $nline]
            if {$lp == $rp} {
                lappend newlines $nline
                unset nline
            }
            
        } else {
            
            lappend newlines $line
        }
    }
    unset lines

    set hierTree {{v regeAQ regeAQ _}}
    
    ################################################################################
    set ts [clock clicks]
    foreach line $newlines {
	if {[regexp {^\(build-version} $line]} {

            regsub -all {\(} $line {[} line
            regsub -all {\)} $line {]} line
            regsub -all {\"} $line {} line

	    eval "set buildVersionOk $line"

	} elseif {[regexp {^\(bluespec-dir} $line]} {

            regsub -all {\(} $line {} line
            regsub -all {\)} $line {} line
            regsub -all {\"} $line {} line

	    eval $line

	} elseif {[regexp {^\(bsv-inst} $line]} {

	    if { $buildVersionOk == 0 } {

		break
	    }

            ############################################################
            # already flattened out
            # turn into a tcl script

	    set foo $line

            regsub -all {\(} $line {[} line
            regsub -all {\)} $line {]} line
            regsub -all {\"} $line {} line
            
            debug 1 "Eval line: $line"
            
            eval "set hierTree $line"

	    breakf

	    set hierTree [lindex $hierTree 4]

	    breakf

            set hierTree [fixHierTree $hierTree]

	    breakf

            debug 1 "New HierTree: $hierTree"

        } elseif {[regexp {^\(add\-bsv\-id\-info} $line]} {
            ############################################################
            # this should execute [add-bsv-id-inst ...]

	    set items [info_line_to_list $line]

            # 0 os add-bsv-id-info
            set lmod  [lindex $items 1]
            set inst  [lindex $items 2]
            set vfile [lindex $items 3]
	    
            if {[regexp {\.v$} $vfile] == 0} {
                set vfile $lmod.v
                # debug 1 "WARNING: no .v file found, set to $vfile"
            }

            if {[lindex $items 4] != -1} {
                set vpos [join [list [lindex $items 4] [expr [lindex $items 5] - 1]] .]
            } else {
                set vpos "-1.-1"
            }
	    
            set bfile [lindex $items 6]
	    
            if {[lindex $items 7] != -1} {
                set bpos [join [list [lindex $items 7] [expr [lindex $items 8] - 1]] .]
            } else {
                set bpos "-1.-1"
            }
	    
            set typ   [lindex $items 9]
	    
#            regsub {^.*/} $vfile {} vfile
	    
            # skip any reference to .bs or other file
            #  ONLY local .bsv files?
            if {([regexp {\.bsv$} $bfile] == 0) && ($typ != "FLATTENED_DEFOF")} continue
            if {$mod != $lmod} {

		set mod $lmod


	    }


            # use hierachy tree for this module
            #
            # ver -> .info file may exist (if so expand)
            #        INSTOF must exist
            #
            # bsv -> no .info file
            #        flattened_defof will exist!
            #        INSTOF must exist

            if {$typ == "DEFOF"} {

                ############################################################
                debug 5 "Add $typ gInfoDefof($lmod) $vfile $vpos $bfile $bpos $mod"
                global gInfoDefof
                set gInfoDefof($lmod:vfile)     $vfile   ;# assoicated verilog file
                set gInfoDefof($lmod:vpos)      $vpos    ;# assoicated verilog position
                set gInfoDefof($lmod:bfile)     $bfile   ;# assoicated bsv file
                set gInfoDefof($lmod:bpos)      $bpos    ;# assoicated bsv position
                set gInfoDefof($lmod:type)      $typ
                set gInfoDefof($lmod:info)      $mod     ;# info file for this module

		lappend gVerilogDefs $inst

                set tmp(signal)  ""
                set tmp(vfile)  $vfile
                set tmp(vpos)   $vpos
                set tmp(bfile)  $bfile
                set tmp(bpos)   $bpos
		set tmp(type)   $typ


		if { $vpos == "-1.-1" } {continue }


		if { $first } {

		    #clear out old data (if reloading)
		    set first 0
		    set gInfoNet($lmod) {}
			
		}

		lappend gInfoNet($lmod) [array get tmp]

            } elseif {$typ == "FLATTENED_DEFOF"} {
                ############################################################

		if {[isLibraryFile $bfile]} {

		    lappend gVerilogDefs $inst
		    lappend gVerilogLibDefs $inst

		} else {

		    set infomod [lindex $items 10]
		    set fmod    $infomod

		    global gInfoDefof

		    # there is no verilog file, it is flattened into the parent!
		    # set gInfoDefof($fmod:vfile)   $lmod.v
		    # set gInfoDefof($fmod:vpos)    1.1
		    set gInfoDefof($fmod:bfile)     $bfile
		    set gInfoDefof($fmod:bpos)      $bpos
		    set gInfoDefof($fmod:type)      $typ
		    set gInfoDefof($fmod:info)      $lmod
		}

	    } elseif {$typ == "BINSTOF"} {
                ############################################################
                global gProgress
                incr gProgress

                set infoinst [lindex $items 10]

                regsub {/} $inst {.} key

                # examples are "ram_inst"
                # examples are "ram_inst.theififo"
                set i [extractPathInfo $inst]
                set hier     [lindex $i 0]
                set vinst    [lindex $i 1]
                set binst    [lindex $i 2]


#                regsub {\.} $inst {_} key

                set tmp(signal)  ""
                set tmp(vfile)  $vfile
                set tmp(vpos)   $vpos
                set tmp(bfile)  $bfile
                set tmp(bpos)   $bpos
                set tmp(hier)   $hier
                set tmp(binst)  $binst
                set tmp(vinst)  $vinst
                set tmp(info)   $infoinst
		set tmp(type)   $typ
		set tmp(key)   $key

                global gInfoInstof gInfoInstofList
                set gInfoInstof($lmod:$key) [array get tmp]
                lappend gInfoInstofList($lmod) $key

                #printf "\n\n"
                #printf "Add BINSTOF: $lmod:$key => $infoinst $vfile $vpos $bfile $bpos {$hier}{$binst}"

#		breakf

		if { $vpos == "-1.-1" } {continue }


		if { $first } {

		    #clear out old data (if reloading)
		    set first 0
		    set gInfoNet($lmod) {}
			
		}

		lappend gInfoNet($lmod) [array get tmp]
		
            } elseif {$typ == "INSTOF"} {
                ############################################################
                global gProgress
                incr gProgress

                set infoinst [lindex $items 10]

                # examples are "ram_inst"
                # examples are "ram_inst.theififo"
                set i [extractPathInfo $inst]
                set hier     [lindex $i 0]
                set vinst    [lindex $i 1]
                set binst    [lindex $i 2]

#                regsub {/} $vinst {.} key
#                regsub {\.} $inst {_} key
                regsub -all {\.} $inst {_} key

                # if module is a library element, then skip it
                #  all others should appear in hierachy ?
                #  even if there is no .info file?
                global gLibraryModule
                if {[info exists gLibraryModule($lmod)]} {
                    debug 1 "Drop INSTOF: $lmod:$inst $info (library element)"
                    continue
                }

                # if no hierachy, module name is unique enough
                # if hierachy then watch out 
                # vinst is unique by definition
                if {[info exists infoInstof($key)]} {
                    set mess "ERROR: overlapping data structure: infoInstof($key) in $mod"
                    debug 1 $mess
                    puts    $mess
                    continue
                }

                set tmp(signal)  ""
                set tmp(vfile)  $vfile
                set tmp(vpos)   $vpos
                set tmp(bfile)  $bfile
                set tmp(bpos)   $bpos
                set tmp(hier)   $hier
                set tmp(binst)  $binst
                set tmp(vinst)  $vinst
                set tmp(info)   $infoinst
		set tmp(type)   $typ

                global gInfoInstof gInfoInstofList
                set gInfoInstof($lmod:$key) [array get tmp]
                set gInfoInstof($bfile:$bpos) [array get tmp]
                set gInfoInstof($lmod:$key:$bfile:$bpos) [array get tmp]
                lappend gInfoInstofList($lmod) $key

#                printf "Add INSTOF: $lmod:$key => $infoinst $vfile $vpos $bfile $bpos {$hier}{$binst}\n"

		if { $vpos == "-1.-1" } {continue }


		if { $first } {

		    #clear out old data (if reloading)
		    set first 0
		    set gInfoNet($lmod) {}
			
		}

		lappend gInfoNet($lmod) [array get tmp]
		
            } elseif {($typ == "NET") || ($typ == "ASSIGN")} {
                ############################################################
#                if {($inst != "UNNAMED") && ([regexp {\.bsv$} $bfile])} 
                if {([regexp {\.bsv$} $bfile])} {
                    set f [split $inst {\.}]
                    set tmp(signal) [llast $f]
                    set tmp(hier)   [lfront $f]
                    set tmp(vfile)  $vfile
                    set tmp(vpos)   $vpos
                    set tmp(bfile)  $bfile
                    set tmp(bpos)   $bpos
                    set tmp(type)   $typ

		    if {$tmp(signal) == "UNNAMED"} {

			set tmp(type)   "UNNET"

		    }

		    if {[regexp {^\$} $tmp(signal)]} {

			set tmp(type)   "UNNET"

		    }

                    # don't redo this for every instance - too much data
                    #   and it's always the same anyway?

		    if { $first } {

			#clear out old data (if reloading)
			set first 0
			set gInfoNet($lmod) {}
			
		    }

		    lappend gInfoNet($lmod) [array get tmp]

                }
		
            } elseif {$typ != "IGNORE"} {
                debug 6 "unknown type of add-bsv-id-info called with \"$typ\""
            }
        }
    }

    breakf
    if { $buildVersionOk == 0 } {
	
	return 0

    }
    
    ############################################################
    # now create new hier tree with all the relevant info in it
    # we have a bsv instance name, but I really need a verilog
    # instance name, since that is what will be unique in the end


    catch {set gHierarchy($mod) [refineHierTree $hierTree]}

    ############################################################
    set tm [expr [clock clicks] - $ts];
    debug 0 "  clock \#1 time = $tm"
    
    
    return 1
}

################################################################################
###
################################################################################

proc get_module_for_info_file {filename} {

    set file [userFindAndOpen $filename]
    set lines [split [read $file] \n]
    close $file

    foreach line $lines {

	if {[regexp {^add\-bsv\-id\-info} $line]} {

            set items [info_line_to_list $line]

            set mod  [lindex $items 1]

	    break
	}
    }

    return $mod
}

################################################################################
###
################################################################################

proc get_all_info_files {filename} {

    set file [userFindAndOpen $filename]
    set lines [split [read $file] \n]
    close $file

    set dir [file dir $filename]

    set file_list {}

    foreach line $lines {

	if {[regexp {^add\-bsv\-id\-info} $line] && [regexp {INSTOF} $line]} {

	    set items [info_line_to_list $line]

	    set typ   [lindex $items 9]

	    if {$typ == "INSTOF"} {

		set infofile $dir/

		append infofile [lindex $items 10]

		append infofile ".info"

		if {[file exists $infofile] && ([lsearch $file_list $infofile] < 0)} {

		    set zow [get_all_info_files $infofile]

		    foreach foo $zow {
		    
			if {[file exists $foo] && ([lsearch $file_list $foo] < 0)} {

			    lappend file_list $foo

			}
		    }

		    lappend file_list $infofile
		    
		}
	    }
	}
    }

    return $file_list
}

################################################################################
###
################################################################################

proc info_line_to_list {line} {

    regsub {add\-bsv\-id\-info} $line {\{} items
    regsub {$} $items {\}$} items

    return $items

}

################################################################################
###
################################################################################

proc isLibraryFile {file_name} {

    global gBLUESPECDIR

    ## This is a hack (that only matters when running on an executable
    ## from a build tree (as opposed to a standard release).
    ## It assumes library directories of the form:
    ## bsc/src/lib and bsc/inst/lib

    regsub {/inst/} $gBLUESPECDIR {/src/} gBLUESPECDIR2

    if {[regexp ^$gBLUESPECDIR $file_name] == 1 } {
	
	return 1

    }

    if {[regexp ^$gBLUESPECDIR2 $file_name] == 1 } {
	
	return 1

    }

    if {[file exists $file_name] == 0} {

	return 1

    }

    return 0

}

################################################################################
###
################################################################################

proc refineHierTree {h_list} {

    global gVerilogDefs gVerilogLibDefs

    set res {}

    foreach i $h_list {

        set tp [lindex $i 0]
        set bi [lindex $i 1]
        set vi [lindex $i 2]
        set def [lindex $i 3]
        set ls [lindex $i 4]
        
	if {$ls != "_"} {
	    set ls [refineHierTree $ls]
	}

	if {[lsearch $gVerilogDefs $def] >= 0} {

	    if {[lsearch $gVerilogLibDefs $def] >= 0} {

		lappend res [list v $bi $vi $def _]

	    } else {

		lappend res [list v $bi $vi $def $ls]

	    }

	} else {

	    lappend res [list $tp $bi $vi $def $ls]

	}
    }

    return $res
}

################################################################################
###
################################################################################

proc compiler-version {version} {

    return 0

}

proc build-version {version} {

    global buildVersionOk

    if { $version < 7560 } {

	set buildVersionOk 0

    } {

	set buildVersionOk 1

    }
}

proc bluespec-dir {directory_name} {

    global gBLUESPECDIR

    set gBLUESPECDIR [file_normalize $directory_name]

}

################################################################################
# recurse wave to read and load info files
# rules


#############################################################################
################################################################################
# signal = Tb.TOP.mesa_inst.the_lpm.mif_response_get_avValue_fst
proc debShowSignal {isBsv} {
    global gInst2TreeIndex gTreeInfo gInfoNet gTreeArea
    global gCurrTreeIndex gCurrViewIsBsv gCurrPosBsv gCurrPosV
    
    # get the first selected signal
    set signals [send_to_debussy "wvGetSelectedSignals "]
    if {[info exists signals]==0} {
        userMessageBox Error [list "No signals selected"]
        return
    }
    
    
    set p    [splitOffInstance [lindex $signals 0]] ;# do only the first signal to be safe
    set hier [lindex $p 0]
    set sig  [lindex $p 1]
    
    debug 2 "$hier $sig"
    
    # remove stuff added by debussy for some reason
    regsub {^\\} $sig {} sig
    regsub {\[.*$} $sig {} sig
    
    # make gInstance name to treeIndex entry so we can get BSV or V file
    if {[array names gInst2TreeIndex $hier] == ""} {
        userMessageBox Warning [list \
                                    "signal hierarchy '$hier' not found" \
                                    "signal name is $sig" \
                                   ]
        return
    }
    
    # this is a *VERILOG* path now, not bsv path
    set gCurrTreeIndex $gInst2TreeIndex($hier)
    debug 2 "tree index = $gCurrTreeIndex"
    
    $gTreeArea selection clear
    $gTreeArea selection see $gCurrTreeIndex
    $gTreeArea selection set $gCurrTreeIndex
    debug 2 "treeindex selected $gCurrTreeIndex"
    
    # just because this is the .v file it is in doesn't mean much.
    # it could be a in flattened .bsv file
    
    # search this info file for the real bsv or v file to go to?
    # TODO
    
    set i $gCurrTreeIndex
    
    # get file information from treeInfo - simple enough
    global gInfoDefof
    if {$isBsv==0} {
        set mod [getInfoMod $i]
        set fl $gInfoDefof($mod:vfile)
    } else {
        set mod $gTreeInfo($i:mod)
        set fl $gInfoDefof($mod:bfile)
    }
    
    debug 3 "search for source of $mod"
    # search through netInfo for $signal (get index to assignment first)
    if {[array names gInfoNet $mod] == ""} {
        userMessageBox Error [list "No module $mod found in gInfoNet"]
        return
    }
    
    # reset in case we don't find signal
    set gCurrPosV   "1.0"
    set gCurrPosBsv "1.0"
    set ntype none
    
    set gCurrViewIsBsv $isBsv
    
    ::userUnHilightHitlist
    
    # todo make this a proper hitlist
    set siglen [string length $sig]
    
    ::userClearHitList
    debug 1 "  check for signal $sig"
    foreach f $gInfoNet($mod) {
        array set tmp $f
        debug 5 "  check $tmp(signal)"
        if {$tmp(signal) == $sig} {
            if {$isBsv} {
                ::userAddToHitList $tmp(bpos) $tmp(type) 0       $tmp(bfile)
            } else {
                ::userAddToHitList $tmp(vpos) $tmp(type) $siglen $tmp(vfile)
            }
        }
    }
    
    set retInx [userFindFirstHitListAssign]
    
    global gTreeInfo
    set bpath $gTreeInfo($i:bpath)
    
    # show file anyway, even if we didn't know what to do with it
    # this shouldn't ever happen, but bugs do exist, don't they?
    ::userOpenSource $fl $mod
    
    # if we found something, light it up, and set position
    if {[lindex $retInx 1] != "1.0"} {
        userHilightHitlist
    }
    
    if {$gCurrViewIsBsv == 0} {   ;# move mouse to the right place
        set gCurrPosV $retInx
    } else {
        set gCurrPosBsv $retInx
    }
    userMoveCursor [lindex $retInx 0]
    
}

#############################################################################
proc askToSaveFile {} {
    global gCurrViewIsBsv gTextArea gTextModified
    if {[info exists gCurrViewIsBsv]==0} return
    if {$gCurrViewIsBsv == 0} return
    if {$gTextModified == 1} {
        if {[MessageDlg::create .top.m -icon question -type yesno \
                 -message {File modified. Do you want to save it?}] == 0} {
            menuSaveBsv
        }
    }
}

## Procedure:  userOpenSource
proc userOpenSource {fl mod} {
    global gWidget gTopWindow
    global gTextArea
    global gCurrViewIsBsv gCurrPosBsv gCurrPosV
    global gFileNameLabel
    
    $gTextArea delete 1.0 end
    
    wm title . [file tail $fl]
    set fp [userFindAndOpen $fl]
    if {$fp == 0} {
        return
    }
    $gTextArea insert end [read $fp]
    close $fp
    
    
    userUntabify

    global gTextModified
    set gTextModified 0
    
    userStatus "Opening $fl"

    ### Joydeep : global gFileNameLabel which
    ###           stores the file name label
    ###           has been configured with the
    ###           current file name path.
    $gFileNameLabel configure -text $fl
    
    # we are now looking at the target file type
    if {[regexp {\.(bsv|bs)$} $fl]} {
        ::userLoadColorInfoBsv $fl
        set gCurrViewIsBsv 1
    } else {
        ::userLoadColorInfoV $mod $fl
        set gCurrViewIsBsv 0
    }
    
    # force signal size loading to rehappen
    global gVerilogRef
    if {[info exists gVerilogRef]} { unset gVerilogRef }
    
    # pull this window to the top of the pile!
    # X11 feature!
    # focus $gTextArea
    # send abc "wvLowerWindow -win abc"
    # TODO: autoraise windows, etc?  wrap nWave in a TCL window so we can lower and raise it?

}


proc getCurrentFileView {} {
    global gTreeInfo gCurrTreeIndex gCurrViewIsBsv gInfoDefof    

    if {$gCurrViewIsBsv==0} {
        set mod [getInfoMod $gCurrTreeIndex]
        set thisfile $gInfoDefof($mod:vfile)
    } else {
        set mod $gTreeInfo($gCurrTreeIndex:mod)

	if {[info exists gInfoDefof($mod:bfile)]} {

	    set thisfile $gInfoDefof($mod:bfile)

	} else {

	    set parent_index $gTreeInfo($gCurrTreeIndex:bparent_index)
	    set gCurrTreeIndex $parent_index
	    set mod [getInfoMod $parent_index]
	    set thisfile $gInfoDefof($mod:bfile)

	}
    }
}

proc userHilightHitlist {} {

    
    global gTextArea gHitList gHitListIndex
    
    
    # get filename we are looking
    set thisfile [getCurrentFileView]
    set count 0

    if {[info exists gHitList]} {
        set limit [llength $gHitList]
        debug 3 "$limit items on hitlist"
	
        for {set i 0} {$i < $limit} {incr i} {
            set f [lindex $gHitList $i]
            set ntype   [lindex $f 0]
            set finx    [lindex $f 1]
            set hitfile [lindex $f 2]
	    
            # WHAT ABOUT HITS TO MULTIPLE FILES????
            if {($hitfile != "") && ($thisfile!=$hitfile)} {
                debug 4 "Skip $thisfile != $hitfile"
                continue
            }
	    
            # gHitListIndex should be set to the assign position
            #   probably the most interesting for now
            if {$gHitListIndex == $i} {
                set hilight tagSelection
            } else {
                set hilight tagSelectionSet
            }
	    
            debug 4 "indecies at $finx"
            if {[lindex $finx 0] != "-1.-1"} {
                # if no width, then go until end of characters!
                set st [$gTextArea index [lindex $finx 0]]
                set en [$gTextArea index [lindex $finx 1]]
		
                if {$st == $en} {
                    # BSV file - we have to figure out how much to hilight
                    debug 4 "find end of $st"
                    set en [getEndOfVarOrOther $st]
                }
                debug 4 "highlight from $st -> $en in $hitfile"
                $gTextArea tag add $hilight $st $en
		set count [expr $count + 1]
            }
        }
	
        # move to first item on hitlist
        # set pnts [lindex [lindex $gHitList $gHitListIndex] 1]
        # set p1   [lindex $pnts 0]
        # ::userMoveCursor $p1
    }
    
    return $count
}

proc userUnHilightHitlist {} {    
    global gTextArea gHitList gHitListIndex
    $gTextArea tag remove tagSelection 1.0 end
    $gTextArea tag remove tagSelectionSet 1.0 end
    if {[info exists gHitList]} { unset gHitList }
    set gHitListIndex -1
}


#############################################################################
proc userUntabify {} {
    global gTextArea gTabStops

    set curpos 1.0
    while 1 {
        set hit [$gTextArea search -forward \t $curpos]
        if {$hit == ""} return
	
        # insert spaces instead of tab upto position for tabstop 8
        set x [split $hit {\.}]
        set ln  [lindex $x 0]
        set col [lindex $x 1]
	
        set spaces " "
        for {set i $col} {($i % $gTabStops) != 7} {incr i} {
            set spaces  " $spaces"
        }

        $gTextArea mark set insert $hit     ;# set cursor
        $gTextArea delete "$ln.$col"        ;# remove tab
        $gTextArea insert $hit $spaces      ;# insert spaces
    }
}

proc userComment {} {
    global gTextArea
    
    # get line comments first
    # now get C style comments
    set curpos "1.0"
    while 1 {
        set hit [$gTextArea search -forward -regexp {//} $curpos end]
        if {$hit == ""} break
        set en  [$gTextArea index "$hit lineend"]
        $gTextArea tag add tagComment $hit $en
        set curpos $en
        $gTextArea mark set insert $curpos
    }
    
    # now get C style comments
    set curpos "1.0"
    while 1 {
        set hit [$gTextArea search -forward -regexp {([^/]/\*|^/\*)} $curpos end]
        if {$hit == ""} break
        set en  [$gTextArea search -forward -regexp {\*/} $hit    end]
        $gTextArea tag add tagComment $hit $en
        set curpos $en
        $gTextArea mark set insert $curpos
    }
    
    # now get pragma
    set curpos "1.0"
    while 1 {
        set hit [$gTextArea search -forward -regexp {\(\*} $curpos end]
        if {$hit == ""} break
        set en  [$gTextArea search -forward -regexp {\*\)} $hit    end]
        $gTextArea tag add tagComment $hit $en
        set curpos $en
        $gTextArea mark set insert $curpos
    }
    
}

# search for all instances of seltxt, and apply style as a tag
proc userTag {seltxt style} {
    global gTextArea
    
    set len [string length $seltxt] 
    set curpos "1.0"
    while 1 {
        set hit [$gTextArea search -forward -regexp "\\m$seltxt\\M" $curpos end]
        if {$hit == ""} return
	
        set en [$gTextArea index "$hit + $len chars"]
        $gTextArea tag add $style $hit $en
        set curpos $en
        $gTextArea mark set insert $curpos
    }
}

################################################################################
proc replaceVerilogIndicies {mod} {
    global gConvert gInfoNet
    
    # convert 1.char to line.col format so we can match and all that stuff
    # but only do it once
    if {[array names gConvert $mod] == ""} {
        foreach f $gInfoNet($mod) {
            array set tmp $f
            set vpos $tmp(vpos)
	    
            set inx  [split $vpos .]
            set col  [lindex $inx 1]
            set pnt  [$gTextArea index "1.0 +$col chars"]
            set tmp(vpos) $pnt
	    
            debug 4 "New Vpos = $pnt (was $vpos)"
            lappend newList [array get tmp]
        }
        set gInfoNet($mod) $newList   ;# replace old list!
        set gConvert($mod) 1
    }
}

################################################################################
proc getCharAtIndex {start} {
    global gTextArea
    
    set eol [$gTextArea index "$start lineend"]
    set inf [$gTextArea dump -text $start]
    while 1 {
        set key [lindex $inf 0]
        if {$key == "text"} {
            set val [lindex $inf 1]
            set inx [lindex $inf 2]
            return $val
        }
    }
    return {}
}

proc getStartOfVarOrOther {init} {
    global gTextArea
    
    set tx [getCharAtIndex $init]
    set limit [$gTextArea index "$init linestart"]
    if {[regexp {[\w\d]} $tx]} {
        # if it starts with a A-Za-z0-9 then scan as variable
        set ws [$gTextArea search -backward -regexp {[^\w\d\$\._]} $init $limit]
	
    } else {
        # otherwise it may be == or <= or unknown
        set ws [$gTextArea search -backward -regexp {[^=<\+\!\*\-\&\|\^]} $init $limit]
    }
    
    if {$ws == {}} {
        set ws $limit
    }
    return $ws
}

proc getEndOfVarOrOther {start} {
    global gTextArea
    
    set tx [getCharAtIndex $start]
    set en [$gTextArea index "$start lineend"]
    if {[regexp {[\w\d]} $tx]} {
        # if it starts with a A-Za-z0-9 then scan as variable
        set we [$gTextArea search -forward -regexp {[^\w\d\$\._']} $start $en]
	
    } elseif {[regexp {\$} $tx]} {

        # if it starts with a $ then scan as variable
        set we [$gTextArea search -forward -regexp {[^\w\d\$\._']} $start $en]

    } else { 
        # otherwise it may be == or <= or unknown
        set we [$gTextArea search -forward -regexp {[^=<\+\!\*\-\&\|\^]} $start $en]
    }
    
    if {$we == {}} {
        set we $en
    }
    return $we
}

################################################################################
###
################################################################################

proc userLoadColorInfoBsv {targetfile} {
    global gWidget
    global gInst2TreeIndex gTreeInfo gInfoNet gCurrTreeIndex
    
    set infomod [getInfoMod $gCurrTreeIndex]

    if {[info exists gInfoNet($infomod)]} {

        # get the waves for this file
        foreach f $gInfoNet($infomod) {
            array set tmp $f
            if {$targetfile != $tmp(bfile)} { continue }
            if {$tmp(bpos) != "-1.-1"} {
                set start $tmp(bpos)
		set en [getEndOfVarOrOther $start]

		update_xref_tag $start $en $tmp(type)

	    }
        }
    }
    
    # gColorize keywords, etc
    foreach tag { begin case endcase if else end \
                      for foreach function method endmethod module endmodule \
                      package endpackage \
                      rule endrule action endaction \
                      typedef package endpackage deriving provisos } {
        ::userTag $tag tagKeyword
    }
    
    foreach tag {Reg Bit Integer Int Nat List ListN enum Bool} {
        ::userTag $tag tagDatatype
    }
    
    foreach tag {FIFO FIFOF FIFON Array ArrayFile GetPut Connectable SDRAM \
                     ClientServer CompletionBuffer RAM CGetPut BGetPut} {
        ::userTag $tag tagLibrary
    }
    
    ::userComment

}

################################################################################
###
################################################################################

proc userLoadColorInfoV {mod targetfile} {
    global gWidget
    global gInfoNet
    
    # these are the things we can select, and hopefully print out
    # set point-min [$gTextArea index 0]
    if {[info exists gInfoNet($mod)]} {

        foreach f $gInfoNet($mod) {
            array set tmp $f
            if {$targetfile != $tmp(vfile)} { continue }
            set vpos $tmp(vpos)
            if {$vpos == "-1.-1"} {
                debug 2 "  strange cross ref indexes for verilog file: $vpos"
                debug 2 "    line = $f"
		
            } else {
                set vstart [getStartOfVarOrOther $vpos]  
                set vend [getEndOfVarOrOther $vpos] 

		update_xref_tag $vpos $vend $tmp(type)

	    }
        }
    }
    
    # gColorize keywords, etc
    foreach tag { `define `else `endif `ifdef `include `timescale
        `undef always and assign begin buf bufif0 bufif1
        case casex casez cmos default defparam disable else end
        endcase endfunction endgenerate endmodule endprimitive
        endspecify endtable endtask event for force forever
        fork function generate if initial integer
        join macromodule makefile module nand negedge nmos nor
        not notif0 notif1 or parameter pmos posedge
        primitive pulldown pullup rcmos real realtime 
        repeat rnmos rpmos rtran rtranif0 rtranif1 signed
        specify supply supply0 supply1 table task time tran
        tranif0 tranif1 tri tri0 tri1 triand trior trireg
        vectored wait wand while wor xnor xor} {
	
        ::userTag $tag tagKeyword
    }
    
    foreach tag {reg wire input output inout} {
        ::userTag $tag tagDatatype
    }
    
    ::userComment
}

################################################################################
###
################################################################################

proc update_xref_tag {start end type} {

    global gTextArea 

    set tags [$gTextArea tag names [$gTextArea index $start]]

    if { $tags == "tagWaveable" } { return }

    if { $type == "UNNET" } {

	$gTextArea tag add tagXRef $start $end

    } elseif { $type == "INSTOF" } {

	$gTextArea tag add tagXRef $start $end

    } elseif { $type == "DEFOF" } {

	$gTextArea tag add tagXRef $start $end

    } else {

	$gTextArea tag add tagWaveable $start $end

    }
}

################################################################################
###
################################################################################
# this routine is used to determine the begin and end of a waveable signal 
#   (though it could be used for any tag ;)
proc getTagRange {t tag mark} {
    set range [$t tag prevrange $tag $mark]
    set str   [lindex $range 0]
    set end   [lindex $range 1]
    
    if {[llength $range] == 0 || [$t compare $end < $mark]} {
        set range [$t tag nextrange $tag $mark]
        if {[llength $range] == 0 || [$t compare $mark < [lindex $range 0]]} { return }
    }

    return $range
}

################################################################################
################################################################################
proc getSelection {{type "tagWaveable"} } {


    global gTextArea gCurrentSelection
    
    # try for selection first (no sure why ;)
    catch {set start [$gTextArea index sel.first]}
    catch {set end   [$gTextArea index sel.last]}

    if {[info exists start]} {
        set gCurrentSelection [list $start $end]
        debug 1 "selected $gCurrentSelection from select cursor"
        return $gCurrentSelection
    }
    
    # use insert cursor as start & end position
    set start [$gTextArea index insert]
    
    # if this is inside a waveable signal (check attributes?)
    #   then go to start of waveable and use that
    #   else just return pointer
    
    set tags [$gTextArea tag names $start]
    
    set char [getCharAtIndex $start]

    if {($tags == {}) &&
	(($char == " ") || 
	 ($char == ":") || 
	 ($char == "\n") || 
	 ($char == "\t") || 
	 ($char == "(") ||
	 ($char == "\[")) } {

	set tags [$gTextArea tag names [$gTextArea index "$start - 1 char"]]

    }
    
    if {[info exists tags] && ([lsearch $tags $type] >= 0)} {
        # back up to start of word where waveable attribute starts
        # goto next, then back to previous since I'm not sure
        #   exactly how to get this range without a gross search

        set gCurrentSelection [getTagRange $gTextArea $type $start]
        
        # debug 1 "selected [$gCurrentSelection] from insert cursor (start was $start)"
        return $gCurrentSelection
    }
    
    # if no word here (i.e. a space) then sol
    return {}
}

################################################################################
proc clickSwitchToRelatedView { } {
    global gMenuDone gTextArea
    global gCurrViewIsBsv gCurrPosV gCurrPosBsv gCurrTreeIndex
    global gKeyList
    
    
    # get selection

    set range [::getSelection tagXRef]

    if {[llength $range] == 0} {

	set range [::getSelection tagWaveable]

    }

    if {[llength $range] == 0} {
	bell  
	userStatus "Notice: The cursor is not inside a tagged region."
#        userMessageBox Error [list " no range selected "]
        return
    } else {

#        userMessageBox Info [list [format "CC range is %s." $range]]
    }
    
    userUnHilightHitlist
#    debug 10 "Current positions are $gCurrPosV (v) and $gCurrPosBsv (bsv)"
    
    # if we are looking at something in BSV, then
    # find driver in .v (& vice versa)

    set err 1
    if {$gCurrViewIsBsv==1} { 
        set gCurrPosBsv $range
        debug 0 "clickSwitchToRelatedView $gCurrViewIsBsv $gCurrPosBsv"
        set tmp [userFindMatchingIndex $gCurrPosBsv $gCurrTreeIndex]
        if {[lindex $tmp 1] != "1.0"} {
            # there is a hit list of matching positions
            #  they may not even be in the same file!
            set gCurrPosV  $tmp
            set gCurrViewIsBsv 0
            set inx [lindex $gCurrPosV 0]
            set err 0
            set treeinx [lindex $tmp 2]
            set file [lindex $tmp 3]
        }
    } else {
        set gCurrPosV $range
        debug 0 "clickSwitchToRelatedView $gCurrViewIsBsv $gCurrPosV"
        set tmp [userFindMatchingIndex $gCurrPosV $gCurrTreeIndex]
        if {[lindex $tmp 1] != "1.0"} {
            set gCurrPosBsv $tmp
            set gCurrViewIsBsv 1 
            set inx [lindex $gCurrPosBsv 0]
            set err 0
            set treeinx [lindex $tmp 2]
            set file [lindex $tmp 3]
        }
    }

    if {$err == 0} {
        userLoadView $treeinx $file ;# load target file
        set count [userHilightHitlist]          ;# color everything that's relevant
        userMoveCursor $inx         ;# leave the cursor in the right place

	if {$count > 1 } {

	    userStatus "More than one correspondence point found. Hit $gKeyList(rotateselections) to cycle through the $count options."

	}
	
    } else {
	bell  
	userStatus "Notice: The cursor is not inside a tagged region."

    }
    
    set gMenuDone 1
}

################################################################################
proc userGetLogo {} {
    return {
        R0lGODlhjAArAIQAABMJRf///05HdHp1l4mEojEoXCIYUcTC0bWyxWxli+Lg6FxVftPR3fHx
        9JeTrqejuj83aP//////////////////////////////////////////////////////////
        /yH+FUNyZWF0ZWQgd2l0aCBUaGUgR0lNUAAsAAAAAIwAKwAABf4gII5kaZ5oqq5s675wLM90
        bd94ru987//AoHBILBqPyKRyyWwCEyTIyGEKAKwAgkCEJQQCh4BA3IA6z7+tSE0wEwptAgFN
        V0lFBdJ8LTqM/FeAYmBfBAh1iCt5fAICcACACWFhY32FYomZLQUGW2oCBAcOBxCAoweSEIcx
        X19qVa1YdIsoi7RQr5AjCVpWvl5cY2NhAXuYMLG5JLGymj8Fn52dEI2LjqytyiPMziPafSN7
        e1dZWlzkgYPDl8LNLMko3N1BAtTSn9D3Bi/JY18D22KVk6MGjpxxAAwQC4BgX4kEClopWBDu
        YMKIDAFsoeVAIsVdABJAYNDqgAJdBP5lFftCaRAXL+0a8GtFUh63WHv8fRmBkRmDZcxacckW
        9FbQLw6HHi3hSSOoNiLexNEiFY6ZFkdj/bzZKqdAAA2yBpB5RexJsUIBLEDrEK0VNX7iegET
        jNDKdoCwui0rFOcagToD7DMAD14BgUEXfoES64GIsF9kzv3iuOYXao7mAJrbcl3dYTMjixiQ
        jGuhv0IhB/gpomNfZg/uKN05u0FgoDvl8c3IqanGUKNKlRrVR7io0Md2T87dFTXzvZaZndxN
        dncA12gDOw6pSwSvSr+wqLMLc1Ded9kCXkLcXCP7vZDE7p5uXXHW5QhhCLiHOd/+eshttxtp
        7wWw3QMFsv6mQgHYtQKBbsws5x0sX9A3iYEpbcNFZ+KVB5oLzEhh2nOFNGgFAl8BgMABLCJw
        22yBBaCAAKpdN+AaLLIIQE+ChRQLQCnU89801azRyTfx7DViVrMdlUcsM/Jo3VIG1BjUSYSh
        FZUa4RGC12lXjCEgemI9CVhQNYrwYFYHiuWYW1dp6Z1Yd/Sm0RrAFScKi6mQch6ZvsSyiDwJ
        aLUbCfYpkBQAa8bSwKCxNCjbnMxcNUJ0q2Xh1CObsVQXMR6OOY9eadEwKR7hbJqXIBy+NEh+
        owLqDj2pZsELHI9kAUqusSI3Ky3ecDfCLXdyl1eG4X1GnhiW9spCjjo6MaSQjj4g6awNwCJ5
        7DK1+hIVS+OBeu24eDRiz38JAEnuLMFqxCsqLAZHgAGLrmvvvfjmq+++/Pbr778AByzwwImE
        AAA7
    }
}

proc textCut {} {
    global gTextArea gReadOnly gCurrViewIsBsv
    if {($gReadOnly==0) && ($gCurrViewIsBsv==1)} {

        tk_textCut $gTextArea

    } else {
	bell
        userStatus "Cut not allowed in verilog files or readonly bsv file"
    }
}

proc textCopy {} {
    global gTextArea
    tk_textCopy $gTextArea
}

proc textPaste {} {
    global gTextArea gReadOnly gCurrViewIsBsv
    if {($gReadOnly==0) && ($gCurrViewIsBsv==1)} {

        tk_textPaste $gTextArea

    } else {

	bell
        userStatus "Paste not allowed in verilog files or readonly bsv file"
    }
}

proc selectAll {} {
    global gCurrentSelection gTextArea
    $gTextArea tag remove sel 1.0 end
    $gTextArea tag add    sel 1.0 end
    set gCurrentSelection [$gTextArea tag ranges sel]
}


proc unSelectAll {} {
    global gTextArea
    $gTextArea tag remove sel 1.0 end

}

################################################################################
################################################################################
################################################################################
################################################################################
# debug / test routines
# dump snapshot of current state
# 
proc dumpStack {} {
    global errorInfo
    set callStack [list $errorInfo]
    for {set i [info level]} {$i >= 0} {set i [expr $i - 1]} {
        lappend callStack "invoked from within"
        lappend callStack [info level $i]
    }
    return [join $callStack "\n"]
}

proc dumpAll {} {
    global gCurrentSelection gTextArea
    global gLogDir gTreeMaxIndex gTreeInfo
    global gSnaplog gVcdFile gDumpLog

    if {[info exists gDumpLog] == 0} {
        set gDumpLog ".blueview/snap.log"
        debug 1 "gDumpLog not set, dumpAll to $gDumpLog"
    }

    # environment stuff first
    catch {eval [list exec printenv | sort > $gDumpLog]}

    # versions of various things we use
    catch {eval [list exec nWave -h >> $gDumpLog]}

    ############################################################
    set gSnaplog [open $gDumpLog a]
    if {$gSnaplog == 0} {
        puts "ERROR: can't create snap shot file $gDumpLog"
        return
    }

    # dump tree view
    puts $gSnaplog "############################################################"
    puts $gSnaplog "** TREE VIEW **"
    foreach item [lsort [array names gTreeInfo]] {
        puts $gSnaplog "  $item = $gTreeInfo($item)"
    }

    puts $gSnaplog "############################################################"
    puts $gSnaplog "** File Info **"
    # for each entry in tree
    for {set i 0} {$i < $gTreeMaxIndex} {incr i} {
        puts $gSnaplog "**tree $i"

        clickTreeSelectAction defof bsv $i

        # dump all the tags
        foreach tag [lsort [$gTextArea tag names]] {
            puts $gSnaplog "tag = $tag"
            foreach r [lsort [$gTextArea tag ranges $tag]] {
                puts $gSnaplog "$r"
            }
        }

        # dump waves to log file
        selectAll

        catch { clickShowSignals }
    }
    
    puts $gSnaplog "** TEST DONE **"
    close $gSnaplog
    unset -nocomplain gSnaplog

    ############################################################
    # tar it all up and gzip it

    catch {exec rm -f .blueview/DumpSnapshot.tgz}

    if {[catch {exec which blueview.tcl}]==0} {
        set f [exec which blueview.tcl]
        catch {exec cp $f .blueview}
    }

    if {[catch {exec which blueview}]==0} {
        set f [exec which blueview]
        catch {exec cp $f .blueview}
    }

    set cmd [list exec tar zcvf .blueview/DumpSnapshot.tgz]
    lappend cmd nWaveLog/nWave.cmd 

    set cmd [concat $cmd [glob .blueview/*]]
    set cmd [concat $cmd [glob *.info]]
    set cmd [concat $cmd [glob *.bsv]]
    set cmd [concat $cmd [glob *.v]]

    if {[info exists gVcdFile] && [file exists $gVcdFile]} {
        lappend cmd $gVcdFile
    }

    # collect it all up for my debugging pleasure
    catch [eval $cmd]
}


#############################################################################
#############################################################################
# open little splash screen when starting up
#    mainly because reading all the .info files can take a while
proc openSplashScreen { mess } {
    global gProgress gProgressText gLibraryModule
    
    # calculate increment as number of lines in .info files
    # - .info file only gets read once
    # - they can be big
    # - they don't necessarily read INSTOF lines


    
    set tmp ".tmp[pid]"
    if { [catch { \
                      eval [list exec grep INSTOF] [glob *.info] | wc -l > $tmp ;\
                      set hits  [lindex [readLines $tmp] 0] ;\
                      file delete -force $tmp \
                  } ]} {
        set hits 1
    }

    debug 5 "progress bar increments = $hits"
    
    # scan [exec ls -al | grep .info | wc] "%d %d %d"  ln ch x
    set gProgress 0
    set gProgressText "Loading library elements"
    ProgressDlg::create .top.m -maximum $hits -variable gProgress -textvariable gProgressText
    update idletasks
    
    global env
    #########################################################
    if {[info exists env(BLUESPECDIR)] == 0} {
        puts "ERROR: environment variable BLUESPECDIR not set"
        exit
    }

    global gIgnoreLibraryModules
    if {[info exists gIgnoreLibraryModules]} {
        set libdir [file join $env(BLUESPECDIR) Verilog]
        catch {eval [list exec ls] [glob $libdir/*.v] > $tmp}
        
        set libdir [file join $env(BLUESPECDIR) Prelude]
        catch {eval [list exec ls] [glob $libdir/*.bi] >> $tmp}
        
        set libdir [file join $env(BLUESPECDIR) Libraries]
        catch {eval [list exec ls] [glob $libdir/*.bi] >> $tmp}

	exec touch $tmp
        
        # read and split lines up
        set lines [readLines $tmp] 

        global gLibraryModule
        foreach line $lines {
            if {[regexp {([\w\d_]+).v$} $line mn m1]} {
#                set gLibraryModule($m1) 1
#                printf "Library Element: $m1"
                
            } elseif {[regexp {([\w\d_]+).bi$} $line mn m1]} {
#                set gLibraryModule(mk$m1) 1
#                printf "Library Element: mk$m1"
            }
        }
    }
    
    ############################################################
    file delete -force $tmp
    set gProgressText "Initializing data structures"
    update idletasks
}

proc closeSplashScreen {} {
    destroy .top.m
}



################################################################################
###
################################################################################

proc clickTreeSelectAction { {type defof} {target bsv} {inx -1} } {

    
    global gTreeInfo gCurrTreeIndex gTreeArea gCurrViewIsBsv gTextArea
    global gInfoDefof gInfoInstof

    set index_orig $gCurrTreeIndex

    # get index into tree info from node number, since that's
    # what we index *every* thing by

    if {$inx < 0} {
        # normal operation
        set i [$gTreeArea selection get]
    } else {
        # debug - program can select
        set i $inx
    }

    if { $i == {} } {

	bell
	userStatus "No tree instance is selected."
	return

    }
    
    ::askToSaveFile
    
    set gCurrTreeIndex $i
    set mod  $gTreeInfo($i:mod)
    set bpath $gTreeInfo($i:bpath)
    set inst [llast $bpath]
    set info   $gTreeInfo($i:info)
    set binstof_key $info:$inst

    set file ""
    set pos  "1.0"
    set binstof_file ""
    set binstof_pos "1.0"
    set vinstof_file ""
    set vinstof_pos "1.0"

    regsub -all {\.} $binstof_key {_} zow

    if {[info exists gInfoInstof($binstof_key)]} {

	array set binstof_array $gInfoInstof($binstof_key)

	set binstof_pos $binstof_array(bpos)
	set binstof_file $binstof_array(bfile)

	set vinstof_key $zow:$binstof_file:$binstof_pos

	if {[info exists gInfoInstof($vinstof_key)]} {

	    array set vinstof_array $gInfoInstof($vinstof_key)

	    set vinstof_pos $vinstof_array(vpos)
	    set vinstof_file $vinstof_array(vfile)

	}
    }
    
    # start at selection and go up tree until we find something defined

#    breakf
    if {($target == "bsv") && ($type == "defof")} {

        if {[info exists gInfoDefof($mod:bfile)]} {

            set file  $gInfoDefof($mod:bfile)  
            set pos   $gInfoDefof($mod:bpos)
	    set index_destination $gCurrTreeIndex
        
        }
        
    }

    if {($target == "bsv") && ($type == "instof")} {

	set file  $binstof_file
	set pos   $binstof_pos
	set index_destination $gTreeInfo($i:bparent_index)

    }

    if {($target == "verilog") && ($type == "defof")} {

        set mod   [getInfoMod $i]
        set vpath $gTreeInfo($i:vpath)
        set file  $gInfoDefof($mod:vfile)
        set pos   $gInfoDefof($mod:vpos)

	if { $gTreeInfo($i:mod) != $mod } {

	    set index_destination $gTreeInfo($i:vparent_index)
	    bell
	    userStatus "No $target definition information available for module '$gTreeInfo($i:mod)'."
	    userStatus "Jumping to parent $target module '$mod'."

	} else {

	    set index_destination $gCurrTreeIndex

	}
    }

    if {($target == "verilog") && ($type == "instof")} {

	set file  $vinstof_file
	set pos   $vinstof_pos
	set index_destination $gTreeInfo($i:vparent_index)

    }

    if {($file == "") || ($pos == "1.0") || ($pos == "-1.-1")} {
	bell
	set gCurrTreeIndex $index_orig
	if { $type == "defof" } {

	    userStatus "No $target definition information available for module '$mod'"

	} else {

	    userStatus "No $target instance information available for this tree instance."

	}
        return
    }

    set mod_destination $gTreeInfo($index_destination:mod)

    set index_final $gCurrTreeIndex

    set gCurrTreeIndex $index_destination

    ##################################################
    if {[userFindFileExists $file]} { 
        ::userOpenSource $file $mod_destination
        set endpos [getEndOfVarOrOther $pos]

	global gHitList gHitListIndex

	userClearHitList 
	lappend gHitList [list "x" [list $pos $endpos] $file]
	incr gHitListIndex
	
        global gTextArea gWaveState
	#only tag as mod select iof not already tagged
	if {[$gTextArea tag names [$gTextArea index $pos]] == {}} {
	 
	    $gTextArea tag add tagSelection $pos $endpos

	} else {

	    $gTextArea tag add tagSelection $pos $endpos

	}
	#move focus to text area
        userMoveCursor $pos
	#turn off highlighting in tree
	$gTreeArea selection toggle [$gTreeArea selection get]
    } else {

    }

#    set gCurrTreeIndex $index_final
}


#################################
# debug and regress routines
proc dumpOpen {} {
    # create dump file, delete old one
}

proc dumpState {} {
    # dump state of treeArea
    # dump state of textArea
    # dump state of statusArea
}

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base} {

    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $top passive
    wm geometry $top 1x1+0+0; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm withdraw $top
    wm title $top "vtcl.tcl"
    bindtags $top "$top Vtcl.tcl all"
    gVTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "gVTcl:FireEvent $top <<DeleteWindow>>"
    
    ###################
    # SETTING GEOMETRY
    ###################
    
    gVTcl:FireEvent $base <<Ready>>
}

proc vTclWindow.top {base} {

    global gColorList 
    global gFileNameLabel 

    if {$base == ""} {
        set base .top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    gVTcl:toplevel $top -class Toplevel \
        -menu "$top.m74" -highlightcolor black 
    wm focusmodel $top passive
#    wm geometry $top 811x718+386+161; update
    wm maxsize $top 1265 994
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "Blueview"
    gVTcl:DefineAlias "$top" "Toplevel1" gVTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    gVTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "gVTcl:FireEvent $top <<DeleteWindow>>"
    
    frame $top.fra48 \
        -borderwidth 2 -relief groove 
    gVTcl:DefineAlias "$top.fra48" "Frame1" gVTcl:WidgetProc "Toplevel1" 1
    set site_3_0 $top.fra48
    
    # panedwindow $site_3_0.pan50 
    PanedWindow $site_3_0.pan50
    $site_3_0.pan50 add  ;# left window
    $site_3_0.pan50 add  ;# right window
    
    set site_5_0 [$site_3_0.pan50 getframe 0]
    
    ############################################################
    # Tree::create by default

    ScrolledWindow $site_5_0.tr
    eval Tree $site_5_0.tr.lis51 $gColorList(treearea) 
    $site_5_0.tr setwidget $site_5_0.tr.lis51
    
    gVTcl:DefineAlias "$site_5_0.tr.lis51" "TreeArea" gVTcl:WidgetProc "Toplevel1" 1
    
    grid columnconf $site_5_0 0 -weight 1
    grid rowconf $site_5_0 0 -weight 1
    grid $site_5_0.tr -in $site_5_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
    
    set site_5_1 [$site_3_0.pan50 getframe 1]
    
    ### Joydeep : empty label created for file name and the global
    ###           gFileNameLabel has been set to the label for
    ###           for future reference.
    eval label $site_5_1.label $gColorList(filenamearea)
    set gFileNameLabel $site_5_1.label
    ############################################################

    ScrolledWindow $site_5_1.txt

    eval text $site_5_1.txt.tex54 $gColorList(textarea) -wrap none 
    $site_5_1.txt setwidget $site_5_1.txt.tex54
    gVTcl:DefineAlias "$site_5_1.txt.tex54" "TextArea" gVTcl:WidgetProc "Toplevel1" 1

    ### Joydeep : The file name label area has been placed below
    ###           the Verilog/BSV text area.
    grid columnconf $site_5_1 0 -weight 1
    grid rowconf    $site_5_1 0 -weight 1
    grid rowconf    $site_5_1 1 -weight 0
    grid $site_5_1.txt   -in $site_5_1 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid $site_5_1.label -in $site_5_1 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky nesw 

    ############################################################

    grid $site_3_0.pan50 -in $site_3_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    frame $top.fra49 -borderwidth 2 -relief groove -height 75 -width 125 
    gVTcl:DefineAlias "$top.fra49" "Frame2" gVTcl:WidgetProc "Toplevel1" 1
    set site_3_0 $top.fra49
    button $site_3_0.lab80 -image [image create photo -data [userGetLogo]] -command { opendebugger }
    
    gVTcl:DefineAlias "$site_3_0.lab80" "Label2" gVTcl:WidgetProc "Toplevel1" 1
    
    ############################################################
    global gTextFont
    eval text $site_3_0.tex81 $gColorList(statusarea) -height 4 \
        -yscrollcommand \{ .top.fra49.scr82 set \}
    
    gVTcl:DefineAlias "$site_3_0.tex81" "StatusArea" gVTcl:WidgetProc "Toplevel1" 1
    scrollbar $site_3_0.scr82 -command {.top.fra49.tex81 yview }
    gVTcl:DefineAlias "$site_3_0.scr82" "ScrollStatusY" gVTcl:WidgetProc "Toplevel1" 1
    grid $site_3_0.lab80 -in $site_3_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w 
    
    # HAND EDIT
    grid columnconf $site_3_0.tex81 0 -weight 1
    grid rowconf $site_3_0.tex81 0 -weight 1
    grid $site_3_0.tex81 -in $site_3_0 -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    
    grid $site_3_0.scr82 -in $site_3_0 -column 2 -row 0 -columnspan 1 -rowspan 1 -sticky ns 
    
    #### file menu
    menu $top.m74 -tearoff 1 
    $top.m74 add cascade -menu "$top.m74.men75" -command {} -label File 
    set site_3_0 $top.m74
    menu $site_3_0.men75 -tearoff 0 
    
    global gKeyList
    set keys [split $gKeyList(opensources) ':']
    $site_3_0.men75 add command -command menuOpenSources  -label {Open/Update Sources} -accelerator  [lindex $keys 0]
    $site_3_0.men75 add command -command menuOpenWaves -label {Open nWave}
    
    $site_3_0.men75 add separator
    $site_3_0.men75 add command -command menuSaveBsv   -label {Save}
    $site_3_0.men75 add command -command menuSaveAsBsv -label {Save As}
    $site_3_0.men75 add check   -label "BSV File ReadOnly" -variable gReadOnly
    $site_3_0.men75 add separator
    $site_3_0.men75 add command -command menuQuit -label Quit 
    
    
    #### navigate menu
    $top.m74 add cascade -menu "$top.m74.men79" -command {} -label {Navigate Tree}
    set site_3_0 $top.m74
    menu $site_3_0.men79 -tearoff 0 

    #### navigate menu
    $top.m74 add cascade -menu "$top.m74.men78" -command {} -label {Navigate Text}
    set site_3_0 $top.m74
    menu $site_3_0.men78 -tearoff 0 

    #### edit menu
    $top.m74 add cascade -menu "$top.m74.men76" -command {} -label {Edit Text}
    set site_3_0 $top.m74
    menu $site_3_0.men76 -tearoff 0 

   #  set keys [split $gKeyList(search) ':']
#     $site_3_0.men76 add command -label {Search} -accelerator [lindex $keys 0] \
#         -command { ::clickSearch }
    
#     set keys [split $gKeyList(rotateselections) ':']
#     $site_3_0.men76 add command -label {Search Again}  -accelerator [lindex $keys 0] \
#         -command { global gTextArea ; clickRotateSelections }

#     set keys [split $gKeyList(clearselections) ':']
#     $site_3_0.men76 add command -label {Search Again}  -accelerator [lindex $keys 0] \
#         -command { global gTextArea ; clickClearSelections }
    

    #### help menu
    $top.m74 add cascade -menu "$top.m74.men77" -command {} -label Help
    set site_3_0 $top.m74
    menu $site_3_0.men77 -tearoff 0 

    # global env 
    # if {$env(BLUEVIEW_MENU_DUMP_ALL) == "sallen"} { }
#    $site_3_0.men77 add command -command dumpAll   -label DumpSnapshot
    set keys [split $gKeyList(help) ':']
#    $site_3_0.men77 add command -command menuHelp  -label {Help} -accelerator [lindex $keys 0]
    $site_3_0.men77 add command -command menuHelp  -label {Help} 
    $site_3_0.men77 add command -command menuAbout -label {Help About}


    
    
    ############################################################
    ############################################################
#    bind $site_5_0.tr.lis51.c <Control-ButtonPress-1> { break }
#    bind $site_5_0.tr.lis51.c <Shift-ButtonPress-2>   { ::clickTreeSelectAction defof verilog }
#    bind $site_5_0.tr.lis51.c <ButtonPress-2>         { ::clickTreeSelectAction defof bsv }
    
    # bind $site_5_0.tr.lis51 <<TreeSelect>> {} 
    
    # TODO read only - unless we want to edit?
    bind $site_5_1.txt.tex54 <Any-Key> { 
        global gReadOnly gCurrViewIsBsv gTextModified
        if {($gReadOnly==1) || ($gCurrViewIsBsv==0)} break
        set gTextModified 1
    }
    
    foreach k { <Control-a> <Control-e> <Control-f> <Control-b> <Control-p> <Control-n> \
                    <End> <Shift-End> \
                    <Next> <Prior> <Up> <Down> <Left> <Right> \
                    <Triple-Button-1> } {
	
        bind $site_5_1.txt.tex54 $k { } ;# reinstate basic navigation commands
    }
    
    # make double click/select include bits for our generated verilog
    bind $site_5_1.txt.tex54 <Double-Button-1> {
        # do NOT include spaces in double click
        # select back & forward all but spaces, {}, (), etc
    }
    

#    bind $site_5_1.txt.tex54 <Control-w> { 
#	break
#	textCut

#    }
    # don't allow paste in a read only view
    bind $site_5_1.txt.tex54 <Button-2> { 
	textPaste
#        global gReadOnly gCurrViewIsBsv
#        puts "Key hit: %K"
#        if {($gReadOnly==1) || ($gCurrViewIsBsv==0)} break
    }

    ### For some reason need to delete F10 or it brings up File menu.
    bind .top "<F10>" { break }
    
    # allow these basic editing commands to pass 
    foreach key [split $gKeyList(opensources) ':'] {
        # these work at top level only!
        bind .top $key { ::menuOpenSources }
	bind $site_5_1.txt.tex54 $key { ::menuOpenSources }
    }

    foreach key [split $gKeyList(help) ':'] {
#        bind .top $key { ::menuHelp }
#        bind $site_5_1.txt.tex54 $key { ::menuHelp }
    }
    
    foreach key [split $gKeyList(quit) ':'] {
        bind .top $key { ::clickQuit }
        bind $site_5_1.txt.tex54 $key { ::clickQuit }
    }
    
    foreach key [split $gKeyList(search) ':'] {
        bind $site_5_1.txt.tex54 $key { ::clickSearch }
    }
    
    foreach key [split $gKeyList(rotateselections) ':'] {
        bind $site_5_1.txt.tex54 $key { 
            ::clickRotateSelections
        }
    }

    foreach key [split $gKeyList(clearselections) ':'] {
        bind $site_5_1.txt.tex54 $key { 
            ::clickClearSelections
        }
    }


    # bind $site_5_1.txt.tex54 $key { ::clickRotateSelections; break }

    set rightTextMenu [menu .top.popup1 -tearoff 0]  ;# create popup menu

    set menu_list  [list $rightTextMenu $site_3_0.men76]
    menuFillEditMenu $site_5_1.txt.tex54 $menu_list

    $rightTextMenu add separator
    set menu_list [list $rightTextMenu $site_3_0.men78]

    menuFillNavigateMenu $site_5_1.txt.tex54 $menu_list

    set rightTreeMenu [menu .top.popup2 -tearoff 0]  ;# create popup menu
    set menu_list  [list $rightTreeMenu $site_3_0.men79]

    menuFillTreeMenu $site_5_0.tr.lis51.c $menu_list





    # bind $site_5_1.txt.tex54 <<CutSelection>>   break
    # bind $site_5_1.txt.tex54 <<PasteSelection>> break
    
    # pop up menu for text area
    bind $site_5_1.txt.tex54 <Button-3> { 
        global gTextArea
        global gMenuDone
	
        set m .top.popup1
        set gMenuDone 0
        tk_popup $m %X %Y
        tkwait variable gMenuDone
    }

    # pop up menu for tree area
    bind $site_5_0.tr.lis51.c <Button-3> { 
	global gTextArea
	global gMenuDone
	
	set m .top.popup2
	set gMenuDone 0
	tk_popup $m %X %Y
	tkwait variable gMenuDone
    }
    
    
    # bind $site_5_1.txt.tex54 <<Modified>> {
    # global gTextModified
    # puts "<<Modified>>"
    # set gTextModified 1
    # }
    
    # selection has changed! just save it for now
    bind $site_5_1.txt.tex54 <<Selection>> {
        global gCurrentSelection gTextArea
        set gCurrentSelection [$gTextArea tag ranges sel]
    }
    
    # bind $site_5_1.txt.tex54 <Control-d> {
    # lappend auto_path /usr/local/RamDebugger
    # package require RamDebugger
    # puts "Debugger loaded"
    # }
    
    ###################
    # SETTING GEOMETRY
    ###################
    grid columnconf $top 0 -weight 1
    grid rowconf $top 0 -weight 1
    grid $top.fra48 \
        -in $top -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid columnconf $top.fra48 0 -weight 1
    grid rowconf $top.fra48 0 -weight 1
    grid $top.fra49 \
        -in $top -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid columnconf $top.fra49 1 -weight 1
    grid columnconf $top.fra49 1 -minsize 20
    grid rowconf $top.fra49 0 -weight 1
    grid rowconf $top.fra49 0 -minsize 20
    
    gVTcl:FireEvent $base <<Ready>>
}


################################################################################
###
################################################################################

proc menuFillNavigateMenu {area menu_list} {

    global gKeyList

    foreach key [split $gKeyList(findrelatedxrefs) ':'] {

	bind $area $key { ::clickSwitchToRelatedView }
	foreach menu $menu_list {
	    $menu add command -label "Switch view to related signals" \
		-command { ::clickSwitchToRelatedView } -accelerator $key
	}
    }
    
    foreach key [split $gKeyList(rotateselections) ':'] {
	bind $area $key { ::clickRotateSelections }
	foreach menu $menu_list {
	    $menu add command -label "Rotate through selections" -command { ::clickRotateSelections } -accelerator $key

	}
    }

    foreach key [split $gKeyList(clearselections) ':'] {
	bind $area $key { ::clickClearSelections }
	foreach menu $menu_list {
	    $menu add command -label "Clear current selections" -command { ::clickClearSelections } -accelerator $key

	}
    }
    
    foreach key [split $gKeyList(sendwaves) ':'] {
	bind $area $key { ::clickShowSignals }
	foreach menu $menu_list {
	    $menu add command -label "Send Signals to nWave" -command { ::clickShowSignals } -accelerator $key
	}
    }
    

    foreach menu $menu_list {
	$menu add command -label "Send All Signals to nWave" -command { 
	    ::selectAll
	    ::clickShowSignals 
	    ::unSelectAll

	}
    }

    ### Joydeep : clear all signals from nWave.
    foreach menu $menu_list {
	$menu add command -label "Clear All Signals from nWave" -command { 
	    ::selectAll
            ::clickDelSignals
	    ::unSelectAll
        }
    }

    ### Joydeep : Send CAN_FIRE signals to nWave
    foreach key [split $gKeyList(sendcanfiresignals2nWave) ':'] {
	bind $area $key { ::sendCanFireSignals }
	foreach menu $menu_list {
	    $menu add command -label "Send CAN FIRE signals to nWave." -command { ::sendCanFireSignals } -accelerator $key
	}
    }

    ### Joydeep : Send WILL_FIRE signals to nWave
    foreach key [split $gKeyList(sendwillfiresignals2nWave) ':'] {
	bind $area $key { ::sendWillFireSignals }
	foreach menu $menu_list {
	    $menu add command -label "Send WILL FIRE signals to nWave." -command { ::sendWillFireSignals } -accelerator $key
	}
    }

    foreach key [split $gKeyList(search) ':'] {
	bind $area $key { ::clickSearch }
	foreach menu $menu_list {
	    $menu add command -label "Text Search" -command { ::clickSearch } -accelerator $key
	}
    }

    foreach menu $menu_list {
	$menu add separator
    }

    foreach menu $menu_list {
	$menu add command -label "Pop Up This Menu (in text area)." -command { } -accelerator "<Right Mouse>"
    }
}

################################################################################
###
################################################################################

proc menuFillTreeMenu {area menu_list} {

    global gKeyList

    ### For some reason need to delete F10 or it brings up File menu.
    set key "<F10>"
    bind $area $key { break }

    foreach key [split $gKeyList(treeJumpToBSVDef) ':'] {
	bind $area $key { ::clickTreeSelectAction defof bsv }
	foreach menu $menu_list {
	    $menu add command -label "Switch to related BSV module definition." -command { ::clickTreeSelectAction defof bsv } -accelerator $key

	}
    }

    foreach key [split $gKeyList(treeJumpToBSVInst) ':'] {
	bind $area $key { ::clickTreeSelectAction instof bsv }
	foreach menu $menu_list {
	    $menu add command -label "Switch to related BSV module instantiation." -command { ::clickTreeSelectAction instof bsv } -accelerator $key

	}
    }

    foreach key [split $gKeyList(treeJumpToVerilogDef) ':'] {
	bind $area $key { ::clickTreeSelectAction defof verilog }
	foreach menu $menu_list {
	    $menu add command -label "Switch to related Verilog module definition." -command { ::clickTreeSelectAction defof verilog } -accelerator $key
	}
    }

    foreach key [split $gKeyList(treeJumpToVerilogInst) ':'] {
	bind $area $key { ::clickTreeSelectAction instof verilog }
	foreach menu $menu_list {
	    $menu add command -label "Switch to related Verilog module instantiation." -command { ::clickTreeSelectAction instof verilog } -accelerator $key

	}
    }

    foreach menu $menu_list {
	$menu add separator
    }

    foreach menu $menu_list {
	$menu add command -label "Pop Up This Menu (in tree area)." -command { } -accelerator "<Right Mouse>"
    }
}

################################################################################
###
################################################################################

proc menuFillEditMenu {area menu_list} {

    global gKeyList

    foreach key [split $gKeyList(textcut) ':'] {

	bind $area $key { textCut; break }
	foreach menu $menu_list {
	    $menu add command -label "Cut Text" \
		-command { textCut } -accelerator $key
	}
    }

    foreach key [split $gKeyList(textcopy) ':'] {

	bind $area $key { textCopy }
	foreach menu $menu_list {
	    $menu add command -label "Copy Text" \
		-command { textCopy } -accelerator $key
	}
    }

    foreach key [split $gKeyList(textpaste) ':'] {

	bind $area $key { textPaste; break }
	foreach menu $menu_list {
	    $menu add command -label "Paste Text" \
		-command { textPaste } -accelerator $key
	}
    }

    foreach menu $menu_list {
	$menu add separator
    }

    foreach menu $menu_list {
	$menu add command -command selectAll  -label "Select All"
    }
}

################################################################################
###
################################################################################



#############################################################################
## Binding tag:  _TopLevel

bind "_TopLevel" <<Create>> {
    if {![info exists _topcount]} {set _topcount 0}; incr _topcount
}
bind "_TopLevel" <<DeleteWindow>> {
    ::userClose
    if {[set ::%W::_modal]} {
        gVTcl:Toplevel:WidgetProc %W endmodal
    } else {
        destroy %W; if {$_topcount == 0} {exit}
    }
}
bind "_TopLevel" <Destroy> {
    if {[winfo toplevel %W] == "%W"} {incr _topcount -1}
}

# Window show .
# Window show .top

# for debugging with RamDebugger.tcl
#
# set argv [list -nowaves mkARM9Core.info /TOP]
# set argv [list -nowaves mkTestbench_top.info /testbench]
# set argv [list -nowaves mkIDCT_top.info design.vcd /testTopModule/idct]
# set argv [list -nowaves -path .:obj mkMixedTestMesa.info mesa.vcd /Tb/TOP]

if {$argc == 0} {
    set initfile [file join $gLogDir init.cmd]
    if {[file exists $initfile ]} {
        source $initfile
    }
}

#main $argc $argv

#  .textarea mark set insert 1.0 
#  .textarea tag ranges sel 

# set start [.textarea index sel.first]
# set end [.textarea index sel.last]
# or...
# set range [.textarea tag ranges sel] 

# tk_popup menu x y ?entry?

# TODO: add to help menu tree
# - Dump file = .vcd or .fsdb of simulation to examine 
#   Info file = top level .info created by bsc --show-module-use 
#   GInstance  = path that matches top level Info file 
#   I.E. /Tb/TOP
#
# - save window locations
# - hit <CR> in opensources dialog
# - save/initial values in opensources dialog
# - search all pages / files 


# Done
#  compilable with tclcompiler by active state
#  start lmgrd if it is not running already
#  user function for selection of signals
#  command line switches broken
#  inline usage/help
#  release via install, etc
#  time licenses
#  wvexit - close windows properly with no errors
#  autoupdate of license info - perl script to modify blueview.tcl
#  multicolor select
#  shutdown of nWave doesn't kill blueview - don't leave this alone
#  deal with sized busses properly! 

# TODO
#  rexec compile on sapphire, or azure
#  documentation
#
#  waveable signals - search file for *all* occurances *OR*
#      search waveable list with string selected to see if there is a 
#      useful match


################################################################################
###
################################################################################

proc create_string {var_name} {

    set code 1

    if {[uplevel 1 [list info exists $var_name]]} {

	set code [catch { set value [uplevel 1 [list set $var_name]] } msg]

    }

    if {$code==1} {

	if {[uplevel 1 [list array exists $var_name]]} {

	    set value [format "<Array %s>" $var_name]

	} else {

	    set value "<undefined>"

	}

    }

    if {[string length $value] > 3000} {

	set value [string range $value 0 2999]
	append value " ... (TRUNCATED)"

    }

    return [outstring $value]

}

################################################################################
###
################################################################################

proc outstring {object} {

    if {[regexp {^[ ]*_sel[0-9]+[ ]*$} $object]} {

	if {[get_attribute $object object_class]==""} {
	
	    set count 0
	
	    foreach_in_collection dummy $object {
	    
		incr count
	    
		if { $count > 1 } {
		
		    set number [string replace $object 0 3]
		    append label [format "<collection \[%s items\] %s>" [sizeof_collection $object] $number]
		    return  $label
		
		}
	    }
	
	    append label $object

	    return  $label

	} else {

	    set class [get_attribute $object object_class]
	
	    set number [string replace $object 0 3]
	    append label "<" $class

	    if {$class == "cell"} {

		append label " " [get_attribute [get_lib_cells -of_objects $object] full_name]

	    }

	    if {$class == "pin"} {

		append label " " [get_attribute [get_lib_pins -of_objects $object] full_name]

	    }

	    if {$class == "lib_cell"||$class == "lib_pin"} {

		append label " " [get_attribute $object full_name]

	    }

	    append label " " $number ">"
	
	    return $label
	
	}
    }
	
    append label $object
	
    return  $label

}

################################################################################
###
################################################################################

### Joydeep : Procedure to send Can Fire Signals to nWave
proc sendCanFireSignals {} {

    global gMenuDone gWaveState

    if {$gWaveState == "nowaves"} { 
	
	    bell
	    userStatus "No .vcd/.fsbd file has been loaded yet."
	    set gMenuDone 1
	    return

    }

    if {![isDebussyRunning]} { 
	
	    bell
	    userStatus "No waveform viewer is running."
	    set gMenuDone 1
	    return

    }

    ::selectAll
    set allSignals [::listOfSignalsFromSelectedArea]
    ::unSelectAll

    if {! [llength $allSignals] } { return }

    set canFireSigs {}

    foreach element $allSignals {
        set sigPathList [split $element "/"]
        set length [llength $sigPathList]
        set sigName [lindex $sigPathList [expr ($length - 1)]]
        if {[regexp {^CAN_FIRE} $sigName] && ![regexp {__q\d} $sigName]} {
            lappend canFireSigs $element
        }
    }

    if { ![llength $canFireSigs] } {
        bell
        userStatus "No CAN_FIRE signals found in the selected rext region."
        return
    }

    set mess "wvAddSignal $canFireSigs" 
    debug 2 $mess

    global gSnaplog
    if {[info exists gSnaplog]} {
        puts $gSnaplog $mess
    }


    if {[::userOpenWaves] == "loaded"} {

	    set count [llength $canFireSigs]

	    if { $count > 1 } {

		    userStatus "Sending $count signals to the wave viewer."

	    } elseif { $count == 1 } {

		    userStatus "Sending $count signal to the wave viewer."

	    }

        set value [send_to_debussy "wvAddSignal $canFireSigs"]

    }
}

### Joydeep : Procedure to send Will Fire Signals to nWave
proc sendWillFireSignals {} {

    global gMenuDone gWaveState

    if {$gWaveState == "nowaves"} { 
	
	    bell
	    userStatus "No .vcd/.fsbd file has been loaded yet."
	    set gMenuDone 1
	    return

    }

    if {![isDebussyRunning]} { 
	
	    bell
	    userStatus "No waveform viewer is running."
	    set gMenuDone 1
	    return

    }

    ::selectAll
    set allSignals [::listOfSignalsFromSelectedArea]
    ::unSelectAll

    if {! [llength $allSignals] } { return }

    set willFireSigs {}

    foreach element $allSignals {
        set sigPathList [split $element "/"]
        set length [llength $sigPathList]
        set sigName [lindex $sigPathList [expr ($length - 1)]]
        if {[regexp {^WILL_FIRE} $sigName] && ![regexp {__q\d} $sigName]} {
            lappend willFireSigs $element
        }
    }

    if { ![llength $willFireSigs] } {
        bell
        userStatus "No WILL_FIRE signals found in the selected rext region."
        return
    }

    set mess "wvAddSignal $willFireSigs" 
    debug 2 $mess

    global gSnaplog
    if {[info exists gSnaplog]} {
        puts $gSnaplog $mess
    }


    if {[::userOpenWaves] == "loaded"} {

	    set count [llength $willFireSigs]

	    if { $count > 1 } {

		    userStatus "Sending $count signals to the wave viewer."

	    } elseif { $count == 1 } {

		    userStatus "Sending $count signal to the wave viewer."

	    }

        set value [send_to_debussy "wvAddSignal $willFireSigs"]

    }
}

### Joydeep : Procedure to delete all signals from nWave.
proc clickDelSignals {} {
    
    global gMenuDone gWaveState

    if {$gWaveState == "nowaves"} { 
	
	    bell
	    userStatus "No .vcd/.fsbd file has been loaded yet."
	    set gMenuDone 1
	    return

    }

    if {![isDebussyRunning]} { 
	
	    bell
	    userStatus "No waveform viewer is running."
	    set gMenuDone 1
	    return

    }

    if {[::userOpenWaves] == "loaded"} {

        set value [send_to_debussy "wvClearAll"]


    }
    
    set gMenuDone 1
}

### Joydeep : Procedure to find list of waveable signals
###           from the selected text area.
proc listOfSignalsFromSelectedArea {} {

    global gCurrViewIsBsv gCurrPosBsv gCurrPosV
    global gCurrentSelection
    global gTreeInfo gCurrTreeIndex
    global gInfoNet  gTextArea
    global gMenuDone gTextArea gInfoDefof
    global gHackMode
    global gVerilogRef

    # just get what we grabbed
    set range [getSelection tagWaveable]

    if {[llength $range] == 0} {
	    set range [getSelection tagXRef]

	    if {[llength $range] == 0} {

            bell
            userStatus "Notice: The cursor is not inside a tagged region."
     	    set gMenuDone 1
    	    return

	    } else {

	        bell  
	        userStatus "The selected region is cross-referenced but has no associated signal net(s)."
	        set gMenuDone 1
	        return

	    }
    }

    set inx   [lindex $range 0]
    set enx   [lindex $range 1]
    
    if {$gCurrViewIsBsv==1} { 
	    set gCurrPosBsv $gCurrentSelection
    } else {
	    set gCurrPosV $gCurrentSelection
    }
    
    set infomod  [getInfoMod $gCurrTreeIndex]
    set mod      $infomod
    set vfile    $gInfoDefof($mod:vfile)
    set vpath    [makepath $gTreeInfo($gCurrTreeIndex:vpath)]
    
    # if no .info file, then go to parent until we find an info file
    if {[array names gInfoNet $infomod]=={}} {
	    userMessageBox Error [list "$infomod.info file not found!"]
	    set gMenuDone 1
	    return
    }
    
    # else we are looking at a verilog file, parse it to find out what signals are valid
    # and what their sizes are if it hasn't been done already
    if {[info exists gVerilogRef] == 0} {
	    ::readVerilogDefines $vfile
    }
    
    if {$gCurrViewIsBsv==1} { 
	######################################################################
	# look for useful signals if we are looking at a BSV
	# this is a list of lists
	# puts "DBG check for match at $inx"
	debug 3 ":signal check for things in range of ($inx -> $enx) in $infomod"
	
	foreach f $gInfoNet($infomod) {
	    array set tmp $f
	    set signal $tmp(signal)
	    
	    # we should really match this to the exact index
		set pnt $tmp(bpos)
	    
	    # if in range, then add signal (word or drag or otherwise :)
	    # debug ":signal check $inx <= $pnt <= $enx"
	    if {[$gTextArea compare $inx <= $pnt] && [$gTextArea compare $pnt <= $enx]} {
		debug 4 "  match at $pnt for $signal"
		
		if {[info exists gVerilogRef($signal)]} {
		    set szH [lindex $gVerilogRef($signal) 0]
		    set szL [lindex $gVerilogRef($signal) 1]
		    set sig [create_debussy_signal 0 $vpath $signal $szH $szL]
		    set sig_1 [create_debussy_signal 1 $vpath $signal $szH $szL]
		    if {[::userSignalMatchFunction $sig] == 1} {
		        if {[info exists driven($sig)]==0} {
		            lappend slist $sig
		            lappend slist_1 $sig_1
		            set driven($sig) 1
		        }
		    }
		}
	    }
	}
	
    } else {
	######################################################################
	
	# now select all the strings in the verilog file and send them to
	# nWave if they are valid signals
	if {$inx != "0.0"} {
	    while 1 {
		set hit [$gTextArea search -count cnt -forward -regexp {[\w\d_\$]+} $inx $enx]
		if {$hit == ""} break
		
		set eom [$gTextArea index "$hit +$cnt chars"]
		set signal [$gTextArea get $hit $eom]
		
		if {[info exists gVerilogRef($signal)]} {
		    set szH [lindex $gVerilogRef($signal) 0]
		    set szL [lindex $gVerilogRef($signal) 1]
		    set sig [create_debussy_signal 0 $vpath $signal $szH $szL]
		    set sig_1 [create_debussy_signal 1 $vpath $signal $szH $szL]
		    if {[::userSignalMatchFunction $sig] == 1} {
		        if {[info exists driven($sig)]==0} {
		            lappend slist $sig
		            lappend slist_1 $sig_1
		            set driven($sig) 1
		        }
		    }
		}
		
		set inx $eom
	    }
	}
    }

    if {[info exists slist] == 0} {
	if {$gCurrViewIsBsv==1} { 
	    bell  
	    userStatus "Notice: The selected region does not include any tagged regions."

	} else {

	    userMessageBox Warning [list "No valid selection" \
		                        "Please select a signal or range including valid signals" \
		                        "(as defined by input, output, wire and reg definitions" ]
	}
	
	set gMenuDone 1
	return
    }
    
    updateHackMode $slist $slist_1

    if {$gHackMode > 0} {
	
	    set ls [join $slist_1 " "]

    } else {

	    set ls [join $slist " "]

    }

    return $ls
}

################################################################################
### Add this temporarily for tcl 8.3/8.4 compatability
################################################################################

if {([package vcompare [package provide Tcl] 8.4] < 0)} {
    
    proc file_normalize {sp} {

	set sp [file split [file dir $sp]/[file tail $sp]]
	
	# Conversion of the incoming path to absolute.
	if {[string equal [file pathtype [lindex $sp 0]] "relative"]} {
	    set sp [file split [eval [list file join [pwd]] $sp]]
	}
	
	# Resolution of symlink components, and embedded relative
	# modifiers (., and ..).
	
	set np {}
	while {[llength $sp]} {
	    set ele    [lindex $sp 0]
	    set sp     [lrange $sp 1 end]
	    set islast [expr {[llength $sp] == 0}]
	    
	    if {[string equal $ele ".."]} {
		if {[llength $np] > 1} {
		    # .. : Remove the previous element added to the
		    # new path, if there actually is enough to remove.
		    set np [lrange $np 0 end-1]
		}
	    } elseif {[string equal $ele "."]} {
		# Ignore .'s, they stay at the current location
		continue
	    } else {
		# A regular element. If it is not the last component
		# then check if the combination is a symlink, and if
		# yes, resolve it.
		
		lappend np $ele
		
		if {!$islast} {
		    if {[string equal link [file type [set p [eval file join $np]]]]} {
			set dst [file readlink $p]
			
			# We always push the destination in front of
			# the source path (in expanded form). So that
			# we handle .., .'s, and symlinks inside of
			# this path as well. An absolute path clears
			# the result, a relative one just removes the
			# last, now resolved component.
			
			set sp [eval [linsert [file split $dst] 0 linsert $sp 0]]
			
			if {![string equal relative [file pathtype $dst]]} {
			    # Absolute|volrelative destination, clear
			    # result, we have to start over.
			    set np {}
			} else {
			    # Relative link, just remove the resolved
			    # component again.
			    set np [lrange $np 0 end-1]
			}
		    }
		    
		}
	    }
	}
	if {[llength $np] > 0} {
	    return [eval file join $np]
	}
    }

} else {
    
    proc file_normalize {sp} {
	
	return [file normalize $sp]
    }
}

################################################################################
###
################################################################################

