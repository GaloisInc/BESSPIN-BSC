#!/bin/sh
# the next line restarts using wish\
    exec wish "$0" -- "$@" 

################################################################################
# This file is Copyright (c) 2004 Bluespec, Inc. 
#
# <add legalese here>
#
# BWidget1.7.0 library redistrubuted as public library for your convinience
#   license terms of the bwidget library available at 
#   $(BLUESPECDIR)/bwidget1.7.0/LICENSE.txt
#   this is an *exact* copy of 1.7.0 available on sourceforge.net as of 
#   today 11/29/2004
#
################################################################################

# get this from ~/bsc/src/comp/Version.hs
set gExpDate [list 11 4]
set gVersion 3.8.42

# build 52 => {Built on 'Mon Jan  3 15:14:47 EST 2005' on 'clef' by 'sallen'}
set gBuild 52

# initialize all global variables
set gVcdFile  ""
set gFsdbFile ""

# will become -output-cross-info soon!
set gInfoSwitch {-cross-info}
set gDebugLevel 50
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
################################################################################
if {![info exists gVTcl(sourcing)]} {
    # package require tcldebugger_attach
    # use tcldebugger_eval {lots of code}
    
    package require Tk 8.3
    
    global gAndroidPresent
    set    gAndroidPresent [catch {package require android}]
    
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
    
    global env
    lappend auto_path $env(BLUESPECDIR)

    # this comes from ActiveState or 'apt-get install bwidget'
    if {[catch {package require BWidget}]==1} {
	puts "ERROR: you have the tk package 'bwidget1.7.0' or better installed"
	puts "       'apt-get install bwidget' will work on debian"
	exit
    }
    
    # defaults
    set gTreeColors   {-background grey90 -highlightcolor blue -selectforeground red}
    set gTextColors   {-background grey90 -highlightcolor blue -foreground black -insertbackground black}
    
    set gStatusColors {-background turquoise -insertbackground black -foreground black}
    
    set gTagKeyword   {-foreground brown}       ;# keyword in bsv
    set gTagDatatype  {-foreground sienna}      ;# Reg, etc
    set gTagLibrary   {-foreground blue}        ;# library type
    set gTagComment   {-foreground darkgreen}   ;# comment colors
    set gTagModselect {-background red}         ;#
    set gTagHilight1  {-background dodgerblue}  ;# when selected
    set gTagHilight2  {-background lightblue}   ;# when selected
    
    set gTagWaveable  {-background lightsalmon}
    
    set gKeyList(help)          "<Control-h>"
    set gKeyList(quit)          "<Control-q>"
    set gKeyList(opendump)      "<Control-o>"       ;# open up dump file
    set gKeyList(search)        "<F2>:<Control-s>"  ;# simple text search
    set gKeyList(searchagain)   "<F3>"              ;# search for same thing again
    set gKeyList(findsignalinV) "<F6>"              ;# find related signals in v or bsv file
    set gKeyList(nextsignal)    "<F7>"              ;# find next related signal
    set gKeyList(sendwaves)     "<F10>"             ;# send signals to wave view
    
    proc ::userSignalMatchFunction {sig} { return 1 }
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

#############################################################################
proc dbg_enter {name {mess ""}} {
    global gdbIndent gDbgStack
    
    set m "Enter $name $mess"
    debug 0 "$m"
    
    set gdbIndent "  $gdbIndent"
    lappend gDbgStack [list $name [clock clicks]]
}

proc dbg_exit { {mess ""} } {
    global gdbIndent gDbgStack
    regsub {  } $gdbIndent {} gdbIndent
    
    # pop off of stack - major yuck
    set item      [llast  $gDbgStack]
    set gDbgStack [lfront $gDbgStack]
    
    set name [lindex $item 0]
    set sttm [lindex $item 1]
    
    set entm [clock clicks]
    set tm [expr ($entm - $sttm)]
    
    set m "Exit $name $mess (time = $tm)"
    debug 0 $m
}


proc debug {pri mess} {
    global gDebug gdbIndent gDebugLevel
    if {$pri > $gDebugLevel} return
    if {[info exists gDebug]} {
	puts $gDebug "$gdbIndent$mess"
	flush $gDebug
    }
}

# this is extremely low tech, but good enough for now
proc ::checkLicense {} {
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
    
    proc ::gVTcl:rename {name} {
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
    
    proc ::Window {args} {
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
    
    proc ::gVTcl:DefineAlias {target alias widgetProc top_or_alias cmdalias} {
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
    
    proc ::gVTcl:DoCmdOption {target cmd} {
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
    
    proc ::gVTcl:FireEvent {target evnt {params {}}} {
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
	    
	    proc ::gVTcl:Toplevel:WidgetProc {w args} {
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
	    
	    proc ::gVTcl:WidgetProc {w args} {
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
	    
	    proc ::gVTcl:toplevel {args} {
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
		namespace eval ::widgets::$site_3_0.men77 {
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
		        clickRotateMatch
		        clickSearch
		        clickShowSignals
		        clickSwitchToRelatedView
		        clickTreeSelectAction
		        debCallback
		        debShowSignal
		        menuAbout
		        menuHelp
		        menuOpenDump
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
		        userOpenDump
		        userOpenSource
		        userOpenWaves
		        userLoadInfoFile
		        userReadInfoFile
		        userLoadView
		        userStatus
		        userTag
		        userUnHilightHitlist
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
proc ::modifyTag {tag value} {
    global gPreferences gPrompt
    # use color selection font to change color
    set color [tk_chooseColor -initialcolor gray -title "Choose Color"]
    
    # replace color with new color
    # puts "color selected = $color"
    # set gPreferences($tag) "-foreground $color"
}

proc ::menuHelp {} {
    global gKeyList
    
    userMessageBox Info \
	[list \
	     [format "%-20s - this menu" $gKeyList(help)] \
	     [format "%-20s - quit blueview" $gKeyList(quit)] \
	     [format "%-20s - open dumpfile" $gKeyList(opendump)] \
	     [format "%-20s - search for text" $gKeyList(search)] \
	     [format "%-20s - search again for text or related signal" $gKeyList(searchagain)] \
	     [format "%-20s - find related signals in V or BVS" $gKeyList(findsignalinV)] \
	     [format "%-20s - send signals to wave viewer" $gKeyList(sendwaves)] \
	     "" \
	     "Left  Mouse Button - select characters, etc" \
	     "Right Mouse Button - popup menu" \
	     \
	     "plus all the generate navigation keys work" \
	     "and if you toggle the read-only mode in the menu"\
	     "then you can write a bsv back out" \
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

proc ::menuAbout {} {
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
	label  $f.l3 -text "Copyright Bluespec, Inc. 2004"
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

proc ::menuQuit {} {
    global gWidget
    global gTkAppList
    clickQuit
}

proc ::menuToggleWritable {} {
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


proc ::menuSaveBsv {} {
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

proc ::menuSaveAsBsv {} {
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
    eval set fl [tk_getOpenFile -filetypes $opts]
    if {$fl=={}} { return {} }
    
    global gPwd
    regsub "$gPwd/?" $fl {} fl   ;# remove all hierarchy to *this* point
    return $fl
}

proc ::menuOpenDump {} {
    
    # create a dialog to get everything we need.
    global gPrompt gVcdFile gInfoFile gInstance
    
    set t .top.m
    if {[DialogUtilsCreate $t Question -borderwidth 10]} {
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
	
	label $f1.l -text {Dump File} -width 12
	entry $f1.m -textvariable gVcdFile -width 50
	button $f1.r -text {...} -command {set gVcdFile [getFile \
		                                              {{Vcd .vcd} \
		                                                   {Fsdb .fsdb} \
		                                                   {{All Files} *}}]}
	
	grid columnconf $f1 1 -weight 1
	grid $f1.l -in $f1  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f1.m -in $f1  -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f1.r -in $f1  -column 2 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	
	label $f2.l -text {Info File}  -width 12
	entry $f2.m -textvariable gInfoFile -width 50
	button $f2.r -text {...} -command {set gInfoFile [getFile {{Info .info} {{All Files} *}}]}
	grid columnconf $f2 1 -weight 1
	grid $f2.l -in $f2  -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f2.m -in $f2  -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	grid $f2.r -in $f2  -column 2 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
	
	label $f3.l -text {GInstance} -width 12
	entry $f3.m -textvariable gInstance -width 50
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
	userStatus "From tcsh: blueview $gInfoFile $gVcdFile $gInstance"
	set gCurrPosBsv [list "1.0" "1.0"]
	set gCurrPosV   [list "1.0" "1.0"]
	
	debug 0 "userOpenDump $gInfoFile $gInstance $gVcdFile "
	userOpenDump $gInfoFile $gInstance $gVcdFile 
    }
}


################################################################################
proc ::userFindFile {fl} {
    global gSearchPath
    if {[file exists $fl]} {
	return 1
    }
    foreach dir $gSearchPath {
	set full [file join $dir $fl]
	if {[file exists $full]} {
	    return 1
	}
    }
    return 0
}


proc ::userFindAndOpen {fl} {
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

######################################################################

proc ::userClose {} {
    global gWidget
    global gTkAppList gWaveState
    
    if {($gWaveState == "loaded") || ($gWaveState == "nofsdbyet")} {
	set gWaveState nowaves
	set gTkAppList [winfo interp]
	if {[lsearch $gTkAppList abc] >= 0} {
	    catch {wvCloseFile -win abc}
	    catch {wvGetSignalClose -win abc}
	    userStatus "Exiting nWave and blueview - this may take a few seconds"
	    update
	    
	    catch {send abc wvExit}
	}
    }
}

proc ::userStatus {mess} {
    global gStatusArea gTextArea
    debug 0 $mess
    $gStatusArea insert end $mess
    $gStatusArea insert end "\n"
    $gStatusArea see end
}

proc ::userMessageBox {type mess} {
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
proc ::splitOffInstance {path} {
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
    
    # just get what we grabbed
    set s     [::getSelection]
    if {[llength $s] == 0} {
	userMessageBox Error [list "no range selected "]
	set gMenuDone 1
	return
    }

    set inx   [lindex $s 0]
    set enx   [lindex $s 1]
    dbg_enter clickShowSignals "$inx $enx"
    
    if {$gCurrViewIsBsv==1} { 
	set gCurrPosBsv $gCurrentSelection
    } else {
	set gCurrPosV $gCurrentSelection
    }
    
    debug 2 ":userSelectMenu 1 inx = $gCurrTreeIndex"

    set infomod  [getInfoMod $gCurrTreeIndex]
    set mod      $infomod
    set vfile    $gInfoDefof($mod:vfile)
    set vpath    [makepath $gTreeInfo($gCurrTreeIndex:vpath)]
    
    debug 3 ":userSelectMenu 2 $mod $vpath $vfile"
    
    # if no .info file, then go to parent until we find an info file
    while {[array names gInfoNet $infomod]=={}} {
	userMessageBox Error [list "$infomod.info file not found!"]
	set gMenuDone 1
	dbg_exit
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
		    if {$szH != -1} {
		        set sig $vpath/\\$signal\[$szH:$szL\]
		    } else {
		        set sig $vpath/$signal
		    }

                    regsub {[ \t]+$} $sig {} sig
		    debug 4 "    show $sig"
		    if {[::userSignalMatchFunction $sig] == 1} {
		        if {[info exists driven($sig)]==0} {
		            lappend slist "\{$sig\}"
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
		    if {$szH != -1} {
		        set sig $vpath/\\$signal\[$szH:$szL\]
		    } else {
		        set sig $vpath/$signal
		    }
		    
                    regsub {[ \t]+$} $sig {} sig
		    if {[::userSignalMatchFunction $sig] == 1} {
		        if {[info exists driven($sig)]==0} {
		            lappend slist "\{$sig\}"
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
	    userMessageBox Warning [list "No matching signals found in crossreference" \
		                        "Please select a 'waveable' signal" \
		                        "higlighted with preference variable 'gTagWaveable'"  ]
	} else {
	    userMessageBox Warning [list "No valid selection" \
		                        "Please select a signal or range including valid signals" \
		                        "(as defined by input, output, wire and reg definitions" ]
	}
	
	set gMenuDone 1
	dbg_exit
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
    set ls [join $slist " "]
    
    set mess "wvAddSignal $ls" 
    debug 2 $mess

    global gSnaplog
    if {[info exists gSnaplog]} {
        puts $gSnaplog $mess
    }
    
    if {[::userOpenWaves] == "loaded"} {
	if {$gAddNewGroup == 1} {
	    send abc "wvAddGroup -win abc $grp"
	    send abc "wvSetPosition -win abc {{\"$grp\" 1}}"
	    send abc "wvAddSignal -win abc -group { \"$grp\" {$ls} }" 
	} else {
	    # add the signal after current selection
            send abc "wvAddSignal -win abc $ls"
	}
    }
    
    set gMenuDone 1
    dbg_exit
}


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

proc createNodeInfoHier {mod parentmod bpath vpath hierlist {level 0}} {
    global gInfoDefof gInfoNet gInfoInstof
    global gTreeMaxIndex gLibraryModule gTreeInfo gInst2TreeIndex

    if {[info exists gLibraryModule($mod)]} {
        debug 10 "skipping library module $mod"
        return
    }

    dbg_enter createNodeInfoHier "$mod {$parentmod} {$bpath} {$vpath} {$level}"

    # create info for this module
    set  i $gTreeMaxIndex
    debug 1 "Create gTreeInfo($i) {$mod} {$bpath} {$vpath} $level"

    set  gInst2TreeIndex([makepath $bpath]) $i
    set  gTreeInfo($i:mod)   $mod
    set  gTreeInfo($i:bpath) $bpath
    set  gTreeInfo($i:vpath) $vpath
    set  gTreeInfo($i:level) $level
    set  parentinx $i
    incr gTreeMaxIndex
    set  i $gTreeMaxIndex

    # check for info file to continue to recurse
    if {$hierlist == "_"} {
        dbg_exit
        return
    }

    # get instof info for this mod

    # loop/recurse children
    foreach item $hierlist {
        set chtype      [lindex $item 0]
        set chbinst     [lindex $item 1]
        set chinst      [lindex $item 2]
        set chhierlist  [lindex $item 3]
        
        if {[info exists gInfoInstof($mod:$chinst)] == 0} {
            # find parent module until we hit one with a .info file that is present
            # and use the verilog instance name since we are in a flat hierachy
            if {[info exists gInfoInstof($parentmod:$chinst)]} {
                set mod $parentmod
            } else {
                debug 1 "Warning: gInfoInstof($mod:$chinst) info file does not exist, but listed in hierarchy!"
                continue
            }
        }

        array set infoInstof $gInfoInstof($mod:$chinst)

        # for each instance name, find related mod defined in gDefof (flat or not)
        debug 1 "check for gInfoInstof( $mod : $chinst : info) \[{$chtype}{$chinst}\]"

        set chmod $infoInstof(info)
        set chvpath $vpath

        if {[file exists "$chmod.info"]} {
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

        createNodeInfoHier $chmod $mod $chbpath $chvpath $chhierlist [expr $level + 1]
    }

    dbg_exit
}

#############################################################################
## Procedure:  userOpenDump
proc ::userLoadTreeView {} {
    dbg_enter UserLoadTreeView
    global gTreeArea gTreeInfo gCurrTreeIndex gInst2TreeIndex
    global gTreeMaxIndex
    
    $gTreeArea delete
    set parent root
    set lastmod ""
    set parent root
    
    for {set i 0} {$i < $gTreeMaxIndex} {incr i} {
        set mod    $gTreeInfo($i:mod)
        set bpath  $gTreeInfo($i:bpath)

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

        # name => index
        # name => bpath joined with .
        $gTreeArea insert end $parent $i -text "$inst$attr"
    }
    
    set gCurrTreeIndex 0
    $gTreeArea selection set $gCurrTreeIndex
    
    $gTreeArea opentree $gCurrTreeIndex 0     ;# open at least one level
    $gTreeArea see      $gCurrTreeIndex       ;# look at index 0 (top of tree)
    $gTreeArea xview    moveto 0              ;# look at far left
    dbg_exit 
}

#############################################################################
proc ::userOpenDump { infofile path dumpfile } {
    dbg_enter userOpenDump "$infofile $path $dumpfile"

    # global gSnaplog
    # set gSnaplog $dumpfile
    
    openSplashScreen "Initializing Data Structures"

    global gWidget gTkAppList gDebApp
    global gStatusArea gTextArea
    global gTreeInfo gInfoSwitch
    global gVcdFile gFsdbFile gWaveState
    global gTopPath gInfoDefof gTopPath
    
    # regsub {/[A-Za-z0-9_]+$} $path  {} gTopPath
    regsub {^/*} $path {} gTopPath
    
    # get file name
    if {($gWaveState == "nowaves") || ($dumpfile=={})} {
        set gWaveState nowaves
	
    } else {
        if {[regexp {.vcd$} $dumpfile]} {
            set gVcdFile $dumpfile        
            regsub {.vcd$} $gVcdFile ".fsdb" gFsdbFile
        } elseif {[regexp {.fsdb$} $dumpfile]} {
            set gVcdFile ""
            set gFsdbFile $dumpfile
        } else {
            # neither, so error for now
            set mess [list "dumpfile '$dumpfile' is neither .fsdb nor .vcd"]
            puts "ERROR: $mess"
            debug 1 "ERROR: $mess"
            dbg_exit "failed $gumpfile"
            exit
            return
        }
        debug 1 "return $gVcdFile $gFsdbFile"
    }
    
    
    ############################################################
    # assume main is +
    # assume top  is this.info
    
    if {[userFindFile $infofile]==0} {
        set mess [list "Could not find needed file: $infofile" \
                       "Please recompile with $gInfoSwitch option" ]
        puts "ERROR: $mess"
        debug 1 "ERROR: $mess"
        dbg_exit "failed - info file $infofile"
        exit
        return ; # ::clickQuit
    }
    
    # parse hierarcy from .info files!
    
    # gotta start somewhere
    foreach info [glob *.info] {
        userReadInfoFile $info
    }

    # now stitch it all together - the fun part!
    ########################################
    # top module is from defof of top info file
    regsub {\.info$} $infofile {} topMod
    regsub {^/}      $gTopPath {} topPath
    
    # blow out hierarchy to it's full glory so we can parse it!
    # set hier [userExpandHierarchy $topPath $topMod 

    global gHierarchy
    createNodeInfoHier $topMod {} [list $topPath] [list $topPath] $gHierarchy($topMod)
    
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
    dbg_exit
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

proc ::userFindFirstHitListAssign { {fl ""} } {
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
proc ::userFindMatchingIndex {fullinx inx} { 
    global gInfoNet gTreeInfo gInfoDefof
    global gTextArea gHitList gHitListIndex gCurrViewIsBsv
    
    dbg_enter userFindMatchingIndex "look for $fullinx at gTreeIndex=$inx"
    
    # set inx  [lindex [split $fullinx {\.}] 0]
    set mod    $gTreeInfo($inx:mod)
    set bpath  $gTreeInfo($inx:bpath)
    
    # get current module name for both bsv and v
    set currinx [lindex $fullinx 0]
    
    ::userClearHitList
    
    if {$currinx == "1.0"} { 
        userStatus "No Related signals were found in the cross reference" 
        dbg_exit
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
        userStatus "No Related signals were found in the cross reference" 
        userStatus "    Toggling view, but selecting top of the file!"
    } 
    
    # third returned element is treeindex
    lappend res $inx
    
    dbg_exit "returning {$res} $gHitListIndex"
    return $res
}

############################################################
proc ::clickRotateMatch {} {
    dbg_enter clickRotateMatch
    
    global gTextArea
    global gCurrViewIsBsv gCurrPosV gCurrPosBsv
    global gHitList gHitListIndex
    
    # move to next selection in the hit list, if there is one
    if {[info exists gHitList]} {
        set len [llength $gHitList]
        set gHitListIndex [expr ($gHitListIndex + 1) % $len]
        set f [lindex $gHitList $gHitListIndex]
        set ntype [lindex $f 0]
        set finx  [lindex $f 1]
        if {[lindex $finx 0] == "-1.-1"} return
	
        debug 5 "next hit at $gHitListIndex $len $finx $gCurrViewIsBsv"
        if {$gCurrViewIsBsv} {
            set gCurrPosBsv $finx
            ::userMoveCursor [lindex $gCurrPosBsv 0]
        } else {
            set gCurrPosV   $finx
            ::userMoveCursor [lindex $gCurrPosV 0]
        }
    }
    
    dbg_exit
}

############################################################
proc readLines {fl} {
    set fp [userFindAndOpen $fl]
    if {$fp == 0} {
        dbg_exit "$fl not found"
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
    dbg_enter readVerilogDefines "$vfile"
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
    dbg_exit "$vfile"
}

proc ::userLoadView { inx } {
    global gTextArea gCurrViewIsBsv gCurrTreeIndex
    global gInfoDefof gTreeInfo
    
    set mod     $gTreeInfo($inx:mod)
    set infomod [getInfoMod $inx]

    if {$gCurrViewIsBsv==1} {
        set bpath $gTreeInfo($inx:bpath)
        set bfile $gInfoDefof($infomod:bfile)
        ::userOpenSource $bfile $mod $bpath
    } else {
        set vfile $gInfoDefof($infomod:vfile)
        ::userOpenSource $vfile $infomod
    }
    
    global gVerilogRef
    if {[info exists gVerilogRef]} { unset gVerilogRef }
    set vfile $gInfoDefof($infomod:vfile)
    ::readVerilogDefines $vfile
}

#############################################################################
## Procedure:  clickQuit
proc ::userMoveCursor {pnt} {
    global gTextArea
    set inx [$gTextArea index  $pnt]
    debug 5 "::userMoveCursor $pnt ($inx)"
    $gTextArea see $inx
    $gTextArea mark set insert $inx
}


proc ::clickQuit {} {
    dbg_enter clickQuit
    debug 0 "clickQuit"
    
    ::askToSaveFile
    ::userClose
    
    dbg_exit
    exit
}

proc ::clickSearch {} {
    dbg_enter clickSearch
    
    global gSearch_dir gSearch_exact gSearch_casesen 
    global gSearch_frombegin gSearch_string gSearch_regexp
    global gCurrentSelection gTextArea
    global gPrompt
    global gHitListIndex gHitList
    
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
	
        labelframe $f.opt1 -text {Direction}
        radiobutton $f.opt1.1 -text Forward  -variable gSearch_dir -value {-forward}
        radiobutton $f.opt1.2 -text Backward -variable gSearch_dir -value {-backward}
        grid $f.opt1.1 -in $f.opt1 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w
        grid $f.opt1.2 -in $f.opt1 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky w
	
        labelframe $f.opt2 -text {Type}
        checkbutton $f.opt2.1 -text Exact  -variable gSearch_exact
        checkbutton $f.opt2.2 -text Regexp -variable gSearch_regexp
        grid $f.opt2.1 -in $f.opt2 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w
        grid $f.opt2.2 -in $f.opt2 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky w
	
        labelframe $f.opt3 -text {Misc}
        checkbutton $f.opt3.1 -text {Case Sensitive} -variable gSearch_casesen
        checkbutton $f.opt3.2 -text {From Beginning} -variable gSearch_frombegin
        grid $f.opt3.1 -in $f.opt3 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky w
        grid $f.opt3.2 -in $f.opt3 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky w
	
        frame $f.b
        button $f.b.cancel -text Cancel -command { set gPrompt cancel }
        button $f.b.ok     -text Ok     -command { set gPrompt ok }
        grid $f.b.cancel   -in $f.b -column 0 -row 0 -columnspan 1 -rowspan 1
        grid $f.b.ok       -in $f.b -column 1 -row 0 -columnspan 1 -rowspan 1
	
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
    if {$res == "cancel"}  {
        dbg_exit cancel
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
    set hilight tagHilight1
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
        ::userHilightHitlist
        set pnts [lindex [lindex $gHitList 0] 1]
        set p1   [lindex $pnts 0]
        ::userMoveCursor $p1
    }
    
    dbg_exit
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
proc ::debCallback {args} {
    global gCurrTime gCurrSignal
    
    catch {
        set reason [lindex $args 0]
        dbg_enter debCallback "$args"
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
        dbg_exit
    }
    
}


proc ::userOpenWaves {} {
    global gWidget gTkAppList
    global gWaveState gDebApp
    global gFsdbFile gVcdFile 
    
    if {$gWaveState == "nowaves"} { return $gWaveState }
    if {$gWaveState == "loaded"}  { return $gWaveState }
    
    dbg_enter userOpenWaves gWaveState=$gWaveState
    
    # catch {[exec ps -C snslmgrd]} err
    # if {[regexp lmgrd $err] == 0} {
    # not running lmgrd right now - try to start it up!
    # puts "Status: lmgrd doesn't appear to be running"
    # puts "       : Novas nWave will not be able to run!"
    # puts "       : Please verify that it is running"
    # exit
    # }
    
    if {$gWaveState == "notstarted"} {
        # make sure it isn't 
        set gTkAppList [winfo interp]
        if {[lsearch $gTkAppList abc] >= 0} {
            puts "ERROR: nWave already running from another blueview?...."
            exit
        }
	
        # set deb  [exec which debussy]
        if {[catch {exec which nWave}]==0} {
            set nWave  [exec which nWave]
        } else {
            set nWave  {}
        }
        set gDebApp $nWave
	
        if {[file exists $gDebApp]} {
            # TODO - can I verify nWave is not alreay running?
            exec $gDebApp -tkName abc &
	    
            # dialog box to cancel?
            global gnWaveTimeout
            set timeout [expr $gnWaveTimeout * 4]
            for {set i 0} {$i < $timeout} {incr i} {
                after 250
                set gTkAppList [winfo interp]
                if {[lsearch $gTkAppList abc] >= 0} break
            }
	    
            # if we timed out, then we are done!
            if {[lsearch $gTkAppList abc] < 0} {
                puts " ERROR: it doesn't seem like nWave started running"
                puts "      : aborting blueview"
                exit
            }
	    
            debug 2 ":userOpenWaves launch $gDebApp!"
            set gWaveState nofsdbyet
	    
            # add callbacks needed
            set name [winfo name .]
            #send abc "AddEventCallback $name ::debCallback wvCloseWindow 1"
            #send abc "AddEventCallback $name ::debCallback wvCursorTimeChange 1"
            #send abc "AddEventCallback $name ::debCallback wvDblClkWaveForm   1"
            send abc "AddEventCallback $name ::debCallback UDR_GOTO_BSV 1"
            send abc "AddEventCallback $name ::debCallback UDR_GOTO_V 1"
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
        debug 1 ":userOpenWaves convert /$gVcdFile/$gFsdbFile"
        if {$gVcdFile != ""} {
            send abc "wvConvertFile -win abc -o $gFsdbFile $gVcdFile"
        }
        send abc "wvOpenFile    -win abc $gFsdbFile"
        send abc "wvZoomAll     -win abc"
        set gWaveState "loaded"
    }
    
    dbg_exit $gWaveState
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

########################################
proc fixHierTree {h {prefix _}} {
    foreach i $h {
        set tp [lindex $i 0]
        set bi [lindex $i 1]
        set vi [lindex $i 2]
        set ls [lindex $i 3]
        
        if {$prefix != "_"} {
            set vi "$prefix$vi"
        }
        
        if {$ls != "_"} {
            set ls [fixHierTree $ls "$vi\_"]
        }

        lappend res [list $tp $bi $vi $ls]
    }
    return $res
}


########################################
# returns bsv inst {instof} {defof} childtree
proc bsv-inst     { inst {lst _} } { return [list b $inst $inst $lst] }
proc verilog-inst { inst {lst _} } { return [list v $inst $inst $lst] }

########################################
proc ::userReadInfoFile {infofile} {
    dbg_enter userReadInfoFile $infofile
    global gProgess
    
    # suck all the information into internal structures so we can do stuff with it
    set ts [clock clicks]
    set fl [userFindAndOpen $infofile]
    set lines [split [read $fl] \n]
    close $fl
    set tm [expr [clock clicks] - $ts]; 
    debug 0 "  file read time = $tm"
    
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
    
    
    ################################################################################
    set ts [clock clicks]
    foreach line $newlines {
        if {[regexp {^\(bsv-inst} $line]} {
            ############################################################
            # already flattened out
            # turn into a tcl script
            regsub {^\(bsv-inst\s+"TOP"\s+} $line {} line
            regsub {\)\s*$}                 $line {} line
            # regsub -all {\"\"} $line null line
            regsub -all {\(} $line {[} line
            regsub -all {\)} $line {]} line
            regsub -all {\"} $line {} line
            
            debug 1 "Eval line: $line"
            
            eval "set hierTree $line"
            set hierTree [fixHierTree $hierTree]

            debug 1 "New HierTree: $hierTree"

        } elseif {[regexp {^\(add\-bsv\-id\-info} $line]} {
            ############################################################
            # this should execute [add-bsv-id-inst ...]

            regsub -all {\"\"} $line null line
            regsub -all {(\(|\)|\")} $line {} line  ;# turn lisp code into tcl code
            regsub -all {[ \t]+} $line { } line     ;# extra spaces

            # puts $line
	    
            # mod inst vfile vline vcol bfile bline bcol typ {infoname} 
            set items [split $line " "]
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
	    
            regsub {^.*/} $vfile {} vfile
	    
            # skip any reference to .bs or other file
            #  ONLY local .bsv files?
            if {[regexp {\.bsv$} $bfile] == 0} continue
            if {$mod != $lmod} continue


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

            } elseif {$typ == "FLATTENED_DEFOF"} {
                ############################################################
                set infomod [lindex $items 10]
                set fmod    $infomod

                debug 5 "Add $typ gInfoDefof($fmod) $vfile $vpos $bfile $bpos $lmod"
                global gInfoDefof

                # there is no verilog file, it is flattened into the parent!
                # set gInfoDefof($fmod:vfile)   $lmod.v
                # set gInfoDefof($fmod:vpos)    1.1
                set gInfoDefof($fmod:bfile)     $bfile
                set gInfoDefof($fmod:bpos)      $bpos
                set gInfoDefof($fmod:type)      $typ
                set gInfoDefof($fmod:info)      $lmod
                
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

                regsub {/} $vinst {.} key

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

                set tmp(vfile)  $vfile
                set tmp(vpos)   $vpos
                set tmp(bfile)  $bfile
                set tmp(bpos)   $bpos
                set tmp(hier)   $hier
                set tmp(binst)  $binst
                set tmp(vinst)  $vinst
                set tmp(info)   $infoinst

                global gInfoInstof gInfoInstofList
                set gInfoInstof($lmod:$key) [array get tmp]
                lappend gInfoInstofList($lmod) $key

                debug 1 "Add INSTOF: $lmod:$key => $infoinst $vfile $vpos $bfile $bpos {$hier}{$binst}"
		
            } elseif {($typ == "NET") || ($typ == "ASSIGN")} {
                ############################################################
                if {($inst != "UNNAMED") && ([regexp {\.bsv$} $bfile])} {
                    set f [split $inst {\.}]
                    set tmp(signal) [llast $f]
                    set tmp(hier)   [lfront $f]
                    set tmp(vfile)  $vfile
                    set tmp(vpos)   $vpos
                    set tmp(bfile)  $bfile
                    set tmp(bpos)   $bpos
                    set tmp(type)   $typ

                    # don't redo this for every instance - too much data
                    #   and it's always the same anyway?
                    global gInfoNet
                    lappend gInfoNet($lmod) [array get tmp]
                    debug 5 "Add NET: $lmod $inst $vfile $vpos $bfile $bpos $typ"
                }
		
            } elseif {$typ != "IGNORE"} {
                debug 6 "unknown type of add-bsv-id-info called with \"$typ\""
            }
        }
    }
    
    ############################################################
    # now create new hier tree with all the relevant info in it
    # we have a bsv instance name, but I really need a verilog
    # instance name, since that is what will be unique in the end
    global gHierarchy
    catch {set gHierarchy($mod) $hierTree}

    ############################################################
    set tm [expr [clock clicks] - $ts];
    debug 0 "  clock \#1 time = $tm"
    
    dbg_exit
    return
}

################################################################################
# recurse wave to read and load info files
# rules


#############################################################################
################################################################################
# signal = Tb.TOP.mesa_inst.the_lpm.mif_response_get_avValue_fst
proc ::debShowSignal {isBsv} {
    global gInst2TreeIndex gTreeInfo gInfoNet gTreeArea
    global gCurrTreeIndex gCurrViewIsBsv gCurrPosBsv gCurrPosV
    
    # get the first selected signal
    set signals [send abc "wvGetSelectedSignals -win abc"]
    if {[info exists signals]==0} {
        userMessageBox Error [list "No signals selected"]
        return
    }
    
    dbg_enter debShowSignal "selected $signals"
    
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
        dbg_exit
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
        dbg_exit
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
    ::userOpenSource $fl $mod $bpath
    
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
    
    dbg_exit
}

#############################################################################
proc ::askToSaveFile {} {
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
proc ::userOpenSource {fl mod {bpath {}} } {
    global gWidget gTopWindow
    global gTextArea
    global gCurrViewIsBsv gCurrPosBsv gCurrPosV
    dbg_enter userOpenSource "$fl $mod {$bpath}"
    
    # get a .bsv or .v file and dump it, with colors into viewing area!
    # if {$fl == ""} {
    # set file [tk_getOpenFile]
    # if {$fl == ""} return
    # }
    
    $gTextArea delete 1.0 end
    
    wm title . [file tail $fl]
    set fp [userFindAndOpen $fl]
    if {$fp == 0} {
        dbg_exit "file not found $fl $mod"
        return
    }
    $gTextArea insert end [read $fp]
    close $fp
    
    userUntabify

    global gTextModified
    set gTextModified 0
    
    userStatus "Opening $fl"
    
    # we are now looking at the target file type
    if {[regexp {\.(bsv|bs)$} $fl]} {
        ::userLoadColorInfoBsv $mod $fl $bpath
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
    dbg_exit 
}


proc ::getCurrentFileView {} {
    global gTreeInfo gCurrTreeIndex gCurrViewIsBsv gInfoDefof    

    if {$gCurrViewIsBsv==0} {
        set mod [getInfoMod $gCurrTreeIndex]
        set thisfile $gInfoDefof($mod:vfile)
    } else {
        set mod $gTreeInfo($gCurrTreeIndex:mod)
        set thisfile $gInfoDefof($mod:bfile)
    }
}

proc ::userHilightHitlist {} {
    global gTextArea gHitList gHitListIndex
    
    dbg_enter userHilightHitlist
    
    # get filename we are looking
    set thisfile [getCurrentFileView]
    
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
                set hilight tagHilight1
            } else {
                set hilight tagHilight2
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
            }
        }
	
        # move to first item on hitlist
        # set pnts [lindex [lindex $gHitList $gHitListIndex] 1]
        # set p1   [lindex $pnts 0]
        # ::userMoveCursor $p1
    }
    
    dbg_exit
}

proc ::userUnHilightHitlist {} {    
    global gTextArea gHitList gHitListIndex
    $gTextArea tag remove tagHilight1 1.0 end
    $gTextArea tag remove tagHilight2 1.0 end
    if {[info exists gHitList]} { unset gHitList }
    set gHitListIndex -1
}


#############################################################################
proc ::userUntabify {} {
    global gTextArea gTabStops
    
    set curpos 1.0
    while 1 {
        set hit [$gTextArea search -forward {\t} $curpos]
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

proc ::userComment {} {
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
proc ::userTag {seltxt style} {
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
proc ::replaceVerilogIndicies {mod} {
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
proc ::getCharAtIndex {start} {
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

proc getEndOfVarOrOther {start} {
    global gTextArea
    
    set tx [getCharAtIndex $start]
    set en [$gTextArea index "$start lineend"]
    if {[regexp {[\w\d]} $tx]} {
        # if it starts with a A-Za-z0-9 then scan as variable
        set we [$gTextArea search -forward -regexp {[^\w\d\$\._]} $start $en]
	
    } else {
        # otherwise it may be == or <= or unknown
        set we [$gTextArea search -forward -regexp {[^=<\+\*\-\&\|\^]} $start $en]
    }
    
    if {$we == {}} {
        set we $en
    }
    return $we
}

proc ::userLoadColorInfoBsv {mod targetfile {bpath {}}} {
    global gWidget gTextArea 
    global gInst2TreeIndex gTreeInfo gInfoNet gCurrTreeIndex
    
    dbg_enter userLoadColorInfoBsv "$mod $targetfile {$bpath}"
    
    set infomod [getInfoMod $gCurrTreeIndex]
    debug 1 "info mod = $infomod"

    if {[info exists gInfoNet($infomod)]} {
        # get the waves for this file
        foreach f $gInfoNet($infomod) {
            array set tmp $f
            if {$targetfile != $tmp(bfile)} {
                # debug 3 "  skip $tmp(bfile) != $targetfile"
                continue
            }
            if {$tmp(bpos) != "-1.-1"} {
                set start $tmp(bpos)
                set waveArray($start) 1
            }
        }
	
        # only tag them once, since this is slow
        foreach start [array names waveArray] {
            # start, search to end of chars of interest (I hope)
            set en [getEndOfVarOrOther $start]
            debug 4 "waveable bsv at $start $en"
            $gTextArea tag add tagWaveable $start $en
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
    dbg_exit
}

################################################################################
proc ::userLoadColorInfoV {mod targetfile} {
    global gWidget
    global gTextArea 
    global gInfoNet
    
    # these are the things we can select, and hopefully print out
    # set point-min [$gTextArea index 0]
    if {[info exists gInfoNet($mod)]} {
        foreach f $gInfoNet($mod) {
            array set tmp $f
            if {$targetfile != $tmp(vfile)} continue
	    
            set vpos $tmp(vpos)
            if {$vpos == "-1.-1"} {
                debug 2 "  strange cross ref indexes for verilog file: $vpos"
                debug 2 "    line = $f"
		
            } else {
                set vend [getEndOfVarOrOther $vpos]  
                $gTextArea tag add tagWaveable $vpos $vend
                debug 4 "waveable verilog at $vpos $vend"
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
################################################################################
# this routine is used to determine the begin and end of a waveable signal 
#   (though it could be used for any tag ;)
proc ::getTagRange {t tag mark} {
    set range [$t tag prevrange $tag $mark]
    set str   [lindex $range 0]
    set end   [lindex $range 1]
    
    # dbg_enter getTagRange $range
    
    if {[llength $range] == 0 || [$t compare $end < $mark]} {
        set range [$t tag nextrange $tag $mark]
        if {[llength $range] == 0 || [$t compare $mark < [lindex $range 0]]} {
            # dbg_exit
            return {}
        }
    }
    # dbg_exit $range
    return $range
}

################################################################################
################################################################################
proc ::getSelection { } {
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
    debug 5 "tags found $tags"
    if {[info exists tags] && ([lindex $tags 0] == "tagWaveable")} {
        # back up to start of word where waveable attribute starts
        # goto next, then back to previous since I'm not sure
        #   exactly how to get this range without a gross search
        set gCurrentSelection [getTagRange $gTextArea tagWaveable $start]
        
        # debug 1 "selected [$gCurrentSelection] from insert cursor (start was $start)"
        return $gCurrentSelection
    }
    
    # if no word here (i.e. a space) then sol
    return {}
}

################################################################################
proc ::clickSwitchToRelatedView { } {
    global gMenuDone gTextArea
    global gCurrViewIsBsv gCurrPosV gCurrPosBsv gCurrTreeIndex
    
    dbg_enter clickSwitchToRelatedView
    
    # get selection
    set range [::getSelection]
    if {[llength $range] == 0} {
        userMessageBox Error [list "no range selected "]
        dbg_exit
        return
    }
    
    userUnHilightHitlist
    debug 10 "Current positions are $gCurrPosV (v) and $gCurrPosBsv (bsv)"
    
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
        }
    }
    
    if {$err == 0} {
        userLoadView $treeinx       ;# load target file
        userHilightHitlist          ;# color everything that's relevant
        userMoveCursor $inx         ;# leave the cursor in the right place
	
    } else {
        userMessageBox Warning [list "No matching signals found in crossreference" \
                                    "Please select a 'waveable' signal" \
                                    "higlighted with preference variable 'gTagWaveable'"
                               ]
    }
    
    set gMenuDone 1
    dbg_exit
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
        userStatus "Paste not allowed in verilog files or readonly bsv file"
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
        userStatus "Paste not allowed in verilog files or readonly bsv file"
    }
}

proc selectAll {} {
    global gCurrentSelection gTextArea
    $gTextArea tag remove sel 1.0 end
    $gTextArea tag add    sel 1.0 end
    set gCurrentSelection [$gTextArea tag ranges sel]
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

        clickTreeSelectAction bsv $i

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
proc ::openSplashScreen { mess } {
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
    ############################################################
    if {[info exists env(BLUESPECDIR)] == 0} {
        puts "ERROR: environment variable BLUESPECDIR not set"
        exit
    }

    global gIgnoreLibraryModules
    if {[info exists gIgnoreLibraryModules]} {
        set libdir [file join $env(BLUESPECDIR) Verilog]
        eval [list exec ls] [glob $libdir/*.v] > $tmp
        
        set libdir [file join $env(BLUESPECDIR) Prelude]
        catch {eval [list exec ls] [glob $libdir/*.bi] >> $tmp}
        
        set libdir [file join $env(BLUESPECDIR) Libraries]
        catch {eval [list exec ls] [glob $libdir/*.bi] >> $tmp}
        
        # read and split lines up
        set lines [readLines $tmp] 
    
        global gLibraryModule
        foreach line $lines {
            if {[regexp {([\w\d_]+).v$} $line mn m1]} {
                set gLibraryModule($m1) 1
                debug 5 "Library Element: $m1"
                
            } elseif {[regexp {([\w\d_]+).bi$} $line mn m1]} {
                set gLibraryModule(mk$m1) 1
                debug 5 "Library Element: mk$m1"
            }
        }
    }
    
    ############################################################
    file delete -force $tmp
    set gProgressText "Initializing data structures"
    update idletasks
}

proc ::closeSplashScreen {} {
    destroy .top.m
}


#############################################################################
#############################################################################
## Procedure:  main
proc ::usage {} {
    puts { usage: blueview [opts] [foo.info bar.vcd /Tb/TOP]}
    puts {    or: blueview [opts] [foo.info bar.fsdb /Tb/TOP]}
    puts {}
    puts {  Opts:  -help        this menu}
    puts {      :  -nowaves     do not open nWave}
    puts {      :  -path        search path (separated by :) to find .info files, etc}
    puts {      :  -v           version number only}
    puts {      : see man page  and docs for more detail}
    puts {  Args: foo.info              name of top level .info file}
    puts {      : /Tb/TOP               verilog instance path to top level .info file}
    puts {      : bar.vcd OR bar.fsdb   dump file to use}
    exit
}

proc ::main {argc argv} {
    global gWidget
    global gWaveState
    global gStatusArea gTextArea gTreeArea gSearchPath
    global gDebug gVersion gBuild gdbIndent
    global gPreferences gPwd gTextModified gCurrPosBsv 
    global gLogDir gTreeMaxIndex
    
    set gTextModified 0
    
    # leave debugging on for a while
    set gdbIndent ""
    catch {[exec mkdir -p $gLogDir]}
    set dbglog [file join $gLogDir dbg.log]
    set gDebug [open $dbglog w]
    
    # might as well not waste time if license check fails..
    # ::checkLicense
    
    # get easier/faster aliases from vtcl
    # gVTcl:DefineAlias "$site_5_1.txt.tex54" "TextArea" gVTcl:WidgetProc "Toplevel1" 1
    set gStatusArea $gWidget(StatusArea)
    set gTextArea   $gWidget(TextArea)
    set gTreeArea   $gWidget(TreeArea)
    set gSearchPath [list .]
    
    set gPwd [eval pwd]
    
    # preferences
    global gTagKeyword gTagDatatype gTagLibrary gTagModselect
    global gTagHilight1 gTagHilight2 gTagComment gTagWaveable
    global gDebugLevel
    
    eval $gTextArea tag configure  tagKeyword  $gTagKeyword
    eval $gTextArea tag configure  tagDatatype $gTagDatatype 
    eval $gTextArea tag configure  tagLibrary  $gTagLibrary
    eval $gTextArea tag configure  tagHilight1 $gTagHilight1
    eval $gTextArea tag configure  tagHilight2 $gTagHilight2
    eval $gTextArea tag configure  tagComment  $gTagComment
    eval $gTextArea tag configure  tagWaveable $gTagWaveable
    eval $gTextArea tag configure  tagModselect $gTagModselect
    
    # waveable signals can be selected so done let them go away
    $gTextArea tag lower tagModselect
    $gTextArea tag lower tagWaveable
    $gTextArea tag raise tagComment
    
    # not used anymore?
    # set gPreferences(tagCursor)   -background black -foreground red
    # $gTreeArea tag configure tagSelected  -foreground green
    # $gTreeArea tag configure tagNormal    -foreground black
    $gStatusArea delete 1.0 end
    ::userStatus "Blueview version $gVersion, build $gBuild started in $gPwd"
    
    set gWaveState notstarted
    if {$argc > 0} {
        userStatus "Called with arguments '$argv'"
    }
    
    set gTreeMaxIndex 0
    
    set dumpfile {}
    set infofile {}
    set path     {}
    
    if {$argc > 0} {
        set i 0
        while {$i < $argc} {
            set arg [lindex $argv $i]
            if {$arg == "-dbg"} {
                # TODO: already enabled make this optional in next rev?
            } elseif {$arg == "-help"} {
                usage
            } elseif {$arg == "-nowaves"} {
                set gWaveState nowaves
            } elseif {$arg == "-dumplog"} {
                global gDumpLog
                incr i
                set gDumpLog [lindex $argv $i]
            } elseif {$arg == "-debug"} {
                incr i
                set gDebugLevel [lindex $argv $i]
            } elseif {$arg == "-path"} {
                incr i
                set gSearchPath [split [lindex $argv $i] {:}]
            } elseif {$arg == "-v"} {
                puts "Blueview version $gVersion, build $gBuild"
                exit
            } elseif {[regexp {(.vcd$|.fsdb$)} $arg]} {
                if {[file exists $arg]} {
                    debug 1 "  dumpfile = $arg"
                    set dumpfile $arg
                } else {
                    debug 1 "  ERROR: $arg supplied but file not found"
                    puts    "  ERROR: $arg supplied but file not found"
                }
            } elseif {[regexp {.info$} $arg]} {
                debug 1 "  infofile = $arg"
                set infofile $arg
            } elseif {[regexp {^/} $arg]} {
                debug 1 "  path     = $arg"
                set path $arg
            } elseif {[regexp {^-} $arg]} {
                debug 1 "unknown switch $arg"
                puts  "unknown switch $arg"
                usage
                exit
            } else {
                debug 1 "unknown argument - can't determine what '$arg' is for..."
                usage
                exit
            }
            incr i
        }
    }
    
    
    ############################################################
    # we need a path to the top level .info file
    if {$path=={}} {
        if {$dumpfile == ""} {
            # gonna have to specifiy this later - via menu options
            set path "/DEFAULT_TOP"

        } elseif {[regexp {\.vcd$} $dumpfile]} {
            userStatus "no path given, attempting to deduce from $dumpfile" 
            # use grep so we don't have to read entire file
            set tmp ".tmp[pid]"
            eval [list exec grep scope $dumpfile] > $tmp
            set lines  [readLines $tmp]
            file delete -force $tmp
            
            # go two levels deep for now - talk about a *hack*
            foreach line $lines {
                if {[regexp {^\$upscope} $line]} {
                    set scope [lfront $scope]
                    break
                } elseif {[regexp {^\$scope} $line]} {
                    set f [split $line " "]
                    lappend scope [lindex $f 2]
                    if {[llength $scope] >= 2} break
                }
            }
            set path [makepath $scope]
            userStatus "deduced path of $path, I hope this works!"

        } else {
            puts "Can't automatically determine path to top from a .fsdb file yet"
            usage
            exit
        }

    }
    
    ############################################################
    # need one of the following, for assorted reasons
    # just so we don't have to check every step of the way
    if {0} {
        set argcnt 0
        if {$infofile != {}} { incr argcnt 4 }
        if {$path     != {}} { incr argcnt 2 }
        if {$dumpfile != {}} { incr argcnt 1 }

        puts "Argc cnt = $argcnt ($infofile $path $dumpfile)"
        if {$argcnt == 0 || $argcnt >= 4} {
            #  prequalify the arguments
            #  blueview.tcl                   000
            #  blueview.tcl .info path        1x0
            #  blueview.tcl .info       .vcd  101
            #  blueview.tcl .info path  .fsdb 111
        } else {
            usage
            exit
        }
    }
    
    
    ############################################################
    if {$dumpfile=={}} {
        userStatus "No .vcd or .fsdb file, starting without wave capabilities"
        set gWaveState nowaves
    }
    
    
    ############################################################    
    set gCurrPosBsv [list "1.0" "1.0"]
    set gCurrPosV   [list "1.0" "1.0"]
    
    if {$infofile != ""} {
        # TODO: only need verilog inst once?
        ::userOpenDump $infofile $path $dumpfile 
    }
    
    if {$gWaveState == "notstarted"} {
        global gProgressText
        set gProgressText "starting nWave...."
        update idletasks
        puts "Initializing nWave"
        ::userOpenWaves
    }
    
    
    if {[info exists gDumpLog]} {
        after 1000
        set dir [exec pwd]
        puts "Blueview build $gBuild dump to $gDumpLog from $dir"
        dumpAll
        clickQuit
    }
}



#############################################################################
## Initialization Procedure:  init
proc ::init {argc argv} {
    global gTkAppList
    global gCurrTreeIndex
    global gCurrPosBsv gCurrPosV gHitListIndex gReadOnly
    global gSearch_dir gSearch_exact gSearch_casesen gSearch_frombegin gSearch_regexp
    
    set gTkAppList [winfo interp]
    set gCurrTreeIndex 0
    set gCurrPosBsv [list "1.0" "1.0"]
    set gCurrPosV   [list "1.0" "1.0"]
    set gHitListIndex -1
    set gReadOnly 1
    set gSearch_dir {-forward}
    set gSearch_exact 1
    set gSearch_regexp 0
    set gSearch_casesen 0
    set gSearch_frombegin 0
    
    global gConvert
    set gConvert(__NULL__) 1   ;# just so it doesn't barf
}

init $argc $argv

################################################################################
################################################################################
proc clickTreeSelectAction { {target bsv} {inx -1} } {
    global gTreeInfo gCurrTreeIndex gTreeArea gCurrViewIsBsv gTextArea
    global gInfoDefof
    
    # get index into tree info from node number, since that's
    # what we index *every* thing by
    if {$inx < 0} {
        # normal operation
        set i [$gTreeArea selection get]
    } else {
        # debug - program can select
        set i $inx
    }
    
    dbg_enter clickTreeSelectAction
    
    ::askToSaveFile
    
    set gCurrTreeIndex $i
    set mod  $gTreeInfo($i:mod)
    
    # open BSV if available, open V if bsv has a different name?????
    debug 1 "look for gInfoDefof($mod:bfile) $i"
    
    # start at selection and go up tree until we find something defined
    
    set file ""
    set pos  "1.0"
    set bpath {}
    
    if {$target == "bsv"} {
        ##################################################
        set bpath $gTreeInfo($i:bpath)
        set inst [llast $bpath]
        
        if {[info exists gInfoDefof($mod:bfile)]} {
            set mod   $gTreeInfo($i:mod)
            set file  $gInfoDefof($mod:bfile)     ;# find bfile from module information
            set pos   $gInfoDefof($mod:bpos)
            debug 1 "found $file $pos for $mod"
        }
        
    } else {
        ##################################################
        set mod   [getInfoMod $i]
        set vpath $gTreeInfo($i:vpath)
        set file  $gInfoDefof($mod:vfile)
        set pos   $gInfoDefof($mod:vpos)
    }
    
    if {($file == "") || ($pos == "1.0") || ($pos == "-1.-1")} {
        userStatus "no $target file available for this level of hierarchy"
        dbg_exit 
        return
    }
    
    ##################################################
    if {[userFindFile $file]} { 
        ::userOpenSource $file $mod $bpath
        set endpos [getEndOfVarOrOther $pos]
        debug 2 "gTagModselect at $pos $endpos"
	
        global gTextArea gWaveState
        $gTextArea tag add tagModselect $pos $endpos
        userMoveCursor $pos
    }
    dbg_exit 
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
    wm geometry $top 811x718+386+161; update
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
    global gTreeColors
    ScrolledWindow $site_5_0.tr
    eval Tree $site_5_0.tr.lis51 $gTreeColors 
    $site_5_0.tr setwidget $site_5_0.tr.lis51
    
    gVTcl:DefineAlias "$site_5_0.tr.lis51" "TreeArea" gVTcl:WidgetProc "Toplevel1" 1
    
    grid columnconf $site_5_0 0 -weight 1
    grid rowconf $site_5_0 0 -weight 1
    grid $site_5_0.tr -in $site_5_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw
    
    set site_5_1 [$site_3_0.pan50 getframe 1]
    
    ############################################################
    global gTextColors
    ScrolledWindow $site_5_1.txt
    eval text $site_5_1.txt.tex54 $gTextColors -wrap none 
    $site_5_1.txt setwidget $site_5_1.txt.tex54
    
    gVTcl:DefineAlias "$site_5_1.txt.tex54" "TextArea" gVTcl:WidgetProc "Toplevel1" 1
    grid columnconf $site_5_1 0 -weight 1
    grid rowconf $site_5_1 0 -weight 1
    grid $site_5_1.txt -in $site_5_1 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    
    ############################################################
    grid $site_3_0.pan50 -in $site_3_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    frame $top.fra49 -borderwidth 2 -relief groove -height 75 -width 125 
    gVTcl:DefineAlias "$top.fra49" "Frame2" gVTcl:WidgetProc "Toplevel1" 1
    set site_3_0 $top.fra49
    button $site_3_0.lab80 -image [image create photo -data [userGetLogo]] -command { opendebugger }
    
    gVTcl:DefineAlias "$site_3_0.lab80" "Label2" gVTcl:WidgetProc "Toplevel1" 1
    
    ############################################################
    global gStatusColors gTextFont
    eval text $site_3_0.tex81 $gStatusColors -height 3 \
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
    set keys [split $gKeyList(opendump) ':']
    $site_3_0.men75 add command -command menuOpenDump  -label {Open Dump} -accelerator  [lindex $keys 0]
    $site_3_0.men75 add command -command userOpenWaves -label {Open nWave}
    
    $site_3_0.men75 add separator
    $site_3_0.men75 add command -command menuSaveBsv   -label {Save}
    $site_3_0.men75 add command -command menuSaveAsBsv -label {Save As}
    $site_3_0.men75 add check   -label "BSV File ReadOnly" -variable gReadOnly
    $site_3_0.men75 add separator
    $site_3_0.men75 add command -command menuQuit -label Quit 
    
    
    #### file menu
    $top.m74 add cascade -menu "$top.m74.men76" -command {} -label Edit
    set site_3_0 $top.m74
    menu $site_3_0.men76 -tearoff 0 
    
    # select all
    $site_3_0.men76 add command -command textCut    -label Cut -accelerator <Ctrl-x>
    $site_3_0.men76 add command -command textCopy   -label Copy -accelerator <Ctrl-c>
    $site_3_0.men76 add command -command textPaste  -label Paste -accelerator <Ctrl-v>
    $site_3_0.men76 add separator
    
    $site_3_0.men76 add command -command selectAll  -label SelectAll
    
    set keys [split $gKeyList(search) ':']
    $site_3_0.men76 add command -label {Search} -accelerator [lindex $keys 0] \
        -command { ::clickSearch }
    
    set keys [split $gKeyList(searchagain) ':']
    $site_3_0.men76 add command -label {Search Again}  -accelerator [lindex $keys 0] \
        -command { global gTextArea ; clickRotateMatch; break }
    # clickSearchAgain [$gTextArea index insert] 
    
    #### help menu
    $top.m74 add cascade -menu "$top.m74.men77" -command {} -label Help
    set site_3_0 $top.m74
    menu $site_3_0.men77 -tearoff 0 

    # global env 
    # if {$env(BLUEVIEW_MENU_DUMP_ALL) == "sallen"} { }
    $site_3_0.men77 add command -command dumpAll   -label DumpSnapshot
    $site_3_0.men77 add command -command menuHelp  -label {Help}
    $site_3_0.men77 add command -command menuAbout -label {Help About}
    
    
    ############################################################
    ############################################################
    bind $site_5_0.tr.lis51.c <Control-ButtonPress-1> { break }
    bind $site_5_0.tr.lis51.c <Shift-ButtonPress-1>   { ::clickTreeSelectAction verilog }
    bind $site_5_0.tr.lis51.c <ButtonPress-1>         { ::clickTreeSelectAction bsv }
    
    # bind $site_5_0.tr.lis51 <<TreeSelect>> {} 
    
    # TODO read only - unless we want to edit?
    bind $site_5_1.txt.tex54 <Any-Key> { 
        global gReadOnly gCurrViewIsBsv gTextModified
        if {($gReadOnly==1) || ($gCurrViewIsBsv==0)} break
        set gTextModified 1
    }
    
    foreach k { <Control-a> <Control-b> <Control-e> <Control-p> <Control-n> \
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
    
    # don't allow paste in a read only view
    bind $site_5_1.txt.tex54 <Button-2> { 
        global gReadOnly gCurrViewIsBsv
        # puts "Key hit: %K"
        if {($gReadOnly==1) || ($gCurrViewIsBsv==0)} break
    }
    
    # allow these basic editing commands to pass 
    foreach key [split $gKeyList(opendump) ':'] {
        # these work at top level only!
        bind .top $key { ::menuOpenDump; break }
    }
    
    foreach key [split $gKeyList(help) ':'] {
        bind $site_5_1.txt.tex54 $key { ::menuHelp; break }
    }
    
    foreach key [split $gKeyList(quit) ':'] {
        bind $site_5_1.txt.tex54 $key { ::clickQuit; break }
    }
    
    foreach key [split $gKeyList(search) ':'] {
        bind $site_5_1.txt.tex54 $key { ::clickSearch; break }
    }
    
    foreach key [split $gKeyList(searchagain) ':'] {
        bind $site_5_1.txt.tex54 $key { 
            ::clickRotateMatch
            # global gTextArea; clickSearchAgain [$gTextArea index insert]
            break
        }
    }
    
    # bind $site_5_1.txt.tex54 $key { ::clickRotateMatch; break }
    
    set m [menu .top.popup1 -tearoff 0]  ;# create popup menu
    
    foreach key [split $gKeyList(findsignalinV) ':'] {
        bind $site_5_1.txt.tex54 $key { ::clickSwitchToRelatedView; break }
        $m add command -label "Switch view to related signals" \
            -command { ::clickSwitchToRelatedView } -accelerator $key
    }
    
    #    foreach key [split $gKeyList(nextsignal) ':'] {
    #        bind $site_5_1.txt.tex54 $key { userSwitchToNextSignal ; break }
    #        $m add command -label "Next related signal" -command { userSwitchToNextSignal } -accelerator $key
    #    }
    
    
    foreach key [split $gKeyList(searchagain) ':'] {
        $m add command -label "Find next related signal" -command { ::clickRotateMatch } -accelerator $key
    }
    
    foreach key [split $gKeyList(sendwaves) ':'] {
        bind $site_5_1.txt.tex54 $key { ::clickShowSignals ; break }
        $m add command -label "Send Signals to nWave" -command { ::clickShowSignals } -accelerator $key
    }
    
    $m add command -label "Send All Signals to nWave" -command { 
        ::selectAll
        ::clickShowSignals 
    }
    
    # bind $site_5_1.txt.tex54 <<CutSelection>>   break
    # bind $site_5_1.txt.tex54 <<PasteSelection>> break
    
    # pop up menu for search, match, etc
    bind $site_5_1.txt.tex54 <Button-3> { 
        global gTextArea
        global gMenuDone
	
        set m .top.popup1
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

Window show .
Window show .top

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

main $argc $argv

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
# - hit <CR> in opendump dialog
# - save/initial values in opendump dialog
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

