#!/bin/sh
### -*- Tcl -*- ##############################################################
###
### (c) Copyright 2004 - 2008, Bluespec Incorporated.
###
### Time-stamp: <2005-01-11 10:30:46 baltus>
###
### $Id$
### $Source:  $
###
##############################################################################
# \
exec tclsh "$0" ${1+"$@"}

################################################################################
###
################################################################################

proc main {argc argv} {
    global gWidget
    global gWaveState gLicVerdi
    global gStatusArea gTextArea gTreeArea gSearchPath
    global gDebug gVersion gBuild gdbIndent
    global gPreferences gPwd gTextModified gCurrPosBsv 
    global gLogDir gTreeMaxIndex
    global gFileNameLabel
    
    set gTextModified 0
    set gLicVerdi 0

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
    global gColorList
    global gDebugLevel
    
    eval $gTextArea tag configure  tagKeyword      $gColorList(tagkeyword)
    eval $gTextArea tag configure  tagDatatype     $gColorList(tagdatatype)
    eval $gTextArea tag configure  tagLibrary      $gColorList(taglibrary)
    eval $gTextArea tag configure  tagSelection    $gColorList(tagselection)
    eval $gTextArea tag configure  tagSelectionSet $gColorList(tagselectionset)
    eval $gTextArea tag configure  tagComment      $gColorList(tagcomment)
    eval $gTextArea tag configure  tagWaveable     $gColorList(tagwavable)
    eval $gTextArea tag configure  tagXRef         $gColorList(tagxref)
    eval $gTextArea tag configure  tagModselect    $gColorList(tagmodselect)
    eval $gTextArea tag configure  tagModselectFg  $gColorList(tagmodselectfg)
    
    # waveable signals can be selected so done let them go away
    $gTextArea tag lower tagWaveable
    $gTextArea tag lower tagXRef
    $gTextArea tag raise tagComment
    $gTextArea tag raise tagModselect
    $gTextArea tag raise tagModselectFg
    
    # not used anymore?
    # set gPreferences(tagCursor)   -background black -foreground red
    # $gTreeArea tag configure tagSelected  -foreground green
    # $gTreeArea tag configure tagNormal    -foreground black
    $gStatusArea delete 1.0 end
    ::userStatus "Blueview version $gVersion, build $gBuild started in $gPwd"
    
    set gWaveState notstarted
    if {$argc > 0} {
        userStatus "Called with arguments: '$argv'"
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
            } elseif {$arg == "-licverdi"} {
                set gLicVerdi 1
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
    
    ################################################################################
    ### we need a path to the top level .info file
    ################################################################################

    if {$path=={}} {

	if {[regexp {\.fsdb$} $dumpfile]} {

            puts "Can't automatically determine path to top from a .fsdb file yet"
            usage
            exit

	} else {

	    set path [update_path_value $path $dumpfile]

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
        userStatus "No .vcd or .fsdb file, starting without wave capabilities."
        set gWaveState nowaves
    }
    
    
    ############################################################    
    set gCurrPosBsv [list "1.0" "1.0"]
    set gCurrPosV   [list "1.0" "1.0"]

    if {$infofile != ""} {
        # TODO: only need verilog inst once?
        ::userOpenSources $infofile $path $dumpfile 
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

################################################################################
###
################################################################################

proc usage {} {
    puts { usage: blueview [opts] [foo.info bar.vcd /Tb/TOP]}
    puts {    or: blueview [opts] [foo.info bar.fsdb /Tb/TOP]}
    puts {}
    puts {  Opts:  -help        this menu}
    puts {      :  -nowaves     do not open nWave}
    puts {      :  -path        search path (separated by :) to find .info files, etc}
    puts {      :  -licverdi    use -licverdi flag when calling nWave}
    puts {      :  -v           version number only}
    puts {      : see man page  and docs for more detail}
    puts {  Args: foo.info              name of top level .info file}
    puts {      : /Tb/TOP               verilog instance path to top level .info file}
    puts {      : bar.vcd OR bar.fsdb   dump file to use}
    exit
}

################################################################################
###
################################################################################

proc update_path_value {path vcdfile} {

    set new_path $path

    if {$new_path=={}} {

        if {$vcdfile == ""} {

            # gonna have to specifiy this later - via menu options
#            set path "/DEFAULT_TOP"

        } elseif {[regexp {\.vcd$} $vcdfile]} {

	    userStatus "No top instance name given, attempting to deduce from $vcdfile." 
            set new_path [deduce_path_from_vcd $vcdfile]
            userStatus "Deduced instance name of '$new_path' (if this is incorrect, specify it explicitly)."

        }
    }

    return $new_path
}

################################################################################
###
################################################################################

proc deduce_path_from_vcd {vcdfile} {

    # use grep so we don't have to read entire file
    set tmp ".tmp"
    append tmp [pid]
    eval [list exec grep scope $vcdfile] > $tmp
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

    return [makepath $scope]

}

################################################################################
###
################################################################################

proc deduce_hack_from_vcd {vcdfile} {

    if { ![file exists $vcdfile] } {

	return -1

    }

    set count_0 0
    set count_1 0

    catch {[set count_0 [exec grep -l -E {\[[0-9]+:} $vcdfile]]}
    catch {[set count_1 [exec grep -l -E { \[[0-9]+:} $vcdfile]]}

    if {($count_0 != 0) && ($count_1 == 0)} {

	# there are no spaces before the bracket in bus values ... i.e. a[31:0]
	return 1

    } else {

	# there are spaces before the bracket in bus values ... i.e. a [31:0]
	return 0

    }
}

################################################################################
###
################################################################################
## Initialization Procedure:  init

proc init {argc argv} {
    global gTkAppList
    global gCurrTreeIndex
    global gCurrPosBsv gCurrPosV gHitListIndex gReadOnly
    global gSearch_dir gSearch_exact gSearch_casesen gSearch_frombegin gSearch_regexp
    global gInfoFile gVcdFile gInstance
    global gHackMode gDebussyProcName
    global gLogFileName

    set gInfoFile  ""
    set gVcdFile  ""
    set gInstance  ""

    set gDebussyProcName [pid]_proc
    set gLogFileName blueview_[pid].log
    
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

    set gHackMode -1
    
    global gConvert
    set gConvert(__NULL__) 1   ;# just so it doesn't barf
}

################################################################################
###
################################################################################

proc blueview_interactive {args} {

    init [llength $args] $args

    Window show .
    Window show .top
    main [llength $args] $args

}

################################################################################
###
################################################################################
