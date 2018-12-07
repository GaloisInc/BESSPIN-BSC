#!/bin/sh
# the next line restarts using wish\
exec wish "$0" "$@" 

if {![info exists vTcl(sourcing)]} {

    # Provoke name search
    catch {package require bogus-package-name}
    set packageNames [package names]

    package require BWidget
    switch $tcl_platform(platform) {
	windows {
	}
	default {
	    option add *ScrolledWindow.size 14
	}
    }
    
    package require Tk
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

}

#############################################################################
# Visual Tcl v1.60 Project
#


#################################
# VTCL LIBRARY PROCEDURES
#

if {![info exists vTcl(sourcing)]} {
#############################################################################
## Library Procedure:  Window

proc ::Window {args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    global vTcl
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
                vTcl:FireEvent $newname <<Show>>
            }
        }
        hide    {
            if {$exists} {
                wm withdraw $newname
                vTcl:FireEvent $newname <<Hide>>
                return}
        }
        iconify { if $exists {wm iconify $newname; return} }
        destroy { if $exists {destroy $newname; return} }
    }
}
#############################################################################
## Library Procedure:  vTcl:DefineAlias

proc ::vTcl:DefineAlias {target alias widgetProc top_or_alias cmdalias} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    global widget
    set widget($alias) $target
    set widget(rev,$target) $alias
    if {$cmdalias} {
        interp alias {} $alias {} $widgetProc $target
    }
    if {$top_or_alias != ""} {
        set widget($top_or_alias,$alias) $target
        if {$cmdalias} {
            interp alias {} $top_or_alias.$alias {} $widgetProc $target
        }
    }
}
#############################################################################
## Library Procedure:  vTcl:DoCmdOption

proc ::vTcl:DoCmdOption {target cmd} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    ## menus are considered toplevel windows
    set parent $target
    while {[winfo class $parent] == "Menu"} {
        set parent [winfo parent $parent]
    }

    regsub -all {\%widget} $cmd $target cmd
    regsub -all {\%top} $cmd [winfo toplevel $parent] cmd

    uplevel #0 [list eval $cmd]
}
#############################################################################
## Library Procedure:  vTcl:FireEvent

proc ::vTcl:FireEvent {target event {params {}}} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    ## The window may have disappeared
    if {![winfo exists $target]} return
    ## Process each binding tag, looking for the event
    foreach bindtag [bindtags $target] {
        set tag_events [bind $bindtag]
        set stop_processing 0
        foreach tag_event $tag_events {
            if {$tag_event == $event} {
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
## Library Procedure:  vTcl:Toplevel:WidgetProc

proc ::vTcl:Toplevel:WidgetProc {w args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
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
## Library Procedure:  vTcl:WidgetProc

proc ::vTcl:WidgetProc {w args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    if {[llength $args] == 0} {
        ## If no arguments, returns the path the alias points to
        return $w
    }
    ## The first argument is a switch, they must be doing a configure.
    if {[string index $args 0] == "-"} {
        set command configure
        ## There's only one argument, must be a cget.
        if {[llength $args] == 1} {
            set command cget
        }
    } else {
        set command [lindex $args 0]
        set args [lrange $args 1 end]
    }
    uplevel $w $command $args
}
#############################################################################
## Library Procedure:  vTcl:toplevel

proc ::vTcl:toplevel {args} {
    ## This procedure may be used free of restrictions.
    ##    Exception added by Christian Gavin on 08/08/02.
    ## Other packages and widget toolkits have different licensing requirements.
    ##    Please read their license agreements for details.

    uplevel #0 eval toplevel $args
    set target [lindex $args 0]
    namespace eval ::$target {set _modal 0}
}
}


if {[info exists vTcl(sourcing)]} {

proc vTcl:project:info {} {
    set base .top75
    namespace eval ::widgets::$base {
        set set,origin 1
        set set,size 1
        set runvisible 1
    }
    namespace eval ::widgets::$base.m76 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -menu 1}
        }
    }
    set site_3_0 $base.m76
    namespace eval ::widgets::$site_3_0.men77 {
        array set save {-tearoff 1}
        namespace eval subOptions {
            array set save {-command 1 -label 1 -menu 1}
        }
    }
    set site_3_0 $base.m76
    namespace eval ::widgets::$site_3_0.men79 {
        array set save {-tearoff 1}
    }
    namespace eval ::widgets::$base.pan81 {
        array set save {}
    }
    set site_5_0 [$base.pan81 getframe 0]
    namespace eval ::widgets::$site_5_0 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_5_0 $site_5_0
    namespace eval ::widgets::$site_5_0.scr83 {
        array set save {-command 1}
    }
    namespace eval ::widgets::$site_5_0.scr84 {
        array set save {-orient 1}
    }
    namespace eval ::widgets::$site_5_0.tre76 {
        array set save {-highlightcolor 1 -selectbackground 1 -selectforeground 1 -xscrollcommand 1 -yscrollcommand 1}
    }
    set site_5_1 [$base.pan81 getframe 1]
    namespace eval ::widgets::$site_5_1 {
        array set save {-background 1 -highlightcolor 1}
    }
    set site_5_0 $site_5_1
    namespace eval ::widgets::$site_5_0.can86 {
        array set save {-background 1 -borderwidth 1 -closeenough 1 -height 1 -insertbackground 1 -relief 1 -selectbackground 1 -selectforeground 1 -width 1 -xscrollcommand 1 -yscrollcommand 1}
    }
    namespace eval ::widgets::$site_5_0.scr76 {
        array set save {-orient 1}
    }
    namespace eval ::widgets::$site_5_0.scr77 {
        array set save {}
    }
    namespace eval ::widgets_bindings {
        set tagslist _TopLevel
    }
    namespace eval ::vTcl::modules::main {
        set procs {
            init
            main
            cmdOpen
            cmdShutdown
            cmdAddSignals
            scrollBoth
            zoom
            vcdRead
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
#############################################################################
## Procedure:  main

proc ::main {argc argv} {
    global widget

    global treeScrollX  canvasScrollX canvasScrollY
    global treeWidget canvasWidget
    global treeLastInx

    set treeScrollX $widget(TreeScrollX)
    set canvasScrollX $widget(CanvasScrollX)
    set canvasScrollY $widget(CanvasScrollY)
    set treeWidget   $widget(TreeArea)
    set canvasWidget $widget(CanvasArea)

    # gotta fix the stuff vtcl doesn't save
    grid columnconf $treeWidget 0 -weight 1
    grid rowconf    $treeWidget 0 -weight 1

    grid columnconf $canvasWidget 0 -weight 1
    grid rowconf    $canvasWidget 0 -weight 1

    set top .top75
    grid columnconf $top 0 -weight 1
    grid rowconf    $top 0 -weight 1

    set treeLastInx 0

    # test run!
    if {1==1} {
	# vcdRead [lindex $argv 0]
	vcdRead dump.vcd
	
	global sigsz
	set signals [list]
	set i 0
	foreach path [lsort [array names sigsz]] {
	    if {[regexp {^(.+)/([\w\d_]+)$} $path mtch hier name]} {
                # puts "found /$hier/$name"
                # if {$hier == "main"} { }
                lappend signals [list $hier $name]
	    }
	}
	
	cmdAddSignals $signals
    }
}
#############################################################################
## Procedure:  cmdOpen

proc ::cmdOpen {file} {
global widget
}
#############################################################################
## Procedure:  cmdShutdown

proc ::cmdShutdown {} {
    global widget
    exit
}
#############################################################################
## Procedure:  cmdAddSignals

proc ::cmdAddSignals {lst} {
    global widget
    global valchanges treeLastInx
    global maxtime siginx timelist sigsz

    # add signals to signal list
    set tree $widget(TreeArea)
    set cnvs $widget(CanvasArea)

    set yspace 15
    set ydiff  10

    set maxPix $maxtime

    # get all the value changes for this signal
    set color [list \#550000 \#002020]
    set cinx  0
    set linecolor \#202020
    set parent root

    # use a tree so I can group things
    foreach sig $lst {

	set hier [lindex $sig 0]
	set name [lindex $sig 1]

	set path   "$hier/$name"
	set vcdinx $siginx($path)
	set sz     $sigsz($path)

        set y         [list [expr $treeLastInx*$yspace] [expr $treeLastInx*$yspace - 10]]
        set lastydst  [lindex $y 0]
        set lasttm 0

        if {[info exists tags]} { unset tags }

	foreach tm $timelist {
	    set key "$tm:$vcdinx"
	    if {[info exists valchanges($key)]} {
		set val $valchanges($key)
		if {$sz == 1} {
                    set ydst [lindex $y $val]
                    set pts [list $lasttm $lastydst \
                                  $tm     $lastydst \
                                  $tm     $ydst]
 		    lappend tags [$cnvs create line $pts -width 2 -fill $linecolor]
                    # puts "$key: $tm => $val ($ydst) (last $tm => $lastydst)";
		    set lastydst $ydst
                    set lasttm   $tm
		} else {
                    set y0 [lindex $y 0]
                    set y1 [lindex $y 1]
		    lappend tags [$cnvs create poly $lasttm $y0 $lasttm $y1 $tm $y1 $tm $y0 \
                                      -fill [lindex $color $cinx]]
                    set cinx [expr ($cinx + 1) & 1]
                    set lasttm $tm
		}
	    }
	}

        if {[info exists tags]} {
            # add trailing signal
            set tm [expr $maxtime + 2]
            
            if {$sz == 1} {
                lappend tags [$cnvs create line $lasttm $lastydst $tm $lastydst -width 2 -fill $linecolor]
            } else {
                set y0 [lindex $y 0]
                set y1 [lindex $y 1]
                lappend tags [$cnvs create poly $lasttm $y0 $lasttm $y1 $tm $y1 $tm $y0 -fill [lindex $color $cinx]]
            }

            $tree insert end $parent $treeLastInx -fill black -text $name
            incr treeLastInx

            global sigtags
            set sigtags($path) $tags
        }
    } 

    zoom 100
}
#############################################################################
## Procedure:  scrollBoth

proc ::scrollBoth {a b} {
    global treeWidget canvasWidget
    $treeWidget   yview $a $b
    $canvasWidget yview $a $b
}
#############################################################################
## Procedure:  zoom

proc ::zoom { percent } {
    global widget sigtags maxtime

    set cnvs $widget(CanvasArea)

    # get canvas
    set w [$cnvs configure -width]
    set h [$cnvs configure -height]
    scan [lindex $w 4] "%x" szX

    # puts "DBG: $szX $maxtime"

    # scale to window size
    # set scl [expr $szX / [expr $maxtime + 50]]
    global gYZoom
    if {$percent == 100} {
        set gYZoom [expr $szX / $maxtime]
    } else {
        set gYZoom [expr ($gYZoom * percent) / 100]
    }

    foreach path [array names sigtags] {
        foreach t $sigtags($path) {
            $cnvs scale $t 1 1 $gYZoom 1
        }
    }

    global treeLastInx
    set maxy [expr $treeLastInx * 15]
    set rg [list 0 0 1000 $maxy]
    $cnvs configure -scrollregion $rg
}
#############################################################################
## Procedure:  vcdRead

proc ::vcdRead {filename} {
    global widget
    global maxtime timelist time
    global valchanges
    global sigsig siginx sigsz
    global treeLastInx

    set fl [open $filename]
    set mode "DEFINES"
    set numsigs 0
    set time 0

    # read one line at a time since it is probably big :P
    set scope [list]
    set path {}

    while {![eof $fl]} {
	set ln [gets $fl]

	set origln $ln
	regsub -all {[\s]+} $ln " " ln 
	regsub -all {^[\s]+} $ln "" ln 
	if {$ln == ""} { 
	    continue 
	}

	set flds [split $ln " "]
	set frst [lindex $flds 0]
	if {$mode == "DEFINES"} {

	    if {$frst eq "\$date"} {
		while {[gets $fl] != "\$end"} { }

	    } elseif {$frst == "\$version"} {
		while {[gets $fl] != "\$end"} { }

	    } elseif {$frst == "\$timescale"} {
		set t [gets $fl]
		regsub -all {[\s]+} $t " " t
		regsub -all {^[\s]+} $t "" t 
		set timescale t
		while {[gets $fl] != "\$end"} { }

	    } elseif {$frst == "\$scope"} {
		# puts "DBG: scope $flds"
		if {[lindex $flds 1] == "module"} {
                    lappend scope [lindex $flds 2]
		    set     path  [join $scope /]
		} else {
		    while {[gets $fl] != "\$upscope \$end"} { }
		}                    
		
	    } elseif {$frst == "\$var"} {
		incr numsigs
		
		set name [lindex $flds 4]
		set inx  [lindex $flds 3]
		set sz   [lindex $flds 2]

		regsub {\[[\d:]+\]} $name {} name   ;# remove unneeded bits ?

		set signame "$path/$name"
		
		# puts "DBG: siginx($signame) => $inx"
		set sigsig($inx)     $signame
		set siginx($signame) $inx
		set sigsz($signame)  $sz
		
		if {[info exists vars($path)] == 0} {
		    set vars($path) [list $signame]
		} else {
		    lappend vars($path) $signame
		}
		
	    } elseif {$frst == "\$upscope"} {
		set scope [lrange $scope 0 end-1]  ;# pop bottom off scope
		
	    } elseif {$frst == "\$enddefinitions"} {
		set mode "DATA"
		
	    } else {
		puts "Info: unknown line in vcd:"
		puts "      $origln"
	    }
	    
	    
	} elseif {$mode == "DATA"} {
	    
	    if {[regexp {^\#(\d+)} $frst mtch tm]} {
		set time $tm
		lappend timelist $tm
		set maxtime [expr $tm + 10]
		
	    } elseif {$frst == "\$dumpvars"} {
		; # skip this
		
	    } elseif {[regexp {^b([01]+) (\S+)$} $ln mtch val inx]} {
		set key "$time:$inx"
		set valchanges($key) $val
		# puts "DBG: valchanges($key) => $val"
	    } elseif {[regexp {^([01x])(.+)$} $ln mtch val inx]} {
		set key "$time:$inx"
		set valchanges($key) $val
		# puts "DBG: valchanges($key) => $val"
	    }

	} else {
	    puts "Info: unknown line in vcd:"
	    puts "      $origln"
	}
    }        

    puts "Info: vcd read, $numsigs signals, max time = $time"
    
    # puts "$timelist"
    # puts $timevals([lindex $timelist 10])

}

#############################################################################
## Initialization Procedure:  init

proc ::init {argc argv} {}

init $argc $argv

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
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    ###################
    # SETTING GEOMETRY
    ###################

    vTcl:FireEvent $base <<Ready>>
}

proc vTclWindow.top75 {base} {
    if {$base == ""} {
        set base .top75
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    set top $base
    ###################
    # CREATING WIDGETS
    ###################
    vTcl:toplevel $top -class Toplevel \
        -menu "$top.m76" -highlightcolor black 
    wm focusmodel $top passive
    wm geometry $top 827x555+65+97; update
    wm maxsize $top 1009 738
    wm minsize $top 1 1
    wm overrideredirect $top 0
    wm resizable $top 1 1
    wm deiconify $top
    wm title $top "Bluewave"
    vTcl:DefineAlias "$top" "Toplevel1" vTcl:Toplevel:WidgetProc "" 1
    bindtags $top "$top Toplevel all _TopLevel"
    vTcl:FireEvent $top <<Create>>
    wm protocol $top WM_DELETE_WINDOW "vTcl:FireEvent $top <<DeleteWindow>>"

    menu $top.m76 \
        -tearoff 1 
    $top.m76 add cascade \
        -menu "$top.m76.men77" -command {} -label File 
    set site_3_0 $top.m76
    menu $site_3_0.men77 \
        -tearoff 0 
    $site_3_0.men77 add command \
        -command {# TODO: Your menu handler here} -label Open 
    $site_3_0.men77 add command \
        -command cmdShutdown -label Quit 
    $top.m76 add cascade \
        -menu "$top.m76.men79" -command {} -label Help 
    set site_3_0 $top.m76
    menu $site_3_0.men79 \
        -tearoff 0 
    PanedWindow $top.pan81
    vTcl:DefineAlias "$top.pan81" "PanedWindow2" vTcl:WidgetProc "Toplevel1" 1
    bind $top.pan81 <Destroy> {
        PanedWindow::_destroy %W
    }
    $top.pan81 add
    $top.pan81 add
    set site_5_0 [$top.pan81 getframe 0]

    scrollbar $site_5_0.scr84 -orient horizontal -command [list $site_5_0.tre76 xview]
    vTcl:DefineAlias "$site_5_0.scr84" "TreeScrollX" vTcl:WidgetProc "Toplevel1" 1
    Tree $site_5_0.tre76 \
        -highlightcolor black -selectbackground #c4c4c4 \
        -selectforeground black \
        -xscrollcommand [list $site_5_0.scr84 set]  \
        -yscrollcommand [list .top75.pan81.f1.frame.scr77 set] 

    vTcl:DefineAlias "$site_5_0.tre76" "TreeArea" vTcl:WidgetProc "Toplevel1" 1
    bind $site_5_0.tre76 <Configure> {
        Tree::_update_scrollregion %W
    }
    bind $site_5_0.tre76 <Destroy> {
        Tree::_destroy %W
    }
    bind $site_5_0.tre76 <FocusIn> {
        after idle {BWidget::refocus %W %W.c}
    }
    grid $site_5_0.scr84 \
        -in $site_5_0 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $site_5_0.tre76 \
        -in $site_5_0 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid columnconf $site_5_0 0 -weight 1
    grid rowconf $site_5_0 0 -weight 1
    set site_5_1 [$top.pan81 getframe 1]
    scrollbar $site_5_1.scr76 -orient horizontal -command [list $site_5_1.can86 xview]
    vTcl:DefineAlias "$site_5_1.scr76" "CanvasScrollX" vTcl:WidgetProc "Toplevel1" 1

    scrollbar $site_5_1.scr77  -command { scrollBoth }
    vTcl:DefineAlias "$site_5_1.scr77" "CanvasScrollY" vTcl:WidgetProc "Toplevel1" 1
    canvas $site_5_1.can86 \
        -background #000070a370a3 -borderwidth 2 -closeenough 1.0 \
        -height 10000 -insertbackground black -relief ridge \
        -selectbackground #c4c4c4 -selectforeground black -width 400 \
        -xscrollcommand [list $site_5_1.scr76 set] \
        -yscrollcommand [list $site_5_1.scr77 set] 
    vTcl:DefineAlias "$site_5_1.can86" "CanvasArea" vTcl:WidgetProc "Toplevel1" 1
    grid $site_5_1.can86 \
        -in $site_5_1 -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 
    grid columnconf $site_5_1.can86 0 -weight 1
    grid rowconf $site_5_1.can86 0 -weight 1
    grid $site_5_1.scr76 \
        -in $site_5_1 -column 0 -row 1 -columnspan 1 -rowspan 1 -sticky ew 
    grid $site_5_1.scr77 \
        -in $site_5_1 -column 1 -row 0 -columnspan 1 -rowspan 1 -sticky nsw 
    grid columnconf $site_5_1 0 -weight 1
    grid rowconf $site_5_1 0 -weight 1
    ###################
    # SETTING GEOMETRY
    ###################
    grid columnconf $top 0 -weight 1
    grid rowconf $top 0 -weight 1
    grid $top.pan81 \
        -in $top -column 0 -row 0 -columnspan 1 -rowspan 1 -sticky nesw 

    vTcl:FireEvent $base <<Ready>>
}

#############################################################################
## Binding tag:  _TopLevel

bind "_TopLevel" <<Create>> {
    if {![info exists _topcount]} {set _topcount 0}; incr _topcount
}
bind "_TopLevel" <<DeleteWindow>> {
    if {[set ::%W::_modal]} {
                vTcl:Toplevel:WidgetProc %W endmodal
            } else {
                destroy %W; if {$_topcount == 0} {exit}
            }
}
bind "_TopLevel" <Destroy> {
    if {[winfo toplevel %W] == "%W"} {incr _topcount -1}
}

Window show .
Window show .top75

main $argc $argv
