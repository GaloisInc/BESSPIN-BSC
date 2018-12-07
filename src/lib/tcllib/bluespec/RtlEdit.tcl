# Copyright 2009--2010 Bluespec, Inc.  All rights reserved.
# porcelain interface on the Bluespec compiled BHdlEdit package


package require BhdlEdit
package require SignalTypes

namespace eval ::RtlEdit {
    # Capitalized -- Namespace varaible
    # UPPERCASE   -- Namespace Array
    variable BoolOptions [list -- --gui --batch --verbose --partition]
    variable ValOptions  [list --outputdir --script --params --params-out -p -bsvmodule]
    variable RequiredFiles " -v $::env(BLUESPECDIR)/Verilog/Empty.v"
    variable BscPath "+libext+.v -y $::env(BLUESPECDIR)/Verilog -y $::env(BLUESPECDIR)/Libraries $RequiredFiles"
    variable OPT
    variable NumPartition 0
    array set OPT [list]

    proc usage {} {
        set str \n
        append str "usage: [file tail $::argv0] <options> <verilog command line>" \n
        append str "Options:" \n
        append str "  --outputdir <dir>                       Output directory for edited hdl file " \n
        append str "  --script <scriptfile>                   Name of script file to run" \n
        append str "  --params <paramsfile>                   Params file to be edited" \n
        append str "  --params-out <edited_paramefile>        Modified params file" \n
        append str "  --blackbox                              Control black boxing of array variables for cosim" \n
        append str "  --batch                                 run in batch mode (requires --script)" \n
        append str "  --partition                             process only partitioning tasks and ignore probe tasks" \n
        append str "  --gui                                   run using graphical interface" \n
        append str "  --verbose                               run in verbose mode" \n
        append str \n
        append str "  -p <path>                               search path for Bluespec .ba files" \n
        append str "  -bsvmodule <module>                     BSV module to load" \n
        append str "" \n
        append str "Default: --gui" \n
        append str "" \n
        return $str
    }


    proc guiStart {args} {
        puts "Starting the graphical interface..."
        exec -ignorestderr 2>@ stderr >@ stdout $::env(BLUESPECDIR)/tcllib/workstation/edithdl_gui.tcl $args 
    }

    proc guiEntry {argsIn} {
        variable BoolOptions
        variable ValOptions
        variable OPT

        if { [catch [list ::utils::scanOptions $BoolOptions $ValOptions false OPT "$argsIn"] opts] } {
            puts stderr "Error: $opts"
            puts [usage]
            exit 1
        }
        processOptions
	set res [eval "analyze $opts"]
	if { $res != "" } {
            tk_messageBox -title "HDL Editor Error" -icon error -message $res
	    return
	}
        if { [catch "processScript $OPT(--script)" err] } {
            tk_messageBox -title "HDL Editor Error" -icon error -message $err
	    return
        }
    }

    proc scriptEntry {args} {
        variable BoolOptions
        variable ValOptions
        variable OPT

        if { [catch [list ::utils::scanOptions $BoolOptions $ValOptions false OPT "$args"] opts] } {
            puts stderr "Error: $opts"
            puts [usage]
            exit 1
        }
        processOptions
	set res [eval "analyze $opts"]
	if { $res != "" } {
            error "An error occurred while analyzing verilog files\n$res"
        }
        processScript $OPT(--script)
        applyEdits
        updateParams
    }

    # Some final updates to the options
    proc processOptions {} {
        variable OPT
        set OPT(doparams) false
        if { [info exists OPT(--params)] } {
            set OPT(doparams) true
        }
        if { ![info exists OPT(--params)] }     {set  OPT(--params) "mkBridge.params" }
        if { ![info exists OPT(--params-out)] } {set  OPT(--params-out) "mkBridge_EDITED.params" }
        if { ![info exists OPT(--outputdir)] }  {set  OPT(--outputdir) "."}
        if { ![info exists OPT(--script)] }     {
            set  OPT(--script) ""
            set OPT(outscript) "replay_edits.script"
        } else {
            set OPT(outscript) $OPT(--script)
        }
        if { [info exists OPT(-p)] } {
            Bluetcl::flags set -p $OPT(-p)
        }
        set OPT(hierview) "hdl"
        if { [info exists OPT(-bsvmodule)] } {
            set OPT(hierview) "bsv"
            Bluetcl::flags set -verilog
            SignalTypes::generateSigTypeMap $OPT(-bsvmodule) "/$OPT(-bsvmodule)"
        }
    }

    # analye with
    proc analyze {args} {
        if { [catch "analyzeCore $args" err] } {
	    return $err
        }
    }
    proc analyzeCore {args} {
        variable BscPath
        eval BhdlEdit::netlist analyze $args $BscPath
    }

    # update the parameter file based on options
    proc updateParams {} {
        variable OPT
        set res ""
        if { $OPT(doparams) } {
            set res [eval BhdlEdit::editnl updateparams $OPT(--params) $OPT(--params-out)]
            if { [info exists OPT(--verbose)] } {
                puts stderr "parameter file has been updated:  $OPT(--params) $OPT(--params-out)"
            }
        }
        return $res
    }

    proc processScript { {scriptname ""} } {

        if { $scriptname == "" } { return }
        if { ! [file readable $scriptname] } {
            error "Cannot open script file $scriptname for reading"
        }
        puts stderr "Processing $scriptname ..."
        if { [catch "source $scriptname" err] } {
            error "An error occurred while processing script file $scriptname:\n$err"
        }
        puts stderr "done Processing"
    }
    proc applyEdits {} {
        variable OPT
        if { [info exists OPT(--verbose)] } {
            puts stderr "compiling edits"
        }
        tracePoint "Compile"
        BhdlEdit::editnl compile

        set toDelete [glob -nocomplain -types f -directory $OPT(--outputdir) *_EDITED_*.v *_EDITED.v]
        if { [info exists OPT(--verbose)] } {
            puts stderr "Deleting previous Edited files: \"$toDelete\""
            puts stderr "applying changes to file(s), output to $OPT(--outputdir)"
        }
        catch "exec rm -f $toDelete"
        tracePoint "Apply Edits"
        BhdlEdit::editnl apply $OPT(--outputdir)
    }

    proc tracePoint {name} {
        if { [info exists ::env(BSC_TRACE_RTLEDIT)] } {
            puts "#####################################################"
            puts "trace: $name"
            utils::ldisplay [BhdlEdit::editnl dump]
            puts "#####################################################"
        }
    }

    # Wrapper for C-based additions
    proc addprobe {name path pattern clk {enable 1'b1} {type ""} {signaltype "Any"}} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
           BhdlEdit::editnl addprobe $name $path $pattern $clk $enable $type $signaltype
	}
   }
    proc addbsvprobes {name pattern {mpattern ".*$PROBE"} {signaltype "Any"}} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
           BhdlEdit::editnl addbsvprobes $name $pattern $mpattern $signaltype
        }
    }
    proc addcosim  {name path clk uclk trigger {width 32} {flavor "OBSERVE"} {key 0}} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
           BhdlEdit::editnl addcosim $name $path $clk $uclk $trigger $width $flavor
        }
    }
    proc addcapture {name path pattern clk {enable 1'b1} {trigger 1'b0} {captureDepth 16} {runWidth 4}  {dumpdelay 16} {type ""} {signaltype "Any"}} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
           BhdlEdit::editnl addcapture $name $path $pattern $clk $enable $trigger $captureDepth $runWidth $dumpdelay $type $signaltype
        }
    }
    proc addtrigger {name path probeexpr clk captures} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
           BhdlEdit::editnl addtrigger $name $path $probeexpr $clk $captures
        }
    }
    proc drawout {path toppath signal port} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
           BhdlEdit::editnl drawout $path $toppath $signal $port
        }
    }
    proc partition {boardspec modspec} {
    	variable NumPartition
	incr NumPartition
	if { $NumPartition > 1 } {
            puts stderr "Error: there are more than 1 partition tasks in the script"
	} else {
            BhdlEdit::editnl partition $boardspec $modspec
        }
    }
    proc genInitSceMi {cfgfile pinfile} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
	    BhdlEdit::editnl geninitscemi $cfgfile $pinfile $OPT(--outputdir)
        }
    }
    proc genFinalSceMi {cfgfile pinfile} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
           BhdlEdit::editnl genfinalscemi $cfgfile $pinfile $OPT(--outputdir)
        }
    }
    proc genTestBench {cfgfile pinfile} {
        variable OPT
	if { ![info exist OPT(--partition)] } {
           BhdlEdit::editnl gentestbench $cfgfile $pinfile $OPT(--outputdir)
        }
    }
    proc verifyDutVCD {path vcdfile portmapfile} {
        BhdlEdit::editnl verifyvcd $path $vcdfile $portmapfile
    }

    proc deleteEdit {key} {
        catch "BhdlEdit::editnl delete $key"
    }

    proc debugAllEdits {} {
        utils::ldisplay [BhdlEdit::editnl dump]
    }

    proc writeScript {filename} {
        utils::makeBackupFile $filename
        set fh [open $filename "w"]

        set scr ""
        append scr "# replay script for adding circuit edits" \n
        append scr "# Generated on: [clock format [clock seconds]]" \n \n
        foreach edit [BhdlEdit::editnl replay]  {
            append scr "$edit" \n
        }
        puts $fh $scr
        close $fh
    }

    # Hierarchy and net viewing
    proc getHierView {} {
        variable OPT
        return $OPT(hierview)
    }
    proc setHierView {view} {
        variable OPT
        set old $OPT(hierview)
        if { [info exists OPT(-bsvmodule)] && $view == "bsv" } { 
            set OPT(hierview) "bsv"
        } else {
            set OPT(hierview) "hdl"
        }
        set changed [string compare $old $OPT(hierview)]
        return $changed
    }
    proc getChildInsts {pathin} {
        variable OPT
        if { $OPT(hierview) == "bsv" && [string is integer $pathin] } {
            return [getChildInstsBSV $pathin]
        } else {
            return [getChildInstsHDL $pathin]
        }
    }
    proc getChildInstsBSV {pathin} {
        if {$pathin == ""} {
            set res [Bluetcl::browseinst list 0]
        } else {
            set res [Bluetcl::browseinst list $pathin]
        }
        # add bsv tag
        set ret [list]
        foreach x $res {
            set tags [lindex $x 2]
            lappend tags bsv
            lappend ret [lreplace $x 2 2 $tags]
        }

        # Check if the type is Primitive e.g. an imported module
        if { $ret == [list] } {
            array set AL [Bluetcl::browseinst detail $pathin]
            switch $AL(Node) {
                "Primitive" {
                    set realPath [getRealPath AL]
                    set ret [getChildInstsHDL $realPath]
                }
            }
        }
        return $ret
    }
    proc getChildInstsHDL {pathin} {

        if { [catch "BhdlEdit::netlist lsinsts $pathin " mem] } { set mem [list] }

        set res [list]
        foreach mpair $mem {
            set m [lindex  $mpair 0]
            set module [lindex $mpair 1]
            # ignore scemi.*Parameter modules
            if { [regexp  {^mkSceMi.*Parameter$} $module] } { continue }
            set inst [format "%s  (%s)" [file tail $m] $module]
            set path $m
            lappend res [list $path $inst [list module]]
        }
        return $res
    }
    proc getNets {pathin} {
        variable OPT
        if { $OPT(hierview) == "bsv" && [string is integer $pathin] } {
            return [getNetsBSV $pathin]
        } else {
            return [getNetsHDL $pathin]
        }
    }
    proc getNetsBSV {pathin} {
        set ret [list]
        if {$pathin == 0 || $pathin == ""} {
            return $ret
        }
        set res [Bluetcl::browseinst detail $pathin]
        array set AL [Bluetcl::browseinst detail $pathin]
        set realPath [getRealPath AL]
        switch $AL(Node) {
            "Primitive" {
                if { [regexp {RegN$|RegA$|RegUN$|^PulseWire$|RWire|BypassWire} $AL(Module)] } {
                    set updir [file dirname $realPath]
                    set ret [getNets $updir]
                    set name [file tail $realPath]
                    set f "^$name$|^$name\\\$"
                    set ret [lsearch -regex -inline -all -index 0 $ret $f]
                } else {
                    set ret [getNets $realPath]
                }
            }
            "Rule" {
                set f "*$AL(RuleName)"
                set ret [getNets $realPath]
                set ret [lsearch -glob -nocase -inline -all -index 0 $ret $f]
            }
            "Instance" {
                set p [lsearch -regexp -inline -all -not $AL(LocalPath) {^_}]
                set name [join $p "_"]
                set f "^$name.*"
                set ret [getNets $realPath]
                set ret [lsearch -regexp -nocase -inline -all -index 0 $ret $f]
            }
            "Synthesized" {
                set ret [getNets $realPath]
            }
            default {
                set ret [getNets $realPath]
                set i 0
                foreach r $res {
                    lappend ret [list $i [list XX $r] [list leaf]]
                    incr i
                }
            }
        }
        return $ret
    }
    proc getRealPath { alname  } {
        upvar 1 $alname AL
        variable OPT
        set rp [join [concat "/$OPT(-bsvmodule)" $AL(SynthPath)] "/"]
        return $rp
    }
    proc getNetsHDL {pathin} {
        set res ""
        if { [catch "BhdlEdit::netlist lsnets $pathin " nets] } { set nets [list] }
        foreach m $nets {
            # format is (path <range>? <input|output>?)
            set net [file tail $m]
            set path [lindex $m 0]
            set nn [file tail $path]
            lappend res [list $nn  $net [list net leaf]]
        }
        return $res
    }

    # Entries from GUI side
    proc getActualPath { pathin } {
        variable OPT
        set ret $pathin
        if { $OPT(hierview) == "bsv" && [string is integer $pathin] } {
            array set AL [Bluetcl::browseinst detail $pathin]
            set ret [getRealPath AL]

            ## Handle inlined modules
            if {$AL(Node) == "Primitive" } {
                set noInlineReg [regexp "no" [Bluetcl::module flags $OPT(-bsvmodule) inline-reg]]
                if {! $noInlineReg && \
                        [regexp {RegN$|RegA$|RegUN$} $AL(Module)] } {
                    set ret [file dirname $ret]
                }
                set noInlineWire [regexp "no" [Bluetcl::module flags $OPT(-bsvmodule) inline-rwire]]
                if {! $noInlineWire && \
                        [regexp {^PulseWire$|RWire|BypassWire} $AL(Module)] } {
                    set ret [file dirname $ret]
                }
            }

        }
        return $ret
    }
    proc getActualNets { pathin nets } {
        variable OPT
        set ret $nets
        if { $OPT(hierview) == "bsv" && [string is integer $pathin] && $nets == "" } {
            # TODO default for primitives
            array set AL [Bluetcl::browseinst detail $pathin]
            switch $AL(Node) {
                "Primitive" {
                    switch -regexp $AL(Module) {
                        {^RegA$|^RegN$|^RegUN$} {
                            set ret $AL(UniqueName)
                        }
                        {^RWire0$} {
                            set ret "$AL(UniqueName)\$WHAS"
                        }
                        {^RWire$} {
                            set ret "$AL(UniqueName)\$WVAL"
                        }
                        {^FIFO1$|^FIFO2$|^FIFOL1$|^FIFOL21$|^SizedFIFOL$|^SizedFIFO$} {
                            set ret "D_OUT"
                        }
                        {^SyncFIFOLevel$|^SyncFIFO$} {
                            set ret "dD_OUT"
                        }
                    }
                }
                "Rule" {
                    set ret "WILL_FIRE_$AL(RuleName)"
                }
            }
        }
        return $ret
    }
    proc getActualType { pathin net } {
        variable OPT
        if { [info exists OPT(-bsvmodule)] && [string is integer $pathin] } {
            set path [getActualPath $pathin]
        } else {
            set path $pathin
        }
        set ret [SignalTypes::lookupSig [join [concat $path $net] "/"] ]
        regsub -all {[ ]+} $ret "" ret
        return $ret
    }

}

package provide RtlEdit 1.0

