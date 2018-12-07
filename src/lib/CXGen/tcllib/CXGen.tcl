# Copyright 2012--2013 Bluespec, Inc.  All rights reserved.
# $Id$

package require Itcl
package require VisitorPattern
package require Virtual
package require Types

package provide CXGen 1.1
namespace eval ::CXGen {}


## This namespace is a wrapper providing the public interface to this
## package.   All the work is done in the class contained within this space.
## Error handling is localized here to prevent error traces into the details
namespace eval ::CXGen {

    ## Everything is exported so careful what you add.   Use the XUtils namespace
    namespace export {[a-z]*}

    proc debug {args} {
        eval $GlobalState::gs debug $args
    }

    proc newXactor { name type args } {
        if { ![ catch "$GlobalState::gs newXactor $name $type $args" err ] } {
            return $err
        }
        CXGen::GlobalState::showErorInfo $err
        return -code error $err
    }
    proc deleteXactor { name } {
        itcl::delete object $name
    }
    proc read_hdl { filename } {
        if { ![ catch "$GlobalState::gs read_hdl $filename" err ] } {
            return $err
        }
        CXGen::GlobalState::showErorInfo $err
        return -code error $err
    }
    proc create_instance { name module } {
        if { ![ catch "CXGen::Instance::mkInstance $name $module" err ] } {
            return $err
        }
        CXGen::GlobalState::showErorInfo $err
        return -code error $err
    }

    proc clear {} {
        GlobalState::clear
    }

    ## set the port attribute to value for all ports
    proc assignPortAttr { attr value ports } {
        if { ![catch [list utils::omap_ [list configure -$attr $value] $ports] err] } {
            return $err
        }
        CXGen::GlobalState::showErorInfo
        return -code error $err
    }

    proc allPorts {} {
        $GlobalState::gs cget -ports
    }
    proc filter {ports args} {
        set cmd "XUtils::filter [list $ports] $args"
        if { ![catch $cmd err ] } {
            return $err
        }
        CXGen::GlobalState::showErorInfo
        return -code error $err
    }
    proc findPorts {args} {
        set cmd "XUtils::filter [list [allPorts]] $args"
        if { ![catch "XUtils::filter [list [allPorts]] $args" err ] } {
            return $err
        }
        CXGen::GlobalState::showErorInfo
        return -code error $err
    }

    proc check {} {
        if { [catch [list ::CXGen::BsvWriter::createBSVCode $GlobalState::gs . Check] err ] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }
        return $err
    }

    proc createBVIWrapper { {outdir .} } {
        if { [catch [list ::CXGen::BsvWriter::createBSVCode $GlobalState::gs $outdir BVI] err ] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }
        return $err
    }
    proc createTopWrapper { {outdir .} } {
        if { [catch [list ::CXGen::BsvWriter::createBSVCode $GlobalState::gs $outdir IFC] err ] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }
        return $err
    }
    proc createEMUWrapper { {outdir .} } {
        if { [catch [list ::CXGen::BsvWriter::createBSVCode $GlobalState::gs $outdir EMU] err ] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }
        return $err
    }
    proc createProject { {outdir .} } {
        if { [catch [list ::CXGen::BsvWriter::createBSVCode $GlobalState::gs $outdir Project] err ] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }
        return $err
    }
    proc createCPP_Proxies { {outdir .} } {
        if { [catch [list ::CXGen::BsvWriter::createBSVCode $GlobalState::gs $outdir Proxies] err ] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }
        return $err
    }
    proc createSystemC_Proxies { {outdir .} } {
        if { [catch [list ::CXGen::BsvWriter::createBSVCode $GlobalState::gs $outdir SysC] err ] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }
        return $err
    }
    proc createWrapper { {outdir .} } {
        if { [catch [list ::CXGen::BsvWriter::createBSVCode $GlobalState::gs $outdir All] err ] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }
        return $err
    }
}

###########################################################################
# A singleton class containing global state information about the design
itcl::class ::CXGen::GlobalState {
    common gs
    common debugMode true
    common TypeClassMap

    # these are list of objects
    public variable ports   [list]
    public variable unusedports   [list]
    public variable xactors [list]
    public variable modules [list]
    public variable instances [list]
    # list of names
    public variable topfiles [list]


    # proc to initial the package -- should only be called ONCE
    proc init {} {
        array set TypeClassMap [list]
        createGS
    }
    proc createGS {} {
        set gs [uplevel #0 ::CXGen::GlobalState #auto]
    }
    proc clear {} {
        itcl::delete object $gs
        createGS
    }

    constructor {} {
    }

    destructor {
        eval delete object $xactors
        eval delete object $instances
        eval delete object $modules
        eval delete object $ports
        eval delete object $unusedports

        # lists should be empty now, verify
        foreach listname [list ports unusedports xactors modules instances] {
            if { [set $listname]  ne [list] } {
                puts stderr "[namespace current] clear did not clear properly [set $listname]"
                catch [utils::mapm debug [set $listname]]
                set $listname [list]
            }
        }
    }

    method debug {args} {
        foreach mod $modules {
            puts stderr "Module:  [$mod getName]"
            set ps [$mod getPorts]
            puts stderr [join [utils::omap "show $args" $ps] "\n"]
            puts stderr ""
        }
        if { $instances ne [list] } {
            puts stderr  [join [utils::omap "show $args" $instances] "\n\n"]
        }
        if { $xactors ne [list] } {
            puts stderr  [join [utils::omap "show $args" $xactors] "\n\n"]
        }
    }

    proc showErorInfo {args} {
        if { $debugMode } {
            set msg "[join $args \n] $::errorInfo"
            puts stderr "[regsub -line -all {^} $msg {>>>> }]"
        }
    }

    method read_hdl { filename args } {
        set fh [open $filename "r"]
        lappend topfiles $filename
        set modname ""
        set portCount 0
        set mod ""
	global names

        proc validMod { fn m } {
            if { $m eq "" } {
                set msg "Error in reading $fn, found port outside of module"
                return -code error $msg
            }
        }

        proc substituteParameters { str } {
            regsub -all {([a-zA-Z]\w*)} $str {$\0}
        }

        while { [gets $fh line] != -1 } {
            #puts "Line -- $line"
            # Kill comments
            set line [regsub {//.*$} $line ""]
            switch -regexp -matchvar ms -- $line {
                {\mmodule\s+(\w+)} {
                    #puts "matched module [lindex $ms 1]"
                    set modname [lindex $ms 1]
                    set mod [CXGen::Module::mkModule $modname]
                    set portCount 0
                }
                {\mparameter\s+(\w+)\s*=\s*\"(\w+)\"} {
                    # String parameter
                    set name [lindex $ms 1]
                    set val  [lindex $ms 2]
                    set $name $val
                    #puts "$name $val -- local -- [info local]"
                }
                {\mparameter\s+(\w+)\s*=(.+);} {
                    set name [lindex $ms 1]
                    set val  [substituteParameters [lindex $ms 2]]
                    set val [expr $val]
                    set $name $val
                    #puts "$name $val -- local -- [info local]"
                }

		{\.(\w+)\((\w+)\)} {
		    if { $portCount == 0 } {
			set names([lindex $ms 2]) [lindex $ms 1]
			puts "names: "
			parray names
		    }
		}
                {\m(input|output|inout)\s+(\w+)}  {
                    #puts "matched [lrange $ms 1 end]"
                    validMod $filename $mod
		    set xx [lindex $ms 2]
		    if { [info exists names($xx)] } {
			set xx $names($xx)
		    }
                    $mod addPort $xx [lindex $ms 1] -width 1
                    incr portCount
                }
                {\m(input|output|inout)\s+\[(.+)\s*:\s*(.+)\s*\]\s+(\w+)}  {
                    set upper [substituteParameters [lindex $ms 2]]
                    set lower [substituteParameters [lindex $ms 3]]
                    set wid [expr ($upper) - ($lower) + 1]
                    validMod $filename $mod
		    set xx [lindex $ms 4]
		    if { [info exists names($xx)] } {
			set xx $names($xx)
		    }
                    $mod addPort $xx [lindex $ms 1] -width $wid
                    incr portCount
                }
                {\mendmodule} {
                    if { $mod eq "" } {
                        set msg "No modules were found in file `$filename'"
                        return -code error $msg
                    }
                    if { $portCount == 0 } {
                        set msg "No ports were found in module `$modname', file: `$filename'"
                        return -code error $msg
                    }
                    set modname ""
                    set mod ""
                }
            }
        }
    }

    ## add and delete objects registered with the global class
    method add_obj { listname obj } {
        lappend $listname $obj
    }
    method delete_obj { listname obj } {
        set $listname [lsearch -inline -all -exact -not [set $listname] $obj]
    }

    method checkSanity {mode} {
        # module name
        set msg ""
        if { $modules == [list]} {
            set msg "No modules have been defined for gateway generation"
            return -code error  $msg
        }

        # at least 1 clock
        # at least 1 reset
        # all signals assoc with clock and reset
        foreach m $modules {
            set ps [$m getPorts]
            set cp [filter $ps isclock true]
            set rp [filter $ps isreset true]
            if { $cp == "" } {
                lappend msg "At least one clock must be defined for module [$m getName]"
            }
            if { $rp == "" } {
                lappend msg "At least one reset must be defined for module [$m getName]"
            }
        }
        if { $msg ne "" } {
            return -code error [join $msg "\n"]
        }

        if {$mode == "BVI" }  {
            return
        }

        # all signals assoc with clock and reset
        foreach m $modules {
            set ps [$m getPorts]
            foreach p $ps {
                if { [$p cget -isclock] } { continue }
                if { [$p cget -clock] == "" } {
                    lappend msg "Port [$p getName] from module [$m getName] must be associated with a clock"
                }
                if { [$p cget -reset] == "" } {
                    lappend msg "Port [$p getName] from module [$m getName] must be associated with a reset"
                }
            }
        }
        if { $msg ne "" } {
            return -code error [join $msg "\n"]
        }

        if { $instances == [list] } {
            set msg "At least one instance must be defined for gateway generation"
            return -code error $msg
        }
        # each transactor is sane
        foreach x $xactors {
            $x checkSanity $mode
        }

    }

    proc addToXactorNameMap { className userNames } {
        foreach u $userNames {
            set TypeClassMap($u) $className
        }
    }
    method showXactorTypes {pre} {
        puts [format "%20s --> %20s" Type  Class]
        foreach un [lsort [array names TypeClassMap]] {
            puts  [format "%s%20s --> %s" $pre $un [namespace tail $TypeClassMap($un)] ]
            lappend res  $TypeClassMap($un)
        }
        lsort [utils::nub $res]
    }
    method lookupXactorClass { name } {
        if { [info exists TypeClassMap($name)] } {
            return $TypeClassMap($name)
        }
        set msg "Unknown transactor type `$name'."
        append msg "Transactor type must be one of:" \n
        append msg [join [lsort [array names TypeClassMap]] ", "] . \n
        return -code error $msg
    }
    method newXactor {name type args} {
        set cl [lookupXactorClass $type]
        set nm [uplevel #0 $cl $name $name $args]
        add_obj xactors "::$nm"
        return ::$nm
    }

}

################################################################
itcl::class ::CXGen::BsvWriter {

    variable gs ""
    variable of ""
    variable outputpath ""
    variable ports [list]
    variable xactors [list]
    variable modules [list]

    variable build_init "project"
    variable p_clocks [list]
    variable p_resets [list]
    variable p_others [list]
    variable clockConfigs [list]
    variable indent "  "
    variable className "Xactor_Proxy"
    ## TODO
    # output clocks and resets

    constructor {globalstate outpath} {
        # gs must be GlobalState
        set gs $globalstate

        # check that dir exists for the outpath
        if { ! [file isdirectory $outpath] } {
            file mkdir $outpath
            puts stderr "Note: Output directory `$outpath' has been created"
        }
        if { ! [file writable $outpath] } {
            error "Output directory `$outpath' is not writable"
        }
        set outputpath $outpath

        # Setup house
        ##  add xactor for all unused(?) ports
        set xactors [$gs cget -xactors]
    }

    destructor {
        # close any open files
        if { $of ne "" } {
            close $of
            set of ""
        }
    }

    method setUpPorts { mod bldClockConfigs } {
        set ports [utils::osortBy bsv_method_name [$mod getPorts]]
        # reset and clock ports arguments
        set clockConfigs [list]
        set p_clocks [list]
        set p_resets [list]
        set p_others  [list]
        foreach p $ports {
            if [$p cget -isclock] {
                lappend p_clocks $p
                if { $bldClockConfigs } {
                    if { [$p cget -dir] ne "input" } { continue }
                    lappend clockConfigs [$p getClockConfig]
                }
                continue
            }
            if [$p cget -isreset] {
                lappend p_resets $p
                continue
            }
            lappend p_others $p
        }
    }

    method genBVIName { modObj type} {
        if { ![ $modObj isa CXGen::Module] } { error "Incorrect object type (not Module)" }
        set mod [$modObj getName]
        switch $type {
            "package"     {return [format "BVI_%s" $mod] }
            "module"      {return [format "mkBVI_%s" $mod] }
            "proxyModule" {return [format "mkProxyBVI_%s" $mod] }
            "ifc"         {return [format "BVI_%s_IFC" $mod] }
            "ifcDual"     {return [format "BVI_%s_dual_IFC"  $mod] }
            "ifcProxy"    {return [format "BVI_%s_proxy_IFC" $mod] }
            default       { return -code error "Bad type..." }
        }
    }
    method genTOPName { instObj type} {
        if { ![ $instObj isa CXGen::Instance] } { error "Incorrect object type (not Instance)" }
        set mod [$instObj getName]
        switch $type {
            "package" {return [format "Top_%s" $mod] }
            "module"  {return [format "mkTop_%s" $mod] }
            "ifc"     {return [format "Top_%s_IFC" $mod] }
            "Ext"     {return [format "Ext_%s_IFC" $mod] }
            "ext"     {return [format "ext_%s_IFC" $mod] }
            "inst"    {return [format "%s" $mod] }
            default   { return -code error "Bad type..." }
        }
    }

    proc createBSVCode { globalstate outpath mode} {
        namespace import ::CXGen::XUtils::decodeBuild
        if { [catch "$globalstate checkSanity $mode" err] } {
            CXGen::GlobalState::showErorInfo
            return -code error $err
        }

        if { $mode == "Check" }  { return "" }

	#puts stderr "1"
	assignUnassignedSignalsBVI $globalstate

	#puts stderr "Unused ports:"
	#puts stderr [$globalstate cget -unusedports]

        set insts [$globalstate cget -instances]
        # build bvi only for instantiated modules
        set mods [list]
        foreach i $insts {
            lappend mods [$i getModule]
        }
        set mods [utils::nub $mods]
        if { $mode == "BVI" } {
            set mods [$globalstate cget -modules]
        }

        foreach mod $mods {
            set wr1 [itcl::local ::CXGen::WriteBVI #auto $globalstate $outpath $mod]
            lappend tmp1 $wr1
        }
        if { $mode == "BVI" } { return }

        if { $insts eq [list] } {
            puts stderr "Cannot create gateway without instances"
            return
        }
        foreach imod $insts {
            set wr2 [itcl::local ::CXGen::WriteInterface #auto $globalstate $outpath $imod]
            lappend tmp2 $wr2
        }
        if { [decodeBuild $mode] <= [decodeBuild "IFC"] } { return "" }

        ## get the local xactors
        set newX [join [utils::omap getLocalXactor $tmp2]]

        set wr3 [itcl::local ::CXGen::WriteTransactors #auto $globalstate $outpath $newX $insts]

        if { $mode == "EMU" } { return "" }
        set wr4 [itcl::local ::CXGen::WriteBuildScript #auto $globalstate $outpath]

        if { $mode == "Project" } { return "" }
        set wr5 [itcl::local ::CXGen::WriteCPP #auto $globalstate $outpath $newX]

        if { $mode == "Proxies" } { return "" }
        set wr6 [itcl::local ::CXGen::WriteSystemC #auto $globalstate $outpath $newX]

        return ""
    }

    proc assignUnassignedSignalsBVI {globalstate} {
        set insts [$globalstate cget -instances]
	set ports [list]
	foreach i $insts {
	    eval lappend ports [$i getPorts]
	}
        set ports [utils::nub $ports]

	#puts stderr "Ports:"
	#puts stderr $ports

	set xactors [$globalstate cget -xactors]

	#puts stderr "Xactors:"
	#puts stderr $xactors

	set used [list]
        foreach x $xactors {
            eval lappend used [$x getAllPorts]
        }
        utils::listToSet UsedPorts $used
        foreach p $ports {
            if { [info exists UsedPorts($p)] } { continue }
            # ignore clocks and resets
            if { [$p cget -isclock] } { continue }
            if { [$p cget -isreset] } { continue }

	    $CXGen::GlobalState::gs add_obj unusedports $p
        }
    }

    method safeOpen {name createMethodName ext} {
        set ofile [file join $outputpath $name$ext]

        if { [catch [list open $ofile "w"] err ] } {
            return -code error $err
        }
        set of $err
        set err [catch "$this $createMethodName" em]
        close $of
        set of ""
        if { $err } {
            CXGen::GlobalState::showErorInfo $em
            file delete $ofile
        } else {
            puts "Created file: $ofile"
        }

        if { $err } {
            return -code error $em
        }
        return $ofile
    }

    method nextIndent { p1 } {
        append px $p1 $indent
    }

    method writeHeader {kind {showXactors false} {comment  "//"} } {
        append ss  "$comment $kind created by: transactor generator on: [exec date]" \n
        if { $showXactors } {
            append ss \n "$comment Transactors are:" \n
            foreach x $xactors {
                append ss "$comment  [$x show]" \n
            }
        }
        puts $of $ss
    }

    method writeImports {seed incldXactors} {
        set imports $seed
        if { $incldXactors } {
            foreach x $xactors {
                eval lappend imports [$x cget -imports]
            }
        }
        set imports [utils::nub $imports]
        if { $imports eq [list] } { return }
        set ss "\n\n"
        foreach i $imports {
            append ss "import $i :: *;" \n
        }
        puts $of $ss
    }
}

#########################################################################
itcl::class ::CXGen::WriteBVI {
    inherit ::CXGen::BsvWriter
    variable module ""
    variable ifcName ""
    variable ifcDualName ""
    variable ifcProxyName ""
    variable modName ""
    variable proxyModName ""
    variable packs [list]
    variable gs ""


    constructor { globalstate outpath mod} {
        chain $globalstate $outpath
    } {
	set gs $globalstate
	set module $mod
        setUpPorts $module false
        set packName     [genBVIName $mod package]
        set ifcName      [genBVIName $mod ifc]
        set ifcDualName  [genBVIName $mod ifcDual]
        set ifcProxyName [genBVIName $mod ifcProxy]
        set modName      [genBVIName $mod module]
        set proxyModName [genBVIName $mod proxyModule]

        foreach p [$mod getPorts] {
            eval lappend packs [[$p getTypeObj] getPackages]
        }

        safeOpen  $packName writeAll ".bsv"
    }

    method writeAll {} {
        writeHeader "BVI Wrapper"
	append ss "import Connectable::*;\n"
	append ss "import Clocks::*;\n"
        puts $of $ss
        writeInterface
	writeDualInterface
	writeBVI
	# writeProxy
    }
    method writeInterface {} {
        append ss \n
        append ss "(* always_ready, always_enabled *)" \n
        append ss "interface $ifcName;" \n
        foreach p $ports {
            $p interfaceDef ss "  "
        }
        append ss "endinterface : $ifcName" \n

        puts $of $ss
    }
    method writeDualInterface {} {
        set insts [$gs cget -xactors]
	set ports [list]
	foreach i $insts {
	    eval lappend ports [$i getAllPorts]
	}
        set ports [utils::nub $ports]

        append ss \n
        append ss "(* always_ready, always_enabled *)" \n
        append ss "interface $ifcDualName;" \n
        foreach p $ports {
            $p interfaceDualDef ss "  "
        }
        append ss "endinterface : $ifcDualName" \n \n

	append ss "`ifdef SCEMI_TCP" \n

	append ss "typeclass NCConnectable#(type ta, type tb);" \n
	append ss "   module mkNCConnection#(ta a, tb b)(Empty);" \n
	append ss "endtypeclass" \n \n
	append ss "instance NCConnectable#(ta, function Action b(ta x)) provisos (Bits#(ta,st));" \n \n
	append ss "   module mkNCConnection#(ta a, function Action b(ta x))(Empty) provisos (Bits#(ta,st));" \n
	append ss "      let ncw <- mkNullCrossing(clockOf(b), a, clocked_by clockOf(a), reset_by noReset);" \n
	append ss "      rule doIt;" \n
	append ss "	 b(ncw);" \n
	append ss "      endrule" \n
	append ss "   endmodule" \n
	append ss "endinstance" \n \n
	append ss "instance NCConnectable#(function Action b(ta x), ta)" \n
	append ss "   provisos (Bits#(ta,st)," \n
	append ss "	     NCConnectable#(ta, function Action b(ta x)));" \n
	append ss "   module mkNCConnection#(function Action b(ta x), ta a)(Empty) provisos (Bits#(ta,st));" \n
	append ss "      mkNCConnection(a, b);" \n
	append ss "   endmodule" \n
	append ss "endinstance" \n \n

	append ss "`define mkCn mkNCConnection" \n
	append ss "`else" \n
	append ss "`define mkCn mkConnection" \n
	append ss "`endif" \n

	append ss "instance Connectable#($ifcName, $ifcDualName);\n"
	append ss "  module mkConnection#($ifcName l, $ifcDualName r)(Empty);\n"
        foreach p $ports {
            $p cnxDef ss "    "
        }
	append ss "  endmodule\n"
	append ss "endinstance\n"

        puts $of $ss
    }
    method writeWireDefs {} {
        foreach p $ports {
            $p wireDef ss "  "
        }
        puts $of $ss
    }
    method writeBVI {} {
        append ss "import \"BVI\" [$module getName] =\n"
        append ss "module $modName ("
        set bports [list]

#        foreach c $p_clocks {
#            if { [$c cget -dir] == "input" } {
#                lappend bports "Clock [$c bsv_method_name]"
#            }
#        }
#        foreach r $p_resets {
#            if { [$r cget -dir] == "input" } {
#                lappend bports "Reset [$r bsv_method_name]"
#            }
#        }
        lappend bports "$ifcName ifc"

        append ss [join $bports ",\n\t\t"]
        append ss "\n\t);\n\n"

        append ss "  default_clock (CLK, (*unused*)GATE);" \n
        append ss "  default_reset (RST_N);" \n
#
#        append ss "  // Clocks" \n
#        foreach o  $p_clocks {
#            append ss "   "
#            $o bvi_entry ss
#        }
#        append ss \n "  // Resets" \n
#        foreach o $p_resets {
#            append ss "  "
#            $o bvi_entry ss
#        }
#        append ss \n "  // Other pins" \n

        foreach o $p_others {
            append ss "  "
            $o bvi_entry ss
        }

        puts $of $ss
        set ss ""

        writeSchedule

        ## Closing the module
        append ss \n "endmodule: $modName" \n
        puts $of $ss
    }

    method writeSchedule { } {
        foreach c $p_clocks {
            set ss ""
            set those [filter $p_others clock $c]
	    set these [list]
	    foreach o $those {
		if {[regexp "dummyClockOut" [$o bsv_method_name]] == 0} {
		    lappend these $o
		}
	    }
            set outs [filter $these dir "output"]
            set ins [filter $these dir "input"]

            set os [utils::omap bsv_method_name $outs]
            set is [utils::omap bsv_method_name $ins]

            append ss "  // Schedule for methods with clock: [$c getName]" \n
            if { $os != {} } {
                append ss "  schedule ([join $os ,]) CF ([join $os ,]);" \n
                if { $is != {} } {
                    append ss "  schedule ([join $os ,]) SB ([join $is ,]);" \n
                }
            }

            while { $is != {} } {
                set is [lassign $is i]
                append ss "  schedule $i C $i;" \n
                if { $is == {} } { break }
                append ss "  schedule $i CF ([join $is ,]);" \n
            }

            puts $of $ss
        }
    }
}

#########################################################################
# This is for the interface of mkTop_dut
itcl::class ::CXGen::WriteInterface {
    inherit ::CXGen::BsvWriter

    variable instance ""
    variable module ""
    variable packName ""
    variable ifcName ""
    variable ExtName ""
    variable modName ""
    variable packBVIName ""
    variable impBVIName ""
    variable ifcBVIName ""
    variable ifcDualName ""
    variable modBVIName ""
    variable packs [list]

    variable local_xactors [list]

    constructor { globalstate outpath inst} {
        chain $globalstate $outpath
    } {
        set instance $inst
	set module [$instance getModule]
        setUpPorts $instance false

        set packBVIName [genBVIName $module package]
        lappend packs $packBVIName
        set impBVIName  [genBVIName $module ifc]
        set ifcBVIName  [genBVIName $module ifcProxy]
        set ifcDualName [genBVIName $module ifcDual]
        set modBVIName  [genBVIName $module proxyModule]

        set packName [genTOPName $instance package]
        set ifcName  [genTOPName $instance ifc]
        set ExtName  [genTOPName $instance Ext]
        set modName  [genTOPName $instance module]

        set xactors [list]
        foreach x [$gs cget -xactors] {
            if { [$x getInstance] == [$instance getName] } {
                lappend xactors $x
            }
        }
        set xactors [utils::nub $xactors]

#        assignUnassignedSignals
        foreach x $local_xactors {
            $x checkSanity EMU
        }

        foreach p [$instance getPorts] {
            eval lappend packs [[$p getTypeObj] getPackages]
        }

        safeOpen $packName writeAll ".bsv"
    }
    destructor {
        eval itcl::delete object $local_xactors
    }

    method getLocalXactor {} { return $local_xactors }

    method writeAll {} {
        writeHeader "Transactor wrapper" true
        writeImports $packs  true
        writeTypes
        writeInterfaces
	writeExtFn
        writeModule
    }
    method writeTypes {} {
        set ss ""
        foreach x $xactors {
            $x generateTypeDef ss
        }
        puts $of $ss
    }
    method writeInterfaces {} {
        append ss "// Interface for external ports" \n
	append ss "(* always_ready, always_enabled *)" \n
        append ss "interface $ExtName;" \n
        foreach p  [$gs cget -unusedports] {
	    if { [$p getName] ne "xtor_clk_enable" } {
		$p interfaceDef ss "  "
	    }
        }
        append ss "endinterface: $ExtName" \n\n

        append ss "// Interface for transactor wrapper" \n
        append ss "interface $ifcName;" \n
        append ss "  interface $ifcDualName toDut;" \n \n
        foreach x $xactors {
            $x interfaceDecl ss "  "
        }
        setUpPorts $module false
        # output clocks and resets
	# --- perhaps discriminate more exactly? XXX
        #foreach p $p_clocks {
        #    if { [$p cget -dir] eq "output" } {
        #        append ss "  interface Clock [$p bsv_method_name];" \n
        #    }
        #}
        #foreach p $p_resets {
        #    if { [$p cget -dir] eq "output" } {
        #        append ss "  interface Reset [$p bsv_method_name];" \n
        #    }
        #}
        append ss "endinterface: $ifcName" \n
        puts $of $ss
    }
    method writeExtFn {} {
	set ss ""
	append ss "function $ExtName ext_fn($impBVIName d) =" \n
        append ss "   (interface $ExtName;" \n
        foreach p  [$gs cget -unusedports] {
	    set n [$p getName]
	    if {$n ne "xtor_clk_enable" && [regexp "dummyClockOut" $n] == 0} {
		append ss "      method $n = d.$n;" \n
	    }
        }
        append ss "    endinterface) ;" \n
        puts $of $ss
    }
    method writeModule {} {
        writeModulePrelude
        writeModuleDut
        writeModuleState
        writeModuleInterfaceDef
        writeModuleClose
    }
    method writeModulePrelude {} {
        set bports [list]
#        foreach c $p_clocks {
#            if { [$c cget -dir] == "input" } {
#                lappend bports "Clock [$c bsv_method_name]"
#            }
#        }
#        foreach r $p_resets {
#            if { [$r cget -dir] == "input" } {
#                lappend bports "Reset [$r bsv_method_name]"
#            }
#        }
#
        lappend bports  "$ifcName ifc"
        append ss "module $modName (Clock uclk, " \n\t
        # write out clocks and resets
        append ss [join $bports ",\n\t"]
        append ss ");" \n\n
        puts $of $ss

    }
    method writeModuleDut {} {
        set instport [list]
        foreach c $p_clocks {
            if { [$c cget -dir] == "input" } {
                lappend instport  [$c bsv_method_name]
            }
        }
        foreach r $p_resets {
            if { [$r cget -dir] == "input" } {
                lappend instport  [$r bsv_method_name]
            }
        }

        append ss "  "
        append ss "$ifcBVIName  proxyDut <- $modBVIName (uclk);" \n
#        append ss [join $instport ",\n\t\t"]
#        append ss \n\t\t ");" \n
	append ss "  let dut = proxyDut.proxy;"
        puts $of $ss
    }
    method writeModuleState {} {
        append ss \n "  // State Elements and Rules..." \n
        foreach x $xactors {
            $x generateState ss "  "
            append ss \n
        }
        puts $of $ss
    }
    method writeModuleInterfaceDef  {} {
        append ss "  // Interface Definitions..." \n
        foreach x $xactors {
            $x generateInterfaceDef ss "  "
            #append ss "\n"
        }

        # output clocks and resets
	# --- see caveat above XXX
        #foreach p $p_clocks {
        #    if { [$p cget -dir] eq "output" } {
        #        append ss "  interface Clock [$p bsv_method_name] = dut.[$p bsv_method_name];" \n
        #    }
        #}
        #foreach p $p_resets {
        #    if { [$p cget -dir] eq "output" } {
        #        append ss "  interface Reset [$p bsv_method_name] = dut.[$p bsv_method_name];" \n
        #    }
        #}

        puts $of $ss
    }
    method writeModuleClose {} {
        append ss "  interface toDut = proxyDut.toDut;" \n
        append ss "endmodule: $modName" \n
        puts $of $ss
    }
    method assignUnassignedSignals {} {
        set used [list]
        foreach x $xactors {
            eval lappend used [$x getAllPorts]
        }
        utils::listToSet UsedPorts $used
        set uclks [list]
        array set Unused [list]

        foreach p $ports {
            if { [info exists UsedPorts($p)] } { continue }
            # ignore clocks and resets
            if { [$p cget -isclock] } { continue }
            if { [$p cget -isreset] } { continue }

	    $CXGen::GlobalState::gs add_obj unusedports $p
        }
    }
    method addTransactor { dir clk prts } {
        # puts "Adding transactor $dir [$clk show -name]: \n[join [utils::omap "show -name" $prts] \n]"
        switch -exact $dir {
            input  {
                set added [CXGen::InputXactor::mkDefaultXactor $clk $prts]
                lappend xactors $added
                lappend local_xactors $added
            }
            output {
                set added [CXGen::OutputXactor::mkDefaultXactor $clk $prts]
                lappend xactors $added
                lappend local_xactors $added
            }
            inout  {error "Inout ports are not part of a transactor: [utils::omap getName $prts]" }
            default {error "Unexpected direction in addTransactor" }
        }
    }
}




#########################################################################
## EmulationTop.bsv
itcl::class ::CXGen::WriteTransactors {
    inherit ::CXGen::BsvWriter

    public variable uclock "u_clock"
    public variable ureset "u_reset"
    public variable scemi_bridge_intnl "scemi_bridge_intnl"

    variable instances [list]
    variable ExtName ""
    variable impBVIName ""
    variable ifcDualName ""
    common toppackage "EmulationTop"
    common scemilayer "SceMiLayer"


    constructor { globalstate outpath newX insts} {
        chain $globalstate $outpath
    } {
        set instances $insts
	set instance [lindex $instances 0]
	set module [$instance getModule]
        set ExtName  [genTOPName $instance Ext]
        set impBVIName  [genBVIName $module ifc]
        set ifcDualName [genBVIName $module ifcDual]
        eval lappend xactors $newX
        safeOpen $toppackage writeAll ".bsv"
    }

    method writeAll {} {
        set pimports [list SceMi Connectable DefaultValue Clocks]
        foreach i $instances {
	    lappend pimports [genBVIName [$i getModule] package]
        }

        writeHeader  "Emulation top" true
	writeImports $pimports true
	writeInclude
        writeBridgeIfc
	writeTypes
	writeInterfaces
	writeExtFn
        writeSceMiBridge
        writeTopModule
	writeDualFn
    }

    method writeInclude {} {
        set ss ""
	append ss \n "`include \"PTM.defines\"" \n
        puts $of $ss
    }

    method writeBridgeIfc {} {
        set ss ""
        set p1 "  "

	append ss "interface PTMTop;" \n
	append ss $p1 "(*prefix=\"\"*) interface ToLink toLink;" \n
        foreach i $instances {
	    append ss $p1 "(*prefix=\"\"*) interface [genTOPName $i Ext] [genTOPName $i ext];" \n
	}
	append ss "endinterface" \n \n


        append ss "interface Bridge_intnl;" \n
        foreach x $xactors {
            ## These are interfaces provided by the transactors
            append ss $p1 "interface [$x getProvidedIfc] [$x getName] ;" \n
        }
#        append ss \n

#        foreach i $instances {
#            setUpPorts $i true
#            foreach c $clockConfigs {
#                append ss $p1 "interface Clock [$c getName] ;" \n
#            }
#            foreach r $p_resets {
#                if { [$r cget -dir] ne "input" }  { continue }
#                append ss $p1 "interface Reset [$r getInstName];" \n
#            }
#        }
#        append ss $p1 "interface Clock $uclock;" \n
#        append ss $p1 "interface Reset $ureset;" \n
        append ss "endinterface: Bridge_intnl" \n
        puts $of $ss
    }

    method writeTypes {} {
        set ss ""
        foreach x $xactors {
            $x generateTypeDef ss
        }
        puts $of $ss
    }

    method writeInterfaces {} {
        append ss "// Interface for external ports" \n
	append ss "(* always_ready, always_enabled *)" \n
        append ss "interface $ExtName;" \n
        foreach p  [$gs cget -unusedports] {
	    if { [$p getName] ne "xtor_clk_enable" } {
		$p interfaceDef ss "  "
	    }
        }
        append ss "endinterface: $ExtName" \n\n
        puts $of $ss
    }
    method writeExtFn {} {
	set ss ""
	append ss "function $ExtName ext_fn($impBVIName d) =" \n
        append ss "   (interface $ExtName;" \n
        foreach p  [$gs cget -unusedports] {
	    set n [$p getName]
	    if {$n ne "xtor_clk_enable" && [regexp "dummyClockOut" $n] == 0} {
		append ss "      method $n = d.$n;" \n
	    }
        }
        append ss "    endinterface) ;" \n
        puts $of $ss
    }

    method writeSceMiBridge {} {
        set ss ""
        set p1 "  "

        append ss "module \[SceMiModule\] $scemi_bridge_intnl #(Bool xtor_enable) (Bridge_intnl);" \n

        append ss \n $p1 "// Transactor definitions" \n
        foreach x $xactors {
            ## These are interfaces provided by the transactors
            set preInst [$x getPreInstance]
            if { $preInst ne "" } {
                append ss [regsub -all -line -- {^} $preInst $p1] \n
            }
            append ss $p1 "[$x getProvidedIfc] x_[$x getName] <- [$x getXactorInstance];" \n
        }


        append ss \n $p1 "// Interface definitions" \n
        foreach x $xactors {
            ## These are interfaces provided by the transactors
            set ifcx [$x getProvidedIfc]
            set ifcx [regsub {\#.*} $ifcx ""]
            append ss $p1 "interface $ifcx [$x getName] = x_[$x getName];" \n
        }
#        append ss \n


#        foreach i $instances {
#            setUpPorts $i true
#            foreach c $clockConfigs {
#                append ss $p1 "interface Clock [$c getName] = i_[$c getName];" \n
#            }
#            foreach r $p_resets {
#                if { [$r cget -dir] ne "input" }  { continue }
#                append ss $p1 "interface Reset [$r getInstName] = i_[$r getInstName];" \n
#            }
#        }
#        append ss $p1 "interface Clock $uclock = i_$uclock ;" \n
#        append ss $p1 "interface Reset $ureset = i_$ureset ;" \n

        append ss "endmodule: $scemi_bridge_intnl" \n

        puts $of $ss
    }
    method writeTopModule {} {
        set ss ""
        set p1 "  "
        set dut "dut"

	append ss "interface SceMiBridge;" \n
	append ss "   (*prefix=\"\"*) interface Bridge toDut;" \n
	append ss "   (*prefix=\"\"*) interface ToLink toLink;" \n
	append ss "endinterface: SceMiBridge" \n \n

	append ss "interface Bridge;" \n
        foreach i $instances {
	    append ss "   interface [genBVIName [$i getModule] ifcDual]  to_[genTOPName $i inst];" \n
            setUpPorts $i true
#            foreach c $clockConfigs {
#                append ss "  interface Clock [$c getName];" \n
#            }
#            foreach r $p_resets {
#                if { [$r cget -dir] ne "input" }  { continue }
#                append ss "  interface Reset [$r getInstName];" \n
#            }
	}
	append ss "endinterface: Bridge" \n \n

        append ss "module \[SceMiModule\] scemi_bridge#(Clock uclk, Reset urst, Bool xtor_enable)(Bridge);" \n
        append ss $p1 "let _bridge <- $scemi_bridge_intnl" "(xtor_enable, clocked_by uclk, reset_by urst);" \n
        foreach i $instances {
	    append ss $p1 "interface to_[genTOPName $i inst] = dualIfc_[$i getModule](_bridge);" \n
	}
	append ss "endmodule: scemi_bridge" \n

        puts $of $ss
	set ss ""

        append ss  "(*synthesize, options=\"-no-keep-fires\", default_clock_osc = \"CLK_step_clk\"*)" \n
        append ss "module \[Module\] ptm_top #(`BridgeParamsDcl) (PTMTop);" \n
        append ss $p1 "SceMiBridge bridge <- ptm_bridge(`BridgeParams);" \n
	append ss "`ifdef SCEMI_TCP" \n
        append ss $p1 "let xtor_rstn <- mkAsyncResetFromCR(1, xtor_clk);" \n
	append ss "`endif" \n

        foreach i $instances {
            # clock configs are per instance
            setUpPorts $i true
	    set n [genTOPName $i inst]
	    append ss "`ifdef SCEMI_TCP" \n
            append ss $p1 "[genBVIName [$i getModule] ifc]  $n <- [genBVIName [$i getModule] module]"
            append ss " (clocked_by xtor_clk, reset_by xtor_rstn);" \n
	    append ss "`else" \n
            append ss $p1 "[genBVIName [$i getModule] ifc]  $n <- [genBVIName [$i getModule] module] ();" \n
	    append ss "`endif" \n
            append ss $p1 "Empty conn_$n <- mkConnection ($n, bridge.toDut.to_$n);" \n
        }

        append ss \n "  interface toLink = bridge.toLink;"
        foreach i $instances {
	    append ss \n "  interface [genTOPName $i ext] = ext_fn([genTOPName $i inst]);"
	}
        append ss \n "endmodule: ptm_top" \n

        puts $of $ss
    }

    method writeDualFn {} {
	set ss ""

	# TODO: Define one function per module
	set module [[lindex $instances 0] getModule]

        append ss "function $ifcDualName dualIfc_$module (Bridge_intnl bridge) =\n"
        append ss "  ( interface $ifcDualName;\n"
        set bports [list]
#
#        foreach c $p_clocks {
#            if { [$c cget -dir] == "input" } {
#                lappend bports "Clock [$c bsv_method_name]"
#            }
#        }
#        foreach r $p_resets {
#            if { [$r cget -dir] == "input" } {
#                lappend bports "Reset [$r bsv_method_name]"
#            }
#        }

	foreach x $xactors {
            $x generateAssignments ss "  "
            append ss \n
        }

        append ss "endinterface );\n"
        puts $of $ss
    }
}

#########################################################################
itcl::class ::CXGen::WriteBuildScript {
    inherit ::CXGen::BsvWriter

    public variable bsvoutpath

    constructor { globalstate outpath } {
        chain $globalstate "./"
    } {
        set bsvoutpath $outpath
        safeOpen $build_init writeCfg ".cfg"

        set buildcmd "build --transactor_build $build_init.cfg"
        if { [catch "exec $buildcmd >@ stdout 2>@ stderr" err] } {
            set msg "Command: `$buildcmd' had an abnormal exit"
            return -code error $msg
        }
        puts "Created file: ./project.bld"
    }

    method writeCfg {} {
        writeHeader "Build configure script" false "#"
        writeBody
    }
    method writeBody {} {
        set ss {[Default]}
        append ss "\n"
        append ss "target-root-directory:	$bsvoutpath" \n
        append ss "top-file:	transactors/EmulationTop.bsv" \n
        append ss "top-module:	ptm_bridge \n"
        append ss "xilinx-ooc-synthesis:	true" \n

        set finalpath [list $bsvoutpath]
	#puts stderr "finalpath: $finalpath"
        set sp [CXGen::XUtils::canonicalPath [lindex [Bluetcl::flags show p] 1]]
	#puts stderr "sp: $sp"
        foreach p [split $sp ":"] {
            # strip out default paths
            switch -exact $p {
                .                   continue
                %/Prelude           continue
                %/Libraries         continue
                %/Libraries/BlueNoC continue
                +                   continue
            }
            lappend finalpath $p
        }
	# CXGen itself needs Xactors for CXactors, and that requires TLM3 for CB:
	lappend finalpath "%/Libraries/Xactors"
	#lappend finalpath "%/Libraries/TLM3"
	set finalpath [utils::nub $finalpath]
        append ss "bsv-source-directories:  $finalpath" \n
        lappend finalpath +
	#puts stderr "finalpath: $finalpath"
        append ss "scemilink-options:       -p  [join ${finalpath} :]" \n

        # include directories base on imported files
        set vldirs [list]
        foreach tf [$gs cget -topfiles] {
            lappend vldirs [file dirname $tf]
        }
	# The ptm_top will not be synthesized, so we don't need the DUT Verilog
        #append ss "verilog-lib-directories: [utils::nub $vldirs]" \n
	# temporarily:
	#append ss "verilog-lib-directories: ../XactorsL [utils::nub $vldirs]" \n

	# "build" will add -keep-fires unless we specify not to
	append ss "bsc-compile-options: -no-keep-fires" \n
	append ss "bsc-link-options: -no-keep-fires" \n

        ## get transactor specific options
        set cxxfiles [list  $bsvoutpath/$className.cpp]
        set cxxoptions [list -O3 -I\$BLUESPECDIR/SceMi/bsvxactors]
        foreach x $xactors {
            eval lappend cxxfiles [$x getCxxFiles]
            eval lappend cxxoptions [$x getCxxOptions]
        }
        set cxxfiles [utils::nub $cxxfiles]
        set cxxoptions [utils::nub $cxxoptions]

        # Testbench code
        append ss "c++-files:             "
        foreach f $cxxfiles {
            append ss " "  $f
        }
        append ss \n

        append ss "c++-options:             "
        foreach f $cxxoptions {
            append ss " "  $f
        }
        append ss \n



        puts $of $ss
    }
}

#########################################################################
itcl::class ::CXGen::WriteCPP {
    inherit ::CXGen::BsvWriter


    constructor { globalstate outpath newX} {
        chain $globalstate $outpath
    } {
        eval lappend xactors $newX

        safeOpen $className  writeSWHeaderFile ".hpp"
        safeOpen $className  writeSWBodyFile   ".cpp"
    }

    method writeSWHeaderFile {} {
        writeHeader "Software-side proxy model" true
        writeIncludes
        writeClassDef
    }

    method writeIncludes {} {
        set incls [list]
        foreach x $xactors {
            eval lappend incls [$x cppIncludes]
        }
        set incls [utils::nub $incls]
        append ss "#pragma once" \n\n
        append ss "// Include files" \n
        foreach i $incls {
            append ss "#include \"$i\"" \n
        }
        append ss \n
        puts $of $ss
    }
    method writeClassDef {} {
        set ss "\n"
        set p1 $indent
        set p2 [nextIndent $p1]
        append ss "class $className {" \n
        append ss  "  public:" \n
        append ss \n
        createMembers ss $p1
        createXstructors ss $p1

        append ss "};" \n\n
        puts $of $ss
    }

    method createMembers {ssname p1} {
        upvar $ssname ss
        set p2 [nextIndent $p1]
        append $p1 "/// Member transactors" \n
        foreach x $xactors {
            append ss $p1 "[$x getCProxyClass] \t m_[$x cget -name];" \n
        }
        append ss \n
        append ss $p1 "/// Optional pointer to Service Thread" \n
        append ss $p1 "SceMiServiceThread *m_pService ;" \n
    }
    method createXstructors {ssname p1} {
        upvar $ssname ss
        set p2 [nextIndent $p1]
        append ss \n\n
        append ss $p1 "/// Constructor" \n
        append ss $p1 "$className (const std::string &path, bool startServiceThread = true);" \n
        append ss $p1 "/// Destructor" \n
        append ss $p1 "~$className () ;" \n\n
        append ss $p1 "/// SceMi initialization, call if needed" \n
        append ss $p1 "static int initSceMi (const std::string &paramFile);" \n
    }
    method writeSWBodyFile {} {
        writeHeader "Software side proxy model" true
        writeBodyInclude
        writeConstrBody
        writeInitSceMiBody
    }
    method writeBodyInclude {} {
        set ss \n
        set includes [list $className.hpp]
        foreach i $includes {
            append ss [format "#include \"%s\"" $i] \n
        }
        puts $of $ss
    }
    method writeInitSceMiBody {} {
        set ss \n
        set p1 [nextIndent ""]
        set p2 [nextIndent $p1]
        append ss "int ${className}::initSceMi (const std::string &paramsFile)" \n
        append ss "{" \n
        append ss $p1 "SceMiParameters cScemiParams( paramsFile.c_str() );" \n
        append ss $p1 "SceMi *cScemi = SceMi::Init(SceMi::Version(SCEMI_VERSION_STRING), &cScemiParams);" \n
        append ss $p1 "if (!cScemi) {" \n
        append ss $p2 "fprintf(stderr, \"@ERROR: Cannot create the scemi object\");" \n
        append ss $p2 "return 1;" \n
        append ss $p1 "}" \n
        append ss $p1 "return 0;" \n
        append ss "}" \n
        puts $of $ss
    }
    method writeConstrBody {} {
        set ss \n
        set p1 [nextIndent ""]
        set p2 [nextIndent $p1]
        append ss "${className}::$className (const std::string &path, bool startServiceThread)" \n
        set punct :
        foreach x $xactors {
            set name [$x cget -name]
            set xtraArgs [$x proxyArgs]
            append ss $p1 $punct [format " m_%s (%s)" $name $xtraArgs] \n
            set punct ,
        }
        append ss $p1 $punct " m_pService (0)" \n
        append ss "{" \n
        foreach x $xactors {
            set name [$x cget -name]
            set postConst [$x cppPostConstr $p1 "m_$name"]
            append ss $postConst
        }

        append ss $p1 "if (startServiceThread) {" \n
        append ss $p2 "SceMi *cScemi = SceMi::Pointer();" \n
        append ss $p2 "m_pService = new SceMiServiceThread (cScemi);" \n
        append ss $p1 "}" \n

        append ss "}" \n\n
        append ss "${className}::~$className () {" \n
        append ss $p1 "if (m_pService != NULL) {" \n
        append ss $p2 "// Destruction stops the thread" \n
        append ss $p2 "delete m_pService;" \n
        append ss $p2 "m_pService = 0;" \n
        append ss $p1 "}" \n
        append ss "}" \n
        puts $of $ss
    }

}


#########################################################################
itcl::class ::CXGen::WriteSystemC {
    inherit ::CXGen::BsvWriter

    private variable cppClassName

    constructor { globalstate outpath newX} {
        chain $globalstate $outpath
    } {
        eval lappend xactors $newX

        set cppClassName $className
        set className [format "SystemC_%s" "Proxy"]

        safeOpen $className  writeSCHeaderFile ".hpp"
        # safeOpen className  writeSCBodyFile   ".cpp"
    }
    method writeSCHeaderFile {} {
        writeHeader "SystemC proxy  model" true
        writeIncludes
        writeClass
    }
    method writeIncludes {} {
        append ss "#pragma once" \n\n
        append ss "#include \"tlm.h\"" \n
        append ss "#include \"tlm_utils/simple_initiator_socket.h\"" \n
        append ss "#include \"tlm_utils/simple_target_socket.h\"" \n
        append ss \n
        append ss "// BSC header found in \$BLUESPECDIR/SceMi/BlueNoC" \n
        append ss "#include \"tlm_systemc.h\"" \n
        append ss "#include \"$cppClassName" ".hpp\"" \n
        puts $of $ss
    }
    method writeClass {} {
        set p1 $indent

        append ss \n
	if { [hasInitiator] } {
	    append ss "class $className;" \n
	    append ss "SC_HAS_PROCESS($className);" \n
	    append ss \n
	}
        append ss "class $className : public sc_core::sc_module {" \n
        append ss "  " "public:" \n
        createMembers ss $p1
        createConstructor ss $p1
        append ss \n\n
        createBtransport ss $p1
        if { [hasInitiator] } {
            append ss \n\n
            createThread ss $p1
        }
        append ss \n
        append ss "};" \n
        puts $of $ss
    }

    private method socket_obj_name { xtor } {
        format "m_%s_socket" [$xtor cget -name]
    }
    private method target_b_transport_name { xtor } {
        format "%s_custom_b_transport" [$xtor cget -name]
    }
    private method hasInitiator {} {
        foreach x $xactors {
            if { ! [$x isTLM] } { continue }
            if { [$x isSCInitiator] } { return true }
        }
        return false;
    }
    method createMembers { sname p1} {
        upvar $sname ss
        set p2 [nextIndent $p1]
        append ss $p1 "// References to C++-based proxies are public" \n
        foreach x $xactors {
            append ss $p1 [format "%s\t & m_%s ;" \
                               [$x getCProxyClass] [$x cget -name] \
                              ] \n
        }
        append ss \n\n
        append ss $p1 "// TLM based proxies have sockets" \n
        foreach x $xactors {
            if { ! [$x isTLM] } { continue }
            set it [$x sysc_i_or_t]
            set stype "tlm_utils::simple_${it}_socket< $className >"

            append ss $p1 [format "%s\t%s ;" \
                               $stype \
                               [socket_obj_name $x] \
                              ] \n
        }
        append ss \n
    }
    method createConstructor {sname p1} {
        upvar $sname ss
        set p2 [nextIndent $p1]
        append ss $p1 "$className ( sc_core::sc_module_name module_name, $cppClassName & cmodel )" \n
        append ss $p2 ": sc_module (module_name)" \n
        foreach x $xactors {
            set nm [$x cget -name]
            append ss $p2 ", m_$nm (cmodel.m_$nm)" \n
        }
        append ss $p1 "{" \n
        append ss $p2 "// register sockets" \n
        foreach x $xactors {
            if { ! [$x isTLM] } { continue }
            set nm [socket_obj_name $x]

            if { [$x isSCTarget] } {
                set bt [target_b_transport_name $x]
                append ss $p2 "$nm.register_b_transport(this, & ${className}::${bt} );" \n
            }
        }
        if { [hasInitiator] } {
            append ss \n
            append ss $p1 "SC_THREAD (thread_$className);" \n
        }
        append ss $p1 "}" \n

        append ss \n $p1 "// Destructor" \n
        append ss $p1 "~$className () {}" \n

    }
    method createBtransport {sname p1} {
        upvar $sname ss
        set p2 [nextIndent $p1]
        append ss "  private:" \n
        append ss $p1 "// Custom b transports for targets" \n
        foreach x $xactors {
            if { ! [$x isTLM] } { continue }
            if { ! [$x isSCTarget] } { continue }
            set xnm [$x cget -name]

            set nm [target_b_transport_name $x]
            append ss $p1 "void $nm ( tlm::tlm_generic_payload &payload,"
            append ss " sc_core::sc_time &delay) {" \n
            append ss $p2 "bsv_tlm_sysc::send_request_response( m_$xnm, payload);" \n
            append ss $p1 "}" \n\n
        }
    }
    method createThread {sname p1} {
        upvar $sname ss
        set p2 [nextIndent $p1]
        set p3 [nextIndent $p2]
        append ss "  private:" \n
        append ss $p1 "void thread_$className (void)" \n
        append ss $p1 "{" \n
        append ss $p2 "sc_core::sc_time delay(1, sc_core::SC_NS);" \n
        append ss $p2 "// Service the initiators" \n
        append ss $p2 "while (1) {" \n
        append ss $p3 "wait(delay);" \n
        foreach x $xactors {
            if { ! [$x isTLM] } { continue }
            if { [$x isSCTarget] } { continue }

            set sock [socket_obj_name $x]
            set xnm   [$x cget -name]
            append ss $p3 "bsc_service_initiator_proxy ( $sock, m_$xnm);" \n

        }
        append ss $p2 "}" \n
        append ss $p1 "}" \n
    }

}


######################################################################
## Utility functions for the package
namespace eval ::CXGen::XUtils {
    namespace export name_clash uid filter canonicalPath prependSearchPath getObjectFromName
    namespace export toBSVInt toBSVBool toBSVId decodeBuild
    namespace import ::itcl::*
    variable _unique 0

    # Build codes
    variable BuildCode [list \
                          Check 0 \
                          BVI 1 \
                          IFC 2 \
                          EMU 3 \
                          Project 4 \
                          Proxies 5 \
                          SysC 6 \
                          All 10
                         ]
    variable BuildCodeTable
    array set BuildCodeTable $BuildCode
}
proc ::CXGen::XUtils::name_clash {obj nameMethod } {
    set allObjs [find objects -class [$obj info class]]
    set allObjs [lsearch -inline -all -exact -not $allObjs $obj]
    set allNames [utils::omap $nameMethod $allObjs]
    set name [$obj getName]
    if { [lsearch -exact $allNames $name] != -1 } {
        error "Name clash: an object named '$name' already exists in class [$obj info class]"
    }
}
proc ::CXGen::XUtils::filter {objs args} {
    proc ff {attr patt obj} {
        set v [$obj cget -$attr]
        regexp $patt $v
    }
    foreach {attr regx} $args {
        set objs [utils::filter [list ff $attr $regx] $objs]
    }
    return $objs
}
proc ::CXGen::XUtils::uid {} {
    variable _unique
    format "%04d" [incr _unique]
}

# canonicial path  eg. % for BLUESPECDIR  no // or trailing /
proc ::CXGen::XUtils::canonicalPath { pathin } {

    regsub -all -- {//} $pathin "/"  pathin
    regsub -all -- {|/:|/$} $pathin "" pathin
    regsub -all -- {//} $::env(BLUESPECDIR) "/" bsvdir
    regsub -all -- {/$} $bsvdir "" bsvdir
    regsub -all -- $bsvdir $pathin %
}

proc ::CXGen::XUtils::prependSearchPath { entry } {

    set sp [canonicalPath [lindex [Bluetcl::flags show p] 1]]
    set entry [canonicalPath $entry]
    if { ![regexp -- $entry $sp] } {
        Bluetcl::flags set -p $entry:+
        set sp [canonicalPath [lindex [Bluetcl::flags show p] 1]]
    }
    return $sp
}
proc ::CXGen::XUtils::getObjectFromName { testobj classname getnamemethod } {
    if {! [catch [list $testobj isa $classname] stat ] } {
        if { $stat } {
            return $testobj
        } else {
            set msg "$testobj is not a $classname"
            return -code error $msg
        }
    }
    # not an object but it could be the name of one.
    set objs [find object -class $classname]
    proc namematch { nm getnamem obj } {
        if { [$obj $getnamem] == $nm } { return true }
        return false
    }
    set matches [utils::filter "namematch $testobj $getnamemethod" $objs]
    if { [llength $matches] == 0 } {
        set msg "$testobj is not a $classname"
        return -code error $msg
    } elseif { [llength $matches] == 1 } {
        return [lindex $matches 0]
    } else {
        set msg "$testobj does not match a unique object of $classname -- $matches [llength $matches]"
        return -code error $msg
    }
    set msg "Impossible condition!"
    return -code error $msg
}

proc ::CXGen::XUtils::toBSVBool { d } {
    if { $d } { return "True" }
    return "False"
}
proc ::CXGen::XUtils::toBSVInt { d } {format "%d" $d}
proc ::CXGen::XUtils::toBSVId { d } {return $d}


proc ::CXGen::XUtils::document { } {
    #  Documentation for some CXGen class
    set m [CXGen::Module::mkModule  documentModule]
    set p [$m addPort documentPort inout]
    set i [CXGen::Instance::mkInstance  documentInstance $m]
    set ip [utils::head [$i getPorts]]

    set gs $::CXGen::GlobalState::gs
    set exppattern [namespace eval ::CXGen {namespace export}]
    set exports [lsort [utils::map "namespace tail" [info commands {::CXGen::[a-z]*}]]]
    puts "Top level commands:"
    foreach ex $exports {
        puts "  $ex \{[info args $ex]\}"
    }

    puts ""

    utils::map_ documentObject [list $p $ip $m $i]

    puts "Transactor classes"
    set classes [$gs showXactorTypes "  "]
    utils::map_ documentClass $classes

    CXGen::clear
}


proc ::CXGen::XUtils::documentClass { cls } {
    # create a class object so we can see the introspect it...
    set shortname [namespace tail $cls]

    set o ""
    if { [catch {
        set o [uplevel #0 [list $cls o1#auto  n2$shortname]]
        documentObject $o
    } err] } {
        puts $err
    }

    if { $o ne "" } {
        catch itcl::delete object $o
    }

}

proc ::CXGen::XUtils::documentObject { o } {

    puts "Documentation for class: [namespace tail [$o info class]]"

    # configure options
    puts " Configure options:"
    set vars [$o info variable]
    foreach v $vars {
        set details [$o info variable $v -protection -type -name -value]
        lassign $details prot typ name val
        if { $prot ne "public" } { continue }
        if { $typ  ne "variable" } { continue }
        puts [format "  %s %s" [namespace tail $name] [list $val] ]
    }

    # methods and proc
    puts " Methods:"
    set fns [$o info function]
    set fns [lsort [utils::nub [utils::map "namespace tail" $fns]]]
    foreach f $fns {
        set details [$o info function $f -protection -type -name -args]
        lassign $details prot typ name ags
        if { $prot ne "public" } { continue }
        if { $typ  ne "method" } { continue }
        puts [format "  %s %s {%s}" $typ [namespace tail $name] $ags ]
    }
    puts ""
}

proc ::CXGen::XUtils::decodeBuild {id} {
    variable BuildCodeTable
    return $BuildCodeTable($id)
}


#########################################################################
## Provide cross references across classes and namespaces
namespace eval ::CXGen::Port {
    namespace import ::CXGen::XUtils::*
}
namespace eval ::CXGen::IPort {
    namespace import ::CXGen::XUtils::*
}
namespace eval ::CXGen::Module {
    namespace import ::CXGen::XUtils::*
}
namespace eval ::CXGen::Instance {
    namespace import ::CXGen::XUtils::*
}
namespace eval ::CXGen::Xactor {
    namespace import ::CXGen::XUtils::*
}
namespace eval ::CXGen::GlobalState {
    namespace import ::itcl::*
    namespace import ::CXGen::XUtils::*
}


#########################################################################
# initialize package -- create singleton here
CXGen::GlobalState::init

# These package depend on the CXGen namespace and hence required late.
package require CXFields
package require CXactors


namespace import CXGen::*
