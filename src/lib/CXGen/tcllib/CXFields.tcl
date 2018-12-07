# Copyright 2012--2013 Bluespec, Inc.  All rights reserved.
# $Id$

package require Itcl

package require CXGen
package provide CXFields 1.1


#####################################################################
itcl::class CXGen::Module {
    variable name ""
    variable ports [list]

    constructor { mname args } {
        set name $mname
        # check for name clash
        name_clash $this getName
        $CXGen::GlobalState::gs add_obj modules $this
        eval configure $args
    }
    destructor {
        $CXGen::GlobalState::gs delete_obj modules $this
        foreach p $ports {
            itcl::delete object $p
        }
    }
    method getName {} { set name }
    method getPorts {} { set ports }
    method addPort { name dir args } {
        set np [uplevel #0 CXGen::Port #auto $name $dir $this $args]
        lappend ports "::$np"
        return ::$np
    }
    method deletePorts { delPorts } {
        foreach port $delPorts {
            set obj [getObjectFromName $port CXGen::Port getName]
            set ports [lsearch -inline -all -exact -not $ports $obj]
            itcl::delete object $obj
        }
    }
    method show {args} {
        set $name
    }
    method debug {args} {
        puts stderr [eval show $args]
    }
    proc mkModule { name args } {
        set cmdName $name
        ::types::import_package false Prelude
        if { [info commands $name] ne [list] } {
            set cmdName "#auto"
        }
        set m [uplevel #0 ::CXGen::Module $cmdName $name $args]
        set msg "Created [namespace tail [::$m info class]] object: $m"
        if { $m ne $name } {
            append msg " representing $name"
        }
        puts $msg
        return "::$m"
    }
}

#####################################################################
itcl::class CXGen::Instance {
    variable name ""
    variable module ""
    variable ports [list]

    constructor { iname mod } {
        set name $iname
        set module $mod
        # check for name clash
        name_clash $this getName
        $CXGen::GlobalState::gs add_obj instances $this
        eval configure

        foreach mp [$mod getPorts] {
            set ip [CXGen::IPort::mkPort $mp $this]
            set PMap($mp) $ip
            lappend ports $ip
        }
        foreach mp [$mod getPorts] {
            foreach fld [list clock reset] {
                set vp [$mp cget -$fld]
                if { $vp ne "" } {
                    set vi $PMap($vp)
                    $PMap($mp) configure -$fld $vi
                }
            }
        }

    }
    destructor {
        $CXGen::GlobalState::gs delete_obj instances $this
    }
    method getName {}   { set name }
    method getModule {} { set module }
    method getPorts {}  { set ports }
    method show {args} {
        if { [lsearch -exact $args "-full"] } {
            lappend args "-ports"
        }

        set ss [format "Instance %s of module %s" [getName] [$module getName]]
        if { [lsearch -exact $args "-ports"] } {
            foreach p $ports {
                append ss "\n " [eval $p show $args]
            }
        }
        return $ss
    }
    method debug {args} {
        puts stderr [eval show $args]
    }
    # name is a string,  mod is a string or an object (or both)
    proc mkInstance { name mod } {
        set cmdName $name
        if { [uplevel #0 info commands $name] ne [list] } {
            set cmdName "#auto"
        }
        # check the mod is a module
        set modObj [getObjectFromName $mod CXGen::Module getName]

        set m [uplevel #0 ::CXGen::Instance $cmdName $name $modObj]
        set msg "Created [namespace tail [::$m info class]] object: $m"
        if { $m ne $name } {
            append msg " representing $name"
        }
        puts $msg
        return "$m"
    }
}


#####################################################################
itcl::class CXGen::Port {
    public variable name "x" check_name_clash
    variable bsv_method_name ""
    public variable dir  "inout" check_direction
    public variable bsvtype ""   check_type
    variable  typeObj ""

    public variable width 1
    public variable isclock false  "stdbool isclock"
    public variable isreset false  "stdbool isreset"
    public variable clock "" "isport clock"
    public variable reset "" "isport reset"

    public variable module   ""    "readOnly module  moduleObj getName"
    variable moduleObj
    public variable  instance ""   "readOnly instance instanceObj getName"
    variable instanceObj ""

    ## Constructor and Destructor are not executed in this scope
    constructor {_name _dir _mod args} {
        set name $_name
        set dir $_dir
        set moduleObj $_mod
        set module [$moduleObj getName]
        eval configure $args

        check_name_clash
        check_direction
        $CXGen::GlobalState::gs add_obj ports $this
    }
    destructor {
        $CXGen::GlobalState::gs delete_obj ports $this
    }
    method isport {p} {
        set v [cget -$p]
        if { $v ne "" } {
            set err [catch "$v isa [info class]" val]
            if { $err || ! $val } {
                # lookup name
                set o [filter [itcl::find objects -class [info class]] name "^$v\$" module $module]
                if { $o == "" } {
                    set $p ""
                    return -code error "Cannot find a port named $v to use a '$p'"
                }
                set $p $o
            } else {
                # v already is the Port object
                set $p $v
            }
            # More checks on p
            # is one object
            if { [llength [set $p]] != 1 } {
                set $p ""
                set msg "Only one $p association is allowed on a port"
                return -code error $msg
            }
            # is from proper module
            if { [ [set $p] cget -module] != $module } {
                set $p ""
                set msg "A $p association must be from the same module"
                return -code error $msg
            }
        }
    }
    method getName {} { return $name }
    method getModule {} { return $moduleObj }
    method getInstanceObj {} { set instanceObj }
    method bsv_method_name {} {return $bsv_method_name}
    method getInstName {} {
        format "%s_%s" $instance $name
    }
    method getType {} {
        if { $bsvtype eq "" } {
            return "Bit#($width)"
        }
        return $bsvtype
    }
    method check_type {} {
        if { ![catch [list ::Virtual::createType [getType]] err] } {
            set typeObj $err
        } else {
            set typeObj ""
            return -code error $err
        }
        return $typeObj
    }

    method getTypeObj {} {
        if { $typeObj == "" } { check_type }
        return $typeObj
    }
    method show {args} {
        switch -regexp $args {
            {-full} {
                set cby "noClock"
                if { $clock ne "" } { set cby [$clock getName]}
                set rby "noReset"
                if { $reset ne "" } { set rby [$reset getName]}
                set cstr ""
                if { $isclock } { set cstr " isClcok"}
                set rstr ""
                if { $isreset } { set rstr " isReset"}
                set type2 ""
                if { $bsvtype ne "" } { set type2 "type:$bsvtype" }
                set inst ""
                if { $instance ne "" } { set inst "$instance." }

                format "Port: '%s%s' dir:%s width:%d clock: %s reset: %s%s%s%s" \
                    $inst $name $dir $width $cby $rby $cstr $rstr $type2
            }
            {-name} {
                format "%s" $name
            }
            default {
                format "%s\t(%s %d)"   $name $dir $width
            }
        }
    }
    method debug {args} {
        puts stderr "[eval show $args]"
    }
    method check_name_clash {} {
        name_clash $this getInstName
        if { ![string is lower [string index $name 0]]} {
            set bsv_method_name m$name
        } else {
            set bsv_method_name $name
        }
    }
    method check_direction {} {
        set valid_dirs [list input output inout]
        if { [lsearch -exact $valid_dirs  $dir] == -1 } {
            error "port direction must be one of $valid_dirs, found $dir"
        }
    }
    method readOnly { field srcObj srcMeth } {
        set $field [[set $srcMeth] $srcMeth]
        puts stderr "Warning: Cannot change a port's $field"
    }

    method stdbool { field } {
        if { [set $field] } { set $field true } else { set $field false }
    }
    # generate line for interface definition
    method interfaceDef {strname p1 } {
        upvar $strname ss
	if {[regexp "dummyClockOut" [bsv_method_name]] == 0} {
	    if { $isclock } {
		switch -exact $dir {
		    output {append ss $p1 "interface Clock \t[bsv_method_name];" \n
		    }
		}
		return
	    }

	    if { $isreset } {
		switch -exact $dir {
		    output {append ss $p1 "interface Reset \t[bsv_method_name];" \n
		    }
		}
		return
	    }
	    switch -exact $dir {
		input  {append ss $p1 "(*prefix=\"\"*) method Action \t[bsv_method_name] ((*port=\"[getName]\"*) [getType] x);" \n}
		output {append ss $p1 "(*result=\"[getName]\"*) method [getType] \t[bsv_method_name] ();" \n }
		inout  {append ss $p1 "interface Inout#([getType]) \t[bsv_method_name];" \n}
	    }
        }
    }
    method interfaceDualDef {strname p1 } {
        upvar $strname ss
	if {[regexp "dummyClockOut" [bsv_method_name]] == 0} {
	    if { $isclock } {
		switch -exact $dir {
		    output {append ss $p1 "interface Clock \t[bsv_method_name];" \n
		    }
		}
		return
	    }
	    if { $isreset } {
		switch -exact $dir {
		    output {append ss $p1 "interface Reset \t[bsv_method_name];" \n
		    }
		}
		return
	    }
	    switch -exact $dir {
		output {append ss $p1 "(*prefix=\"\"*) method Action \t[bsv_method_name] ((*port=\"[getName]\"*) [getType] x);" \n}
		input  {append ss $p1 "(*result=\"[getName]\"*) method [getType] \t[bsv_method_name] ();" \n }
		inout  {append ss $p1 "interface Inout#([getType]) \t[bsv_method_name];" \n}
	    }
        }
    }
    method wireDef {strname p1 } {
        upvar $strname ss
        set clk "no_clock"
        if { $clock ne "" } {
            set clk [$clock bsv_method_name]
        }
        set rst "no_reset"
        if { $reset ne "" } {
            set rst [$reset bsv_method_name]
        }
        if { $isclock } {
            return
        }
        if { $isreset } {
            return
        }
        switch -exact $dir {
            input  {append ss $p1 "Wire#( [getType] ) w_[bsv_method_name] <- mkBypassWireCU(uclk);" \n}
            output {append ss $p1 "Wire#( [getType] ) w_[bsv_method_name] <- mkBypassWireUC(uclk);" \n}
        }
    }
    method methodDef {strname p1 } {
        upvar $strname ss
        if { $isclock } {
            return
        }
        if { $isreset } {
            return
        }
        switch -exact $dir {
            input  {append ss $p1 "method \t[bsv_method_name] = w_[bsv_method_name]._write;" \n}
            output {append ss $p1 "method \t[bsv_method_name] = w_[bsv_method_name]._read;" \n}
            inout  {append ss $p1 "interface Inout#([getType]) \t[bsv_method_name];" \n}
        }
    }
    method dualMethodDef {strname p1 } {
        upvar $strname ss
        if { $isclock } {
            return
        }
        if { $isreset } {
            return
        }
        switch -exact $dir {
            output {append ss $p1 "method [bsv_method_name] = bridge.[bsv_method_name];" \n}
            input  {append ss $p1 "method [bsv_method_name] = bridge.[bsv_method_name];" \n}
            #inout  {append ss $p1 "interface Inout#([getType]) \t[bsv_method_name];" \n}
        }
    }
    method cnxDef {strname p1 } {
        upvar $strname ss
        if { $isclock } {
            return
        }
        if { $isreset } {
            return
        }
        switch -exact $dir {
            output {append ss $p1 "`mkCn()(l.[bsv_method_name], r.[bsv_method_name]);" \n}
            input  {append ss $p1 "`mkCn()(l.[bsv_method_name], r.[bsv_method_name]);" \n}
            #inout  {append ss $p1 "interface Inout#([getType]) \t[bsv_method_name];" \n}
        }
    }
    method bvi_entry {strname} {
        upvar $strname ss

        set clk "no_clock"
        if { $clock ne "" } {
            set clk [$clock bsv_method_name]
        }
        set rst "no_reset"
        if { $reset ne "" } {
            set rst [$reset bsv_method_name]
        }

        # special case clock and resets
	if {[regexp "dummyClockOut" [bsv_method_name]] == 0} {
	    if { $isclock } {
		switch -exact $dir {
		    input {
			append ss "input_clock ($name, (*inhigh*) gate_$name) = [bsv_method_name];"
		    }
		    output {
			append ss "output_clock [bsv_method_name]($name);"
		    }
		}
		append ss \n
		return
	    }
	    if { $isreset } {
		switch -exact $dir {
		    input {
			append ss "input_reset ($name) clocked_by ($clk) = [bsv_method_name];"
		    }
		    output {
			append ss "output_reset [bsv_method_name]($name);"
		    }
		}
		append ss \n
		return
	    }

	    switch -exact $dir {
		input {
		    append ss "method /*Action*/ [bsv_method_name] ($name) enable((*inhigh*) u[uid]);\n"
		    #                append ss " clocked_by ($clk) reset_by ($rst);"
		}
		output {
		    append ss "method /*Value*/ $name [bsv_method_name] ();\n"
		    #                append ss " clocked_by ($clk) reset_by ($rst);\n"
		}
		inout  {
		    append ss "ifc_inout [bsv_method_name] ([bsv_method_name]);\n"
		}
	    }
        }
        append ss \n
    }
}

#####################################################################
## An instances of a port
itcl::class CXGen::IPort {
    inherit CXGen::Port
    variable modport
    variable clockConfig ""

    constructor {_modport _instanceO} {
        set nm   [$_modport getName]
        set dir  [$_modport cget -dir]
        set modO [$_modport getModule]

        set childargs [list]
        foreach a [list width isclock isreset bsvtype] {
            lappend childargs "-$a" [$_modport info variable $a -value]
        }
        eval chain  $nm $dir $modO $childargs
    } {
        ## data in iport
        set modport $_modport
        set instance [$_instanceO getName]
        set instanceObj $_instanceO
        setClockConfig
    }

    proc mkPort {origPort inst} {
        set m [uplevel #0 ::CXGen::IPort #auto $origPort $inst]
        return "::$m"
    }

    method setClockConfig {} {
        if { $isclock } {
            set args ""
            set clockConfig [uplevel #0 CXGen::ClockConfiguration #auto $this $args]
        } else {
            if { $clockConfig ne "" } {
                itcl::delete object $clockConfig
            }
        }
    }
    method getClockConfig {} {
        return $clockConfig
    }
}




#####################################################################
itcl::class ::CXGen::ClockConfiguration {
    common nextClockNum 0
    common exportFields [list clockNum ratioNumerator ratioDenominator \
                             dutyHi dutyLo  phase resetCycles]

    variable port ""
    public variable name ""
    variable clockNum 0
    # public variable clockGroup "noClockGroup"
    public variable ratioNumerator 1
    public variable ratioDenominator 1
    public variable dutyHi 0
    public variable dutyLo 100
    public variable phase 0
    public variable resetCycles 8

    constructor { prt args } {
        set port $prt
        eval configure $args
        set clockNum [incr nextClockNum]
    }
    method getPort {} { return $port }
    method getName {} {
        if { $name ne "" } { return $name }
        $port getInstName
    }
    method getClockPortInstName {} {
        return "icp_[getName]"
    }
    method getClockInstName {} {
        return "i_[getName]"
    }
    method dumpConfig { cfgname p1 } {
        set p2 "$p1  "
        append ss $p1 "SceMiClockConfiguration $cfgname = defaultValue;" \n
        foreach f $exportFields {
            set init [info variable $f -init]
            set val [info variable $f -value]
            # if { $init == $val } { continue }
            append ss $p2 "$cfgname.$f = $val;" \n
        }
        return $ss
    }
    method getClockNum {} { return $clockNum }

}


#####################################################################
## FieldNames objects
## TODO -- concept how to handle ready/enable signals.....
itcl::class ::CXGen::FieldNames {
    public variable direction     "unknown"
    public variable primType      ""
    public variable nameHierarchy [list]
    public variable vlNameHierarchy [list]
    public variable ports         [list]

    constructor {prim dir namehier vlhier} {
        set primType $prim
        set direction $dir
        set nameHierarchy $namehier
        set vlNameHierarchy $vlhier
    }
    # Primary type,  direction,  name Hierarchy,  verilog hierarchy
    proc mkFieldName {p d nh vh} {
        ## puts " ::CXGen::FieldNames #auto $p $d $vh ]"
        set fn [uplevel #0 [list ::CXGen::FieldNames #auto $p $d $nh $vh]]
        return "::$fn"
    }
    method getName {} {
        join $nameHierarchy "."
    }
    method getVLName {} {
        join $vlNameHierarchy "."
    }
    method show {args} {
        set ss [getName]
        if { [regexp -- {-full} $args] } {
            set args [list -direction -type -match -vl]
        }
        foreach a $args {
            switch -regex $a {
                {-vl} {
                    append ss "\t([getVLName])"
                }
                {-direction} {
                    append ss "\t$direction"
                }
                {-match}      {
                    append ss "\t==> {" [join  [utils::omap "show" $ports] ",\n\t"] "}"
                }
                {-type}     {
                    append ss " " [$primType show]
                }

            }
        }
        return $ss
    }
    method isReady {} {
        string equal "RDY" [lindex $nameHierarchy end]
    }
    method isEnable {} {
        string equal "EN" [lindex $nameHierarchy end]
    }
    method isReadyName {} {
        string equal "RDY" [lindex $vlNameHierarchy end]
    }
    method isEnableName {} {
        string equal "EN" [lindex $vlNameHierarchy end]
    }
    method addPort {prt} {
        lappend ports $prt
    }
    method getWidth {} {
        $primType getWidth
    }

    proc findUnmatchedFields { flds } {
        set ret [list]
        foreach f $flds {
            if {[$f cget -ports] == [list]} {
                lappend ret $f
            }
        }
        return $ret
    }
    proc findMultipleMatchedFields { flds } {
        set ret [list]
        foreach f $flds {
            if { [llength [$f cget -ports]] > 1} {
                lappend ret $f
            }
        }
        return $ret
    }
}

#####################################################################
## Creates a Fieldname objects for each tranactor

itcl::class ::CXGen::FieldNameCollector {
    inherit ::VisitorPattern::Visitor
    # array key::  val: FieldName
    public variable Collection
    public variable ordered [list]
    public variable expandPortTypes false

    proc collectPorts { xactor } {
        set collector [uplevel #0 ::CXGen::FieldNameCollector #auto]
        set  x $xactor
        set typObj [$x getTypeHandle]
        if { $typObj == "" } {
            set msg [format "Invalid type handle for transactor `%s', with type `%s'." \
                         [$x show] [$x getTypeName]]
            append msg \n [$x cget -lastError]
            return -code error $msg
        }

        set xname [$x cget -name]
        set  nhier [list $xname]
        set  vhier [list $xname]
        set dir unknown
        $collector configure -expandPortTypes [$x cget -expandPortTypes]

        $typObj accept $collector $dir $nhier $vhier

        # Copy data to transactor
        $x configure -fieldnames [$collector cget -ordered]

        return  [$collector cget -ordered]
    }

    constructor {} {
        array set Collection [list]
    }
    destructor {}
    method clear {} {
        array unset Collection
        array set Collection [list]
        set ordered [list]
    }
    method debug {} {
        puts "Collection: of $this [info class]"
        foreach n [array names Collection] {
            puts "Key: $n --> [$Collection($n) show]"
        }
    }

    # visitor arg are direction hier
    method visit_VInterface { o dir nhier vhier } {
        foreach m [$o getMembers] {
            set nhier2 $nhier
            set vhier2 $vhier

            set name [lindex $m 0]
            set subo [lindex $m 1]
            lappend nhier2 $name

            if {[$subo getPrefixName prefix]} {
                eval lappend vhier2 $prefix
            } else {
                lappend vhier2 $name
            }
            $subo accept $this $dir $nhier2 $vhier2
        }
    }
    method visit_VMethodType { o dir nhier vhier } {

        set prefix [lindex $nhier end]
        set hasPrefix [$o getPrefixName prefix]
        if { $prefix ne "" } {
            set prefix [format "%s_" $prefix]
        }

        # do not add hierarchy here -- look at expandPortTypes
        foreach subo [$o getArguments] subn [$o getArgNames] {
            set vhier2 [lreplace $nhier end end "$prefix$subn"]
            set nhier2 [concat $nhier [list $subn]]
            if { $expandPortTypes } {
                $subo accept $this "input" $nhier2 $vhier2
            } else {
                visit_VPrimary $subo "input" $nhier2 $vhier2
            }
        }

        set vhier2 $vhier
        set resName [$o getResultName]
        if { $resName ne "" } {
            set vhier2 [lreplace $nhier end end $resName]
        }

        # Look at return type
        set retT [$o getReturnType]
        if { [$o isAction] || $expandPortTypes } {
            $retT accept $this "output" $nhier $vhier2
        } else {
            visit_VPrimary $retT "output" $nhier $vhier2
        }

        # Look for ready and enable signals
        set boolType [::Virtual::createType "Bool" ""]
        set attrs [$o getAttributes]

        set AR [$o isAlwaysReady]
        set AE [$o isAlwaysEnabled ]
        set ACT [$o isAction]

        set doRDY true
        set doEN $ACT

        if { $AR } { set doRDY false }
        if { $ACT && $AE } { set doRDY false; set doEN false }


        if { $doRDY } {
            set name [$o getReadyName]
            set nhier2 [concat $nhier "RDY"]
            if { $name ne "" } {
                set vher2 [lreplace $vhier end end $name]
            } else {
                set vhier2 [concat $vhier "RDY"]
            }
            visit_VPrimary $boolType "output" $nhier2 $vhier2
        }

        if { $doEN } {
            set name [$o getEnableName]
            set nhier2 [concat $nhier "EN"]
            if { $name ne "" } {
                set vhier2 [lreplace $vhier end end $name]
            } else {
                set vhier2 [concat $vhier "EN"]
            }
            visit_VPrimary $boolType "input" $nhier2 $vhier2
        }
    }

    method visit_VPrimary { o dir nhier vhier } {
        if { [$o getWidth] != -1 } {
            addCollection $o $dir $nhier $vhier
        }
    }
    method visit_VStruct {o dir nhier vhier } {
        foreach m [$o getMembers] {
            set nhier2 $nhier
            set vhier2 $vhier

            set name [lindex $m 0]
            set subo [lindex $m 1]
            lappend nhier2 $name
            lappend vhier2 $name

            $subo accept $this $dir $nhier2 $vhier2
        }
    }
    method visit_VTaggedUnion { o dir nhier vhier } {
        set ctr [$o getConstr]
        if { [$o getWidth] != -1 } {
            addCollection $o $dir $nhier $vhier
        } else {
            puts "Unspecified taggedunion [$o show] in [info class]"
        }
    }
    method visit_VEnum  { o dir nhier vhier } {
        addCollection $o $dir $nhier $vhier
    }

    method visit_VAlias { o dir nhier vhier } {
        # redirect
        [$o subtype] accept $this $dir $nhier $vhier
    }

    ## return type for methods...
    method visit_VActionValue { o dir nhier vhier } {
        # redirect
        set retT [$o getSubtype]
        if { $expandPortTypes } {
            $retT accept $this "output" $nhier $vhier
        } else {
            visit_VPrimary $retT "output" $nhier $vhier
        }
    }

    ## return type of methods....
    method visit_VAction { o dir nhier vhier } {
    }


    method visit_VType { o args } {
        puts stderr "Unimplemented visitor in [$this info class] for object [$o info class]"
        puts "[$o show]"
    }
    # nothing to do for these types....
    method visit_VTypeclass {o dir nhier vhier } {}

    #
    method addCollection { o dir nhier vhier } {
        set fn [::CXGen::FieldNames::mkFieldName $o $dir $nhier $vhier]
        set key [join [utils::map utils::fst $nhier] ,]
        set Collection($key) $fn
        lappend ordered $fn
    }
}
