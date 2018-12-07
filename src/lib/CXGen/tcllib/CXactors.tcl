# Copyright 2012--2013 Bluespec, Inc.  All rights reserved.
# $Id$

package require CXGen
package provide CXactors 1.1
package require SceMiMsg

#####################################################################
itcl::class CXGen::Xactor {
    public variable name       ""    check_name
    public variable imports    [list]
    public variable ports      [list]
    public variable fieldnames [list]
    public variable matchRule  [list]
    public variable clockConf  ""
    public variable resetPort  ""
    #
    public variable type       ""   setTypeHandle
    protected variable typeHandle ""
    public variable lastError ""
    public variable providedIfc "NOT_DEFINED"
    public variable expandPortTypes false

    public variable usePipes false   "validateStringParam usePipes boolean"
    public variable pipeFifoDepth 32  "validateStringParam pipeFifoDepth integer"
    public variable pipeMode  Fifo

    constructor {cname args} {
        set name $cname
        set imports ""
        # do not do this here we want to track specially added one.
        # $CXGen::GlobalState::gs add_obj xactors $this
    }
    destructor {
        ## remove from global list
        $CXGen::GlobalState::gs delete_obj xactors $this
    }
    method show {args} {
        set ss ""
        append ss [format "Transactor: '%s'\tType: '%s'\tHWIfc: '%s'\tProxy: '%s'" \
                       $name [namespace tail [info class]]  [getTypeName] [getCProxyClass] ]
        if { [regexp -- {-ports} $args] } {
            append ss "\n  Port: " [join [utils::omap  "show -name" [getAllPorts]] "\n  Port: "] \n
        }
        if { [regexp -- {-fields} $args] } {
            checkTypeHandle
            matchFields
            append ss "\n  FieldName: " [join [utils::omap  "show $args" $fieldnames] "\n  FieldName: "] \n
        }
        return $ss
    }
    method debugFields {} {
        set ss ""
        append ss "\n  FieldName: " [join [utils::omap  "show -match" $fieldnames] "\n  FieldName: "] \n
        puts stderr $ss
    }
    method debug {args} {
        puts stderr "[eval show $args]"
    }
    method check_name {} {
        # name must begin lower case
        if { ![string is lower [string index $name 0]] } {
            set msg "Transactor name must begin with lower case letter"
            set lastError $msg
            return -code error $msg
        }
        name_clash $this getName
    }
    method getName {} { return $name }
    method addPorts {prts} {
        foreach p $prts {
            lappend ports $p
        }
    }
    method getAllPorts {} {
        return $ports
    }
    method removePorts {prts} {
        foreach p $prts {
            set ports [lsearch -inline -all -exact -not $ports $p]
        }
    }

    method setTypeHandle {} {
        set typeHandle ""
        if { ![catch "::Virtual::createType [list $type]" err] } {
            set typeHandle $err
        }
        set lastError $err
        return $typeHandle
    }
    method getTypeHandle {} { return $typeHandle }
    method getTypeName {} {
        if { $typeHandle ne "" } {
            return [$typeHandle getName]
        }
        return $type
    }

    method getModuleArguments {} {
    }
    method getProvidedIfc {} { return $providedIfc }
    method getXactorModule {} { return "UNDEFINED" }
    method getPreInstance {} { return "" }
    method getXactorInstance {} {
        format "%s (%s)"  [getXactorModule] [toModuleArgs]
    }
    protected method toModuleArgs {} {
        set margs [join [getModuleArguments] ", "]
        set cc [getClockConfig]
        set cname [$cc getClockInstName]
        set rname "i_[[getResetPort] bsv_method_name]"

        ## DOC -- these are BSV substitutions for module instantiations
        set margs [regsub -all {%X} $margs [cget -name]]
        set margs [regsub -all {%P} $margs {xtor_enable}]
        set margs [regsub -all {%N} $margs [$cc getClockNum]]
        set margs [regsub -all {%CC} $margs [$cc getClockInstName]]
        set margs [regsub -all {%CR} $margs "i_[[getResetPort] getInstName]"]
        set margs [regsub -all {%UC} $margs "i_u_clock"]
        set margs [regsub -all {%UR} $margs "i_u_reset"]
        set margs [regsub -all {%FD} $margs $pipeFifoDepth]
        set margs [regsub -all {%FT} $margs $pipeMode]

        return $margs
    }
    protected method toProxyInstArgs {} {
        set margs [join [getProxyConstrArgs] ", "]

        ## DOC -- these are substitutions for proxy instantiations
        set margs [regsub -all {%X} $margs [cget -name]]
        set margs [regsub -all {%SI} $margs [format "x_%s" [scemiPath]]]
        set margs [regsub -all {%ST} $margs [getSceMiType]]
        return $margs
    }

    method getSceMiType {} {
        if { $usePipes } {
            return "XactorAdapter::Pipes"
        }
        return "XactorAdapter::Ports"
    }

    # return clock configuration object,  determine this if not set
    method getClockConfig {} {
        if { $clockConf eq "" } {
            set clockConf [[[utils::head $ports] cget -clock] getClockConfig]
        }
        return $clockConf
    }
    method getResetPort {} {
        if { $resetPort eq "" } {
            set resetPort [[utils::head $ports] cget -reset]
        }
        return $resetPort
    }

    method getCxxFiles   {} { return "" }
    method getCxxOptions {} { return "" }

    method checkSanity { mode } {
        checkTypeHandle
        # checks before code generation
        if { [llength $ports] == 0 } {
            set msg [format "Port list is empty for transactor `%s'" \
                         $name]
            set lastError $msg
            return -code error $msg
        }
        # all ports must be from same instance
        set instNames [utils::nub [utils::omap "cget -instance" $ports]]
        if { [llength $instNames] != 1 } {
            set msg [format "Ports must all be from same instance for transactor `%s', found \[%s\]`" $name $instNames]
            set lastError $msg
            return -code error $msg
        }

        # All ports must be matched....
        matchFields
        checkFieldPortMatch
    }

    method checkTypeHandle {} {
        if { [getTypeHandle] == "" } {
            setTypeHandle
            if { [getTypeHandle] == "" } {
                set msg [format "Illegal or unspecified interface `%s' for transactor `%s'.\n%s" \
                         $type $name $lastError]
                set lastError $msg
                return -code error $msg
            }
        }
    }

    method checkFieldPortMatch {} {
        set ok 1
        set msgExtra ""
        set unmatched [::CXGen::FieldNames::findUnmatchedFields $fieldnames]
        foreach un $unmatched {
            set ok 0
            lappend msgExtra [format "Field `%s' is not matched to any port" \
                                  [$un show -direction -type]  ]
        }
        set multi [::CXGen::FieldNames::findMultipleMatchedFields $fieldnames]
        foreach mm $multi {
            set ok 0
            set mmports [$mm cget -ports]
            lappend msgExtra [format "Field `%s' is matched to multiple ports:\n %s" \
                                  [$mm show -direction -type]  \
                                  [join  [utils::omap "show" $mmports] ", "]
                                 ]
        }
        if { ! $ok } {
            set msg [format "Error in matching fields to ports in transactor `%s'"\
                         $name]
            foreach m $msgExtra {
                append msg "\n " $m
            }
            set lastError $msg
            return -code error $msg
        }
    }

    method interfaceDecl {strname p1} {
        upvar $strname ss
        # XXX need to ensure that name is proper bsv
        append ss $p1 "interface [getTypeName] $name;" \n
    }
    method generateTypeDef {strname} {
        upvar $strname ss
        #append ss "// Typedefs for Transactor $name" \n
    }
    method generateState {strname p1} {
        upvar $strname ss
        append ss $p1 "// State Elements for transactor $name" \n
    }
    method generateAssignments {strname p1} {
        upvar $strname ss
        append ss $p1 "// Assignments for transactor $name" \n
    }
    method generateInterfaceDef {strname p1} {
        upvar $strname ss
        append ss $p1 "// Interface Definition for transactor $name" \n
    }
    # usage:  xactor addMatchRules fieldName pattern fieldName2 pattern2 ...
    method addMatchRules { args } {
        foreach {fieldName pattern}  $args {
            lappend matchRule [list $name.$fieldName $pattern]
        }
    }
    method matchFields {} {
        ::CXGen::FieldNameCollector::collectPorts $this

        utils::listToSet UnmatchedPorts $ports
        utils::listToSet UnmatchedFields $fieldnames

        ### pass 1
        ## check for name matches via rules
        foreach f $fieldnames {
            set hier [$f cget -nameHierarchy]
            set hierStr [join $hier .]
            set tail [lindex $hier end]
            set fdir [$f cget -direction]

            set vlhier [$f cget -vlNameHierarchy]
            set vlhierStr [join $vlhier .]


            # matching rules
            set rules [concat \
                           [lsearch -index 0 -all -inline $matchRule  $hierStr] \
                           [lsearch -index 0 -all -inline $matchRule  $vlhierStr] \
                          ]

            foreach p $ports {
                set n [$p cget -name]
                set pdir [$p cget -dir]
                if { $pdir != $fdir } { continue }

                set matched [checkSpecialRules $rules $n]

                if { $matched } {
                    $f addPort $p

                    unset -nocomplain UnmatchedFields($f)
                    if { [catch "unset UnmatchedPorts($p)"] } {
                        # puts stderr "port already matched [$p show] adding to\n\t[$f show]"
                    }
                    break;
                }
            }
        }

        ## pass 2
        foreach f $fieldnames {
            if { ![info exists UnmatchedFields($f)] } { continue }
            set hier [$f cget -nameHierarchy]
            set hierStr [join $hier .]
            set tail [lindex $hier end]
            set tail1 [lindex $hier end-1]
            set fdir [$f cget -direction]

            # ignore short names in fields
            if { [string length $tail] == 1 } {
                set tail $tail1
            }

            set isRDY [$f isReadyName]
            set isEN  [$f isEnableName]

            set matchedPortList [list]

            foreach p $ports {
                if { ![info exists UnmatchedPorts($p)] } { continue }
                set pdir [$p cget -dir]
                if { $pdir != $fdir } { continue }
                set n [$p cget -name]

                if { $isRDY } {
                    set matched [regexp -nocase "^RDY_.*$tail1" $n]
                } elseif { $isEN }  {
                    set matched [regexp -nocase "^EN_.*$tail1" $n]
                } else {
                    set matched [regexp -nocase $tail $n]
                }

                # toss out some matches
                if { $matched } { set matched [checkMatch $p $f] }

                if { $matched } {
                    lappend matchedPortList $p
                }
            }
            # cleanup multiple matches
            set matchedPortList [cleanupMultipleMatched $f $matchedPortList]
            foreach p $matchedPortList {
                $f addPort $p
                unset -nocomplain UnmatchedFields($f)
                if { [catch "unset UnmatchedPorts($p)"] } {
                    # puts stderr "port already matched [$p show] adding to\n\t[$f show]"
                }
            }

        }

        set multis [::CXGen::FieldNames::findMultipleMatchedFields $fieldnames]

        if { [array size UnmatchedPorts] }  {
            puts  "Unmatched ports:\n\t[join [utils::omap show [array names UnmatchedPorts]] "\n\t"]"
        }
        if { [array size UnmatchedFields] }  {
            puts  "Unmatched fields:\n\t[join [utils::omap {show -vl -direction -type} [array names UnmatchedFields]] \n\t]"
        }
        if { [llength $multis] } {
            puts  "Multiple matched fields:\n\t[join [utils::omap {show -direction -match -type -ports} $multis]  \n\t]"
        }
    }
    protected method cleanupMultipleMatched {fld matches} {
        if { [llength $matches] <= 1 } { return $matches }
        set best [list]
        foreach m $matches {
            if { [$fld getWidth] == [$m cget -width] } {
                lappend best $m
            }
        }
        if { [llength $best] == 0 } { set best $matches }
        return $best
    }
    protected method checkSpecialRules {rules name} {
        foreach r $rules {
            set pat [lindex $r 1]
            if { [regexp -- $pat $name] } {
                #puts stderr "Found special case match $r --> $name"
                return true
            }
        }
        return false
    }
    ## rules to toss out matches
    protected method checkMatch { p f } {
        set pn [$p getName]
        set fn [$f cget -nameHierarchy]
        if { [regexp {^RDY}  $pn] && ![$f isReady] } {
            return false
        }
        if { [regexp {^EN}  $pn] && ![$f isEnable] } {
            return false
        }
        return true
    }

    method getInstance {} {
        eval [getInstanceObj] getName
    }

    method getInstanceObj {} {
        # all ports must have same instance
        eval [utils::head $ports] getInstanceObj
    }

    method getCProxyClass     {} { return "UNDEFINED" }
    method getProxyConstrArgs {} { return "UNDEFINED" }

    method cppIncludes {} { list "bsv_scemi.h" }
    method scemiPath {} { return $name }
    method proxyArgs {} { toProxyInstArgs }
    method cppPostConstr { pre objName } { return "" }

    method isTLM {} { return false }
    method isSCTarget {return false}
    method isSCInitiator {} {return false}

    protected method validateStringParam {nm strClass} {
        set val [$this cget -$nm]
        if { ! [string is $strClass $val] } {
            set msg "Illegal value `$val' for configuration parameter `$nm'."
            append msg " Value must be of type `[string totitle $strClass]'"
            set lastError $msg
            return -code error $msg
        }
    }
}

#####################################################################
## A transactor which ignores all the ports connected
itcl::class CXGen::IgnoreXactor {
    inherit CXGen::Xactor
    ## XXX TODO
}


#####################################################################
## Common attributes for transactors with dynamically built structures
itcl::class CXGen::DynamicXactor {
    inherit CXGen::Xactor

    public variable structName ""
    public variable useProxyQueue false  "validateStringParam useProxyQueue boolean"
    public variable tightlyCoupled false  "validateStringParam tightlyCoupled boolean"
    protected {
        variable clock ""
        variable reset ""
        variable clockedBy ""
        variable resetBy ""
    }

    constructor {name args} {
        chain $name
    } {
        set structName S_$name
    }

    ## No type handles since we build these
    method checkTypeHandle {} {}
    ## No fields to match.
    method matchFields {} {}

    ## Shared code to generate the structure
    method generateTypeDef {strname} {
        upvar $strname ss
        chain ss
        set p1 "  "
        ## if there is only 1 element, then we can jsut use a typedef
        if { [llength $ports] == 1 } {
            set p [lindex $ports 0]
            append ss "typedef [$p getType] $structName;" \n\n
        } else {
            append ss "typedef struct {" \n
            foreach p $ports {
                append ss $p1 $p1 "[$p getType] \t [$p bsv_method_name];" \n
            }
            append ss $p1 "} $structName deriving (Bits, Eq, FShow);" \n\n
        }
    }


    protected method checkPortDirection { ports expectedir msgName prefix } {
        upvar $msgName msg
        set err 0
        foreach p $ports {
            if { [$p cget -dir] ne $expectedir } {
                incr err
                append msg "\n" "$prefix [$p show] in not an $expectedir"
            }
        }

        return $err
    }

    method getXactorModule { prefix } {
        set pp "Port"
        if { $usePipes } {
            set pp "Pipe"
        }
        set st ""
        if { $tightlyCoupled } {
            set st "Stall"
        }
        format "%s%s%sXactor" $prefix $pp $st
    }

    method cppIncludes {} {
        set incls [chain]
        lappend incls  "SceMiHeaders.h"
    }
    method getProxyConstrArgs {} { list "path" "\"%SI\"" %ST }

    protected method getProxyType {dir} {
        set ptype "ProxyT"
        format "%s%s <%s >" $dir $ptype [getCValueType]
    }

    method getCValueType {} {
        ::SceMiMsg::bsvTypeToCppClass $structName UNUSED
    }

    # used to setup clocks, etc
    method checkSanity { mode } {
        set prts [cget -ports]
        if { [llength $prts] == 0} {
            set msg "Transactor `$name' must define at least 1 port"
            set lastError $msg
            return -code error $msg
        }

        set clock [[utils::head $prts] cget -clock]
        set reset [[utils::head $prts] cget -reset]
        set clockedBy [$clock bsv_method_name]
        set resetBy   [$reset bsv_method_name]
    }

    method getModuleArguments {} {
        set ma [list]
        if { $usePipes } {
            lappend ma "%FD" "%FT"
        }
        if { $tightlyCoupled } {
            lappend ma "%N"
        }
        lappend ma %P
        eval lappend ma [chain]
        return $ma
    }

    method show {args} {
        # Fields are handled special in these classes...
        set fields [regsub -all -- {-fields} $args "" argsX]
        eval chain $argsX
    }
}


#####################################################################
## A transactor which has BSV ready/enable protocol
itcl::class CXGen::ReadyEnableXactor {
    inherit CXGen::DynamicXactor
    public variable enable ""
    public variable ready  ""


    constructor {cname args} {
        chain $cname
    } {
        configure -imports [list GetPut Connectable DefaultValue FShow CXGetPut]
    }

    method checkSanity { mode } {
        chain $mode

        set msg "Transactor [show] is in error:"
        set err 0

        # ready enable must be set
        if { $ready  eq "" } {
            incr err
            append msg \n "ready signal has not been set"
        } else {
            incr err [checkPortDirection $ready "output" msg "Ready"]
        }

        if { $enable  eq "" } {
            incr err
            append msg \n "enable signal has not been set"
        } else {
            incr err [checkPortDirection $enable "input" msg "Enable"]
        }
        if { $err } {
            set lastError $msg
            return -code error $msg
        }
    }

    method getAllPorts {} {
        set most [chain]
        eval lappend most $enable $ready
    }
    method generateState {strname p1} {
        upvar $strname ss
        chain ss $p1

        set p2 "${p1}  "
        set p3 "${p2}  "
        append ss $p1  "Wire#(Bool) ${name}_enable <- mkDWire (False);" \n

        append ss $p1 "rule ${name}_drive_enable (True);" \n
        append ss  $p2 "dut.[$enable bsv_method_name] (pack (${name}_enable));" \n
        append ss $p1 "endrule: ${name}_drive_enable" \n
    }
    method generateAssignments {strname p1} {
        upvar $strname ss
        chain ss $p1
    }
    method show {args} {
        # Fields are handled specially in these classes...
        set fields [regsub -all -- {-fields} $args "" argsX]
        set ss [eval chain $argsX]
        if { $fields } {
            append ss "\n Fields:"
            append ss "\n  Ready: "
            if { $ready ne "" } { append ss [$ready show] }
            append ss "\n  Enable: "
            if { $enable ne "" } { append ss [$enable show] }
            foreach p $ports {
                append ss "\n  Data:   [$p show]"
            }
        }
        return $ss
    }

}

#####################################################################
## A transactor which has BSV Put interface
itcl::class CXGen::PutXactor {
    inherit CXGen::ReadyEnableXactor

    ::CXGen::GlobalState::addToXactorNameMap CXGen::PutXactor  [list Put]


    constructor {cname args} {
        chain $cname
    } {
        set xactorModule "mkCXPutInPortXactor"
        set type "Put#($structName)"
        set providedIfc "CXPut#($structName)"
        eval configure $args
    }

    method checkSanity { mode } {
        chain $mode

        # data must be outputs
        set msg "Transactor [show] is in error:"
        set err 0
        incr err [checkPortDirection $ports "input" msg "Port"]

        if { $err } {
            set lastError $msg
            return -code error $msg
        }
    }
    method getXactorModule {} { chain "mkCXPutIn" }
    method scemiPath {} {
        set pp "inport"
        if { $usePipes } { set pp "inpipe" }
        return ${name}_${pp}
    }
    method getCProxyClass {} { getProxyType "In" }
    method generateState {strname p1} {
        upvar $strname ss
        chain ss $p1
        set p2 "${p1}  "
        set p3 "${p2}  "
        append ss $p1  "Wire#($structName) ${name}_data <- mkDWire (?);" \n
        append ss $p1 "rule ${name}_drive_data (True);" \n
        if { [llength $ports] == 1 } {
            set p [utils::head $ports]
            append ss  $p2 "dut.[$p bsv_method_name] (${name}_data);" \n
        } else {
            foreach p $ports {
                append ss  $p2 "dut.[$p bsv_method_name] (tmp.[$p bsv_method_name]);" \n
            }
        }
        append ss $p1 "endrule: ${name}_drive_data" \n
    }
    method generateAssignments {strname p1} {
        upvar $strname ss
        chain ss $p1
        set p2 "${p1}  "
        set p3 "${p2}  "
	set p [utils::head $ports]
        append ss $p3  "method [$p bsv_method_name] = bridge.${name}.put_DATA;" \n
        append ss $p3  "method [$enable bsv_method_name] = bridge.${name}.put_EN;" \n
        append ss $p3  "method [$ready bsv_method_name] = bridge.${name}.put_RDY;" \n
    }
    method generateInterfaceDef {strname p1} {
        upvar $strname ss
        chain ss $p1
        set p2 "${p1}  "
        set p3 "${p2}  "

        set rdyName "dut.[$ready bsv_method_name]"
        append ss $p1 "interface Put $name;" \n
        append ss $p2 "method Action put ($structName x) if (unpack($rdyName));" \n
        append ss $p3 "${name}_enable._write(True);" \n
        append ss $p3 "${name}_data._write(x);" \n
        append ss $p2 "endmethod // Action put ( ) ;" \n
        append ss $p1 "endinterface // $name" \n

    }

}

#####################################################################
## A transactor which has BSV Get interface
itcl::class CXGen::GetXactor {
    inherit CXGen::ReadyEnableXactor

    ::CXGen::GlobalState::addToXactorNameMap CXGen::GetXactor  [list Get]


    constructor {cname args} {
        chain $cname
    } {
        set xactorModule "mkCXGetOutPortXactor"
        set type "Get#($structName)"
        set providedIfc "CXGet#($structName)"
        eval configure $args
    }

    method checkSanity { mode } {
        chain $mode

        # data must be outputs
        set msg "Transactor [show] is in error:"
        set err 0
        incr err [checkPortDirection $ports "output" msg "Port"]

        if { $err } {
            set lastError $msg
            return -code error $msg
        }
    }
    method getXactorModule {} { chain "mkCXGetOut" }
    method generateAssignments {strname p1} {
        upvar $strname ss
        chain ss $p1
        set p2 "${p1}  "
        set p3 "${p2}  "
	set p [utils::head $ports]
        append ss $p3  "method [$p bsv_method_name] = bridge.${name}.get_DATA;" \n
        append ss $p3  "method [$enable bsv_method_name] = bridge.${name}.get_EN;" \n
        append ss $p3  "method [$ready bsv_method_name] = bridge.${name}.get_RDY;" \n
    }
    method scemiPath {} {
        set pp "outport"
        if { $usePipes } { set pp "outpipe" }
        return ${name}_${pp}
    }
    method getCProxyClass {} { getProxyType "Out" }

    method generateInterfaceDef {strname p1} {
        upvar $strname ss
        chain ss $p1
        set p2 "${p1}  "
        set p3 "${p2}  "
        set p4 "${p3}  "

        set rdyName "dut.[$ready bsv_method_name]"
        append ss $p1 "interface Get $name;" \n
        append ss $p2 "method ActionValue#($structName) get () if (unpack($rdyName));" \n
        append ss $p3 "${name}_enable._write(True);" \n

        if { [llength $ports] == 1 } {
            set p [utils::head $ports]
            append ss $p3 "return dut.[$p bsv_method_name] ();" \n

        } else {
            # Need to build a struct
            append ss $p3 "$structName tmp;" \n
            foreach p $ports {
                append ss $p3 "tmp.[$p bsv_method_name] = dut.[$p bsv_method_name] ();" \n
            }
            append ss $p3 "return tmp;" \n
        }
        append ss $p2 "endmethod // Action get ( ) ;" \n
        append ss $p1 "endinterface // $name" \n

    }

}


#####################################################################
## A transactor which determines data based on assigned signals
itcl::class CXGen::InputXactor {
    inherit CXGen::DynamicXactor

    public variable allowFirstClock false  "validateStringParam allowFirstClock boolean"

    ::CXGen::GlobalState::addToXactorNameMap CXGen::InputXactor  [list Input]

    constructor {cname args} {
        chain $cname
    } {
        configure -imports [list GetPut Connectable DefaultValue FShow SceMiXactors BypassReg]
        set type "Put#($structName)"
        set xactorModule "mkInPortXactor"
        set providedIfc "Get# ($structName)"
        eval configure $args
    }
    destructor {
        chain
    }
    # creates a default
    proc mkDefaultXactor {clock ports} {
        uplevel #0 [list CXGen::InputXactor #auto "inputs_[$clock getName]" -ports $ports]
    }

    method checkSanity {mode} {
        chain $mode

        # all ports must be inputs
        set msg "Transactor [show] is in error:"
        set err 0
        incr err [checkPortDirection $ports "input" msg "Port"]

        if { $err } {
            set lastError $msg
            return -code error $msg
        }
    }

    method generateState {strname p1} {
        upvar $strname ss
        chain ss $p1
        foreach p $ports {
            set clk [[$p cget -clock] bsv_method_name]
            set rst [[$p cget -reset] bsv_method_name]

            append ss $p1 "WReg#([$p getType]) r_[$p bsv_method_name] "
            append ss "<- mkBypassReg (unpack(0));" \n
            append ss $p1 "mkConnection (dut.[$p bsv_method_name], r_[$p bsv_method_name]._read);" \n
            append ss \n
        }
    }
    method generateInterfaceDef {strname p1} {
        upvar $strname ss
        chain ss $p1
        set p2 "${p1}  "
        set p3 "${p2}  "

        append ss $p1 "interface Put $name;" \n
        append ss $p2 "method Action put ($structName x) ;" \n
        if { [llength $ports] == 1 } {
            set p [utils::head $ports]
            append ss $p3 "r_[$p bsv_method_name].bypass (x) ;" \n
        } else {
            foreach p $ports {
                append ss $p3 "r_[$p bsv_method_name].bypass (x.[$p bsv_method_name]) ;" \n
            }
        }
        append ss $p2 "endmethod // Action put ( ) ;" \n
        append ss $p1 "endinterface // $name" \n

    }
    method getXactorModule {} { chain "mkIn" }
    method getModuleArguments {} {
        set pre [chain]
        if { $tightlyCoupled } {
            set pre [linsert $pre 1 [expr $allowFirstClock ? "True" : "False"]]
        }
        return $pre
    }
    method scemiPath {} {
        set pp "inport"
        if { $usePipes } { set pp "inpipe" }
        return ${name}_${pp}
    }
    method getCProxyClass {} { getProxyType "In" }
}


#####################################################################
## A transactor which determines data based on assigned signals
itcl::class CXGen::OutputXactor {
    inherit CXGen::DynamicXactor

    variable fifoName ""
    variable regName ""
    variable tmpName ""

    ::CXGen::GlobalState::addToXactorNameMap CXGen::OutputXactor  [list Output]

    constructor {cname args} {
        chain $cname
    } {
        configure -imports [list GetPut Connectable DefaultValue FShow FIFOF SceMiXactors]

        set fifoName ${name}_fifo
        set regName  ${name}_reg
        set tmpName ${name}_tmp

        set type "Get#($structName)"
        set xactorModule "mkOutPortXactor"
        set providedIfc "Put# ($structName)"
        eval configure $args
    }
    destructor {
        chain
    }
    # creates a default
    proc mkDefaultXactor {clock ports} {
        uplevel #0 [list CXGen::OutputXactor #auto "outputs_[$clock getName]" -ports $ports]
    }

    # setup class vars before generation
    method checkSanity { mode } {
        chain $mode

        # all ports must be output
        set msg "Transactor [show] is in error:"
        incr err [checkPortDirection $ports "output" msg "Port"]

        if { $err } {
            set lastError $msg
            return -code error $msg
        }

    }

    method generateState {strname p1} {
        upvar $strname ss
        chain ss $p1
        set cbrb "clocked_by $clockedBy, reset_by $resetBy"
        set p2 "${p1}  "

        if { ! $tightlyCoupled } {
            append ss $p1 "FIFOF#($structName) $fifoName <- mkFIFOF ();" \n
            append ss $p1 "Reg#($structName) $regName <- mkRegA (unpack(0));" \n
            append ss \n
        }

        if { [llength $ports] == 1 } {
            set p [utils::head $ports]
            append ss $p1 "$structName $tmpName = dut.[$p bsv_method_name];" \n
        } else {
            append ss $p1 "$structName $tmpName;" \n
            foreach p $ports {
                append ss $p2 "$tmpName.[$p bsv_method_name]\t= dut.[$p bsv_method_name];" \n
            }
        }

        if { ! $tightlyCoupled } {
            append ss $p1 "rule pushOutputs_$name ($regName != $tmpName);" \n
            append ss $p2 "$fifoName.enq ($tmpName);" \n
            append ss $p2 "$regName._write ($tmpName);" \n
            append ss $p1 "endrule: pushOutputs_$name" \n\n
        }
    }
    method generateInterfaceDef {strname p1} {
        upvar $strname ss
        chain ss $p1
        set gname $fifoName
        if { $tightlyCoupled } { set gname $tmpName }
        append ss $p1 "interface Get $name = toGet ($gname);" \n
    }
    method getXactorModule {} { chain "mkOut" }
    method scemiPath {} {
        set pp "outport"
        if { $usePipes } { set pp "outpipe" }
        return ${name}_${pp}
    }
    method getCProxyClass {} { getProxyType "Out" }
}


#####################################################################
## A transactor which determines data based on assigned signals
itcl::class ::CXGen::Generic {
    inherit CXGen::Xactor

    public variable bsvpackage "" "loadpackage"

    constructor { cname args } {
        chain $cname
    } {
    }
    destructor {
    }

    method generateTypeDef {strname} {
        upvar $strname ss
        chain ss
    }
    method generateState {strname p1} {
        # Nothing to print,  drop header...
        # upvar $strname ss
        # chain ss $p1
    }
    method generateInterfaceDef {strname p1} {
        upvar $strname ss
        chain ss $p1
        set str [::CXGen::InterfaceCreator::writeInterfaceDef $p1 $this]
        append ss $str
    }
    method generateAssignments {strname p1} {
        upvar $strname ss
        chain ss $p1
        set str [::CXGen::InterfaceCreator::writeInterfaceDef $p1 $this]
        append ss $str
    }

    method loadpackage {} {
        if { $bsvpackage ne "" } {
            # add the package as a required import
            eval lappend imports $bsvpackage
            set ret [::types::import_package false $bsvpackage]
            setTypeHandle
        }
    }
}


#####################################################################
itcl::class ::CXGen::Custom {
    inherit CXGen::Generic

    # default parameter for transactors
    public variable xactorModule    "NOT_DEFINED"
    public variable moduleArguments [list]

    public variable proxyClass      "UNDEFINED"
    public variable proxyConstrArgs [list "path" "\"%SI\""]
    public variable cppHeaders      [list]
    # user proc which returns string for BSV file before module instancation
    # called as eval $preInstanceProc $this
    public variable preInstanceProc ""

    ::CXGen::GlobalState::addToXactorNameMap CXGen::Custom  [list Custom Generic]

    constructor {cname args} {
        chain $cname
    } {
        eval configure $args
    }

    method getPreInstance  {}    {
        if { $preInstanceProc eq "" } { return "" }
        set cmd "$preInstanceProc $this"
        if { [catch $cmd err] } {
            set msg "Error during exeuction of preInstanceProc `$preInstanceProc' for transactor `$name`"
            append msg \n $errorInfo
            set lastError $msg
            return -code error $msg
        }
        return $err
    }
    method getXactorModule {}    { set xactorModule }
    method getModuleArguments {} { set moduleArguments }
    method getCProxyClass     {} { set proxyClass }
    method getProxyConstrArgs {} { set proxyConstrArgs }
    method cppIncludes        {} { set cppHeaders }
}


#####################################################################
itcl::class ::CXGen::StdTLM {
    inherit CXGen::Generic

    protected {
        common ValidAddrs [list 32 64]
        common ValidDatas [list 8 16 32 64 128 256 512 1024]
        common ValidUsers [list 0 32 64]
        common ValidVis   [list Deferred Immediate Fifo]

        common XactorArgsFields [list \
                                     big_endian       toBSVBool\
                                     keep_bursts      toBSVBool \
                                     write_bypass     toBSVBool \
                                     interleave_depth toBSVInt \
                                     flow_depth       toBSVInt \
                                     usePipes         toBSVBool \
                                     inPipeDepth      toBSVInt \
                                     inPipeVis        toBSVId \
                                     outPipeDepth     toBSVInt \
                                     outPipeVis       toBSVId \
                                    ]

        common ExpectedSearch [list TLM3 Axi Apb Ahb Axi4 Xactors]

    }

    # default Type parameter for BSV transactors
    public variable addrWidth 		32  "validateParam ValidAddrs addrWidth"
    public variable dataWidth 		32  "validateParam ValidDatas dataWidth"
    public variable userWidth 		0   "validateParam ValidUsers userWidth"
    public variable general_type        ""  setConcreteType

    # default TLMArgs for transactor
    public variable big_endian         false  "validateStringParam big_endian boolean"
    public variable keep_bursts        true   "validateStringParam keep_bursts boolean"
    public variable write_bypass       false  "validateStringParam write_bypass boolean"
    public variable interleave_depth   1      "validateStringParam interleave_depth integer"
    public variable flow_depth         0      "validateStringParam flow_depth integer"
    public variable inPipeDepth        32     "validateStringParam inPipeDepth integer"
    public variable outPipeDepth       32     "validateStringParam outPipeDepth integer"
    public variable inPipeVis          Fifo   "validateParam ValidVis inPipeVis"
    public variable outPipeVis         Fifo   "validateParam ValidVis outPipeVis"

    protected {
        # these are max size set by the SW configuration
        variable idWidth   	10
        variable lengthWidth 	15
        # set in the constructor
        variable isMaster
        variable xactorModule "UNDEFINED"
    }

    constructor { cname isM args } {
        chain $cname
        set usePipes true
    } {
        set isMaster $isM
        lappend bsvpackage CXactors DefaultValue TLM3 XactorsDefines
        foreach p $ExpectedSearch {
            CXGen::XUtils::prependSearchPath %/Libraries/$p
        }
    }
    protected method validateParam { validListName name } {
        set val [$this cget -$name]
        if { [lsearch [set $validListName] $val] == -1 } {
            set msg "Illegal value `$val' for configuration parameter `$name'."
            append msg " Values must one of `[join [set $validListName] " "]'."
            set lastError $msg
            return -code error $msg
        }
        setConcreteType
    }
    protected method setConcreteType {} {
        configure -type [format "%s #(%s)" $general_type [bsv_type_args]]
    }
    method getProvidedIfc {} {
        format "%s #(%s)" $providedIfc [bsv_type_args]
    }

    protected method genArgName {} {
        format "xactorParams_%s" $name
    }
    method getPreInstance  {}    {
        namespace import ::CXGen::XUtils::*
        set sname [genArgName]
        set ss "TLMXActorArgs $sname = defaultValue;"
        append ss \n
        foreach {fld cvt} $XactorArgsFields {
            set data [eval $cvt [cget -$fld]]
            append ss [format " %s.%s = %s;\n" $sname $fld $data]
        }
        return $ss
    }
    method getXactorModule {}    { set xactorModule }
    method getModuleArguments {} {
        list [genArgName] "xtor_enable"
    }
    method getProxyConstrArgs {} {
        list "path" "\"%SI\"" %ST
    }
    method cppIncludes {} {
        set incls [chain]
        lappend incls  "tlm_xactors.h"
    }
    protected method bsv_type_args {} {
        format "%d,%d,%d,%d,%d" $idWidth $addrWidth $dataWidth $lengthWidth $userWidth
    }
    # this method captures common code for each sub class contruction
    protected method commonConstrEnd { args } {
        loadpackage
        eval configure $args
        setConcreteType
    }
    method isTLM {} { return true }
    method getCProxyClass {} {
        if { $isMaster } {
            format "MasterProxy<%du, %du, %du >" $addrWidth $dataWidth $userWidth
        } else {
            format "SlaveProxy<%du, %du, %du >" $addrWidth $dataWidth $userWidth
        }
    }
    method cppPostConstr { pre objName } {
        format "%s%s.setAdapterDebug ( 0x00 );\n" $pre $objName
    }


    method sysc_i_or_t {} {
        if { $isMaster } {
            return "initiator"
        } else {
            return "target"
        }
    }
    method isSCTarget {} {
        return [expr ! $isMaster]
    }
    method isSCInitiator {} {
        return $isMaster
    }


}


# A tcl proc to generate tcl classes.
proc ::CXGen::TLMConstrGenerator { tclass names pkgs isMstr gtype } {
    set classStr "
itcl::class ::CXGen::$tclass {
    inherit CXGen::StdTLM

    ::CXGen::GlobalState::addToXactorNameMap ::CXGen::$tclass  [list $names]
    constructor { cname args } {
        chain \$cname $isMstr
    } {
        lappend bsvpackage $pkgs
        set general_type $gtype
        set xactorModule mk$tclass
        set providedIfc $tclass
        eval commonConstrEnd \$args
    }
}
"
    uplevel #0 $classStr
}

# Table describing details of the BSV transactors
#  Class/ProvidedIfc                  UserNames                   pkg  isMaster   Connecting_IFC
set ::CXGen::TLMXactorTable [list \
\
  "CXAxi4LRdWrMasterSceMiXactor" [list MasterAxi4L Axi4LMaster]     Axi4   1   "CXAxi4LRdWrMaster"    \
  "CXAxi4LRdMasterSceMiXactor"   [list RdMasterAxi4L Axi4LRdMaster] Axi4   1   "CXAxi4LRdMaster"      \
  "CXAxi4LWrMasterSceMiXactor"   [list WrMasterAxi4L Axi4LWrMaster] Axi4   1   "CXAxi4LWrMaster"      \
\
  "CXAxi4LRdWrSlaveSceMiXactor"  [list SlaveAxi4L Axi4LSlave]       Axi4   0   "CXAxi4LRdWrSlave"     \
  "CXAxi4LRdSlaveSceMiXactor"    [list RdSlaveAxi4L Axi4LRdSlave]   Axi4   0   "CXAxi4LRdSlave"       \
  "CXAxi4LWrSlaveSceMiXactor"    [list WrSlaveAxi4L Axi4LWrSlave]   Axi4   0   "CXAxi4LWrSlave"       \
\
  "CXAxi4RdWrMasterSceMiXactor"  [list MasterAxi4 Axi4Master]       Axi4   1   "CXAxi4RdWrMaster"     \
  "CXAxi4RdMasterSceMiXactor"    [list RdMasterAxi4 Axi4RdMaster]   Axi4   1   "CXAxi4RdMaster"       \
  "CXAxi4WrMasterSceMiXactor"    [list WrMasterAxi4 Axi4WrMaster]   Axi4   1   "CXAxi4WrMaster"       \
\
  "CXAxi4RdWrSlaveSceMiXactor"   [list SlaveAxi4 Axi4Slave]         Axi4   0   "CXAxi4RdWrSlave"      \
  "CXAxi4RdSlaveSceMiXactor"     [list RdSlaveAxi4 Axi4RdSlave]     Axi4   0   "CXAxi4RdSlave"        \
  "CXAxi4WrSlaveSceMiXactor"     [list WrSlaveAxi4 Axi4WrSlave]     Axi4   0   "CXAxi4WrSlave"        \
\
  "CXAxiRdWrMasterSceMiXactor"   [list MasterAxi AxiMaster]         Axi   1    "CXAxiRdWrMaster"      \
  "CXAxiRdMasterSceMiXactor"     [list RdMasterAxi AxiRdMaster]     Axi   1    "CXAxiRdMaster"        \
  "CXAxiWrMasterSceMiXactor"     [list WrMasterAxi AxiWrMaster]     Axi   1    "CXAxiWrMaster"        \
\
  "CXAxiRdWrSlaveSceMiXactor"    [list SlaveAxi AxiSlave]           Axi   0    "CXAxiRdWrSlave"       \
  "CXAxiRdSlaveSceMiXactor"      [list RdSlaveAxi AxiRdSlave]       Axi   0    "CXAxiRdSlave"         \
  "CXAxiWrSlaveSceMiXactor"      [list WrSlaveAxi AxiWrSlave]       Axi   0    "CXAxiWrSlave"         \
\
  "CXApbMasterSceMiXactor"       [list MasterApb ApbMaster]         Apb   1    "CXApbXtorMaster"      \
  "CXApbSlaveSceMiXactor"        [list SlaveApb ApbSlave]           Apb   0    "CXApbXtorSlave"       \
\
  "CXAhbMasterSceMiXactor"       [list MasterDualAhb AhbMasterDual] Ahb   0    "CXAhbXtorMasterDual"  \
  "CXAhbSlaveSceMiXactor"        [list SlaveDualAhb AhbSlaveDual]   Ahb   1    "CXAhbXtorSlaveDual"   \
\
                               ]

foreach {a1 a2 a3 a4 a5} $::CXGen::TLMXactorTable {
    ::CXGen::TLMConstrGenerator $a1 $a2 $a3 $a4 $a5
}

unset -nocomplain a1 a2 a3 a4 a5

#####################################################################
## Class which traverses an interface hierarchy building the interface
## definition in the TOP-*_IFC model.  The names are taken from FieldInfo
itcl::class ::CXGen::InterfaceCreator {
    inherit ::VisitorPattern::Visitor
    public variable ostr ""
    variable xactor
    variable mode unknown
    variable argName ""
    variable FieldName
    variable expandPortTypes false
    public variable errorOccured false

    proc writeInterfaceDef { p1 xactr} {
        set visitor [uplevel #0 [list CXGen::InterfaceCreator #auto $xactr]]
        set visitor "::$visitor"
        set ifc [$xactr getTypeHandle]
        if { $ifc == "" } {
            set if [$xactr setTypeHandle]
        }
        if { $ifc == "" } {
            set msg "Cannot evaluate the transactor type `[$xactr getTypeName]'"
            append msg " for transactor [$xactr cget -name]"
            append msg "; perhaps the -bsvpackage has not been set on the transactor" \n
            append msg [$xactr cget -lastError]
            return -code error  $msg
        }

        set name [$xactr cget -name]
        $ifc accept $visitor $name $p1 $name
        if { [$visitor cget  -errorOccured] } {
            set msg "An internal error occured during the creation of this interface"
            append msg "\n" [$xactr show -fields -match]
            return -code error $msg
        }
        $visitor cget -ostr
    }
    constructor {xactr} {
        set xactor $xactr
        array set FieldName [list]
        set expandPortTypes [$xactr cget -expandPortTypes]

        foreach f [$xactor cget -fieldnames] {
            set name [join [$f cget -nameHierarchy] . ]
            set FieldName($name) $f
        }
    }
    protected method pp {p1 str} {
        append ostr $p1 $str \n
    }
    protected method lookupPort { hier } {
        if { [info exists FieldName($hier)] } {
            set p  [$FieldName($hier) cget -ports]
            if { [llength $p] == 0  } {
                puts stderr  "No port found for $hier -- [utils::omap show $p]"
                return ""
            } elseif { [llength $p] > 1  } {
                puts stderr  "Multiple ports found for $hier -- [utils::omap show $p]"
                return ""
            }
            ## puts stderr "LOOKUP $hier [$p show]"
            return $p
        }
        puts stderr  "Cannot find the hierarchy or port $hier"
        My::debug XXXXXXXXXXXXX
        set errorOccured true
        # $xactor debugFields
        return ""
    }

    protected method portName { prt hier } {
        checkDirection $prt $hier
        $prt bsv_method_name
    }

    protected method checkDirection {prt hier} {
        set dir [$prt cget -dir]
        set msg ""
        switch $mode {
            "args" {
                if { $dir ne "input" } {
                    append msg "Port direction mismatch: expected input for `$hier' but matched port"
                    append msg " `[$prt show]'"
                }
            }
            "output" {
                if { $dir ne "output" } {
                    append msg "Port direction mismatch: expected output for `$hier' but matched port"
                    append msg " `[$prt show]'"
                }
            }
            "outstruct" {
                if { $dir ne "output" } {
                    append msg "Port direction mismatch: expected output for `$hier' but matched port"
                    append msg " `[$prt show]'"
                }
            }
            default {
                append msg "internal error:  unexpected conditon [info level 0]"
            }
        }
        if { $msg ne "" } {
            return -code error $msg
        }
        return $msg
    }
    # return string for conversion between types.
    protected method portConversion { fromT toT name field} {
        # types are same, no conversion needed
        if { [$fromT  getName] == [$toT getName] } {
            return "$name"
        } elseif { [$fromT  getWidth] == -1 } {
            # let return handle error
        } elseif { [$toT  getWidth] == -1 } {
            # let return handle error
        } elseif { [$fromT  getWidth] == [$toT getWidth] } {
            # Can pack or unpack
            if { [regexp {^Bit#} [$fromT getName]] } {
                return "unpack ($name)"
            }
            if { [regexp {^Bit#} [$toT getName]] } {
                return "pack ($name)"
            }
            return "unpack (pack ($name))"
        } elseif { [$fromT  getTopType] == [$toT getTopType] } {
            # width are not equal
            if { [$fromT  getWidth] > [$toT getWidth] } {
                puts stderr "Warning: truncating field $field from [$fromT getName] to [$toT getName]"
                return "truncate ($name)"
            } else {
                puts stderr "Warning: extending field $field from [$fromT getName] to [$toT getName]"
                return "extend ($name)"
            }
        }

        return -code error "Cannot convert between types: [$fromT getName] -> [$toT getName]"
    }

    protected method portConvertor { fromT toT field} {
        # types are same, no conversion needed
        if { [$fromT  getName] == [$toT getName] } {
            return "none"
        } elseif { [$fromT  getWidth] == -1 } {
	    set ty [$fromT getName]
            puts stderr "FromType: ty"
        } elseif { [$toT  getWidth] == -1 } {
	    set ty [$toT getName]
            puts stderr "ToType: ty"
        } elseif { [$fromT  getTopType] == [$toT getTopType] } {
            # width are not equal
            if { [$fromT  getWidth] > [$toT getWidth] } {
                puts stderr "Warning: truncating field $field from [$fromT getName] to [$toT getName]"
                return "trn"
            } else {
                puts stderr "Warning: extending field $field from [$fromT getName] to [$toT getName]"
                return "ext"
            }
        }

        return -code error "Cannot specify conversion between types: [$fromT getName] -> [$toT getName]"
    }

    # arguments:
    # o    --  object being visited
    # name -- of the object
    # p1   -- print prefix
    # hier -- hier names (field name)

    method visit_VInterface { o name p1 hier} {
        set p2 "$p1  "
        set ifcname [$o getTopType]
        #pp $p1 "interface $ifcname  $name;"
        foreach m [$o getMembers] {
            set nm [lindex $m 0]
            set it [lindex $m 1]
            $it accept $this $nm $p2 $hier.$nm
        }
        #pp $p1 "endinterface\n"
        return ""
    }
    method visit_VMethodType { o name p1 hier} {
        set p2 "$p1  "
        set rt [$o getReturnType]

        set argT [$o getArguments]
        set anames [list]
        set astr   [list]
        foreach a $argT an [$o getArgNames] {
            lappend anames [list $an $a]
            lappend astr   "[$a getName] x_$an"
        }

        set args [join $astr ,]

        set AR [$o isAlwaysReady]
        set AE [$o isAlwaysEnabled ]
        set ACT [$o isAction]

        set doRDY true
        set doEN $ACT

        if { $AR } { set doRDY false }
        if { $ACT && $AE } { set doRDY false; set doEN false }

        set methodrdy ""
        if { $doRDY } {
            set mode "output"
            set boolType [::Virtual::createType "Bool" ""]
            set res [visit_VEnum $boolType "" "" $hier.RDY]
            set methodrdy " if ($res)"
        }

        if { $doEN } {
            set mode "input"
            ## XXX
        }

	set res ""
	set mode "args"
        foreach a $anames {
            set argName [lindex $a 0]
            set argType [lindex $a 1]
	    set res [visit_VPrimary $argType $argName $p2 $hier.$argName]
        }

	if {$res == ""} {
	    set mode "output"
	    # Look at return type
	    set retT [$o getReturnType]
	    set res [visit_VPrimary $retT "result_$name" $p2 $hier]
	}
	set cnv  [lindex $res 0]
	set bsvn [lindex $res 1]
	set rhs "bridge.$hier"
	if {$cnv ne "none"} {
	    switch "$cnv $ACT" {
		"ext true" {
		    set rhs "extend($rhs)"
		}
		"ext false" {
		    set rhs "compose($rhs, extend)"
		}
		"trn true" {
		    set rhs "truncate($rhs)"
		}
		"trn false" {
		    set rhs "compose($rhs, truncate)"
		}
		default {
		    pp "" "ERROR:"
		    puts "method $bsvn = $rhs;"
		    #return -code error "cnvACT = '$cnv $ACT'"
		}
	    }
	}
	pp $p1 "method $bsvn = $rhs;"
        set mode "unknown"
        return ""
    }

    method visit_VStruct { o name p1 hier } {
        set p2 "$p1  "
        set oldmode $mode

        switch $mode {
            "output" {
                pp $p1 "[$o getName] $name;"
                set mode "outstruct"
            }
        }

        foreach m [$o getMembers] {
            set mname [lindex $m 0]
            set mtype [lindex $m 1]

            $mtype accept $this $name.$mname $p2 $hier.$mname
        }

        set mode $oldmode
        return $name
    }
    method visit_VPrimary { o name p1 hier} {
        if { [$o getWidth] == -1 } {
            return ""
        }
        set p2 "$p1  "
        set prt [lookupPort $hier]

        if { $prt eq "" } {
            return ""
        }

        set bsvn [portName  $prt $hier]
        set res ""
        switch $mode {
            "args" {
                set res [list [portConvertor $o [$prt getTypeObj] $hier] $bsvn]
            }
            "output" {
                set res [list [portConvertor [$prt getTypeObj] $o $hier] $bsvn]
            }
            default {
                puts stderr "[$o info class] found unexpected mode: $mode"
            }
        }
        return $res
    }
    method visit_VEnum { o name p1 hier} {
        set p2 "$p1  "
        set prt [lookupPort $hier]
        if { $prt eq "" } {
            return ""
        }

        set bsvn [portName  $prt $hier]
        set res ""
        switch $mode {
            "args" {
                set pc [portConversion $o [$prt getTypeObj] x_$name  $hier]
                pp $p1 "dut.$bsvn ($pc);"
            }
            "output" {
                set res [portConversion [$prt getTypeObj] $o dut.$bsvn  $hier]
            }
            "outstruct" {
                set pc [portConversion [$prt getTypeObj] $o dut.$bsvn  $hier]
                pp $p1 "$name = $pc;"
            }
            default {
                puts stderr "[$o info class] found unexpected mode: $mode"
            }
        }
        return $res
    }

    method visit_VAlias { o name p1 hier} {
        # redirect
        [$o subtype] accept $this $name $p1 $hier
    }


    ## return type of methods....
    method visit_VAction { o name p1 hier } {
        return ""
    }

    method visit_VActionValue { o name p1 hier } {
        set retT [$o getSubtype]
        if { $expandPortTypes } {
            set res [$retT accept $this $name $hier]
        } else {
            set res [visit_VPrimary $retT $name $hier]
        }
        return $res
    }



    method visit_VType { o args } {
        puts stderr "Unimplemented visitor in [$this info class] for object [$o info class]"
        puts stderr "[$o show]"
    }
}
