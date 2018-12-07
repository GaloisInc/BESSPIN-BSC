
package require utils
package require Unique
package require SceMiMsg

################################################################################
###
################################################################################

namespace eval ::SceMiParams  {

    proc \/\/ {args} {
	# Ignore comments
    }
    proc ClockBinding   {num attribute attribute_value} {
    }

    proc Clock          {num attribute attribute_value} {
    }

    # currently not supporting probes here.
    proc OutputPipe    {num attribute attribute_value} {
	global OPT PACKAGES TYPES NAME

	if {$attribute == "PortName"} {
	    set NAME $attribute_value
	}

	if {[info exists OPT(-outputs)]} {
	    if {$attribute == "Type"} {
		set type_name $attribute_value

		if {[info exists OPT(-outputs)]} {
		    set packages [getPackages $type_name]
		    set ::TYPES($type_name) 1
		    foreach package $packages {
			set PACKAGES($package) 1
		    }
		}
	    }
	}	
    }

    proc InputPipe     {num attribute attribute_value} {
	global OPT PACKAGES TYPES

	if {[info exists OPT(-inputs)]} {
	    if {$attribute == "Type"} {
		set type_name $attribute_value
		set packages [getPackages $type_name]
		set TYPES($type_name) 1
		foreach package $packages {
		    set PACKAGES($package) 1
		}
	    }
	}
    }

    proc MessageInPort {num attribute attribute_value} {
	global OPT PACKAGES TYPES

	if {[info exists OPT(-inputs)]} {
	    if {$attribute == "Type"} {
		set type_name $attribute_value
		set packages [getPackages $type_name]
		set TYPES($type_name) 1
		foreach package $packages {
		    set PACKAGES($package) 1
		}
	    }
	}
    }

    proc MessageOutPort  {num attribute attribute_value} {
	global OPT PACKAGES TYPES NAME PROBES

	if {$attribute == "PortName"} {
	    set NAME $attribute_value
	}

	if {[info exists OPT(-outputs)] || [info exists OPT(-probes)] || [info exists OPT(-probe-code)]} {
	    if {$attribute == "Type"} {
		set type_name $attribute_value

		if {[isAProbe $type_name]} {
		    set packages [getPackages $type_name]
		    set TYPES($type_name) 1
		    foreach package $packages {
			set PACKAGES($package) 1
		    }

		    ## This is fragile!!!
		    set inner [string range $type_name 20 end-1]
		    lappend PROBES [list $NAME $inner]

		} else {
		    if {[info exists OPT(-outputs)]} {
			set packages [getPackages $type_name]
			set ::TYPES($type_name) 1
			foreach package $packages {
			    set PACKAGES($package) 1
			}
		    }
		}
	    }
	}
    }

    proc Link           {num attribute attribute_value} {
    }

    proc Serial         {num attribute attribute_value} {
	global OPT PACKAGES TYPES NAME PROBES SERIALPROBES PROBENUM
	if {$attribute == "Label"} {
	    set NAME $attribute_value
	}

	if {[info exists OPT(-outputs)] || [info exists OPT(-probes)] || [info exists OPT(-probe-code)]} {
	    if {$attribute == "Type"} {
		set type_name $attribute_value

		if {$type_name == "Empty"} {
		    return
		}
		lappend SERIALPROBES [list $NAME $PROBENUM $type_name]
                set ::TYPES($type_name) 1

	    } elseif {$attribute == "PrbNum"} {

		set PROBENUM $attribute_value
	    }
	}
    }

    ################################################################################
    ### "eval" the SceMi parameters file
    ################################################################################

    proc evalParamsFile {OPTS params_file} {
	global OPT PACKAGES TYPES HEADERTYPES ALLTYPES NAME PROBES SERIALPROBES PROBENUM
	upvar 1 $OPTS TEMP

	array set OPT [array get TEMP]

	utils::listToSet PACKAGES [list PreludeBSV]
	utils::listToSet TYPES [list]
	utils::listToSet HEADERTYPES [list]
	utils::listToSet ALLTYPES [list]
	set PROBES [list]
	set SERIALPROBES [list]

	#source $params_file
	set FH [open $params_file]
	while { [gets $FH line] >= 0} {
	    regsub -all {\$} $line {\$} line
	    eval $line
	}
	close $FH

	if {[info exists OPT(-package)]} {
	    set PACKAGES($OPT(-package)) 1
        }

	puts "Loading all packages ..."
	foreach package [utils::setToList PACKAGES] {
	    types::import_package false $package
	}
	puts "Loading all packages complete."


 	foreach type [utils::setToList TYPES] {
 	    set allTypes [findAllNestedTypes $type]
 	    foreach inner $allTypes {
		if {![isAProbe $inner]} {
		    set HEADERTYPES($inner) 1
		}
 	    }
 	    set allTypes [findAllNestedTypes $type 1]
 	    foreach inner $allTypes {
		if {![isAProbe $inner]} {
		    set ALLTYPES($inner) 1
		}
 	    }
 	}

	variable ::SceMiMsg::MemberPrefix
	variable ::SceMiMsg::EnumPrefix

	if {[info exists OPT(-memberPrefix)]} {
            set ::SceMiMsg::MemberPrefix $OPT(-memberPrefix)
        }
        if {[info exists OPT(-enumPrefix)]} {
            set ::SceMiMsg::EnumPrefix $OPT(-enumPrefix)
        }

	set header_list [utils::setToList HEADERTYPES]

	## Add probe code
        if { [info exists OPT(-probes)] || [info exists OPT(-probe-code)] } {
	    genProbeFile $OPT(-outdir) $SERIALPROBES
	}

	## Add headers
	if { [info exists OPT(-outputs)] || \
	     [info exists OPT(-inputs)]  || \
	     [info exists OPT(-probes)] } {
	    puts "Creating header files ..."

	    set header_files [::SceMiMsg::genAllHeaders     $OPT(-outdir) $header_list]
	    if {[info exists OPT(-aliases)]} {

		## Add Aliases (if any)
		foreach package [::Bluetcl::bpackage list] {
		    if {$package == "Prelude"} { continue }
		    if {$package == "PreludeBSV"} { continue }
		    if {$package == "SceMi"} { continue }
		    if {$package == "SceMiDefines"} { continue }
		    if {$package == "SceMiUtils"} { continue }
		    foreach type [::Bluetcl::bpackage types $package] {
			if { [catch "::Bluetcl::type constr $type" result] } {
			} else {
			    if { [catch "::Bluetcl::type full [list $result]" result] } {
                                puts stderr "unknown or unexpected type found in params file, $type, $result"
                                error "unknown or unexpected type found in params file, $type"
			    } else {
				if {[lindex $result 0] == "Alias"} {
				    set name [lindex $result 1]
				    set def  [removeSpaces [lindex $result 2]]
				    if {[info exists ALLTYPES($def)]} {
					set hfile [::SceMiMsg::genHeaderFile $OPT(-outdir) $name]
                                        append header_files " " $hfile
					set HEADERTYPES([utils::unQualType $name]) 1
				    }
				}
			    }
			}
		    }
		}

		set header_list [utils::setToList HEADERTYPES]
	    }

	    ::SceMiMsg::genMetaHeaderFile $OPT(-outdir) $header_files
	    puts "Creating header files complete."
	}
    }

    proc isAProbe {type_name} {
	return [regexp -all "^SceMiProbe::Probes#" $type_name]
    }

    proc findAllNestedTypes {types {include_primaries 0}} {
        array set F [list]
        while {0 != [llength $types]} {
            set t [utils::head $types]
            set types [utils::tail $types]

            if { [catch "::Bluetcl::type full [list $t]" ft] } {
                puts stderr $ft
                error "Cannot find or analysize type $t"
            }
            set kind [lindex $ft 0]
#            set tn [utils::unQualType $t]
            set tn [removeSpaces $t]

#	    if { $tn   == "Bool" } continue
            if { $kind == "List" } continue
            if { [info exists F($tn)] } { continue }

            switch $kind {
                "Struct" {
                    set F($tn) 1
                    foreach mem [types::getMembers $ft] {
                        lappend types [utils::fst $mem]
                    }
                }
                "Alias" {
                    set F($tn) 1
                    lappend types [lindex $ft 2]
                }
                "Enum" {
                    set F($tn) 1
                }
                "Vector" {
                    set F($tn) 1
                    lappend types [types::getElem $ft]
                }
                "TaggedUnion" {
                    set F($tn) 1
                    foreach mem [types::getMembers $ft] {
                        lappend types [utils::fst $mem]
                    }
                }
                "Primary" {
		    if {$include_primaries} {
			set F($tn) 1
		    }
                }
                default { # ignore the rest
		    puts "XXXXXXX $ft"
                    set F($tn) 1
                }
            }
        }
        return [array names F]
    }

    proc genProbeFile {outdir serialp_list} {

	puts "Creating in-line probe code file ..."

	set fname "SceMiProbes.cxx"
	set fname [file join  $outdir $fname]
	set oFILE [open "$fname" "w"]
	set probe_types [list]
	utils::listToSet done_probe [list]

        set p [list "// This file automatically generated by SceMiParams::genProbeFile" \
                   "// DO NOT EDIT" \
                   "// Generated on: [clock format [clock seconds]]" \
                   "// Bluespec version: [::Bluetcl::version]" \
                   "" \
                   "#include \"bsv_scemi.h\"" \
                  ]
        puts $oFILE [join $p \n]

	foreach typ $serialp_list {
	    set probe_name [lindex $typ 0]
	    set probe_num [lindex $typ 1]
	    set probe_type [lindex $typ 2]
	    set class [SceMiMsg::bsvTypeToCppClass $probe_type unused]

            if { [info exist done_probe($probe_num)] } { continue }
            lappend probe_types [list $probe_name $probe_num $probe_type $class]
        }

        set alltypes [list]
        foreach typ  $serialp_list {
            lappend alltypes  [lindex $typ 2]
        }
        set headers [SceMiMsg::bsvTypesToHeaders $alltypes]
        foreach header $headers {
		puts $oFILE "\#include \"$header.h\" "
        }

	puts $oFILE "\n"

	puts $oFILE [genProbeCreateDataCode [lsort -unique -integer -index 1 $probe_types]]


        puts "Code file $fname has been created."
	close $oFILE
	puts "Creating in-line probe code file complete."
    }

    proc genProbeCreateDataCode {probe_types} {

	set s ""
	append s "BSVType *ProbesXactor::createData(unsigned int probeNum, Packet *data, SceMiU64 cycle)\n"
	append s "\{\n"
	append s "  BSVType *type = NULL;\n"
	if {[llength $probe_types]} {
            append s "  unsigned int off = 0;\n"
            append s "  SceMiMessageData md(NULL, data, cycle);\n\n"
        } else {
            append s " // No probes are defined!" \n
        }

        foreach info $probe_types {
            set probe_num [lindex $info 1]
            set class [lindex $info 3]
            append s "  static $class p_$probe_num ;" \n
        }
        append s \n


	if {[llength $probe_types]} {
	    append s "  switch (probeNum) \{\n\n"

	    foreach info $probe_types {
		set probe_num [lindex $info 1]
		set class [lindex $info 3]

		append s "  case $probe_num:" \n
                append s "    p_$probe_num = $class (&md, off);" \n
		append s "    type = & p_$probe_num;" \n
		append s "    break;" \n
	    }
	    append s "  default:" \n
	    append s "    std::cerr << \"Error ProbeXactor::createData: Unknown probe number \" << probeNum << std::endl;" \n
	    append s "  \}" \n \n
	}
        append s "  return type;" "\n\}\n"


	append s "\n"
	append s "unsigned int ProbesXactor::getBitSize(unsigned int probeNum)\n"
	append s "\{\n"
	if {[llength $probe_types]} {
	    append s "  switch (probeNum) \{\n\n"

	    foreach info $probe_types {
		set probe_num [lindex $info 1]
		set probe_type [lindex $info 2]
                set width [Types::get_width $probe_type]
                if { $width == "" } {
                    error "Cannot determine width of probe: number=$probe_num type: $probe_type"
                }
		append s "  case $probe_num:  // $probe_type\n"
                append s "     return $width;" \n

	    }
	    append s "  default:\n"
	    append s "    return 0;\n"
            append s "  \}\n\n"
	}
	append s "  return 0;\n\}\n"
	return $s;
    }

    proc generateAllSceMiFiles {OPTS params_file} {
	upvar 1 $OPTS OPT

	::SceMiParams::evalParamsFile OPT $params_file

    }

    ################################################################################
    ### Look through a type name to find the associated packages
    ################################################################################

    proc getPackages {type_name} {
	regsub -all {\(|\)|,} $type_name " " mod
	regsub -all {::} $mod ":: " mod

	utils::listToSet packages [list]
	foreach id $mod {
	    set package [string trimright $id "::"]
	    if {[string length $package] < [string length $id]} {
		set packages($package) 1
	    }
	}
	return [utils::setToList packages]
    }

    ################################################################################
    ### Remove spaces from a string
    ################################################################################

    proc removeSpaces {string} {
	regsub -all { } $string "" mod
	return $mod
    }
}
package provide SceMiParams 1.0
