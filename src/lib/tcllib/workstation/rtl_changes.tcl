# Copyright 2009--2010 Bluespec Inc. All rights reserved

package require RtlEdit
package require Iwidgets

############################################################
itcl::class rtl_change_base {
    inherit itk::Widget

    # key as seen on C-side
    variable _masterId 0
    variable _status UNKNOWN
    common _options
    set _options(signaltype) [list Any Flop Reg Input Output]
    common TypeMap
    array set TypeMap [list \
                           SimpleProbe 		rtl_change_probe   \
                           BSVProbe	        rtl_change_bsv_probes   \
                           CaptureProbe 	rtl_capture_probe   \
                           ProbeTrigger		rtl_probe_trigger   \
                           DrawOut     		rtl_change_pullout \
                           CosimProbe 	        rtl_cosim_probe \
                          ]

    constructor {args} {
        itk_component add top {
            iwidgets::labeledframe $itk_interior.top -labelpos nw
        }
        pack $itk_component(top) -anchor nw -expand 1 -fill x -side top -padx 0 -pady 0
        pack $itk_interior -expand 1 -fill x -side top -padx 0 -pady 0

        set childsite [$itk_component(top) childsite]

        # Left side frame
        itk_component add lf {
            frame $childsite.lf
        }
        pack $itk_component(lf) -side left -expand 1 -fill x -anchor nw -pady 0 -padx 0

        # Right side frame
        itk_component add rf {
            frame $childsite.rf
        }
        pack $itk_component(rf) -side right -expand 1 -fill y -anchor ne -pady 0 -padx 0

        addFields
        packFields
        fillRightSide
        setStatus UNKNOWN
    }
    destructor {
    }

    private method fillRightSide {} {
        # status indicator
        itk_component add gif {
            label $itk_component(rf).gif -image [::fonts::get_image exclamation] -bd 0
        }
        pack $itk_component(gif) -side top -anchor n

        itk_component add masterid {
            iwidgets::entryfield $itk_component(rf).id -state readonly -width 5 -textvariable [itcl::scope _masterId]
        }
        #pack $itk_component(masterid) -side top -anchor n

        # buttons
        itk_component add bb {
            iwidgets::buttonbox  $itk_component(rf).bb -orient vertical -padx 0 -pady 0
        }
        set bb $itk_component(bb)
        $bb add show -text "Expand"   -padx 1 -pady 1 -command "$this expand"
        $bb add hide -text Reduce -padx 1 -pady 1 -command "$this reduce"
        $bb add delete -text Delete -padx 1 -pady 1 -command "$this deleteChangeBase"
        $bb add commit -text Commit -pady 1 -padx 1 -command "$this commitChangeBase"
        pack $bb -side bottom -expand 0 -fill x -anchor se -padx 0 -pady 0

        $bb hide show
    }

    public method reduce {} {
        set bb $itk_component(bb)
        $bb hide hide
        $bb hide commit
        $bb show show
        $bb hide delete

        set w $itk_component(lf)
        set i 0
        foreach {f namestr assoc widget attr checker} [getFields] {
            if {$i > 1} {
                pack forget $itk_component($f)
            }
            incr i
        }
    }
    public method expand {} {
        set bb $itk_component(bb)
        $bb show commit
        $bb show hide
        $bb show delete
        $bb hide show

        packFields
    }

    private method addFields {} {
        set w $itk_component(lf)
        foreach {f namestr assoc widget attr checker} [getFields] {
	    if { $widget == "Entry" } {
		addEntryField $w.$f $f $namestr $attr
	    } elseif { $widget == "CheckBox" } {
		addCheckBoxFields $w.$f $f $namestr $attr
            }
        }
    }
    
    private method addEntryField {pw f namestr attr} {
	itk_component add $f {
	    eval iwidgets::entryfield $pw -highlightthickness 0  -labelpos w \
		-labeltext $namestr -width 45 $attr
	}
	$itk_component($f) component entry configure  -validate key -validatecommand "$this entryValidate"
    }
    private method addCheckBoxFields {pw f namestr attr} {
	itk_component add $f {
	    eval iwidgets::checkbox $pw \
		-labeltext $namestr -orient horizontal $attr
	}
	set count 0
	foreach c $_options($f) {
	    $itk_component($f) add $c -text [string totitle $c]
	    $itk_component($f) buttonconfigure $count -command "$this checkbuttonvalidate $f $c"
	    incr count
	}
	#$itk_component($f) select 0
    }
    public method packFields {} {
        set w $itk_component(lf)
        foreach {f namestr assoc widget attr checker} [getFields] {
	    if { $widget == "Entry" } {
		pack $itk_component($f) -side top -anchor nw -fill x -expand 1 -pady 0
	    } elseif { $widget == "CheckBox" } {
		pack $itk_component($f) -side top -anchor nw -fill x -expand 1 -pady 0
	    }
        }
        alignFields
    }

    private method alignFields {} {
        set lab {}
        foreach {f n assoc widget attr checker} [getFields] {
	    if { $widget == "Entry" } {
		lappend lab $itk_component($f)
	    }
        }
	eval iwidgets::Labeledwidget::alignlabels $lab
    }

    # Populated the widget based on data from the C-side
    method populate {key} {
        array set XX [lindex [BhdlEdit::editnl dump $key] 0]
        foreach {f namestr assoc widget attr checker} [getFields] {
            set val ""
            if { [info exists XX($assoc)] } {
                set val $XX($assoc)
            }
            setField $f $val $widget
        }
        set _masterId $XX(UniqueId)
        setStatus OK
    }

    method getEditId {} { return $_masterId }
    method getStatus {} { return $_status }
    method setStatus {stat} {
        set _status $stat

        # update image
        switch -exact $stat {
            OK      {set img tick}
            UNKNOWN {set img asterisk_yellow}
            ERROR   {set img exclamation}
            default {set img error}
        }
        $itk_component(gif) configure -image [fonts::get_image $img]

        if { $stat == "UNKNOWN" } { $itk_component(bb) invoke show }

        # disable commit button if OK
        set ustate normal
        if { $stat == "OK" } { set ustate disable }
        $itk_component(bb) buttonconfigure commit -state $ustate
        setEdited
    }

    # set a value on an entry field
    method setField {field val {widget "Entry"} {postprocess ""}} {
	if { $widget == "Entry" } {
	    set ef $itk_component($field)
	    set st [$ef cget -state]
	    $ef configure -state normal

	    $ef clear
	    $ef insert end $val

	    $ef configure -state $st
	} elseif { $widget == "CheckBox" } {
	    set ef $itk_component($field)
	    foreach v $val {
		set index [$ef index $v]
		$ef select $index
		if { $postprocess != "" } {
		    if { [catch "$postprocess $field $val" msg] } {
			setStatus ERROR
			report_error $msg
		    }
		}
	    } 
	}
    }
    method deleteChangeBase {} {
        deleteChange
        RtlEdit::deleteEdit $_masterId
        setEdited
        destroy $itk_interior
    }
    # Entry call for the commit button
    method commitChangeBase {} {
        commitChange
    }
    # Not the usual validation, but one to set the status to edited.
    method entryValidate {} {
        setStatus UNKNOWN
        return 1
    }
    # virtual methods  ...
    method getFields {}    {}
    method getCommand {}   {}
    method getArgsOrder {} {}
    method deleteChange {} {}
    # worker call for the commit button
    method commitChange {} {
        set cmd [getCommand]
        set ok true;
        foreach {f namestr assoc widget attr checker} [getFields] {
            if { [catch "$checker $f" msg] } {
                setStatus ERROR
                report_error $msg
                set ok false }
        }
        if { $ok } {
            # build the command based on argument order
            foreach a [getArgsOrder] {
                set val [string trim [$itk_component($a) get]]
                append cmd " \{$val\}"
            }
            if { $_masterId > 0 } {
                RtlEdit::deleteEdit $_masterId
                set _masterId 0
            }
            if { [catch "$cmd" err] } {
                setStatus ERROR
                report_error $err
                set ok false
            } else {
                populate $err
            }
        }
        return $ok
    }

    # Methods to verify fields entries. -- TODO
    # Verify the entry in the field, and return the field
    method nocheck { field } {
        return  [string trim [$itk_component($field) get]]
    }
    method fielderror {field explain } {
        set msg "'[$itk_component($field) cget -labeltext]' $explain"
        append msg " for [$itk_component(top) cget -labeltext]."
        error $msg
    }
    # Any string except an empty string
    method anyname { field } {
        set val [string trim [$itk_component($field) get]]
        if { $val == "" } {
            fielderror $field "must not be empty"
        }
        return $val
    }
    # Any alpha-numeric string including "_"
    method simplename { field } {
        set val [string trim [$itk_component($field) get]]
        if { $val == "" } {
            fielderror $field "must not be empty"
        }
        if { ! [regexp {^[A-Za-z0-9_]+$} $val] } {
            fielderror $field "must be a alpha-numeric name"
        }
        return $val
    }
    method simplenamelist { field } {
        set vals [string trim [$itk_component($field) get]]
        if { $vals == "" } {
            fielderror $field "must not be empty"
        }
        foreach val $vals {
            if { ! [regexp {^[A-Za-z0-9_]+$} $val] } {
                fielderror $field "must be a list of alpha-numeric names"
            }
        }
            return $val
    }
    method numeric { field } {
        set num [string trim [$itk_component($field) get]]
        if { ! [string is integer -strict $num] } {
            fielderror $field "must be a valid integer"
        }
        return $num
    }
    method pathname { field }    { anyname $field }
    method expression { field }  { anyname $field }
    method expression1 { field } { anyname $field }
    method signal { field }      { anyname $field }
    method bsvtypeopt { field }  { nocheck $field }
    method flavoropt { field }   { anyname $field }
    method scanwidth { field }   { nocheck $field }

    # tell the parent we've been changed
    method setEdited {} {
        [winfo toplevel $itk_interior] setEdited
    }
    method report_error { msg } {
        [winfo toplevel $itk_interior] showEdit $itk_interior
        tk_messageBox -title "HDL Editor Error" -icon error -message $msg -parent $itk_interior
    }

    # create the right window based on the typepair
    proc createEntryForms { win typepairs } {
        array set XX $typepairs
        set ty $XX(Type)
        if { ! [info exists TypeMap($ty)] } {
            puts stderr "No entry form for $ty"
            return
        }
        set new [$TypeMap($ty)::create $win ""]
        $new populate $XX(UniqueId)
    }

    # create "probe names" from path/net
    proc genName { path net } {
        set pname ""
        if {$net != ""} {
            set n1 [lindex $net 0]
            set p1 [file tail $path]
            regsub -all {[\{\} /\[\]:\$]+} $p1/$n1 _ pname
            set pname [string trim $pname " _"]
        }
        return $pname
    }

    # method to process a checkbutton select action
    method checkbuttonvalidate { f val } {
	if { $val == "Any" } {
	    set ef $itk_component($f)
	    set index [$ef index $val]
	    set st [$ef get $index]
	    if { $st != 0 } {
		foreach c $_options($f) {
		    if { $c != $val } {
			set index [$ef index $c]
			$ef deselect $index
		    }
		}
	    }
	} else {
	    #deselect Any
	    set ef $itk_component($f)
	    set index [$ef index Any]
	    $ef deselect $index
	}
	setStatus UNKNOWN
        setEdited
    }
    # method to validate a checkbox
    method checkboxvalidate { f } {
	set ef $itk_component($f)
	foreach val $_options($f) {
	    set index [$ef index $val]
	    set st [$ef get $index]
	    if { $val == "Any" } {
		if { $st != 0 } {
		    foreach c $_options($f) {
			if { $c != $val } {
			    set cindex [$ef index $c]
			    $ef deselect $cindex
			}
		    }
		}
	    } elseif { $st == 1 } {
		#deselect Any
		set index [$ef index Any]
		$ef deselect $index
	    }
	}
    }
}

############################################################
itcl::class rtl_change_probe {
    inherit rtl_change_base

    # Array (field Prompt AssocArraykey WidgetType arguments verifyproc)
    common fields [list \
                       name     Name    Name     Entry  {} 	simplename \
                       path     Path    Path     Entry  {} 	pathname \
                       pattern  Pattern Patterns Entry  {} 	expression \
		       signals  Signals Signals  Entry  {-state readonly} nocheck \
                       enable   Valid   Enable   Entry  {} 	expression1 \
                       clock    Clock   Clock    Entry  {} 	expression \
                       bsvtype  BSVType BSVType  Entry  {}      bsvtypeopt \
                       width    Width   Width    Entry  {-state readonly} nocheck \
		       signaltype SignalType  SignalType  CheckBox  {}  checkboxvalidate \
                      ]
    common command "RtlEdit::addprobe"
    common argorder [list name path pattern clock enable bsvtype signaltype]

    constructor {args} {
        eval itk_initialize $args
        $itk_component(top) configure -labeltext "Value Probe"
        $itk_component(signaltype) configure -labeltext "Signal Types"
        $itk_component(signaltype) configure -relief solid
        setField clock CLK
        setField enable "1'b1"
    }
    destructor {}
    method getFields {} { return $fields }
    method getCommand {}  { return $command }
    method getArgsOrder {} { return $argorder }

    proc create {win currpath {net ""} {type ""} {signaltype "Any"}} {
        set newp [rtl_change_probe $win.#auto]

        $newp setField path $currpath
        $newp setField name [genName $currpath $net]
        $newp setField pattern $net
        $newp setField bsvtype $type
        $newp setField signaltype $signaltype CheckBox checkbuttonvalidate
        return $newp
    }
}

############################################################
itcl::class rtl_change_bsv_probes {
    inherit rtl_change_base

    # Array (field Prompt AssocArraykey WidgetType arguments verifyproc)
    common fields [list \
                       name       Name        Name        Entry     {} 	simplename \
                       pattern    TopModule   HierPattern Entry     {} 	expression \
                       mpattern   SigPattern  Pattern     Entry     {} 	expression \
		       signaltype SignalType  SignalType  CheckBox  {}  checkboxvalidate \
                      ]
    common command "RtlEdit::addbsvprobes"
    common argorder [list name pattern mpattern signaltype]

    constructor {args} {
        eval itk_initialize $args
        $itk_component(top) configure -labeltext "BSV Probes"
        $itk_component(signaltype) configure -labeltext "Signal Types"
        $itk_component(signaltype) configure -relief solid
    }
    destructor {}
    method getFields {} { return $fields }
    method getCommand {}  { return $command }
    method getArgsOrder {} { return $argorder }
 
    proc create {win currpath {mpattern ".*$PROBE"} {signaltype "Any"}} {
        set newp [rtl_change_bsv_probes $win.#auto]

        #$newp setField path $currpath
        $newp setField name [genName $currpath "BSVProbes"]
        $newp setField pattern "$currpath"
        $newp setField mpattern "$mpattern"
        $newp setField signaltype $signaltype CheckBox checkbuttonvalidate
        return $newp
    }
}

############################################################
itcl::class rtl_cosim_probe {
    inherit rtl_change_base

    # Array (field Prompt AssocArraykey WidgetType arguments verifyproc)
    common fields [list \
                       name     Name    Name     Entry  {} 	simplename \
                       path     Path    Path     Entry  {} 	pathname \
                       clock    Clock   Clock    Entry  {} 	expression \
                       uclock   UClock  UClock   Entry  {} 	nocheck \
                       trigger  Trigger Trigger  Entry  {} 	expression1 \
                       flavor   Flavor  Flavor   Entry  {}      flavoropt \
                       width    Width   Width    Entry  {}      scanwidth \
                      ]
    common command "RtlEdit::addcosim"
    common argorder [list name path clock uclock trigger width flavor]

    constructor {args} {
        eval itk_initialize $args
        $itk_component(top) configure -labeltext "Cosim Probe"
        setField clock CLK
        setField uclock ""
	setField trigger "1'b1"
        setField width 32
    }
    destructor {}
    method getFields {} { return $fields }
    method getCommand {}  { return $command }
    method getArgsOrder {} { return $argorder }

    proc create {win currpath {flavor "OBSERVE"}} {
        set newp [rtl_cosim_probe $win.#auto]

        $newp setField path $currpath
        $newp setField name [genName $currpath "cosim"]
	$newp setField flavor $flavor
        return $newp
    }
}

############################################################
itcl::class rtl_change_pullout {
    inherit rtl_change_base

    # Array (field Prompt AssocArraykey WidgetType arguments verifyproc)
    common fields [list \
                       path     Path      Path     Entry  {} pathname	\
                       toppath  TopPath   TopPath  Entry  {} pathname  \
                       port     Port      Port     Entry  {} simplename \
                       signal   Signal    Signal   Entry  {} signal \
                       width    Width     Width    Entry  {-state readonly} nocheck \
                      ]
    common command "RtlEdit::drawout"
    common argorder [list path toppath signal port ]

    constructor {args} {
        eval itk_initialize $args
        $itk_component(top) configure -labeltext "Pull Out Signal"
        setField toppath "/"
    }
    destructor {}
    method getFields {} { return $fields }
    method getCommand {}  { return $command }
    method getArgsOrder {} { return $argorder }

    proc create {win currpath {net ""}} {
        set newp [rtl_change_pullout $win.#auto]

        set net [lindex $net 0]
        $newp setField path $currpath
        $newp setField signal  $net
        $newp setField port [string toupper $net]
        return $newp
    }
}

############################################################
itcl::class rtl_capture_probe {
    inherit rtl_change_base

    # Array (field Prompt AssocArraykey WidgetType arguments verifyproc)
    common fields [list \
                       name     	Name       Name       Entry  {} 	simplename \
                       path     	Path       Path       Entry  {} 	pathname \
                       pattern     	Pattern    Patterns   Entry  {} 	expression \
		       signals  	Signals    Signals    Entry  {-state readonly} nocheck \
                       enable   	Valid      Enable     Entry  {} 	expression1 \
                       clock    	Clock      Clock      Entry  {} 	expression \
                       trigger   	Trigger    Trigger    Entry  {} 	expression1 \
                       bsvtype  	BSVType    BSVType    Entry  {}     	bsvtypeopt \
                       cdepth   	Depth      Depth      Entry  {}     	numeric \
                       runwidth   	RunWidth   RunWidth   Entry  {}     	numeric \
                       dumpdelay 	DumpDelay  DumpDelay  Entry  {}     	numeric \
                       width    	Width      Width      Entry  {-state readonly} nocheck \
		       signaltype       SignalType SignalType CheckBox {}       checkboxvalidate \
                      ]
    common command "RtlEdit::addcapture"
    common argorder [list name path pattern clock enable trigger cdepth runwidth dumpdelay bsvtype signaltype]

    constructor {args} {
        eval itk_initialize $args
        $itk_component(top) configure -labeltext "Capture Probe"
        $itk_component(signaltype) configure -labeltext "Signal Types"
        $itk_component(signaltype) configure -relief solid
        setField clock CLK
        setField enable "1'b1"
        setField trigger "1'b0"
        setField cdepth 32
        setField runwidth 4
        setField dumpdelay 50
    }
    destructor {}
    method getFields {} { return $fields }
    method getCommand {}  { return $command }
    method getArgsOrder {} { return $argorder }

    proc create {win currpath {net ""} {type ""} {signaltype "Any"}} {
        set newp [rtl_capture_probe $win.#auto]

        $newp setField path $currpath
        $newp setField name [genName $currpath $net]
        $newp setField pattern  $net
        $newp setField bsvtype $type
        $newp setField signaltype $signaltype CheckBox checkbuttonvalidate
        return $newp
    }
}

############################################################
itcl::class rtl_probe_trigger {
    inherit rtl_change_base

    # Array (field Prompt AssocArraykey WidgetType arguments verifyproc)
    common fields [list \
                       name     Name      Name       Entry  {} 	simplename \
                       path     Path      Path       Entry  {} 	pathname \
                       expr     Expr      Expr       Entry  {} 	expression \
                       clock    Clock     Clock      Entry  {} 	expression \
                       caps     Clients   Captures   Entry  {} 	simplenamelist \
                      ]
    common command "RtlEdit::addtrigger"
    common argorder [list name path expr clock caps]

    constructor {args} {
        eval itk_initialize $args
        $itk_component(top) configure -labeltext "Probe Trigger"
        setField clock CLK
    }
    destructor {}
    method getFields {} { return $fields }
    method getCommand {}  { return $command }
    method getArgsOrder {} { return $argorder }

    proc create {win currpath {net ""}} {
        set newp [rtl_probe_trigger $win.#auto]

        $newp setField path $currpath
        $newp setField name [genName $currpath $net]
        $newp setField expr  $net
        return $newp
    }
}
