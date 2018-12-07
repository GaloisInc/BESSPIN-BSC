#
# Copyright 2009--2010 Bluespec Inc. All rights reserved

package require Iwidgets 4.0
package require Itk
package require BhdlEdit

#catch "itcl::delete class rtl_editor"
itcl::class rtl_editor {
    inherit itk::Toplevel

    protected variable _lasthier ""

    constructor {args} {
        itk_component add apply {
            rtl_editor_apply::create $itk_interior.apply
        } {}

        itk_component add loadfile {
            rtl_editor_loadfile::create $itk_interior.loadfile
        } {}

        mkMenuBar
        mkPanedWin

        pack $itk_component(menubar) -fill x -side top
        pack $itk_component(pw) -fill both -expand true -side top

        eval itk_initialize $args

        set btags [bindtags $itk_component(hull)]
        lappend btags [$itk_component(hull) component hier]
        bindtags $itk_component(hull) $btags

        wm title $itk_interior "Bluespec HDL Editor & Probe Insertion Tool"
        wm minsize $itk_interior 750 400

        $itk_interior refreshEdits
        $itk_component(apply) setSaved
    }

    destructor {}

    private method mkMenuBar {} {
        itk_component add menubar {
            base::menubar $itk_interior.mb -helpvariable helpVar -menubuttons {
                menubutton file -text "File" -underline 0 -menu {
                    options -tearoff false

                    command open -label "Load VL" -underline 0
                    separator sep1
                    command script -label "Load Script ..." -underline 5
                    command apply  -label "Apply Changes ..." -underline 0
                    command applyNQ  -label "Quick Apply" -underline 4
                    separator sep2
                    command quit -label "Quit" -underline 0
                }
                menubutton hdl -text "HDLEdits" -underline 0 -menu {
                    ########    Hot Keys  used --  R X  M V D   P C T O
                    command reduce -label "Reduce All" -underline 0
                    command expand -label "Expand All" -underline 1
                    separator seph1
                    command commitall -label "Commit All" -underline 2
                    command revert    -label "Revert"     -underline 2
                    command remove    -label "Delete All" -underline 0
                    separator seph2
                    command addprobe   -label "Add Probe"     -underline 4
                    command addbsvprobes -label "Add BSV Probes" -underline 4
                    command addcosim    -label "Add Cosim"     -underline 4
                    command addcapture -label "Add Capture"   -underline 4
                    command addtrigger -label "Add Trigger"   -underline 4
                    command addpullo   -label "Add PullOut"   -underline 8
                }
            }
        } { }
        $itk_component(menubar) menuconfigure file.open -command  "$itk_interior loadFile"
        $itk_component(menubar) menuconfigure file.script -command  "$itk_interior loadScript"
        $itk_component(menubar) menuconfigure file.applyNQ -command [itcl::code $this applyNoQuestions $itk_component(hull)]

        $itk_component(menubar) menuconfigure file.apply -command  "$itk_interior applyDialog"
        $itk_component(menubar) menuconfigure file.quit  -command  "$itk_interior close"

        $itk_component(menubar) menuconfigure hdl.reduce -command "$itk_interior doToAllEdits reduce"
        $itk_component(menubar) menuconfigure hdl.expand -command "$itk_interior doToAllEdits expand"

        $itk_component(menubar) menuconfigure hdl.commitall -command "$itk_interior commitAll"
        $itk_component(menubar) menuconfigure hdl.revert    -command "$itk_interior refreshEdits"
        $itk_component(menubar) menuconfigure hdl.remove    -command "$itk_interior removeAll"

        $itk_component(menubar) menuconfigure hdl.addprobe    -command "$itk_interior edit_action signal addProbe"
        $itk_component(menubar) menuconfigure hdl.addbsvprobes -command "$itk_interior edit_action signal addBSVProbes"
        $itk_component(menubar) menuconfigure hdl.addcosim     -command "$itk_interior edit_action signal addCosim"
        $itk_component(menubar) menuconfigure hdl.addcapture  -command "$itk_interior edit_action signal addCapture"
        $itk_component(menubar) menuconfigure hdl.addtrigger  -command "$itk_interior edit_action signal addTrigger"
        $itk_component(menubar) menuconfigure hdl.addpullo    -command "$itk_interior edit_action signal pullOut"


    }
    private method mkPanedWin {} {
        itk_component add pw {
            base::panedwindow $itk_interior.pw -width 850p -height 600p -orient vertical
        }

        ##  Hierarchy pane  ##########################################
        $itk_component(pw) add hier
        set cs [$itk_component(pw) childsite hier]
        set texttags [list {bsv {-foreground #100842}} \
                          {rule {-foreground blue}} \
                          {synth {-foreground #100842}} \
                          {hilite {-foreground white -background #100842}} \
                          {lolite {-foreground black -background white}} \
                     ]
        itk_component add hier {
            base::hierarchy $cs.h \
                -labeltext "Design Hierarchy" -labelpos nw \
                -querycommand "RtlEdit::getChildInsts %n" -visibleitems 5x5 \
                -selectcommand "$itk_interior select_node %n %s" -alwaysquery false \
                -texttags $texttags
        } { }
        configure_hierviewer hier
        pack $itk_component(hier) -fill both -expand 1

        itk_component add bsvOrV {
            iwidgets::radiobox $cs.rb \
                -labeltext "View hierarchy as" -orient horizontal
        } { }
        $itk_component(bsvOrV) add bsv -text "BSV"
        $itk_component(bsvOrV) add hdl -text "HDL"
        $itk_component(bsvOrV) buttonconfigure bsv -command [itcl::code $this setHierView bsv]
        $itk_component(bsvOrV) buttonconfigure hdl -command [itcl::code $this setHierView hdl]
        set view [RtlEdit::getHierView]

        if { $view == "bsv" } {
            # only show if the default view is bsv
            $itk_component(bsvOrV) select bsv
            pack $itk_component(bsvOrV) -fill x -expand 0 -pady 0 -side bottom
        }

        #####   Signal pane ##########################################
        $itk_component(pw) add signal
        set cs [$itk_component(pw) childsite signal]
        itk_component add filter {
            iwidgets::combobox  $cs.filter \
                -labeltext {Filter} -command "$itk_interior filterSignalUpdate" \
                -completion false
        } {}
        itk_component add signal {
            base::hierarchy $cs.h -labeltext "Signals" -labelpos nw \
                -alwaysquery true  -visibleitems 5x5 \
                -querycommand "$itk_interior listSignals %n" \
                -selectcommand "$itk_interior selectSignal {%n} %s" \
                -selectonemorecommand "$itk_interior selectAnotherSignal {%n} %s"
        } {}
        $itk_component(filter) insert list end * WILL_FIRE_*
        pack $itk_component(signal) -fill both -expand 1  -pady 0
        pack $itk_component(filter) -fill x -expand 0 -pady 0 -side bottom
        configure_hierviewer signal

        #####   Editor pane ##########################################
        $itk_component(pw) add editpane
        set cs [$itk_component(pw) childsite editpane]
        itk_component add edits {
            iwidgets::scrolledframe $cs.edits -width 5.5i \
                -hscrollmode static -vscrollmode static \
                -labeltext "Design Edits" -labelpos nw
        }
        pack $itk_component(edits) -fill both -expand 1

        itk_component add bb {
            iwidgets::buttonbox  $cs.bb -padx 0 -pady 0
        }
        $itk_component(bb) add commit -text "Commit All" -command "$itk_interior commitAll"
        $itk_component(bb) add apply -text "Apply..." -command "$itk_interior applyDialog"
        $itk_component(bb) add doit  -text "Quick Apply" -command [itcl::code $this applyNoQuestions $itk_component(hull)]
        $itk_component(bb) add close -text Exit -command "$itk_interior close"
        pack $itk_component(bb) -side top -anchor se -expand 0 -fill x

        ## Adjust proportions
        $itk_component(pw) fraction 30 25 45
    }

    private method configure_hierviewer { name } {
        set h $itk_component($name)

        $h component itemMenu add command -label "Add Probe" 	-command "$itk_interior edit_action $name addProbe"
	if { $name == "hier" } {
	  $h component itemMenu add command -label "Add BSV Probes" -command "$itk_interior edit_action $name addBSVProbes"
        }
        $h component itemMenu add command -label "Add Cosim" 	-command "$itk_interior edit_action $name addCosim"
        $h component itemMenu add command -label "Add Capture" 	-command "$itk_interior edit_action $name addCapture"
        $h component itemMenu add command -label "Add Trigger" 	-command "$itk_interior edit_action $name addTrigger"
        $h component itemMenu add command -label "Add PullOut"  -command "$itk_interior edit_action $name pullOut"
        $h component itemMenu add separator

        if { $name == "hier" } {
            $h component itemMenu add command -label "Expand" \
                -command "$itk_interior action_on_node hier expand"
            $h component itemMenu add command -label "Collapse" \
                -command "$itk_interior action_on_node hier collapse"
            $h component itemMenu add command -label "Collapse All" \
                -command "$itk_interior collapse_all"
            $h component itemMenu add separator

            ### Back ground menu #####################################
            $h component bgMenu add command -label "Collapse All" \
                -command "$itk_interior collapse_all"
        }
        if { $name == "signal" } {
            $h component itemMenu add command -label "Clear Selections" \
                -command "$h selection clear"

            ### Back ground menu #####################################
            $h component bgMenu add command -label "Clear Selections" \
                -command "$h selection clear"
        }
    }

    # Signal in signal list --  list of children signals
    method listSignals {node} {
        set res [list]
        if {$node == ""} {
            set res [RtlEdit::getNets $_lasthier]

            set f [$itk_component(filter) get]
            if { $f != "" } {
                set res [lsearch -glob -nocase -inline -all -index 0 $res $f]
            }
        }
        return $res
    }
    # Signal in signal list
    method selectSignal {uid status} {
        set win $itk_component(signal)
        $win selection clear
        if { $status} {
            $win selection remove $uid
        } else {
            $win selection add $uid
        }
    }
    method selectAnotherSignal {uid status} {
        set win $itk_component(signal)
        if { $status} {
            $win selection remove $uid
        } else {
            $win selection add $uid
        }
    }

    method action_on_node {comp action} {
        set c [lindex [$itk_component($comp) current] 1]
        $itk_component($comp) $action $c
    }

    method setHierView { view } {
        set changed [RtlEdit::setHierView $view]
        if { $changed } {
            refreshHier
            refreshSignalList
        }
    }
    # Node in hierarchy
    method select_node {uid status } {
        set win $itk_component(hier)
        set _lasthier $uid
        $win selection clear
        if {$status} {
            $win selection remove $uid
        } else {
            $win selection add $uid
            refreshSignalList
        }
    }
    # reload the signal list either via a new hier node or filter.
    method refreshSignalList {} {
        $itk_component(signal) configure -querycommand [$itk_component(signal) cget -querycommand]
    }
    method filterSignalUpdate {} {
        set pat [string trim [$itk_component(filter) get]]
        $itk_component(filter) clear entry
        $itk_component(filter) insert entry 0 $pat
        if { [lsearch -exact [$itk_component(filter) component list get 0 end] $pat] == -1 } {
            $itk_component(filter) insert list 0 $pat
        }
        refreshSignalList
    }
    method selection_clear {} {
        set win $itk_component(hier)
        $win selection clear
    }
    method collapse_all {} {
        set cmd [$itk_component(hier) cget -querycommand]
        $itk_component(hier) configure -querycommand $cmd
    }
    method edit_action {name action} {

        set nets ""
        if { $name == "signal" } {
            set nets [$itk_component($name) selection get]
            $itk_component($name) selection clear
        }

        set path [RtlEdit::getActualPath $_lasthier]
        set nets [RtlEdit::getActualNets $_lasthier $nets]
        set type [RtlEdit::getActualType $_lasthier $nets]

        edit_action_do $action $path $nets $type
        $itk_component(edits) justify bottom
    }

    method edit_action_do { action path net type} {
        set editcs [$itk_component(edits) childsite]
        switch -exact $action {
            "addProbe" 		{rtl_change_probe::create     $editcs $path $net $type}
            "addBSVProbes"	{rtl_change_bsv_probes::create $editcs $path}
            "addCosim" 		{rtl_cosim_probe::create       $editcs $path}
            "addCapture" 	{rtl_capture_probe::create    $editcs $path $net $type}
            "addTrigger" 	{rtl_probe_trigger::create    $editcs $path $net}
            "pullOut"  		{rtl_change_pullout::create   $editcs $path $net}
            default    		{error "Unknown action -- $action"}
        }
        setEdited
    }

    method refreshEdits {} {
        set editcs [ $itk_component(edits) childsite]
        set edits [winfo children $editcs]
        foreach fr $edits {
            destroy $fr
        }

        foreach edit [::BhdlEdit::editnl list] {
            rtl_change_base::createEntryForms $editcs $edit
        }
        $itk_component(edits) justify bottom
    }

    method refreshHier {} {
        set cmd [$itk_component(hier) cget -querycommand]
	$itk_component(hier) configure -querycommand $cmd 
    }

    # bring a particular edit into view
    method showEdit {win} {
        set y [winfo y $win]
        set h [winfo height [$itk_component(edits) childsite]]
        set frac [expr (1.0 * $y )/$h]
        $itk_component(edits) yview moveto $frac
    }

    method applyDialog {} {
        set stat [commitAll]
        if { $stat } {
            $itk_component(apply) activate
            raise $itk_component(apply) $itk_interior
        }
    }

    method applyNoQuestions {parentwin} {
        set stat [commitAll]
        update
        if {! $stat} { return $stat}
        set stat [$itk_component(apply) applyEdits $parentwin ]
        if {! $stat} { return $stat }
        if { ! [$itk_component(apply) getSaved] } {
                $itk_component(apply) writeScript false
            }
        return $stat
    }

    method loadFile {} {
        $itk_component(loadfile) activate
        raise $itk_component(loadfile) $itk_interior
    }

    method loadScript {} {
        set types [list {{Script files} {.script}}  {{All files} {*}}]
        set f [tk_getOpenFile -title "Load Script"  -parent "$itk_interior" -filetypes $types]
        if { $f == "" } { return }
        if { [catch "RtlEdit::processScript $f" err] } {
            tk_messageBox -title "HDL Editor Error" -icon error -message $err
        }

        # update the edit window,
        # keeping the non-commited edits
        set editcs [ $itk_component(edits) childsite]
        set edits [winfo children $editcs]
        foreach fr $edits {
            if { "OK" == [$fr getStatus] } {
                set Displayed([$fr getEditId]) 1
            }
        }

        foreach edit [::BhdlEdit::editnl list] {
            catch "unset XX"
            array set XX $edit
            if { [info exists Displayed($XX(Key))] } { continue }
            set Displayed($XX(Key)) 1
            rtl_change_base::createEntryForms $editcs $edit
        }
        $itk_component(edits) justify bottom

    }

    method commitAll {} {
        set ok true
        foreach w [winfo  children [.editor component edits childsite] ] {
            if {[$w getStatus] != "OK" } {
                set stat [$w commitChange]
                if { ! $stat } { set ok false }
                setEdited
            }
        }
        if { [catch "::BhdlEdit::editnl crossreference" err] } {
            tk_messageBox -title "HDL Editor Error" -icon error -message $err -parent $itk_interior
            set ok false
        }
        return $ok
    }
    method removeAll {} {
        foreach w [winfo  children [.editor component edits childsite] ] {
            $w deleteChangeBase
        }
        setEdited
        $itk_component(edits) justify top

    }

    method doToAllEdits { meth } {
        foreach w [winfo  children [.editor component edits childsite] ] {
            $w $meth
        }
        $itk_component(edits) justify top
    }

    method setEdited {} {
        $itk_component(apply) setEdited
    }

    method close {} {
        # Allow an unprompted closing only when script and apply has completed
        set prompt false
        set resp yes
        set msg "Are you sure you want to exit?"
        if { ! [$itk_component(apply) getApplied] } {
            set prompt true
            append msg "\n-- HDL edits have not been applied to source"
        }
        if { ! [$itk_component(apply) getSaved] } {
            set prompt true
            append msg "\n-- A replay script has not been saved"
        }
        if { $prompt } {
            set resp [tk_messageBox -title "Confirm Exit" -type yesno -icon question \
                          -parent $itk_interior -message $msg -default no]
        }
        if { $resp }  exit
    }
    proc create {pathName args} {
        set w [uplevel #1 ::rtl_editor $pathName $args]
        wm protocol $w WM_DELETE_WINDOW "$w close"
    }
}

############################################################
itcl::class rtl_editor_apply {
    inherit iwidgets::Dialogshell

    private variable   _Saved true
    private variable   _Applied false

    constructor {args} {
        eval itk_initialize $args
        set win [$this childsite]

        mkOutputDir
        mkScemiParam
        mkReplayScript
        alignLabels

        $itk_component(hull) add save  -text "Save To Script" -command "$this writeScript true"
        $itk_component(hull) add apply -text "Apply" -command "$this applyEdits $itk_component(hull)"
        $itk_component(hull) add close -text "Close" -command "$this deactivate"

        doCheckButton

        wm title $itk_component(hull) "Apply Changes Dialog"
    }

    destructor {
    }

    method setEdited {} {
        set _Saved   false
        set _Applied false
        $itk_component(hull) buttonconfigure save -state normal
        $itk_component(hull) buttonconfigure apply -state normal
        # validate needs a return true command
        return true
    }
    method setApplied {} {
        set _Applied true
        $itk_component(hull) buttonconfigure apply -state disable
    }
    method setSaved {} {
        set _Saved true
        $itk_component(hull) buttonconfigure save -state disable
    }
    method getApplied {} { return $_Applied }
    method getSaved   {} { return $_Saved }

    method applyEdits {parentwin} {
        eval upvar #0 [$itk_component(outdir) cget -textvariable] outdir
        set ok true
        if { [catch "::RtlEdit::applyEdits" outfile] } {
            set msg "Error in applying edits\n$outfile"
            tk_messageBox -title "HDL Editor Error" -icon error -message $msg -parent $itk_interior
            return false
        }
        set msg "Edited files are in $outdir/\n"
        foreach f $outfile {
            append msg " " [file tail $f] \n
        }

        # Do SceMi update as well
        eval upvar #0 [$itk_component(scheckb) cget -variable] bstate

        if { $bstate } {
            if { [catch "::RtlEdit::updateParams" m2] } {
                set m2 "Error during the update of SceMi params file\n$m2"
                tk_messageBox -title "HDL Editor Error" -icon error -message $m2 -parent $itk_interior
                set ok false
            } else {
                append msg \n "SceMi Params file updated into\n $m2"
            }
        }

        tk_messageBox -icon info  -type ok -title Message -parent $parentwin \
            -message $msg
        if { $ok } { setApplied }
        return $ok
    }

    method writeScript {ack} {
        set replay [$itk_component(replay) get]
        if {[catch "::RtlEdit::writeScript $replay" status]} {
            set err "Error during the creation of reply script\n$status"
            tk_messageBox -title "HDL Editor Error" -icon error -message $m2 -parent $itk_interior
            return false
        } else {
            if { $ack } {
                set msg "Replay script file has been written to $replay"
                tk_messageBox -icon info  -type ok -title Message -parent $itk_interior \
                    -message $msg
            }
            setSaved
            return true
        }
    }

    private method  mkOutputDir {} {
        ## File sub frame directory -- fileframe
        itk_component add ff {
            iwidgets::Labeledframe $itk_interior.ff -labelpos nw -labeltext "Files"
        } {}
        pack $itk_component(ff)  -anchor nw -expand 1 -fill x

        itk_component add outdir {
            iwidgets::entryfield  [$itk_component(ff) childsite].outdir \
                -labeltext {Output Directory} -width 50 -textvariable ::RtlEdit::OPT(--outputdir)
        }
        $itk_component(outdir) component entry configure  -validate key -validatecommand "$this setEdited"
        pack $itk_component(outdir)  -anchor nw -expand 1 -fill x
    }

    private method  mkScemiParam {} {
        ## File sub frame directory -- fileframe
        itk_component add sf {
            iwidgets::Labeledframe $itk_interior.sf -labelpos nw -labeltext "Scemi Params Files"
        } {}
        pack $itk_component(sf)  -anchor nw -expand 1 -fill x

        itk_component add scheckb {
            checkbutton  [$itk_component(sf) childsite].scb \
                -text {Update SceMi Params File} -command "$this doCheckButton" \
                -variable ::RtlEdit::OPT(doparams)
        } {}

        pack $itk_component(scheckb)  -anchor nw
        itk_component add infile {
            iwidgets::entryfield  [$itk_component(sf) childsite].infile \
                -labeltext {Input File} -width 50 -textvariable ::RtlEdit::OPT(--params)
        }
        $itk_component(infile) component entry configure  -validate key -validatecommand "$this setEdited"
        pack $itk_component(infile)  -anchor nw -expand 1 -fill x

        itk_component add outfile {
            iwidgets::entryfield  [$itk_component(sf) childsite].outfile \
                -labeltext {Output File} -width 50 -textvariable ::RtlEdit::OPT(--params-out)
        }
        $itk_component(outfile) component entry configure  -validate key -validatecommand "$this setEdited"
        pack $itk_component(outfile)  -anchor nw -expand 1 -fill x

        if { $::RtlEdit::OPT(doparams) } {
            $itk_component(scheckb) select
        }
    }

    private method mkReplayScript {} {

        itk_component add rf {
            iwidgets::Labeledframe $itk_interior.rf -labelpos nw -labeltext "Replay Script"
        } {}
        pack $itk_component(rf)  -anchor nw -expand 1 -fill x

        itk_component add replay {
            iwidgets::entryfield  [$itk_component(rf) childsite].replay \
                -labeltext {Replay Script} -width 50 -textvariable ::RtlEdit::OPT(outscript)
        }
        $itk_component(replay) component entry configure  -validate key -validatecommand "$this setEdited"
        pack $itk_component(replay)  -anchor nw -expand 1 -fill x
    }

    private method alignLabels {} {
        set labs [list]
        foreach f [list outdir infile outfile replay ] {
            lappend labs $itk_component($f)
        }
        eval iwidgets::Labeledwidget::alignlabels $labs
    }

    method doCheckButton {} {
        eval upvar #0 [$itk_component(scheckb) cget -variable] bstate
        if { $bstate } {
            $itk_component(infile) configure -state normal
            $itk_component(outfile) configure -state normal
        } else {
            $itk_component(infile) configure -state disabled
            $itk_component(outfile) configure -state disabled
        }
        setEdited
    }

    proc create  {pathname args} {
        uplevel #1 ::rtl_editor_apply $pathname -modality none $args
    }
}

############################################################
itcl::class rtl_editor_loadfile {
    inherit iwidgets::Dialogshell

    private variable   _Saved true
    private variable   _Applied false

    constructor {args} {
        eval itk_initialize $args
        set win [$this childsite]

        mkFileSelection

        $itk_component(hull) add load  -text "Load" -command "$this loadVLFile"

        wm title $itk_component(hull) "Load Verilog File Dialog"
    }

    destructor {
    }

    private method  mkFileSelection {} {
        set types [list {{Verilog files} {.v}}  {{All files} {*}}]
        itk_component add lf {
            iwidgets::Labeledframe $itk_interior.lf -labelpos nw -labeltext "Files"
        } {}
        pack $itk_component(lf)  -anchor nw -expand 1 -fill x

        itk_component add lfdialog {
	    iwidgets::extfileselectionbox [$itk_component(lf) childsite].lfd \
		-width 8i -height 4i -mask "*.v" -fileslabel "Verilog Files"
        } {}
        pack $itk_component(lfdialog)  -anchor nw -expand 1 -fill x

        itk_component add optlabel {
	    iwidgets::labeledwidget [$itk_component(lf) childsite].lab -labeltext "Options"
        }
        pack $itk_component(optlabel)  -anchor nw -expand 1 -fill x

        itk_component add options {
            iwidgets::entryfield  [$itk_component(lf) childsite].opts \
                -width 50 
        }
        #$itk_component(options) insert 0 "+libext+.v -y $::env(BLUESPECDIR)/Verilog"
        pack $itk_component(options)  -anchor nw -expand 1 -fill x
    }

    method loadVLFile {} {
        set file [$itk_component(lfdialog) get]
        set options [$itk_component(options) get]
        if {[catch "::RtlEdit::analyze $file $options" status]} {
            set err "Error during the analyzing verilog file\n$status"
            tk_messageBox -title "HDL Editor Error" -icon error -message $m2 -parent $itk_interior
        } else {
            #set msg "Verilog file analyzed"
            #tk_messageBox -icon info  -type ok -title Message -parent $itk_interior \
            #    -message $msg
	    set pn [winfo parent $itk_component(hull)]
	    $pn refreshHier
	    $this deactivate
        }
    }

    proc create  {pathname args} {
        uplevel #1 ::rtl_editor_loadfile $pathname -modality none $args
    }
}

############################################################
## Code IS NOT Complete nor used...
itcl::class browse_entry_field {
    inherit itk::Archetype
    constructor {} {
        eval itk_initialize $args

        itk_component add fr {
            frame $itk_interior.ff
        } {}
        pack $itk_component(fr)

        itk_component add lr {
            frame $itk_interior.lf
        } {}
        pack $itk_component(lr)

        itk_component add ef {
             iwidgets::entryfield  $itk_component(fr).ef
        } {}
        pack $itk_component(ef) -side left -anchor nw -fill x -expand 1

        itk_component add br {
             button $itk_component(fr).br -text "Browse..." -command "puts Browsing"
        } {}
        pack $itk_component(br) -side left -anchor nw -fill x -expand 0
    }
    destructor {}
}
