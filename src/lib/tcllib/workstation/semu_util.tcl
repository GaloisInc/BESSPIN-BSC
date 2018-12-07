package require Itk
package require Iwidgets


namespace eval base {
    catch "itcl::delete class base::buttonfield"
    itcl::class buttonfield {
        inherit itk::Widget
    
        constructor {args} {
            eval itk_initialize $args
            
            itk_component add button {
                ttk::checkbutton $itk_interior.button
            } {}

            itk_component add field {
                ttk::entry $itk_interior.field 
            } {}

            pack $itk_component(button) -side left
            pack  $itk_component(field) -side left
           
        }
        
        method setVariables {varname setvalue comvar tv} {
	    
            $itk_component(button) configure -variable $varname
            $itk_component(button) configure -onvalue $setvalue\__ON
            $itk_component(button) configure -offvalue $setvalue\__OFF
            $itk_component(button) configure -command $comvar
            $itk_component(field) configure -textvariable  $tv
        }

        
        method getField {} {
            $itk_component(field) get
        }

        method lockField {} {
            $itk_component(field) configure -state readonly
        }

        method unlockField {} {
            $itk_component(field) configure -state normal
        }

        # method setValue {newValue} {
        #     $itk_component(button) configure -value $newValue
        # }

        # method getCget {var} {
        #     $itk_component(button) cget -$var
        # }
        
        method getState {} {
             $itk_component(button) cget -state
	}

        # method select {} {
        #     $itk_component(button) select
        # }

	# method invoke {} {
	#    $itk_component(button) invoke
	# }
        
       
    }


    ################################################################################
    ### Create 16-bit code for a 4-input LUT corresponding to the "brk_expr".
    ### 4 input terms are: A, B, C, D
    ################################################################################

    proc create_lut_code {brk_expr} {

	regsub  -all {A} $brk_expr "\[set t0\]" brk_expr
	regsub  -all {B} $brk_expr "\[set t1\]" brk_expr
	regsub  -all {C} $brk_expr "\[set t2\]" brk_expr
	regsub  -all {D} $brk_expr "\[set t3\]" brk_expr

	set t0 0
	set t1 0
	set t2 0
	set t3 0

	set brk_expr "set x \[expr $brk_expr \]"
	##    puts "EXPR: $brk_expr"

	set code ""
	
	set res [catch $brk_expr msg]

	if {$res} {
	    error "ERROR: $msg"
	}
	
	for {set t3 0} {$t3 < 2} {incr t3} {
	    for {set t2 0} {$t2 < 2} {incr t2} {
		for {set t1 0} {$t1 < 2} {incr t1} {
		    for {set t0 0} {$t0 < 2} {incr t0} {
			set res [catch $brk_expr value]
			if {$x == true}  {set x 1}
			if {$x == false} {set x 0}
			set code "$x$code"
			##		    puts "$t3 $t2 $t1 $t0 VALUE: $x"
		    }
		}
	    }
	}

	return $code
    }

    proc is_valid_expr {brk_expr {do_error false} {verbose false}} {

	set check $brk_expr
	regsub  -all {\|\|}  $check "" check
	regsub  -all {&&}    $check "" check
	regsub  -all {A}     $check "" check
	regsub  -all {B}     $check "" check
	regsub  -all {C}     $check "" check
	regsub  -all {D}     $check "" check
	regsub  -all {==}    $check "" check
	regsub  -all {!=}    $check "" check
	regsub  -all {!}     $check "" check
	regsub  -all { }     $check "" check
	regsub  -all {\^}    $check "" check
	regsub  -all {~}     $check "" check
	regsub  -all {true}  $check "" check
	regsub  -all {false} $check "" check
	regsub  -all {\(}    $check "" check
	regsub  -all {\)}    $check "" check

	if {[string length $check] != 0 } {
	    if {$verbose} {
		puts "ERROR: Use of unsupported syntax in expression '$brk_expr'"
	    }
	    if {$do_error} {
		error "ERROR: Use of unsupported syntax in expression '$brk_expr'"
	    }
	    return false
	}

	set bb $brk_expr
	regsub  -all {A} $bb "\[set t0\]" bb
	regsub  -all {B} $bb "\[set t1\]" bb
	regsub  -all {C} $bb "\[set t2\]" bb
	regsub  -all {D} $bb "\[set t3\]" bb
	regsub  -all {\]\[} $bb "\] \[" bb

	set t0 0
	set t1 0
	set t2 0
	set t3 0

	set bb "set x \[expr $bb \]"

	set res [catch $bb msg]

	if {$res} {
	    if {$verbose} {
		puts "ERROR: $msg"
	    }
	    if {$do_error} {
		error "ERROR: $msg"
	    } 
	    return false
	}

	return true
    }

    ################################################################################
    ###
    ################################################################################

    proc setTooltip {widget text} {
        if { $text != "" } {
	    # 2) Adjusted timings and added key and button bindings. These seem to
	    # make artifacts tolerably rare.
	    set delta 1000
	    bind $widget <Any-Enter>    [list after $delta [list base::showTooltip %W $text]]
	    bind $widget <Any-Leave>    [list after $delta [list destroy %W.tooltip]]
	    bind $widget <Any-KeyPress> [list after $delta [list destroy %W.tooltip]]
	    bind $widget <Any-Button>   [list destroy %W.tooltip]
        }
    }
    proc showTooltip {widget text} {
        global tcl_platform
        if { [string match $widget* [winfo containing  [winfo pointerx .] [winfo pointery .]] ] == 0  } {
	    return
        }
	
        catch { destroy $widget.tooltip }
	
        set scrh [winfo screenheight $widget]    ; # 1) flashing window fix
        set scrw [winfo screenwidth $widget]     ; # 1) flashing window fix
        set tooltip [toplevel $widget.tooltip -bd 1 -bg black]
        wm geometry $tooltip +$scrh+$scrw        ; # 1) flashing window fix
        wm overrideredirect $tooltip 1
	
        if {$tcl_platform(platform) == {windows}} { ; # 3) wm attributes...
	    wm attributes $tooltip -topmost 1   ; # 3) assumes...
        }                                           ; # 3) Windows
        pack [label $tooltip.label -bg lightyellow -fg black -text $text -justify left]
	
        set width [winfo reqwidth $tooltip.label]
        set height [winfo reqheight $tooltip.label]
	
        set pointer_below_midline [expr [winfo pointery .] > [expr [winfo screenheight .] / 2.0]]                ; # b.) Is the pointer in the bottom half of the screen?
	
        set positionX [expr [winfo pointerx .] - round($width / 2.0)]    ; # c.) Tooltip is centred horizontally on pointer.
        set positionY [expr [winfo pointery .] + 35 * ($pointer_below_midline * -2 + 1) - round($height / 2.0)]  ; # b.) Tooltip is displayed above or below depending on pointer Y position.
	
        # a.) Ad-hockery: Set positionX so the entire tooltip widget will be displayed.
        # c.) Simplified slightly and modified to handle horizontally-centred tooltips and the left screen edge.
        if  {[expr $positionX + $width] > [winfo screenwidth .]} {
	    set positionX [expr [winfo screenwidth .] - $width]
        } elseif {$positionX < 0} {
	    set positionX 0
        }
	
        wm geometry $tooltip [join  "$width x $height + $positionX + $positionY" {}]
        raise $tooltip
	
        # 2) Kludge: defeat rare artifact by passing mouse over a tooltip to destroy it.
        bind $widget.tooltip <Any-Enter> {destroy %W}
        bind $widget.tooltip <Any-Leave> {destroy %W}
    }
}



