#  Copyright Bluespec Inc. 2009-2012

# Tcl/Tk package for building a graphical interface to the VCD Player

namespace eval ::VCDGui {

    package require BSDebug
    package require Waves 2.0
    package require types
    package require Iwidgets 4.0
    package require ProbeGui 1.0
    package require Bluetcl

    variable curIn 1.0
    variable lastIn 1.0
    variable cycle 1.0
    variable nextIn 1.0
    
    #Buttons for the wave viewer
    variable wave_button
    variable wave_reload_button
    variable vcd_file
    variable updatetime 1000
    variable Viewer ""
    variable vcd_button
    variable vcdtext
    

    fonts::set_colours
    fonts::initialize


    proc mkSource {tl} {
        
        set source_frame [ttk::labelframe $tl.source_frame -text "Input Source" ]
        
        set file_dia [options_dialog::create .filesel]

        # set man_radio [tk::radiobutton $source_frame.man_radio -text "Manual" -background gray85]
        # set vcd_radio [tk::radiobutton $source_frame.vcd_radio -text "VCD Player" -background gray85]
        # set capi_radio [tk::radiobutton $source_frame.capi_radio -text "C API" -background gray85]
        
        set input manual
        set man_radio [ttk::radiobutton $source_frame.man_radio -text "Manual" -variable input -value manual ]
        set vcd_radio [ttk::radiobutton $source_frame.vcd_radio -text "VCD Player" -variable input -value vcd]
        set capi_radio [ttk::radiobutton $source_frame.capi_radio -text "C API" -variable input -value capi]
                      
        set api_field [iwidgets::entryfield $source_frame.api_field -labeltext "Input File" -background gray85 -width 50]
        set browse_button [ttk::button $source_frame.browse -text "Browse..." -command  "VCDGui::mkOpenFile $file_dia $api_field"]

         
        grid $man_radio -row 1 -sticky w
        grid $vcd_radio -row 2 -sticky w
        grid $capi_radio  $api_field $browse_button -row 3 -sticky w
            
        return $source_frame
    }


    proc mkOpenFile {file_dia api_field} {
        if {[$file_dia activate]} {
            $api_field insert 0 [$file_dia getSelection]
        } else {
            $api_field insert 0 ""
        }
    }

    
    proc mkVCDFrame {tl} {

        variable vcdframe
        variable vcdouttext
        variable vcdtext 

        #Define VCD Player Text Box
        set vcd_frame [ttk::panedwindow $tl.vcd_frame -orient horizontal]
        set vcd_inframe [ttk::frame $vcd_frame.vcd_inframe ]
        set vcd_outframe [ttk::frame $vcd_frame.vcd_outframe ]

        $vcd_frame add $vcd_inframe
        $vcd_frame add $vcd_outframe

        #Define VCD Player Input Box
        set vcd_inframetext [iwidgets::scrolledtext $vcd_inframe.vcd_inframetext -labeltext "VCD Stimulus" \
                           -wrap none -vscrollmode dynamic -hscrollmode dynamic]
        pack $vcd_inframetext -expand no -fill x  -anchor c
     
        set vcdintext [$vcd_inframetext childsite]
        set vcdstimfile [open "./stimulus.dat" "r"]
        set data [read $vcdstimfile]
        $vcd_inframetext insert 0.0 $data

        #Define VCD Player Output Box
        set vcd_outframetext [iwidgets::scrolledtext $vcd_outframe.vcd_outframetext -labeltext "VCD Output" \
                          -wrap none -vscrollmode static -hscrollmode dynamic]
        pack $vcd_outframetext -expand no -fill x -anchor c
        set vcdouttext [$vcd_outframetext childsite]
        # set vcdstimfile [open "./stimulus.dat" "r"]
        #set data [read $vcdstimfile]
        $vcd_outframetext insert 0.0 $data
        
#        pack $vcd_frame -fill both

        return $vcd_frame 

  } 

    proc mkButtons {tl} {

        #Define Button Frame
        set button_frame [ttk::labelframe $tl.button_frame -text "Control"]
        set vcd_button_frame [ttk::labelframe $button_frame.vcd_button_frame -text "VCD Player"]
        set wave_button_frame [ttk::labelframe $button_frame.wave_button_frame -text "Wave Viewer" ]

        #set next_button [ttk::button $vcd_button_frame.next_button -text "Next" -command "VCDGui::highlightIn $vcdintext"]
        set next_button [ttk::button $vcd_button_frame.next_button -text "Next" ]
        set run_button [ttk::button $vcd_button_frame.run_button -text "Run"]
	set wave_button [ttk::button $wave_button_frame.wave -text "Wave Viewer" -command "VCDGui::start_viewer"]
	set wave_reload_button [ttk::button $wave_button_frame.wave_reload -text "Reload VCD" -command "ProbeGui::reload_vcd"]

        pack $wave_button -side left -padx 15 -pady 15
        pack $wave_reload_button -side left -padx 15 -pady 15
        pack $next_button -side left -padx 15 -pady 15
        pack $run_button -side left -padx 15 -pady 15

        pack $wave_button_frame  $vcd_button_frame -side left -padx 15 -fill y 
#        pack $vcd_button_frame   -padx 15 -fill y -side right

	set vcd_file [bsdebug::probe query_vcdfile]
        $wave_button state !disabled

        return $button_frame 
}
        

    proc highlightIn {textin} {

        variable curIn 
        variable lastIn
        variable cycle
        variable nextIn
        
        set curIn [$textin search Input $curIn end]

        if { $curIn == ""} {
                set curIn 1.0
            }

        set cycle [$textin search -regexp {[0-9]+:} $curIn end]
        set clockend [$textin search : $curIn end]
        set clockvalue [$textin get $cycle $clockend]

        puts "clockvalue: $clockvalue"        
        set newclockvalue $clockvalue
        
        set nextIn $curIn

        while {$newclockvalue <= $clockvalue} {
         
            set nextIn [$textin index "$nextIn + 1l"]

            set newclockvalue  [$textin get [$textin search -regexp {[0-9]+:} $nextIn end] [$textin search : $nextIn end] ]
            
            if  { $curIn == 1 } {
                $textin tag add highlight1 $curIn $nextIn 
            }
            if { $curIn > 1 } {
                $textin tag add highlight1 $curIn $nextIn
                $textin tag delete $lastIn $curIn
                $textin tag add removehighlight $lastIn $curIn
            } 

            puts "curIn: $curIn"
            puts "nextIn: $nextIn"
            puts "lastIn: $lastIn"
        }

        $textin tag configure highlight1 -background yellow -relief raised
        $textin tag configure removehighlight -background white -relief raised
        $textin tag raise removehighlight
        
        set lastIn [$textin index $curIn]
        set curIn  $nextIn

    }

    proc start_viewer {} {
        variable Send_button
        variable wave_button
        variable vcd_file
        variable Viewer

        $wave_button state disabled
        bsdebug::probe query_timestamp
        set Viewer [Waves::create_viewer]
        $Viewer configure -nonbsv_hierarchy ""
	$Viewer start
	$Viewer load_dump_file $vcd_file
        $wave_button state disabled

    }

    proc reload_vcd {} {
        variable Viewer

	puts "Reloading vcd"
        bsdebug::probe query_timestamp
	$Viewer reload_dump_file
	puts "Done reloading"
    }


}

package provide VCDGui 1.0

#catch "itcl::delete class options_dialog"
itcl::class options_dialog {
    inherit iwidgets::Dialog
    ##
    constructor {args} {

        lappend args -modality application -title "Input File" \
                                                        -height 350 -width 400

        # construct the dialog
        eval itk_initialize $args

       set types [list {{C-API Files} {.c}}  {{All files} {*}}]
        itk_component add lf {
            iwidgets::Labeledframe $itk_interior.lf -labelpos nw -labeltext "Files"
        } {}
        pack $itk_component(lf)  -anchor nw -expand 1 -fill x

        itk_component add lfdialog {
	    iwidgets::extfileselectionbox [$itk_component(lf) childsite].lfd \
		-width 8i -height 4i -mask "*.*" -fileslabel "C Files"
        } {}
        
        $itk_component(hull)  hide Apply 
        $itk_component(hull)  hide Help
        pack $itk_component(lfdialog)  -anchor nw -expand 1 -fill x

    }
    destructor {}

    method getSelection {} {
        set selection [$itk_component(lfdialog) get]
        puts "selection: $selection"
        return $selection
    }

    proc create {pathname args} {
        uplevel #0 options_dialog $pathname -modality none $args
    }

}



