################################################################################
# preferences that you should feel free to modify on your own
# but you know how this works: keep a back up copy for when you break it :)

# puts "Load user preferences and colors"

# the attributes for the tree view (left panel)
set gTreeColors   {-background grey90 -highlightcolor blue \
                       -selectforeground red}

# the attributes for the text view (right panel)
set gTextColors   {-background grey90 -highlightcolor blue \
                       -foreground black -insertbackground black}

# the attributes for the status view (bottom panel)
set gStatusColors {-background turquoise -insertbackground black -foreground black}

set gTagKeyword   {-foreground brown}       ;# keyword in bsv
set gTagDatatype  {-foreground sienna}      ;# Reg, etc
set gTagLibrary   {-foreground blue}        ;# library type
set gTagComment   {-foreground darkgreen}   ;# comment colors
set gTagModselect {-background red}         ;#
set gTagHilight1  {-background dodgerblue}  ;# when selected
set gTagHilight2  {-background lightblue}   ;# when selected

# this is for bsv/verilog state that has some kind of relevant signals to
#   send to debussy for debugging - I recomment either a highlight or a font
#   since there are colors for everything else
# set gTagWaveable  {-font {times 10 underline italic}}
set gTagWaveable  {-background lightsalmon}

# these are the keys
set gKeyList(help)          "<Control-h>"
set gKeyList(quit)          "<Control-q>"
set gKeyList(opendump)      "<Control-o>"       ;# open up dump file
set gKeyList(search)        "<F2>:<Control-s>"  ;# simple text search
set gKeyList(searchagain)   "<F3>"              ;# search for same thing again
set gKeyList(findsignalinV) "<F6>"              ;# find related signals in v or bsv file
set gKeyList(nextsignal)    "<F7>"              ;# find next related signal
set gKeyList(sendwaves)     "<F10>"             ;# send signals to wave view

################################################################################
# here the user can add special functionality
# to toss out signals on never cares about.
# this function is called for every signal set the to wave form view
# TODO: more documentation
proc ::userSignalMatchFunction {sig} {
    return 1
}

################################################################################
# this is the number of seconds to wait for nWave to come up before
# aborting.   This is set to 30 seconds by default
set gnWaveTimeout 30

################################################################################
# tab stops are every 8 by default
# tabs are turned into spaces locally in .bsv files so that xref works correctly
# things will look screwy if you use a different tab stop
set gTabStops 8

################################################################################
# this will show library elements in hierachy, which is probably more than you
# want, since we can't show you in side of them
# if you want to turn this off change this to "unset gIgnoreLibraryModules"
set gIgnoreLibraryModules 1
