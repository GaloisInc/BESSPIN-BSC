################################################################################
# preferences that you should feel free to modify on your own
# but you know how this works: keep a back up copy for when you break it :)
################################################################################

################################################################################
###
################################################################################

# the attributes for the tree view (left panel)
set gColorList(treearea)   {-background grey90 -highlightcolor blue -selectforeground red}

# the attributes for the text view (right panel)
set gColorList(textarea)   {-background grey90 -highlightcolor blue -foreground black -insertbackground black}

# the attributes for the status view (bottom panel)
set gColorList(statusarea) {-background turquoise -insertbackground black -foreground black}


################################################################################
### The default colors for keywords and tags
################################################################################
    
set gColorList(tagkeyword)    {-foreground brown}       ;# keyword in bsv
set gColorList(tagdatatype)   {-foreground sienna}      ;# Reg, etc
set gColorList(taglibrary)    {-foreground blue}        ;# library type
set gColorList(tagcomment)    {-foreground darkgreen}   ;# comment colors
set gColorList(tagmodselect)  {-background dodgerblue}         ;#
set gColorList(tagmodselectfg) {-foreground dodgerblue}        ;#
set gColorList(tagselection) {-foreground blue -font  {-family courier -underline 1 -weight bold}}  ; # the current selected item
set gColorList(tagselectionset) {-foreground blue}   ;# the set of selected items

set gColorList(tagwavable)    {-background orange}
set gColorList(tagxref)       {-background yellow}

################################################################################
### The default accelerator keys
################################################################################

set gKeyList(help)                  "<Control-h>"
set gKeyList(quit)                  "<Control-q>"
set gKeyList(opensources)           "<Control-o>"      ;# open/update source files
set gKeyList(textcut)               "<Control-w>"      ;# cut text
set gKeyList(textcopy)              "<Meta-w>"         ;# copy text
set gKeyList(textpaste)             "<Control-y>"      ;# paste text
set gKeyList(search)                "<Control-s>"      ;# simple text search
set gKeyList(rotateselections)      "<F3>"             ;# rotate through current selections
set gKeyList(clearselections)       "<Control-g>"      ;# clear through current selections
set gKeyList(findrelatedxrefs)      "<F6>"             ;# find related locations/signals in verilog or bsv file
set gKeyList(sendwaves)             "<F10>"            ;# send signals to wave view
set gKeyList(treeJumpToBSVDef)      "B"                ;# send signals to wave view
set gKeyList(treeJumpToBSVInst)     "b"                ;# send signals to wave view
set gKeyList(treeJumpToVerilogDef)  "V"                ;# jump to verilog definition
set gKeyList(treeJumpToVerilogInst) "v"                ;# jump to verilog instantiation


################################################################################
# here the user can add special functionality
# to toss out signals one never cares about.
# this function is called for every signal set the to wave form view
# TODO: more documentation
################################################################################

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
