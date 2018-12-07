#  Copyright (C) 1997, 1998 John E. Stump
#
#  This software is covered by the Starving Artist Artistic License. Please
#  see the file LICENSE in the toplevel directory of the distribution.
#
#  contact author: iliad@doitnow.com
#       home page: http://www.doitnow.com/~iliad/Tcl/tuba
#
# this is a parser for Tcl code. It parses off a single token at a time. When
# the end of a logical statement is reached, "EOS" is returned. When the end
# of the code is reached, "EOF" is returned. (But you should get the values for
# these two via the geteos and geteof procs and not the string directly.)
#
# The parser can be called recursively by just creating a new parser for the
# code block.
#
# Because this is written entirely in Tcl and paws through the code one
# character at a time, it is pretty slow. Use the C++ version of Tclparser
# (same interface) instead.


namespace eval Tclparser {
  # running sequence number for creating parser "objects"
  variable seq 0
  
  # array for holding the code for each "object"
  variable code
  
  # array for holding the length of the code
  variable codelen
  
  # array for holding the line numbers for each "object"
  variable lineno
  variable linestart
  
  # array for holding the block start and end characters for each "object"
  variable blockchars
  
  # array for holding the current program code counter for each "object"
  variable pc
  
  # array of eos flags for each object
  variable eos
  
  namespace export new_Tclparser
  proc new_Tclparser {code_buf start_lineno} {
    variable seq
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos
    
    set new $seq
    incr seq
    
    # any blocking here?
    set c [string index $code_buf 0]
    switch $c {
      \x7b -
      \x5b -
      \" {
        set len [string length $code_buf]
        incr len -1
        # get the end character
        set e [string index $code_buf $len]
        
        # save the block begin and block end characters
        set blockchars($new) [list $c $e]
        
        # save the code inside the block
        incr len -1
        set code($new) [string range $code_buf 1 $len]
      }
      
      default {
        set blockchars($new) [list " " " " ]
        set code($new) $code_buf
      }
    }
    
    set codelen($new) [string length $code($new)]
    set pc($new) -1
    set lineno($new) $start_lineno
    set linestart($new) $start_lineno
    set eos($new) 0
    
    # create the access proc for this object
    proc ::$new {method args} "uplevel [namespace current]::\$method $new \$args"
    
    return $new
  }

  namespace export delete_Tclparser
  proc delete_Tclparser {parser} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos
    
    if { [info procs ::$parser] != "::$parser" } {
      return
    }
    
    unset code($parser)
    unset codelen($parser)
    unset lineno($parser)
    unset linestart($parser)
    unset blockchars($parser)
    unset pc($parser)
    unset eos($parser)
    
    rename ::$parser {}
  }
  
  proc getlineno {this} {
    variable linestart
    return $linestart($this)
  }
  
  proc getBlockStart {this} {
    variable blockchars
    return [lindex $blockchars($this) 0]
  }
  
  proc getBlockEnd {this} {
    variable blockchars
    return [lindex $blockchars($this) 1]
  }
  
  namespace export geteos
  proc geteos {} {
    return "\x01EOS\x01"
  }
  namespace export geteof
  proc geteof {} {
    return "\x01EOF\x01"
  }
  
  proc gettok {this} {
    set tok [gettok_internal $this]
    global env
    if { [info exists env(DEBUG)] } {
      puts "line: [getlineno $this] token: $tok"
    }
    return $tok
  }
  
  proc gettok_internal {this} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos
    
    set inbrace 0
    set inbrack 0
    set inescape 0
    
    # NEW
    # set the current token delimiter to nothing initially
    # possible values for tokdel:
    # ws - space, tab, or newline
    # nl - newline only (for comments)
    # qt - quote
    # br - brace (the curly kind)
    # sq - square bracket
    set tokdel ""
    
    set token ""
    
    # set up the brack level context array
    set brackcontext(0) ""
    set inquote 0
    
    if { $pc($this) >= $codelen($this) } {
      return [geteof]
    } elseif { $eos($this) != 1 } {
      incr pc($this)
    }
    
    if { $eos($this) } {
      set eos($this) 0
      return [geteos]
    }

    set linestart($this) $lineno($this)
    
    # NEW
#     # eat leading whitespace
#     for {set c [string index $code($this) $pc($this)]} \
#         {$c == " " || $c == "\t"} \
#         {incr pc($this); set c [string index $code($this) $pc($this)]} {
#     }

    set eatws 1
    
    # now keep reading characters until we find something interesting
    for {} {$pc($this) < $codelen($this)} {incr pc($this)} {
      set c [string index $code($this) $pc($this)]
      
      # NEW
      # swallow any leading whitespace
      if { $eatws && ( $c == " " || $c == "\t" ) } continue
      
      if { $c == "\x7f" && $inescape == 1 } {
        # this is our special way of encoding escaped newlines so they are
        # not lost during RT instrumenting
        continue
      }
      
      if { $inescape } {
        set inescape 0

        if { $c == "\n" } {
          incr lineno($this)
          
          if { $eatws } {
            set tokdel ""
            set linestart($this) $lineno($this)
            # eat it because it's just whitespace
            continue
          }
          
          # if we're waiting for a space, this is it
          if { $tokdel == "ws" } {
            return $token
          }
          
          # NEW
          # if we haven't specified a token yet, then ignore the newline
          # and eat leading whitespace
          if { $tokdel == "" } {
            set eatws 1
            set linestart($this) $lineno($this)
            # need to reset the token delimiter
            set tokdel ""
            
            # fix for defect #13; need to turn these things into spaces
            append token " "
          } else {
            append token "\\$c"
          }
        } else {
          append token "\\$c"
          set eatws 0
        }
        continue
      }
      
      if { $c == "\\" } {
        set inescape 1
        # if we haven't set the token delimiter yet, then it is a whitespace
        if { $tokdel == "" } { set tokdel ws }
        continue
      }

      # clear the flag
      set eatws 0
      
      if { $tokdel == "nl" && $c != "\n" } {
        append token $c
        continue
      }
      
      switch -- $c {
        \# {
          if { $token == "" } {
            # NEW
            # only a newline (nonescaped) can delimit a comment
            set tokdel "nl"
          }
          append token $c
        }
        
        \; -
        \n {
          if { $c == "\n" } {
            incr lineno($this)
          }
          
          # NEW
          # if there is no token delimiter set yet, then this is an EOS
          if { $tokdel == "" } {
            return [geteos]
          }
          
          # if whitespace is a token delimiter, then we've reached the end
          # of a token
          if { $tokdel == "ws" || ( $tokdel == "nl" && $c == "\n" ) } {
            if { $token != "" } {
              set eos($this) 1
              return $token
            } else {
              return [geteos]
            }
          } else {
            append token $c
          }
        }
        
        \t -
        \x20 {
          # NEW
          # if whitespace is a token delimiter, then we've reached the end
          # of a token
          if { $tokdel == "ws" } {
            return $token
          } else {
            # NEW (got rid of the elseif)
            append token $c
          }
        }
        
        \" {
          append token $c
          set inquote [expr {1 - $inquote}]
          
          if { $tokdel == "" } {
            set tokdel qt
          } elseif { $tokdel == "qt" } {
            return $token
          } else {
            # don't have to do anything
          }
        }
        
        \x7b {
          append token $c
          # NEW
#           set inws 0
          if { $tokdel == "" } {
            set tokdel "br"
          }
          if { $tokdel == "br" } {
            incr inbrace
          }
        }
        
        \x7d {
          append token $c
          # NEW
          if { $tokdel == "" } {
            set tokdel ws
          }
          
          if { $tokdel == "br" } {
            incr inbrace -1
            # this assumes a correct Tcl syntax
            if { $inbrace == 0 } {
              return $token
            }
          }
        }
        
        \x5b {
          append token $c
          # NEW
          # if the token delimiter hasn't been set, then set it 
          if { $tokdel == "" || $tokdel == "ws" } {
            set tokdel sq
          }
          
          # if we're not in a brace token, increment the bracket level
          if { $tokdel != "br" } {
            incr inbrack
            set brackcontext($inbrack) $inquote
          }
#           set inws 0
        }
        
        \x5d {
          append token $c
          # NEW
          if { $tokdel == "" } {
            set tokdel ws
          } 
          
          # if we we're not in a brace token, decrement the bracket level
          if { $tokdel != "br" } {
            if { $brackcontext($inbrack) == $inquote } {
              incr inbrack -1
            }
            if { $inbrack < 0 } {
              set inbrack 0
            }
          }
          
          # if we're terminating the bracket, reset the delimiter back to ws
          if { $tokdel == "sq" && $inbrack == 0 } {
            set tokdel ws
          }
        }
        
        default {
          append token $c
          # NEW
          if { $tokdel == "" } {
            set tokdel ws
          }
#           set inws 0
        }
      }
    }
    
    if { $token == "" } {
      return [geteof]
    } else {
      return $token
    }
  }
}

namespace import Tclparser::*
