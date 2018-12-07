#  Copyright (C) 1998 John E. Stump
#
#  This software is covered by the Starving Artist Artistic License. Please
#  see the file LICENSE in the toplevel directory of the distribution.
#
#  contact author: iliad@doitnow.com
#       home page: http://www.doitnow.com/~iliad/Tcl/tuba
#
# this is a parser for Tcl code. It parses off a single token at a time. When
# the end of a logical statement is reached, [geteos] is returned. When the end
# of the code is reached, [geteof] is returned.
#
# The parser can be called recursively by just creating a new parser for the
# code block.
#
# Because this is written entirely in Tcl and paws through the code one
# character at a time, it is somewhat slow. Use the C++ version of Tclparser
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
    set pc($new) 0
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
    variable pc
    variable codelen
    variable eos
    
    if { $pc($this) >= $codelen($this) } {
      return [geteof]
    } 
    
    if { $eos($this) } {
      set eos($this) 0
      set tok [geteos]
    } else {
      set tok [begin $this]
    }
    
    global env
    if { [info exists env(DEBUG)] } {
      puts "line: [getlineno $this] token: $tok"
    }

    return $tok
  }
  

  proc begin {this} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos

    while {1} {
      set c [string index $code($this) $pc($this)]
      incr pc($this)
      if { $pc($this) > $codelen($this) } {
        return [geteof]
      }

      switch -exact -- $c {
        \x20 -
        \t {
          # eat leading white space
          continue
        }

        \n {
          # eat leading newlines, after bumping line counter
          incr lineno($this)
          return [geteos]
        }

        ; {
          return [geteos]
        }

        \\ {
          set c [escape $this]
          if { $c == " " || $c == "\\\n" } {
            # whitespace
            continue
          } else {
            set linestart($this) $lineno($this)
            return [begin2 $this $c]
          }
        }

        # {
          set linestart($this) $lineno($this)
          return [comment $this]
        }

        default {
          set linestart($this) $lineno($this)
          return [begin2 $this $c]
        }
      }
    }
  }

  proc begin2 {this c} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos

    set token ""

    while {1} {
      switch -exact -- $c {
        \x20 -
        \t {
          # token terminator
          return $token
        }

        ; {
          set eos($this) 1
          return $token
        }

        \n {
          # statement terminator
          incr lineno($this)
          set eos($this) 1
          return $token
        }

        \x7b {
          append token [brace $this]
        }

        \x5b {
          append token [bracket $this]
        }

        \" {
          if { $token == "" } {
            append token [quote $this]
          } else {
            append token $c
          }
        }

        \\ {
          set c [escape $this]
          if { $c == " " || $c == "\\\n" } {
            # this is a whitespace token terminator
            return $token
          } else {
            append token $c
          }
        }

        default {
          append token $c
        }
      }

      set c [string index $code($this) $pc($this)]
      incr pc($this)
      if { $pc($this) > $codelen($this) } {
        return $token
      }
    }
  }

  proc brace {this} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos


    set token "\x7b"

    while {1} {
      set c [string index $code($this) $pc($this)]
      incr pc($this)
      if { $pc($this) > $codelen($this) } {
        error "premature EOF while inside brace block"
      }

      switch -exact -- $c {
        \x7d {
          append token $c
          return $token
        }

        \x7b {
          append token [brace $this]
        }

        \n {
          incr lineno($this)
          append token $c
        }

        \\ {
          append token [escape $this]
        }

        default {
          append token $c
        }
      }
    }
  }

  proc quote {this} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos

    set token "\""

    while {1} {
      set c [string index $code($this) $pc($this)]
      incr pc($this)
      if { $pc($this) > $codelen($this) } {
        error "premature EOF while inside quote, started somewhere around line $lineno($this)"
      }

      switch -exact -- $c {
        \" {
          append token $c
          return $token
        }

        \x5b {
          append token [bracket $this]
        }

        \n {
          incr lineno($this)
          append token $c
        }

        \\ {
          append token [escape $this]
        }

        default {
          append token $c
        }
      }
    }
  }

  proc bracket {this} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos

    set token "\x5b"

    while {1} {
      set c [string index $code($this) $pc($this)]
      incr pc($this)
      if { $pc($this) > $codelen($this) } {
        error "premature EOF inside bracket"
      }

      switch -exact -- $c {
        \x5d {
          append token $c
          return $token
        }

        \" {
          set len [string length $token]
          incr len -1
          set ws [string index $token $len]
          if { $ws == " " || $ws == "\t" || $ws == "\n" } {
            append token [quote $this]
          } else {
            append token $c
          }
        }

        \x5b {
          append token [bracket $this]
        }

        \x7b {
          append token [brace $this]
        }

        \n {
          incr lineno($this)
          append token $c
        }

        \\ {
          append token [escape $this]
        }

        default {
          append token $c
        }
      }
    }
  }

  proc comment {this} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos

    set token "#"

    while {1} {
      set c [string index $code($this) $pc($this)]
      incr pc($this)
      if { $pc($this) > $codelen($this) } {
        return $token
      }

      switch -exact -- $c {
        \n {
          set eos($this) 1
          incr lineno($this)
          return $token
        }

        \\ {
          append token [escape $this]
        }

        default {
          append token $c
        }
      }
    }
  }

  proc escape {this} {
    variable code
    variable codelen
    variable lineno
    variable linestart
    variable blockchars
    variable pc
    variable eos

    set token "\\"

    set c [string index $code($this) $pc($this)]
    incr pc($this)
    if { $pc($this) > $codelen($this) } {
      # tcl seems to ignore an escape at the end of a file
      return ""
    }

    switch -exact -- $c {
      \x7f {
        # skip over this and assume we have a newline next
        incr pc($this)
        incr lineno($this)
        set token " "
      }
      
      \n {
        incr lineno($this)
        append token "\n"
      }

      default {
        append token $c
      }
    }

    return $token
  }
}

namespace import Tclparser::*

# test code
# set f [open [lindex $argv 0] r]
# set buf [read $f]
# close $f
# 
# parse $buf 1
