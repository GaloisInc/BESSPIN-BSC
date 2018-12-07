#
# statetrans.tcl - this defines state transition procs. 
#
#  Copyright (C) 1997,1998 John E. Stump
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#  contact author: iliad@doitnow.com
#       home page: http://www.doitnow.com/~iliad/Tcl/tuba

namespace eval stateTrans {
  # current state
  variable currentState 
  
  # list of all valid states
  variable allStates 
  
  # this array is indexed by (enter_or_leave,state) and the value is a list of
  # listener ids
  variable listeners
  
  # this array maps ids to commands to execute on notify
  variable idmap
  
  # this is for creating the unique ids
  variable id_counter 0
  
  # this little doohickey makes it possible to put an export keyword on
  # the procs below
  proc export {proc name args body} {
    uplevel namespace export $name
    uplevel proc $name [list $args] [list $body]
  }
  
  # called by client to initialize the starting state and to set up list
  # of valid states
  export proc initStateModel {state validstates} {
    variable currentState $state
    variable allStates $validstates
  }
  
  # Adds a listener to an event. The event is either entering or leaving a 
  # particular state. The command is run when the event happens. This proc 
  # returns a unique id that the listener must use for 
  # removeStateTransListener.
  export proc addStateTransListener {enter_or_leave state command} {
    variable listeners
    variable idmap
    variable id_counter
    
    # create the id
    set id stxl_$id_counter
    incr id_counter
    
    # map the command to the id
    set idmap($id) $command
    
    # simplify the enter_or_leave var
    set c [string tolower [string index $enter_or_leave 0]]
    if { $c == "e" } {
      set enter_or_leave E
    } elseif { $c == "l" } {
      set enter_or_leave L
    } else {
      error "Unknown enter_or_leave parameter to addStateTransListener: '$enter_or_leave'"
    }
    
    # register the listener
    if { $state == "*" } {
      foreach state [getValidStates] {
        lappend listeners($enter_or_leave,$state) $id
      }
    } else {
      lappend listeners($enter_or_leave,$state) $id
    }
    
    return $id
  }
  
  # Removes a listener from an event. The id parm was returned by the 
  # addStateTransListener proc.
  export proc removeStateTransListener {id} {
    variable listeners
    variable idmap
    
    if { [info exists idmap($id)] } {
      unset idmap($id)
    }
    
    # do we look for the id in the listeners? It will be slow.
    foreach el [array names listeners] {
      set i [lsearch -exact $listeners($el) $id]
      if { $i != -1 } {
        set listeners($el) [lreplace $listeners($el) $i $i]
      }
    }
  }
  
  # Called by the stateTrans proc to notify all listeners of the specific 
  # event. The commands registered will be invoked at level #0.
  export proc notifyStateTransListeners {enter_or_leave state} {
    variable listeners
    variable idmap
    
    if { [info exists listeners($enter_or_leave,$state)] } {
      foreach id $listeners($enter_or_leave,$state) {
        if { [info exists idmap($id)] } {
          uplevel #0 $idmap($id)
        } else {
          puts "Internal error. Expected id '$id' to be in idmap"
        }
      }
    }
  }
  
  # Called by the internals when the state transitions from one state to 
  # another. This proc will then call notifyStateTransListeners.
  export proc stateTrans {to_state} {
    variable currentState
    variable allStates
    
    # verify this is a valid state to transition to
    if { [lsearch -exact $allStates $to_state] == -1 } {
      error "Trying to transition to invalid state '$to_state'"
    }
    
    # notify any leave state listeners
    notifyStateTransListeners L $currentState
    
    # set the new state
    set currentState $to_state
    
    # notify any enter state listeners
    notifyStateTransListeners E $currentState
  }

  # Returns the current state.
  export proc getState {} {
    variable currentState
    return $currentState
  }

  # returns the list of valid states
  export proc getValidStates {} {
    variable allStates
    return $allStates
  }
  
  # wait for the state to reach the specified value
  export proc waitForState {state} {
    variable currentState
    
    while {1} {
      vwait [namespace current]::currentState
      if { $currentState == $state } break
    }
  }
}

namespace import stateTrans::*
