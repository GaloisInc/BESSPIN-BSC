# this example stress tests the parser with some pretty weird yet valid
# Tcl syntax.

# comments test
# more comments test; handle the semi ok?

puts -nonewline "tea> " ; flush stdout

# try to set a break point on the following line. You should get a dialog
proc hello {} {return "hello"}; proc hello2 {} {}
proc there {} {
  set h "something to think about"
  something
  return "there"
}

proc something {} {
  puts "hi"
}

namespace eval ::test {
  variable joe
  variable john "hi"
  proc testing {} {
    puts "this is a test"
  }
}

# The regexp wants to check, if the entry is enclosed in 
# double quotes. Else it should switch to uppercase. 

proc upperIfNotQuoted {value} { 
  if ![regexp "^\[ \t]*\".*\"\[ \t]*$" $value] { 
    return [string toupper $value] 
  } else { 
    return $value 
  } 
} 

::test::testing

puts [upperIfNotQuoted {Hallo}] 
puts [upperIfNotQuoted {"Hallo"}] 

puts "this is 
a test"
set i \
1
set i [
exec ls]

set rer hi
set c ${rer}dsds
puts "[hello][there]"

set a(1) 1
set a(2) 2
set a(3) 3
parray a

set y 8; set u $y
set i 0
if { $i == 0 } {
  puts "this is a test"
} elseif { $i == 1 } then { 
  puts "this is another test" 
} else {
  puts "no more"
  if { $y == 3 } then {
    set i [expr $y + 2]
  } else { set i 0 }
  
  set i 100
}

while { $i > 3 
} {
  incr i -1
  if { $i == 2 } { while {$i} {puts "yep"}}
}

foreach y {1 2 3 4} {
  puts "y is $y"
}

foreach {i j} {1 a 2 b 3 c} {puts "hi there"
puts "$i:$j" }

for {set i 0} {$i < 3} {incr i} {
  puts "i is $i"
}

for {
  set i 0
} {
  $i < 3
} {
  incr i
} \
{
  puts "i is $i"
}

switch $i 1 {puts "1"} 2 {puts "2"}

switch $i {
  1 {
    puts "it's a 1"
  }
  
  2 {
    puts "it's a 2!"
  }
  
  default { puts "I don't know what the hell you're doing!" }
}

catch {unset u}
if { [catch {
  set i p
  set o p
  set u p }] } {
  puts "goll dern"
}

proc testproc {a b} {puts "hello $a $b"}

proc testproc2 {} {
  switch $i {
    1 {
      puts "it's a 1"
    }

    2 {
      puts "it's a 2!"
    }

    default { puts "I don't know what the hell you're doing!" }
  }
}  

if { $i == 0 } {
  puts "this is a test"
}
