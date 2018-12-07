# this is a test of debugging the source command

set nothing {}

proc donothing {} {
  puts "hello from script [info script]"
}
 
puts "coming from [info script]"
