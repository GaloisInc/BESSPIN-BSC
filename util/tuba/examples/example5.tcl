# this example tests the argument passing

puts "argv0 is $argv0"
puts "the number of arguments is [llength $argv]"
puts "here they are:"
foreach arg $argv {
  puts $arg
}

