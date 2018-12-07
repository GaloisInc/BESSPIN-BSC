
# this is a normal proc
proc normal {} {
  puts "hello, I'm normal"
}

# dyna proc case 1
set name john
proc $name {} {
  puts "hi, I'm not normal"
}

# dyna proc case 2
proc joe {} "
  puts \"hi, I'm $name\"
"

normal
john
joe
