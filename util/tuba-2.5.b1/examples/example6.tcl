#!/bin/sh
#
# The next line restarts with tclsh \
exec tclsh "$0" "$@"
# so I can't put a } here without screwing things up?

proc test {} {
  set a "hi"
  # here's the error
  parray a
  puts "hello from test"
  return "nothing"
  puts "you shouldn't see this!"
}

# this first time, because we have catch around the error, tuba won't intercept
# it
if { [set r [catch {test} rc]] != 0 } {
  puts "error code is '$r'"
  puts "caught the error '$rc'"
} else {
  puts "test returned '$rc'"
}

# this time choose to ignore the error
test
puts "I bet you ignored the error"

# and now abort the application
test
puts "if you see this you've erred"
