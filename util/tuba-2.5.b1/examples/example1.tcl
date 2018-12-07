
proc call1 {} {
  call2 hi
}
  
proc call2 {msg} {
  call3 $msg "there"
}
  
proc call3 {a b} {
  call4
}
  
proc call4 {} {
}

call1
