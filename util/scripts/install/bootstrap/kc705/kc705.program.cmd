setMode -bscan
setCable -port auto
identify
assignFile -p 1 -file kc705.bit
program -p 1
quit
