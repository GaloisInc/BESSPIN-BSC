setMode -bscan
setCable -port auto
identify
assignFile -p 1 -file vc707.bit
program -p 1
quit
