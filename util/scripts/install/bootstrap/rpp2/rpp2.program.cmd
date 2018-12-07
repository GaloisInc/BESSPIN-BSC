setMode -bscan
setCable -port auto
identify
assignFile -p 1 -file rpp2.bit
program -p 1
quit
