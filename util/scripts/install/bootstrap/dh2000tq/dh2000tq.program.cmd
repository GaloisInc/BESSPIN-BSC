setMode -bscan
setCable -port auto
identify
assignFile -p 1 -file dh2000tq.bit
program -p 1
quit
