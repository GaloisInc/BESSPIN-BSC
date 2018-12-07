setMode -bscan
setCable -port auto
identify
assignFile -p 1 -file dnv7f2a.bit
program -p 1
quit
