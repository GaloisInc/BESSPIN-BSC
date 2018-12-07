setMode -bscan
setCable -port auto
Identify -inferir
identifyMPM
assignFile -p 3 -file unknown_sram.bsd
assignFile -p 1 -file 10gk7ll.bit
program -p 1
quit
