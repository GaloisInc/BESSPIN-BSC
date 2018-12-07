.PHONY: clean 

mkTopLevel.v: 
	svn co --non-interactive https://svn.bluespec.com:8080/bs/bsc/trunk/testsuite/bsc.bsv_examples/pong src
	mv src/*.bsv .
	mv src/kbscan.v .
	rm -rf src
	cp Ball2.bsv Ball.bsv
	cp Island2.bsv Island.bsv
	cp TopLevel2.bsv TopLevel.bsv
	bsc -u -verilog -O -show-module-use ./TopLevel.bsv

clean:
	rm -rf *.bsv *.bi *.bo *.v *.use src


