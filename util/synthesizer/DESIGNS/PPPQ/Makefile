.PHONY: clean 

mkTopLevel.v: 
	svn co --non-interactive https://svn.bluespec.com:8080/bs/bsc/trunk/testsuite/bsc.bsv_examples/memq src
	mv src/*.bsv .
	rm -rf src
	bsc -u -verilog -O -show-module-use ./DQueue.bsv

clean:
	rm -rf src *.bsv *.bi *.bo *.v *.use


