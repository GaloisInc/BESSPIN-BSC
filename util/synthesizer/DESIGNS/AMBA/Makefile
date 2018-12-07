default:
	svn checkout --non-interactive https://svn.bluespec.com:8080/bs/bsc/trunk/testsuite/bsc.bsv_examples/AmbaAdapters/ source
	mv source/*.bsv .
	rm -rf source
	bsc -u -verilog -show-module-use ./DMA.bsv

clean:
	rm -rf *.bsv *.bi *.bo *.v *.use source
