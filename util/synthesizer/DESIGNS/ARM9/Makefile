.PHONY: clean 

mkTopLevel.v: 
	svn co --non-interactive https://svn.bluespec.com:8080/bs/customers/trunk/ARM/2004_09_10_arm9ebsv src
	mv src/*.bsv .
	rm -rf src
	bsc -u -verilog -show-module-use ARM9Core.bsv


clean:
	rm -rf src *.bsv *.bi *.bo *.v *.use


