.PHONY: clean 

mkTopLevel.v: 
	svn co --non-interactive https://svn.bluespec.com:8080/bs/customers/trunk/BitBoyz/Version_2005_0622 src
	mv src/*.bsv .
	bsc -u -verilog -show-module-use PP.bsv

clean:
	rm -rf src *.bsv *.bi *.bo *.v *.use


