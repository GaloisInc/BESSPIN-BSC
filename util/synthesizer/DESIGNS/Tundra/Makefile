.PHONY: default clean

default:
	svn checkout --non-interactive https://svn.bluespec.com:8080/bs/customers/trunk/Tundra/BSV/snap.20060908.noassert/ source
	mv source/*.v .
	mv source/*.bsv .
	rm -rf source
	bsc -u -verilog \
	       -show-schedule -no-remove-unused-modules \
	       -steps 100000 -show-module-use \
	       -no-strict-method-scheduling \
	       i2c_cntrlwp.bsv


clean:
	rm -rf source *.bsv *.v *.bi *.bo *.use
