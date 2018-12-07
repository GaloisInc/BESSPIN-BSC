default:
	svn cat --non-interactive https://svn.bluespec.com:8080/bs/bsc/trunk/testsuite/bsc.interra/rtl_quality/booth_multiplier/Design.bsv > Design.bsv

	bsc -verilog -g mkDesign Design.bsv

clean:
	rm -f *.bsv *.bi *.bo *.v *.use
	rm -rf source
