default:
	mkdir -p source
	svn checkout --non-interactive https://svn.bluespec.com:8080/bs/customers/trunk/ADI_Wilmington/DMA/russ_20050816/ver2 source
	cp source/*.bsv .
	rm -rf source
	bsc -u -verilog -aggressive-conditions -show-module-use DMA.bsv

clean:
	rm -f *.bsv *.bi *.bo *.v *.use
	rm -rf source
