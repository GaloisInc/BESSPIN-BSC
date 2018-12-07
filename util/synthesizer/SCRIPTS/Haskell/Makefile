.PHONY: clean

StartBatchAuto: 
	ghc --make -static -o StartBatchAuto StartBatchAuto.hs
	mv StartBatchAuto ..

updateDB: 
	ghc --make -static -o updateDB UpdateBatchDB.hs
	mv updateDB ..

clean: rm *.hi *.o
