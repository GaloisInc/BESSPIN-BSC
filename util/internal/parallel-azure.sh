#! /bin/sh
set -x
set -e
#DEBUG=
#DEBUG=--debug=b
MYMAKE=make

$MYMAKE  BSC_BUILD=bsc_ghc6_parallel_default AZUREVERILOG=false AZURESIM=false -j $PARALLEL_AZURE || true
$MYMAKE -C src/lib azure-verilog -j $PARALLEL_AZURE
$MYMAKE -C src/lib BSC_BUILD=bsc_ghc6_parallel_default AZUREVERILOG=true AZURESIM=false -j $PARALLEL_AZURE install || true
$MYMAKE -C src/lib/Libraries -f parallel.mk rm-genVerilog
$MYMAKE -C src/lib azure-sim -j $PARALLEL_AZURE
#for hunting down the files that need recompiling
#$MYMAKE -C src/lib BSC_BUILD=bsc_ghc6_parallel_default AZUREVERILOG=false INORDERVERILOG= REDELETE= AZURESIM=true AZURECONTINUE=false -j $PARALLEL_AZURE install
$MYMAKE BSC_BUILD=bsc_ghc6_parallel_default AZUREVERILOG=false INORDERVERILOG= REDELETE= AZURESIM=true -j $PARALLEL_AZURE

