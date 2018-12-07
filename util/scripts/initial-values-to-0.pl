#!/usr/bin/perl -pi~

# :SLA: convert all initial values from AAAAAAA to 0  which is slightly nicer for fpgas
# of course you should still have a reset test plan!

$inside = 1 if (/ifdef BSV_NO_INITIAL_BLOCKS/);
$inside = 0 if (/endif \/\/ BSV_NO_INITIAL_BLOCKS/);

if ($inside == 1) {
    s/([A20]+)\;/0\;/;
}
