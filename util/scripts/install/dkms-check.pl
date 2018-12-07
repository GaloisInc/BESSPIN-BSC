#! perl -wl
$version_file="version.dat";
{
local $/=undef; # note this affects chomp
open V,$version_file or die "cannot open: $version_file";
$version=<V>;
close V;
}
chomp$version;
chomp($release=`uname -r`);
chomp($machine=`uname -m`);
$happy=0;
while(<>){  #this script expects the output of 'dkms status'
    next unless ($gotversion, $gotrelease, $gotmachine, $extra) = /^bluespec-bluenoc, (\S+), (\S+), (\S+): (.*)/;
    if ($version eq $gotversion and $release eq $gotrelease and $machine eq $gotmachine and 
        ($extra eq "installed" or $extra =~ /^installed-weak from/)) {
        $happy=1;
    }
}
if ($happy){
    exit 0;
} else {
    exit 1;
}
