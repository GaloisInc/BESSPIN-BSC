#! perl -wl
while(<*.sh>){
    if (/\d\d-(.*\.sh)/){
        print "mv $_ $1";
    }
}
