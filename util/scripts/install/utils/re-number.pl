#! perl -w
open FI,"00-order" or die;
$c=1;
while(<FI>){
    next if /^#/;
    chomp;
    printf("mv %s %02d-%s\n",$_,$c,$_);
    $c++;
}
