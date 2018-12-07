#!/usr/bin/perl -pi~

$_ = "" if (/\.bs\"/);

if ($pwd eq "") {
    $pwd = `pwd`;
    chop $pwd;
}

s@$pwd/@@g;
