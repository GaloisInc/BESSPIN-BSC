#! perl -wl
die "need VERSION" unless defined($version=$ARGV[0]);

#only a halfhearted attempt to escape special characters in version strings, which you should not be doing anyway.
$version =~ s/(["'\\])/\\$1/g;
while(<STDIN>){
    chomp;
    if ($_ eq '// release script sets MODULE_VERSION.  Do not delete this comment.') {
        $_=qq{MODULE_VERSION ("$version");};
        $changed=1;
    }
    print;
}
die "unable to find MODULE_VERSION comment line" unless $changed;
