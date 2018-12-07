#! perl -nlw
#print "$ARGV $.";
if (/^#\s*Description:\s*(.*)/){
    $d=$1;
    $d =~ s/\$/\\\$/g;
#    $d =~ s/\$/\\verb\!\$\!/g;
    $d =~ s,((\w|/)*_(\w|/)*),\\verb+$1+,g;
    if ($ARGV =~ /bootstrap-bluenoc/) {
        $more_args= "{board model name}";
    } else {
        $more_args="";
    }
    if ($ARGV =~ /digilent/) {
        $more_text = "Please be sure that the \\verb+XIL_CSE_PLUGIN_DIR+ variable in your \\texttt{.bashrc} or \\texttt{.cshrc} is set to \\verb+\$HOME/.cse+."
    } else {
        $more_text = "";
    }
    print << "EOF";
$d
\\begin{centerboxverbatim}
./$ARGV $more_args
\\end{centerboxverbatim}

$more_text

EOF
}

