#! perl -plw
BEGIN{
$WE='http://dl.fedoraproject.org/pub/epel';
$W6='6/x86_64';
$W5='5/x86_64';
$E5='epel-release-5-4.noarch.rpm';
$E6='epel-release-6-8.noarch.rpm';
(system 'wget', ('-S','--spider', $WE.'/'.$W6.'/'.$E6))==0 or die;
(system 'wget', ('-S','--spider', $WE.'/'.$W5.'/'.$E5))==0 or die;

$WA='http://dl.atrpms.net/all';
$A6='atrpms-repo-6-7.el6.x86_64.rpm';
$A5='atrpms-repo-5-7.el5.x86_64.rpm';
$wa6=$WA.'/'.$A6;
(system 'wget', ('-S','--spider', $wa6))==0 or die;
(system 'wget', ('-S','--spider', $WA.'/'.$A5))==0 or die;
(system 'wget', ('-O','atrpm-newkey','http://packages.atrpms.net/RPM-GPG-KEY.atrpms'))==0 or die;

$cmd='cmp atrpm-newkey keys/RPM-GPG-KEY.atrpms 1>&2';
print STDERR "+ $cmd";
(system $cmd)==0 or die;
}
s!__WA__!$WA!g;
s!__WE__!$WE!g;
s!__W6__!$W6!g;
s!__W5__!$W5!g;

s!__A6__!$A6!g;
s!__E6__!$E6!g;
s!__A5__!$A5!g;
s!__E5__!$E5!g;
