#! perl -w
die unless @ARGV==2;
die unless defined($RPM_NAME=$ARGV[0]);
die unless defined($VERSION=$ARGV[1]);
print << "EOF";
\%define module $RPM_NAME
\%define version $VERSION
EOF
    ;
print << 'EOF';
Summary: Packages not distributed by Redhat upon which Bluespec depends
Name: %{module}
Version: %{version}
Release: el6
Vendor: Bluespec
License: Commercial
Group: Applications/Engineering
Packager: Bluespec <support@bluespec.com>
BuildArch: noarch
Requires: fxload libftdi-devel libftdi gtkwave
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root/
Source: empty.tar.gz

%description
This empty package exists only for its dependencies.  The dependencies
are the packages needed by the Bluespec tools which are not
distributed by Redhat.  Popular places to get these packages include
EPEL and Atrpms.

%prep

%clean
if [ "$RPM_BUILD_ROOT" != "/" ]; then
true
	rm -rf $RPM_BUILD_ROOT
fi

%files


EOF
    ;
