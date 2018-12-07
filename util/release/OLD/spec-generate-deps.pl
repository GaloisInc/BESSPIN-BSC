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
Summary: Packages distributed by Redhat upon which Bluespec depends
Name: %{module}
Version: %{version}
Release: el6
Vendor: Bluespec
License: Commercial
Group: Applications/Engineering
Packager: Bluespec <support@bluespec.com>
BuildArch: noarch
Requires: gmp gcc-c++ libX11 fontconfig libXft python libusb-devel xterm zlib-devel glibc-devel pciutils
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root/
Source: empty.tar.gz

%description
This empty package exists only for its dependencies.  The dependencies
are the Redhat-distributed packages needed by the Bluespec tools.

%prep

%clean
if [ "$RPM_BUILD_ROOT" != "/" ]; then
true
	rm -rf $RPM_BUILD_ROOT
fi

%files


EOF
    ;
