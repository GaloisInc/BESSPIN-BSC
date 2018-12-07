#! perl -w
die unless @ARGV==2;
die unless defined($RPM_NAME=$ARGV[0]);
die unless defined($VERSION=$ARGV[1]);
# This is adapted from sample.spec in the DKMS documentation.
print << "EOF";
\%define module $RPM_NAME
\%define version $VERSION
EOF
    ;
print << 'EOF';
Summary: Bluespec Bluenoc module DKMS package
Name: %{module}
Version: %{version}
Release: 2dkms
Vendor: Bluespec
License: Commercial
Group: Applications/Engineering
Packager: Bluespec <support@bluespec.com>
BuildArch: noarch
# Perl is required by kernel driven building
Requires: make, dkms >= 1.00, udev, perl
# There is no Source# line for dkms.conf since it has been placed
# into the source tarball of SOURCE0
Source0: %{module}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root/

%description
This package contains the Bluespec Bluenoc module wrapped for
the DKMS framework.

%prep
%setup

%install
echo RPM_BUILD_ROOT $RPM_BUILD_ROOT
if [ "$RPM_BUILD_ROOT" != "/" ]; then
	rm -rf $RPM_BUILD_ROOT
fi
mkdir -p $RPM_BUILD_ROOT/usr/src/%{module}-%{version}
#pwd
#ls
cp -rf * $RPM_BUILD_ROOT/usr/src/%{module}-%{version}
mkdir -p $RPM_BUILD_ROOT/etc/udev/rules.d
install -m644 99-bluespec.rules $RPM_BUILD_ROOT/etc/udev/rules.d

%clean
if [ "$RPM_BUILD_ROOT" != "/" ]; then
true
	rm -rf $RPM_BUILD_ROOT
fi

%files
%defattr(-,root,root)
/usr/src/%{module}-%{version}/
/etc/udev/rules.d/99-bluespec.rules

%pre

%post
dkms add -m %{module} -v %{version} --rpm_safe_upgrade

if [ `uname -r | grep -c "BOOT"` -eq 0 ] && [ -e /lib/modules/`uname -r`/build/include ]; then
		dkms build -m %{module} -v %{version}
		dkms install -m %{module} -v %{version}
elif [ `uname -r | grep -c "BOOT"` -gt 0 ]; then
		echo -e ""
		echo -e "Module build for the currently running kernel was skipped since you"
		echo -e "are running a BOOT variant of the kernel."
else
		echo -e ""
		echo -e "Module build for the currently running kernel was skipped since the"
		echo -e "kernel source for this kernel does not seem to be installed."
fi
exit 0

%preun
echo -e
echo -e "Uninstall of bluespec module (version %{version}) beginning:"
dkms remove -m %{module} -v %{version} --all --rpm_safe_upgrade
exit 0
EOF
    ;
