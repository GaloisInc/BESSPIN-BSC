#
# spec file for Bluespec compiler and libraries
#
# note: this file is incomplete and untested
#
%global release_num 2017.07.alpha99
%global release_date 20170726

Name: bluespec-%{release_num}
Version: %{release_date}
Release: 1
Summary: Bluespec compiler
License: Proprietary
Group: Development/Tools/Other
Source0: %{name}_%{version}.orig.tar.xz
BuildRoot: %{_tmppath}/%{name}-%{version}-build
Requires: bluespec-%{release_num}-common = %{version}, bluespec-%{release_num}-run, python, tcl8.5, tk8.5, iwidgets4, itcl3, itk3, tklib
BuildRequires: libghc-syb-dev, libghc-quickcheck1-dev, libghc-regex-compat-dev, libboost-dev, libboost-filesystem-dev, libboost-regex-dev, libboost-signals-dev, libboost-thread-dev, tcl8.5-dev, tk8.5-dev, itcl3-dev, itk3-dev, flex, bison, gperf, git, git-svn, texlive, texlive-latex-extra, texlive-font-utils, texlive-fonts-extra, ghostscript, inkscape
Requires(post): alternatives or update-alternatives
Requires(preun): alternatives or update-alternatives
Summary: bsc compiler and related applications
%description
This package provides the architecture dependent portion of the
Bluespec compiler.

%package -n bluespec
Requires: bluespec-%{release_num}
Summary: Metapackage for the most recent Bluespec version
Group: Development/Tools/Other
%description -n bluespec
This is a metapackage for the most recent Bluespec compiler release.

%package -n bluespec-run
BuildArch: noarch
Summary: Bluespec compiler wrapper scripts
Group: Development/Tools/Other
%description -n bluespec-run
This package provides wrappers for the Bluespec command line tools.
These wrappers allow run time selection of the version of the
compiler used.

%package common
BuildArch: noarch
Summary: bsc compiler architecture independant files
%description common
This package provides the architecture independent portion of the
Bluespec compiler.

%package doc
BuildArch: noarch
Summary: Bluespec documentation
Group: Development/Tools/Other
%description doc
This package provides documentation for BSV and the Bluespec
compiler.

%package util
Summary: Bluespec utilities
Group: Development/Tools/Other
%description util
This package contains some extra support utilities that may be
useful, but are not essential, when using BSV or the Bluespec
compiler.

%package training
BuildArch: noarch
Summary: Bluespec training
Group: Development/Tools/Other
%description training
This package provides training material for the Bluespec compiler and
BSV.

%package -n bluespec-bluenoc
Summary: bluenoc command line utility
%description -n bluespec-bluenoc
The package provides the bluenoc command for configuring emulation
boards connected via BlueNoC.

%package -n bluespec-modules-dkms
Summary: Bluespec kernel modules
%description -n bluespec-modules-dkms
This package provides the source for the kernel module necessary to
communicate with a emulation board connected via BlueNoC over
PCI-Express.

%prep
%setup

%build
make -f util/release/Makefile.release-native VERSION=__RELEASE_NUM__ BSCSRC=`pwd` build-dirs
make -f util/release/Makefile.release-native VERSION=__RELEASE_NUM__ BSCSRC=`pwd` build-tools
make -C src/lib/board_support/BlueNoC/bluenoc
make -C doc PREFIX=${RPM_BUILD_ROOT}/usr
make -C src/manpages

%install
cp -a /tmp/build-bsc-__RELEASE_NUM__/build/Bluespec-__RELEASE_NUM__/* ${RPM_BUILD_ROOT}/usr

mv ${RPM_BUILD_ROOT}/usr/lib/bin ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/BlueNoC ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/Bluesim ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/Libraries ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/Prelude ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/SAT ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/SceMi ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/VPI ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}

mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/emacs ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/lib.linux* ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib

ln -s ../../share/bluespec/board_support ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/board_support
ln -s ../../share/bluespec/BSVSource ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/BSVSource
ln -s ../../share/bluespec/Verilog ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/Verilog

ln -s ../../share/bluespec/tcllib/bluespec ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/bluespec
ln -s ../../share/bluespec/tcllib/gvim ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/gvim
ln -s ../../share/bluespec/tcllib/include ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/include
ln -s ../../share/bluespec/tcllib/itcl3.4 ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/itcl3.4
ln -s ../../share/bluespec/tcllib/itk3.4 ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/itk3.4
ln -s ../../share/bluespec/tcllib/iwidgets4.0 ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/iwidgets4.0
ln -s ../../share/bluespec/tcllib/novas ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/novas
ln -s ../../share/bluespec/tcllib/scemi ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/scemi
ln -s ../../share/bluespec/tcllib/tablelist ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/tablelist
ln -s ../../share/bluespec/tcllib/tcl8.5 ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/tcl8.5
ln -s ../../share/bluespec/tcllib/tk ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/tk
ln -s ../../share/bluespec/tcllib/tk8.5 ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/tk8.5
ln -s ../../share/bluespec/tcllib/workstation ${RPM_BUILD_ROOT}/usr/lib/bluespec-%{release_num}/tcllib/workstation

mv ${RPM_BUILD_ROOT}/usr/README.txt %{buildroot}%{_defaultdocdir}/%{name}-%{version}
mv ${RPM_BUILD_ROOT}/usr/ReleaseNotes*.pdf %{buildroot}%{_defaultdocdir}/%{name}-%{version}

mv ${RPM_BUILD_ROOT}/usr/lib/board_support ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/BSVSource ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}
mv ${RPM_BUILD_ROOT}/usr/lib/Verilog ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}

mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/bluespec ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/gvim ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/include ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/itcl3.4 ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/itk3.4 ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/iwidgets4.0 ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/novas ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/scemi ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/tablelist ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/tcl8.5 ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/tk ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/tk8.5 ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib
mv ${RPM_BUILD_ROOT}/usr/lib/tcllib/workstation ${RPM_BUILD_ROOT}/usr/share/bluespec-%{release_num}/tcllib

mv ${RPM_BUILD_ROOT}/usr/doc/BSV/* %{buildroot}%{_defaultdocdir}/%{name}-%{version}
mv ${RPM_BUILD_ROOT}/usr/index.html %{buildroot}%{_defaultdocdir}/%{name}-%{version}

mv ${RPM_BUILD_ROOT}/usr/util/* %{buildroot}%{_defaultdocdir}/%{name}-%{version}

mv ${RPM_BUILD_ROOT}/usr/training/BSV/* %{buildroot}%{_defaultdocdir}/%{name}-%{version}

%files
%{buildroot}/usr/lib/bluespec-%{release_num}
%{buildroot}%{_defaultdocdir}/%{name}-%{version}

%files -n bluespec-run
%{buildroot}/usr/bin/*

%files common
%{buildroot}/usr/share/bluespec-%{release_num}

%files doc
%{buildroot}%{_defaultdocdir}/%{name}-%{version}

%files util
%{buildroot}%{_defaultdocdir}/%{name}-%{version}

%files training
%{buildroot}%{_defaultdocdir}/%{name}-%{version}

%files -n bluespec-bluenoc

%files -n bluespec-modules-dkms

%post
update-alternatives --install /usr/lib/bluespec bluespec /usr/lib/bluespec-%{release_num} 50 || alternatives --install /usr/lib/bluespec bluespec /usr/lib/bluespec-%{release_num} 50 || :

%preun
update-alternatives --remove bluespec /usr/lib/bluespec-%{release_num} || alternatives --remove bluespec /usr/lib/bluespec-%{release_num} || :

%changelog
* Mon Apr 8 2013 - support@bluespec.com
- initial version
