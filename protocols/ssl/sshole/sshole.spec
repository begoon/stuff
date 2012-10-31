Summary: SSHole: Secure Sockets Man-in-the-Middle Tool
Name: sshole
Version: 0.1
Release: 1
Copyright: GPL
Group: Applications/Internet
URL: http://thekonst.net/sshole/
Packager: Konstantin Klyagin <konst@konst.org.ua>
Source: %{name}-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-buildroot/

%description
This is just a small proggy useful for debugging SSL-encrypted
protocols. What it does is listening on a specified port, then upon
receiving an incoming connection it connects to the remote host and
operates in the normal way, with the only difference is that all the
traffic is copied to the local stdout. Sshole can serve many connections
at once.

%prep
%setup

%build
./configure --prefix=/usr
make

%install
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT/usr sysconfdir=$RPM_BUILD_ROOT/etc install

find $RPM_BUILD_ROOT/usr/ -type f -print | \
    grep -v '\/(README|COPYING|INSTALL|TODO|ChangeLog)$' | \
    sed "s@^$RPM_BUILD_ROOT@@g" | \
    sed 's/^\(.\+\/man.\+\)$/\1*/g' \
    > %{name}-%{version}-filelist

%clean
rm -rf $RPM_BUILD_ROOT

%files -f %{name}-%{version}-filelist
%defattr(-, root, root)

%doc README COPYING INSTALL ChangeLog

%changelog
