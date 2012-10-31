          Winetd - inetd for Windows.
          by Alexander Pristenski 2007

CONTACTS:

  email: <rekod@users.sourceforge.net>
    ICQ: 150211485 (preferable contact, online 11:00 - 17:00 Moscow time)

  ICQ contact more preferable, so don't hesitate to contact me using ICQ :)
  Lastest patches by Denis Golovan (<denis_golovan@mail.ru>, ICQ 105051989)

README:

1.   Introduction.
2.   How to build it.
2.1. How to completely disable logging.
3.   How to install/uninstall it.
4.   How to use it.

1. Introduction.

  Winetd (inetd for Windows) - UNIX's inetd daemon analog for Windows.

  Winetd acts as a control service for other internet services which
  are just console applications which stdin and stdout are forwarded
  to and from socket respectively.

  When Winetd accepts a connection, it determines for which service
  this connection belongs, then calls corresponding service and provides
  a socket to the service.

  Essentially, allows running one service to invoke several others,
  reducing load on the system.

2. How to build it.

  Unpack Winetd archive to directory you want.

  If you have Microsoft Visual Studio 6.0 (or higher) installed on your PC,
  just run allmake.bat and winetd.exe will appear in /out directory.

2.1. How to completely disable logging.

  Logging can't be disabled _at_runtime_ for now,
  because logging starts _before_ reading winetd.conf

  To completely disable logging _at_compile_time_ do the following:

  a) open \winetd\logger.h
  b) change "#define LOGGER_ENABLED 1" to "#define LOGGER_ENABLED 0"
  c) save the changes
  d) run allmake.bat and winetd.exe will appear in /out directory

  Or simple use winetd_nolog.exe

3. How to install/uninstall it.

  To install Winetd execute:

  >winetd.exe -i

  To uninstall Winetd execute:

  >winetd.exe -u

  To check if Winetd is installed/uninstalled correctly you may look the
  list of services:
  Right click on 'My Computer', then select 'Manage'.
  In 'Computer Management' window appeared,
  select 'Services and Applications' -> 'Services'.
  If you see 'Winetd' service - installation was sucessfull.

4. How to use it.

  To use Winetd you should place 'winetd.conf' file in the same directory
  where 'winetd.exe' placed.

  Config file is a XML file which has the following structure (example):

  <?xml version='1.0'?>
  <config>
     <cmd iface='0.0.0.0' port='12345' run='c:\windows\system32\cmd.exe' description='Command line' />
     <cmd iface='192.168.0.1' port='54321' run='c:\windows\system32\nslookup.exe' description='DNS lookup' />
     <cmd iface='192.168.0.2' port='11111' run='c:\windows\system32\systeminfo.exe' description='SysInfo' />
  </config>

  NOTE: if iface='0.0.0.0' - winetd will listen on ALL interfaces.

  If Winetd not works - inspect 'winetd.log' file and look at error reports.

  So, now you may write your own internet services (POP3, SMTP, HTTP, etc.)
  just as console applications without sockets programming.
  Simply run that applications from Winetd !
  Enjoy! :)

-eof-
