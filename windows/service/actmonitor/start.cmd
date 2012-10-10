copy actservice.exe c:\windows\
sc create ActivityMonitor binpath= c:\windows\actservice.exe start= auto
sc start ActivityMonitor
