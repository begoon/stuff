# Microsoft Developer Studio Generated NMAKE File, Based on winetd.dsp
!IF "$(CFG)" == ""
CFG=winetd - Win32 Release
!MESSAGE No configuration specified. Defaulting to winetd - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "winetd - Win32 Release" && "$(CFG)" != "winetd - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "winetd.mak" CFG="winetd - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "winetd - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "winetd - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "winetd - Win32 Release"

OUTDIR=.\../out
INTDIR=.\../tmp
# Begin Custom Macros
OutDir=.\../out
# End Custom Macros

ALL : "$(OUTDIR)\winetd.exe" "..\tmp\winetd.bsc"


CLEAN :
  -@erase "$(INTDIR)\console.obj"
  -@erase "$(INTDIR)\console.sbr"
  -@erase "$(INTDIR)\logger.obj"
  -@erase "$(INTDIR)\logger.sbr"
  -@erase "$(INTDIR)\net.obj"
  -@erase "$(INTDIR)\net.sbr"
  -@erase "$(INTDIR)\utf8.obj"
  -@erase "$(INTDIR)\utf8.sbr"
  -@erase "$(INTDIR)\vc60.idb"
  -@erase "$(INTDIR)\winetd.obj"
  -@erase "$(INTDIR)\winetd.sbr"
  -@erase "$(INTDIR)\xmpp_att.obj"
  -@erase "$(INTDIR)\xmpp_att.sbr"
  -@erase "$(INTDIR)\xmpp_tag.obj"
  -@erase "$(INTDIR)\xmpp_tag.sbr"
  -@erase "$(OUTDIR)\winetd.exe"
  -@erase "..\tmp\winetd.bsc"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MT /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /FR"$(INTDIR)\\" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"../tmp/winetd.bsc" 
BSC32_SBRS= \
  "$(INTDIR)\console.sbr" \
  "$(INTDIR)\logger.sbr" \
  "$(INTDIR)\net.sbr" \
  "$(INTDIR)\utf8.sbr" \
  "$(INTDIR)\winetd.sbr" \
  "$(INTDIR)\xmpp_att.sbr" \
  "$(INTDIR)\xmpp_tag.sbr"

"..\tmp\winetd.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\winetd.pdb" /machine:I386 /out:"$(OUTDIR)\winetd.exe" 
LINK32_OBJS= \
  "$(INTDIR)\console.obj" \
  "$(INTDIR)\logger.obj" \
  "$(INTDIR)\net.obj" \
  "$(INTDIR)\utf8.obj" \
  "$(INTDIR)\winetd.obj" \
  "$(INTDIR)\xmpp_att.obj" \
  "$(INTDIR)\xmpp_tag.obj"

"$(OUTDIR)\winetd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"

OUTDIR=.\../out
INTDIR=.\../tmp
# Begin Custom Macros
OutDir=.\../out
# End Custom Macros

ALL : "$(OUTDIR)\winetd.exe"


CLEAN :
  -@erase "$(INTDIR)\console.obj"
  -@erase "$(INTDIR)\logger.obj"
  -@erase "$(INTDIR)\net.obj"
  -@erase "$(INTDIR)\utf8.obj"
  -@erase "$(INTDIR)\vc60.idb"
  -@erase "$(INTDIR)\vc60.pdb"
  -@erase "$(INTDIR)\winetd.obj"
  -@erase "$(INTDIR)\xmpp_att.obj"
  -@erase "$(INTDIR)\xmpp_tag.obj"
  -@erase "$(OUTDIR)\winetd.exe"
  -@erase "$(OUTDIR)\winetd.ilk"
  -@erase "$(OUTDIR)\winetd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MTd /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"../tmp/winetd.bsc" 
BSC32_SBRS= \
  
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\winetd.pdb" /debug /machine:I386 /out:"$(OUTDIR)\winetd.exe" /pdbtype:sept 
LINK32_OBJS= \
  "$(INTDIR)\console.obj" \
  "$(INTDIR)\logger.obj" \
  "$(INTDIR)\net.obj" \
  "$(INTDIR)\utf8.obj" \
  "$(INTDIR)\winetd.obj" \
  "$(INTDIR)\xmpp_att.obj" \
  "$(INTDIR)\xmpp_tag.obj"

"$(OUTDIR)\winetd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("winetd.dep")
!INCLUDE "winetd.dep"
!ELSE 
!MESSAGE Warning: cannot find "winetd.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "winetd - Win32 Release" || "$(CFG)" == "winetd - Win32 Debug"
SOURCE=.\console.cpp

!IF  "$(CFG)" == "winetd - Win32 Release"


"$(INTDIR)\console.obj" "$(INTDIR)\console.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"


"$(INTDIR)\console.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\logger.cpp

!IF  "$(CFG)" == "winetd - Win32 Release"


"$(INTDIR)\logger.obj"  "$(INTDIR)\logger.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"


"$(INTDIR)\logger.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\net.cpp

!IF  "$(CFG)" == "winetd - Win32 Release"


"$(INTDIR)\net.obj" "$(INTDIR)\net.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"


"$(INTDIR)\net.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\utf8.cpp

!IF  "$(CFG)" == "winetd - Win32 Release"


"$(INTDIR)\utf8.obj"  "$(INTDIR)\utf8.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"


"$(INTDIR)\utf8.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\winetd.cpp

!IF  "$(CFG)" == "winetd - Win32 Release"


"$(INTDIR)\winetd.obj"  "$(INTDIR)\winetd.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"


"$(INTDIR)\winetd.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\xmpp_att.cpp

!IF  "$(CFG)" == "winetd - Win32 Release"


"$(INTDIR)\xmpp_att.obj"  "$(INTDIR)\xmpp_att.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"


"$(INTDIR)\xmpp_att.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\xmpp_tag.cpp

!IF  "$(CFG)" == "winetd - Win32 Release"


"$(INTDIR)\xmpp_tag.obj"  "$(INTDIR)\xmpp_tag.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"


"$(INTDIR)\xmpp_tag.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\cse.cpp

!IF  "$(CFG)" == "winetd - Win32 Release"


"$(INTDIR)\cse.obj"  "$(INTDIR)\cse.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winetd - Win32 Debug"


"$(INTDIR)\cse.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 

