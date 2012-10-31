@echo	**********************************************************************
@echo	Make Winetd.
@echo	by Alexander S. Pristenski 2007
@echo	Change log:
@echo	31.01.2007	Alexander S. Pristenski - initial creation.
@echo	**********************************************************************

@echo Building Winetd...
cd winetd
nmake /f winetd.mak CFG="winetd - Win32 Release"
cd ..

@echo Erasing temporary files...
rmdir /Q /S tmp
