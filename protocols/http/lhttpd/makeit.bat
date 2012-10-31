@echo off
bcc32 -elhttpd.exe main.c util.c protocol.c
del *.obj
del *.tds
