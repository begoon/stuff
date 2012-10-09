@echo off
for /F "skip=1 tokens=3" %%i in ('query process') do logoff.exe %%i
