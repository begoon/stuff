upx -9 xporttest.exe
winrar a -m5 -x*.rar -x*.zip -x*.rar -x*.bmp -m5 xporttest-bin.zip xporttest.exe
winrar a xporttest-bin.rar xporttest-bin.zip
del xporttest-bin.zip
