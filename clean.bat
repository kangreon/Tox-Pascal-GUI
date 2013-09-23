del *.o *.ppu *.a *.res~ *.dcu *.ddp *.local *.identcache *.exe *.or *.compiled *.cfg *.dbg *.dof *.lps *.res
rd __history /S /Q
rd backup /S /Q
rd Win32 /S /Q

cd src
del *.o *.ppu *.a *.res~ *.dcu *.ddp *.local *.identcache *.exe *.or
rd __history /S /Q
rd backup /S /Q

cd ./../gui
del *.o *.ppu *.a *.res~ *.dcu *.ddp *.local *.identcache *.exe *.or
rd __history /S /Q
rd backup /S /Q

cd ./forms
del *.o *.ppu *.a *.res~ *.dcu *.ddp *.local *.identcache *.exe *.or
rd __history /S /Q
rd backup /S /Q

cd ./../../PngImage_D7
del *.o *.ppu *.a *.res~ *.dcu *.ddp *.local *.identcache *.exe *.or
rd __history /S /Q
rd backup /S /Q