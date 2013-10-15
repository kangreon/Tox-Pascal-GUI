set REMOVE_EXTENTIONS=*.db *.o *.ppu *.a *.res~ *.dcu *.ddp *.local *.identcache *.exe *.or *.compiled *.cfg *.dbg *.dof *.lps *.res

for /F "delims=" %%A in ('dir %REMOVE_EXTENTIONS% /b /s /a:-d 2^>nul') do (
  del /Q "%%A"
)

for /F "delims=" %%A in ('dir *__history *backup *Build /b /s /a:D 2^>nul') do (
  rd "%%A" /S /Q
)
