@ECHO OFF

set REMOVE_EXTENTIONS=*.db *.o *.ppu *.a *.res~ *.dcu *.ddp *.local *.identcache *.exe *.or *.compiled *.cfg *.dbg *.dof *.lps *.res

for /F "delims=" %%A in ('dir %REMOVE_EXTENTIONS% /b /s /a:-d') do (
	del /Q "%%A"
)

for /F "delims=" %%A in ('dir *__history *backup /b /s /a:D') do (
	rd "%%A" /S /Q
)

pause