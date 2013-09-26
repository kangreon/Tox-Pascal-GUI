@ECHO OFF

set REMOVE_EXTENTIONS=*.db *.o *.ppu *.a *.res~ *.dcu *.ddp *.local *.identcache *.exe *.or *.compiled *.cfg *.dbg *.dof *.lps *.res

for /F %%A in ('dir %REMOVE_EXTENTIONS% /b /s /a:-d') do (
	del /Q %%A
)

for /F %%A in ('dir __history backup Win32 /b /s /a:D') do (
	rd %%A /S /Q
)