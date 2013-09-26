@ECHO OFF
cls

set BRCCPATH=C:\Program Files\Embarcadero\RAD Studio\12.0\bin\brcc32.exe

for %%X in (brcc32.exe) do (
	set FOUND=%%~$PATH:X
)

if defined FOUND (
	goto compileresource
) else (
	set FOUND=%BRCCPATH%
	goto compileresource
)

:compileresource
"%FOUND%" "Resource\images.rc" -fo "images.res"
"%FOUND%" "Resource\tox-res.rc" -fo "tox-res.res"
goto end

:error
echo Insert path to resource compiler in variable BRCCPATH.
goto end

:end
pause
exit