@ECHO OFF

echo Clean
call clean.bat

echo Build resources
call build-resource.bat >NUL

call rsvars.bat
call %FrameworkDir%\msbuild /p:config=Release tox.dproj

pause