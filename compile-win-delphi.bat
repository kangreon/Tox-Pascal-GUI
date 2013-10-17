@ECHO OFF

echo Clean
call clean.bat

call rsvars.bat
call %FrameworkDir%\msbuild /p:config=Release tox.dproj

copy libtoxcore-0.dll Build\libtoxcore-0.dll
copy sqlite3.dll Build\sqlite3.dll


pause