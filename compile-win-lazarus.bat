@ECHO OFF

echo Clean
call clean.bat

set PathLazarus="c:\lazarus"
set PathFPC="C:\lazarus\fpc\2.6.2\bin\i386-win32"

md Build
md Build\dcu

call %PathFPC%\fpc tox.dpr -MDelphi -Shi -O1 -WG -Fisrc -Figui -Fiinc -Figui\Forms -Fugui -Fuinc -Fusrc -Fugui\Forms -Fusqlite -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fu. -FUBuild\dcu\ -vm5023,5024 -FEBuild\ -dLCL -dLCLwin32

copy libtoxcore-0.dll Build\libtoxcore-0.dll
copy sqlite3.dll Build\sqlite3.dll

pause