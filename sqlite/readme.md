4 February 2011
- Added support for SQLite Backup API. See example in uTestSqlite.pas
- Recompiled DLL with latest SQLite 3.7.5. Compiled with Visual C++ 10 but with static linking; runs OK on Windows XP

16 October 2008
- Delphi 2009 compatibility

- amended code in FieldAsBlobText, the memorystream must not be freed here as this raises an exception in TSqliteTable.Destroy, and prevents the value being accessed more than once.

- added following amendments from Tobias Gunkel

1. Freepascal compatibility (works with win/linux/mac).
FPC gives a few warnings that if Dispose() is called with untyped pointer arguments (in TSQLiteTable.Destroy) no memory is freed. I wrote a test program to check if memory is freed in FPC and it was, although it might not be freed on other platforms (just tested it on win32). To be on the safe side either the memory should be allocated/freed with Getmem/Freemem instead or the pointer should be casted to the correct type in the dispose call.

2. Format()-like queries to make value binding as easy as possible.
For example:

    DB.ExecSQL('INSERT INTO ['+COVER_TBL+'] ' +
               '([Filename], [Date], [Width], [Height]) VALUES' +
               '(?, ?, ?, ?)',
               [UTF8Encode(Filename), DateTimeToUnixTime(FileDate),
                Info.CoverWidth, Info.CoverHeight]);

Binding is handled by TSQLiteDatabase.BindData() which supports vtString, vtAnsiString, vtPChar, vtWideString, vtPWideChar, vtChar, vtWideChar, vtInteger, vtInt64, vtExtended, vtBoolean, vtPointer (for nil), TCustomMemoryStream and TStream (the latter two for BLOB data).

3. TSQLiteUniTable.FieldAsBlobPtr() to retrieve a pointer to BLOB data, so the data is not copied to a TMemoryStream first (as FieldAsBlob() does). The pointer is valid as long as the table is not freed, TSQLiteUniTable.Next was called or the column value was converted to another type (See http://www.sqlite.org/c3ref/column_blob.html).

4. Removed unused TSQLiteUniTable.fColTypes. Determining the column type this way is very error-prone as SQLite uses dynamic typing. Even if a field was requested to be of a certain type, e.g. INTEGER the user can store any value (e.g. a string) in it changing the type of the field. 
The type should be retrieved dynamically with sqlite3_column_type() instead as different rows of the same column might have different types.


5 June 2008
Updated DLL to version 3.5.9 (built with MSVC 6.0)
Added code from Andrew Retmanski to support prepared queries (see comments in SQLIteTable3.pas
Lukas added support for named parameters - see comments in code
User nebula enhanced error message; also modified code to call sqlite3_reset before checking error message


27 Aug 2007
Amended TSQLiteDatabase constructor to convert filename to UTF8,for compatibility with latest SQLite3 DLL.

Updated DLL to version 3.4.2 (built with MSVC 6.0).

14 Aug 2005

The following changes were made by Lukas Gebauer (geby@volny.cz). In addition, some changes from a previous D5-compatible version were merged, and the supplied sqlite3.dll is updated to version 3.2.2

Notes from Lukas:

- added support for delphi 4+

- datatype constants matches SQlite datatypes contants. (otherwise in some situations you got bad column datatype!)

- removed dependency on strutils

- code is reformatted to better look (official borland formatting rules)

- added some pragma's after database is open (temp is in memory)

- TSQLiteDatabase.GetTableValue(const SQL: string): int64 for easy call of SQL commands what returning one number only. (like select
count(*)...)

- TSQLiteDatabase.GetTableString(const SQL: string): String for easy call of SQL commands what returning one string only. (like PRAGMA
integrity_check)

- TSQLiteDatabase.SetTimeout(Value: integer); you can set timeout for accessing to some table. Good for database sharing!

- TSQLiteDatabase.version: string; returns SQLITE version string

- removed bool fieldtype (it is not natural SQLite3 type)

- fild type detection by Sqite3_columnType knows REAL too.

- integer filedtype is based on Int64

- GetFields can get data from any supported fieldtype

- changed some integers to cardinal for avoid signed and unsigned mismatch

- TSqliteTable.FieldAsInteger(I: cardinal): int64; returns int64


3 May 2005 Fixed bug where strupper called on column type before checking for nil

2 May 2005 Add extra check for nil in TSqliteTable.Destroy, thanks to Tim Maddrell

22 Apr 2005 Revise TSqliteTable.Destroy to fix memory leak with dtStr type (thanks to
Jose Brito)

21 Apr 2005 Quick revision to fix case sensitivity in detecting column type,
and remove PRAGMA full_column_names = 1 which is deprecated. Warning: may break code. Fix your SQL code so that all column names in a result set are unique.

21 Feb 2005 Sqlite DLL now 3.1.3 

19 Feb 2005 Revised for Sqlite 3.1.2 

21 Dec 2004 First public release 

The following notice appears in the Sqlite source code:

*
** 2001 September 15
**
** 
** The author disclaims copyright to this source code.  In place of

** a legal notice, here is a blessing:

**
 May you do good and not evil.

** May you find forgiveness for yourself and forgive others.

** May you share freely, never taking more than you give.


For more information about SQLite, see http://www.sqlite.org

For more information about this simple wrapper, see http://www.itwriting.com/sqlitesimple.php





