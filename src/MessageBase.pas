//  MessageBase.pas
//
//  Работает с базой данных. Загружает и сохраняет сообщения для конкретного
//  пользователя
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit MessageBase;

interface
  {$I tox.inc}

uses
  ClientAddress, SQLiteTable3, SQLite3, SysUtils, StringUtils, MessageItem,
  FriendList;

type
  TMessageBase = class
  private
    FClientInt: Integer;
    FClient: AnsiString;
    FBase: TSQLiteDatabase;
    FCount: Integer;
  public
    constructor Create(Client: Integer; DataBase: TSQLiteDatabase);
    destructor Destroy; override;

    procedure Insert(Mess: TMessageItem);

    function Select(Friend: TFriendItem; Number: Integer): TMessageItem;
    function SelectRange(Friend: TFriendItem; From, Count: Integer;
      out Messages: TMessageArray): Boolean;
    procedure Update(Mess: TMessageItem);

    property Client: Integer read FClientInt;
    property Count: Integer read FCount;
  end;

implementation

{ TMessageBase }

constructor TMessageBase.Create(Client: Integer; DataBase: TSQLiteDatabase);
var
  Table: TSQLiteTable;
begin
  FBase := DataBase;
  FClientInt := Client;
  FClient := AnsiString(IntToStr(Client));
  Table := FBase.GetTable('SELECT * FROM dialog WHERE user_id = ' + FClient);
  try
    FCount := Table.RowCount;
  finally
    Table.Free;
  end;
end;

destructor TMessageBase.Destroy;
begin

  inherited;
end;

function BoolToInt(Value: Boolean): Integer;
begin
  if Value then
    Result := 1
  else
    Result := 0;
end;

function IntToBool(Value: Integer): Boolean;
begin
  Result := Value <> 0;
end;

procedure TMessageBase.Insert(Mess: TMessageItem);
var
  SQL: AnsiString;
  Query: TSQLiteQuery;
  p_text: RawByteString;
begin
  p_text := UTF8Encode(Mess.Text);
  SQL := 'INSERT INTO "dialog" ("text","time","is_my","is_send","is_read","user_id") VALUES (?1,?2,?3,?4,?5,?6)';
  Query := FBase.PrepareSQL(SQL);
  try
    sqlite3_bind_text(Query.Statement, 1, PAnsiChar(p_text), Length(p_text), Pointer(SQLITE_STATIC));
    sqlite3_bind_double(Query.Statement, 2, Double(Mess.Time));
    sqlite3_bind_int(Query.Statement, 3, BoolToInt(Mess.IsMy));
    sqlite3_bind_int(Query.Statement, 4, BoolToInt(Mess.IsSend));
    sqlite3_bind_int(Query.Statement, 5, BoolToInt(Mess.IsRead));
    sqlite3_bind_int(Query.Statement, 6, FClientInt);
  finally
    FBase.ExecSQL(Query);
  end;
  
  Mess.BaseId := FBase.GetLastInsertRowID;

  Inc(FCount);
end;

function TMessageBase.Select(Friend: TFriendItem;
  Number: Integer): TMessageItem;
begin

end;

function TMessageBase.SelectRange(Friend: TFriendItem; From, Count: Integer;
  out Messages: TMessageArray): Boolean;
var
  SQL: AnsiString;
  Table: TSQLiteTable;
  c, i: Integer;
begin
  Result := False;
  SQL := 'SELECT * FROM "dialog" WHERE "user_id" = ' + intToStr(Friend.BaseId) +
    ' LIMIT ' + IntToStr(From) + ', ' + IntToStr(Count) + ';';
  Table := FBase.GetTable(SQL);
  try
    c := Table.RowCount;
    SetLength(Messages, c);

    for i := 0 to c - 1 do
    begin
      Messages[i] := TMessageItem.Create;
      Messages[i].Text := UTF8Decode(Table.FieldAsString(1));
      Messages[i].Time := TDateTime(Table.FieldAsDouble(2));
      Messages[i].IsMy := IntToBool(Table.FieldAsInteger(3));
      Messages[i].IsSend := IntToBool(Table.FieldAsInteger(4));
      Messages[i].IsRead := IntToBool(Table.FieldAsInteger(5));
      
      Messages[i].BaseId := Table.Row;
      if not Messages[i].IsMy then
        Messages[i].Friend := Friend;

      Table.Next;
    end;

    Result := True;
  finally
    Table.Free;
  end;
end;

procedure TMessageBase.Update(Mess: TMessageItem);
var
  SQL: AnsiString;
  Query: TSQLiteQuery;
  p_text: RawByteString;
begin
  SQL := 'UPDATE "dialog" SET "text" = ?1, "time" = ?2, "is_my" = ?3, '+
    '"is_send" = ?4, "is_read" = ?5, "user_id" = ?6 WHERE  "id" = ' + 
    IntToStr(Mess.BaseId);
    
  p_text := UTF8Encode(Mess.Text);
  
  Query := FBase.PrepareSQL(SQL);
  sqlite3_bind_text(Query.Statement, 1, PAnsiChar(p_text), Length(p_text), Pointer(SQLITE_STATIC));
  sqlite3_bind_double(Query.Statement, 2, Double(Mess.Time));
  sqlite3_bind_int(Query.Statement, 3, BoolToInt(Mess.IsMy));
  sqlite3_bind_int(Query.Statement, 4, BoolToInt(Mess.IsSend));
  sqlite3_bind_int(Query.Statement, 5, BoolToInt(Mess.IsRead));
  sqlite3_bind_int(Query.Statement, 6, Mess.Friend.BaseId);  
  FBase.ExecSQL(Query);
end;

end.
