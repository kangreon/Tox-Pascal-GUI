//  DataBase.pas
//
//  Окрывает доступ к файлу базы данных и осуществляет с ним взаимодействия
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit DataBase;

interface

uses
  Settings, SysUtils, SQLiteTable3, StringUtils, FriendBase;

type
  TDataBase = class
  private
    FBase: TSQLiteDatabase;
    FSettings: TSettings;
    FIsLoad: Boolean;
    FErrorText: DataString;
    FFriendBase: TFriendBase;
    function CheckingTables: Boolean;
  public
    constructor Create(Settings: TSettings);
    destructor Destroy; override;

    function LoadBase: Boolean;

    property Base: TSQLiteDatabase read FBase;
    property ErrorText: DataString read FErrorText;
    property FriendBase: TFriendBase read FFriendBase;
    property IsLoad: Boolean read FIsLoad;
  end;

implementation

{ TDataBase }

constructor TDataBase.Create(Settings: TSettings);
begin
  FSettings := Settings;
  FIsLoad := False;
end;

destructor TDataBase.Destroy;
begin
  if FIsLoad then
    FBase.Free;
  inherited;
end;

{ *  Открывает базу данных и проверяет целостность таблиц.
  *  В случае ошибки вернет False и описание ошибки в ErrorText
  * }
function TDataBase.LoadBase: Boolean;
var
  BaseFile: DataString;
  IsLoad: Boolean;
begin
  IsLoad := False;

  BaseFile := FSettings.ConfigPath + 'messages.sqlite';
  try
    FBase := TSQLiteDatabase.Create(BaseFile);
    IsLoad := True;
  except on e: Exception do
    begin
      FErrorText := e.Message;
    end;
  end;

  if IsLoad then
  begin
    FIsLoad := CheckingTables;

    if FIsLoad then
    begin
      FFriendBase := TFriendBase.Craete(FBase);
    end;
  end;

  Result := FIsLoad;
end;

{ *  Проверяет на существование всех необходимых таблиц. Если таблицы
  *  отсутствуют, происходит их создание.
  *
  *  В случае удачной проверки вернет TRUE.
  *  В случае ошшибки вернет FALSE и описание ошибки в свойстве класса
  *  ErrorText.
  * }
function TDataBase.CheckingTables: Boolean;
var
  IsError: Boolean;
begin
  IsError := False;

  if not FBase.TableExists('user') then
  begin
    try
      FBase.ExecSQL('CREATE TABLE "user" ("id" INTEGER PRIMARY KEY  AUTOINCREMENT  NOT NULL  UNIQUE , "name" TEXT, "local_name" TEXT, "status" TEXT, "client_id" TEXT, "address" TEXT)');
    except on e: Exception do
      begin
        IsError := True;
        FErrorText := e.Message;
      end;
    end;
  end;

  if (not IsError) and (not FBase.TableExists('dialog')) then
  begin
    try
      FBase.ExecSQL('CREATE TABLE "dialog" ("id" INTEGER PRIMARY KEY  AUTOINCREMENT  NOT NULL  UNIQUE , "text" TEXT, "time" DOUBLE, "is_my" BOOL, "is_send" BOOL, "is_read" BOOL, "user_id" INTEGER)');
    except on e: Exception do
      begin
        IsError := True;
        FErrorText := e.Message;
      end;
    end;
  end;

  Result := not IsError;
end;

end.
