//  Settings.pas
//
//  Содержит класс, отвечающий за сохранение и загрузку различных настроек
//  приложения.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit Settings;

interface
{$I tox.inc}

uses
  {$I tox-uses.inc}
  {$IFDEF Win32}ShFolder,{$ENDIF} IniFiles, Classes, ServerList, SysUtils;

type
  TSettings = class
  private
    FIniFile: TIniFile;
    FDataStream: TFileStream;
    FUserName: string;
    FServerList: TServerList;
    FUseIPv6: Boolean;
    FUserListWidth: Integer;
    function GetConfigPath: string;
    function GetHomePath: string;
    procedure SetUserName(const Value: string);
    procedure SetUseIPv6(const Value: Boolean);
    procedure SetUserListWidth(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function LoadData(out Size: Integer): PByte;
    procedure SaveData(Data: PByte; Size: Integer);

    property ServerList: TServerList read FServerList;

    property ConfigPath: string read GetConfigPath;
    property UseIPv6: Boolean read FUseIPv6 write SetUseIPv6;
    property UserName: string read FUserName write SetUserName;

    // Настройки списка пользователей
    property UserListWidth: Integer read FUserListWidth write SetUserListWidth;
  end;

const
  USER_LIST_MIN_WIDTH = 223;
  USER_LIST_MAX_WIDTH = 300;

implementation

const
  DEFAULT_USER_NAME: string = 'default';
  TOX_CONFIG_PATH: string = 'dTOX';
  SEPARATOR: string = {$IFDEF Win32}'\'{$ELSE}'/'{$ENDIF};

{ TSettings }

constructor TSettings.Create;
var
  FileSettings: string;
  FileData: string;
  OpenMode: Word;
begin
  FileSettings := ConfigPath + 'settings.ini';
  FIniFile := TIniFile.Create(FileSettings);
  FServerList := TServerList.Create(ConfigPath);

  FileData := ConfigPath + 'save.dat';
  if FileExists(FileData) { *Converted from FileExists*  } then
    OpenMode := fmOpenReadWrite or fmShareDenyWrite
  else
    OpenMode := fmCreate or fmOpenReadWrite or fmShareDenyWrite;

  FDataStream := TFileStream.Create(FileData, OpenMode);

  FUserName := {$IFDEF FPC}UTF8Encode{$ENDIF}(FIniFile.ReadString('user', 'name', DEFAULT_USER_NAME));
  FUseIPv6 := False;

  FUserListWidth := FIniFile.ReadInteger('UserList', 'Width', USER_LIST_MIN_WIDTH);
end;

destructor TSettings.Destroy;
begin
  FServerList.Free;
  FIniFile.Free;
  inherited;
end;

function TSettings.GetConfigPath: string;
var
  HomePath: string;
begin
  HomePath := GetHomePath {$IFDEF Win32} + SEPARATOR + TOX_CONFIG_PATH{$ENDIF} + SEPARATOR;

  if not DirectoryExists(HomePath) { *Converted from DirectoryExists*  } then
    CreateDir(HomePath); { *Converted from CreateDir*  }

  Result := HomePath;
end;

function TSettings.GetHomePath: string;
{$IFDEF Win32}
const
  CSIDL_APPDATA = $001A;
var
  LStr: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ELSE}
begin
  Result := ExtractFilePath(GetAppConfigFile(False, True));
end;
{$ENDIF}

function TSettings.LoadData(out Size: Integer): PByte;
begin
  Size := FDataStream.Size;

  if Size > 0 then
  begin
    Result := GetMemory(Size);

    FDataStream.Position := 0;
    FDataStream.Read(Result^, Size);
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TSettings.SaveData(Data: PByte; Size: Integer);
begin
  FDataStream.Size := 0;
  FDataStream.Position := 0;
  FDataStream.Write(Data^, Size);
end;

procedure TSettings.SetUseIPv6(const Value: Boolean);
begin
  FUseIPv6 := Value;
end;

procedure TSettings.SetUserListWidth(const Value: Integer);
begin
  if FUserListWidth <> Value then
  begin
    FUserListWidth := Value;
    FIniFile.WriteInteger('UserList', 'Width', Value);
  end;
end;

procedure TSettings.SetUserName(const Value: string);
begin
  if FUserName <> Value then
  begin
    FUserName := Value;
    FIniFile.WriteString('user', 'name', {$IFDEF FPC}UTF8Decode{$ENDIF}(Value));
  end;
end;

end.
