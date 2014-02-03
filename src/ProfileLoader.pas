//  ProfileLoader.pas
//
//  Загрузка и сохранение профилей чата
//
//  The MIT License (MIT)
//
//  Copyright (c) 2014 Dmitry
//
unit ProfileLoader;

interface

uses
  Settings, libtox, IniFiles, Classes, SysUtils, StringUtils, ClientAddress;

type
  TDataInFile = (dfNone, dfOriginal, dfLock);

  TProfileItem = record
    Name: string;
    FileName: string;
  end;

  TProfileList = array of TProfileItem;

  TProcBool = procedure(Sender: TObject; var Value: Boolean) of object;

  TMessengerError = (meNone, meNameExist, meSelectProfileNotFound, meHasBeenLoaded,
    meOpenFile, meOpenFileLock, meLongSize, meReadError, meInitTox, meBackUpError,
    mePassBad, meProfileNotLoad, meSaveError, meSaveWriteError);

  TProfileLoader = class
  private
    FSettings: TSettings;
    FIni: TIniFile;
    FIsLoad: Boolean;
    FItems: TProfileList;
    FData: PByte;
    FDataSize: Integer;
    FKey: PByte;
    FKeySize: Integer;
    FProfilePath: string;
    FSelected: Integer;
    FSelectedItem: TProfileItem;
    FStream: TFileStream;
    FStreamLock: TFileStream;
    FError: TMessengerError;
    FErrorInfo: string;
    FTox: TTox;
    FAddress: TFriendAddress;
    FOnLoadLock: TProcBool;
    function EventLoadLock: Boolean;

    function Add(Name, FIleName: string): Integer;
    function BackUp(FromSource: Boolean): Boolean;
    function FileExistInItems(FileName: string): Boolean;
    procedure LoadProfiles;
    function NameExistInItems(Name: string): Boolean;
    procedure SetSelected(const Value: Integer);
    function RandomFileName: string;
    function Read(Stream: TStream; var Size: Integer): PByte;
    procedure SetError(Error: TMessengerError; Info: string = '');
    function LoadTox(DataFile: TDataInFile): Boolean;
    function LoadFileOriginal: Boolean;
    function LoadFileLock: Boolean;
    function LoadFileNone: Boolean;
  public
    constructor Create(Settings: TSettings);
    destructor Destroy; override;

    procedure Delete(Index: Integer);
    function New(Name, Key: string): Boolean;
    function Load(Index: Integer; Key: string): Boolean;
    procedure Unload;
    function SaveData: Boolean;

    property Address: TFriendAddress read FAddress;
    property Tox: TTox read FTox;

    property Items: TProfileList read FItems;

    property Error: TMessengerError read FError;
    property ErrorInfo: string read FErrorInfo;
    property Selected: Integer read FSelected write SetSelected;
    property SelectedItem: TProfileItem read FSelectedItem;

    property OnLoadLock: TProcBool read FOnLoadLock write FOnLoadLock;
  end;

implementation

{ TMessagerLoad }

constructor TProfileLoader.Create(Settings: TSettings);
begin
  FIsLoad := False;
  FSettings := Settings;
  FAddress := TFriendAddress.Create;

  LoadProfiles;
end;

destructor TProfileLoader.Destroy;
begin
  FAddress.Free;
  FIni.Free;
  inherited;
end;

{ *  Запрос у пользователя разрешения на загрузку информации из .lock файла т.к.
  *  не удалось произвеси четение из основоного файла.
  * }
function TProfileLoader.EventLoadLock: Boolean;
begin
  Result := False;
  if Assigned(FOnLoadLock) then
  begin
    FOnLoadLock(Self, Result);
  end;
end;

procedure TProfileLoader.SetError(Error: TMessengerError; Info: string);
begin
  FError := Error;
  FErrorInfo := Info;
end;

{ *  Сохраняет резервную копию для файла данных или восстанавливает ее
  *  FromSource = true
  *  Копирует содержимое Оригинального файла в файл .lock
  *  FromSource = false
  *  Копирует наоборот
  * }
function TProfileLoader.BackUp(FromSource: Boolean): Boolean;
var
  Data: PByte;
  Size: Integer;
begin
  if FromSource then
  begin
    FIni.WriteInteger(SelectedItem.Name, 'fileindex', 1);

    Size := FStream.Size;
    GetMem(Data, Size);
    try
      FStream.Position := 0;
      FStream.Read(Data^, Size);

      FStreamLock.Size := 0;
      FStreamLock.Position := 0;
      Result := FStreamLock.Write(Data^, Size) = Size;
    finally
      FreeMem(Data);
    end;
  end
  else
  begin
    FIni.WriteInteger(SelectedItem.Name, 'fileindex', 2);

    Size := FStreamLock.Size;
    GetMem(Data, Size);
    try
      FStreamLock.Position := 0;
      FStreamLock.Read(Data^, Size);

      FStream.Size := 0;
      FStream.Position := 0;
      Result := FStream.Write(Data^, Size) = Size;

      FIni.WriteInteger(SelectedItem.Name, 'fileindex', 1);
    finally
      FreeMem(Data);
    end;

  end;
end;

function TProfileLoader.FileExistInItems(FileName: string): Boolean;
var
  Item: TProfileItem;
begin
  Result := False;
  FileName := AnsiLowerCase(FileName);

  for Item in FItems do
  begin
    if AnsiLowerCase(Item.FileName) = FileName then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TProfileLoader.NameExistInItems(Name: string): Boolean;
var
  Item: TProfileItem;
begin
  Result := False;
  Name := AnsiLowerCase(Name);

  for Item in FItems do
  begin
    if AnsiLowerCase(Item.Name) = Name then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ *  Добавление нового профиля в список
  * }
function TProfileLoader.Add(Name, FIleName: string): Integer;
var
  Item: TProfileItem;
  i: Integer;
begin
  Item.Name := Name;
  Item.FileName := FIleName;

  i := Length(FItems);
  SetLength(FItems, i + 1);
  FItems[i] := Item;

  Result := i;
end;

{ *  Удаляет профиль с индексом
  * }
procedure TProfileLoader.Delete(Index: Integer);
var
  i: Integer;
  Item: TProfileItem;
begin
  if FIsLoad then
  begin
    SetError(meHasBeenLoaded);
    // Нельзя выполнять операции со списком профилей во время работы профиля

    Exit;
  end;

  Selected := Index;
  if Selected = -1 then Exit;

  Item := FItems[Index];
  FIni.EraseSection(Item.Name);

  for i := Index to High(FItems) - 1 do
  begin
    FItems[i] := FItems[i + 1];
  end;

  if FileExists(Item.FileName) then
  begin
    DeleteFile(Item.FileName);
  end;

  if Index = 0 then
    Selected := 0
  else
    Selected := Index - 1;
end;

{ *  Сохраняет данные в файл с использованием резервного копирования
  * }
function TProfileLoader.SaveData: Boolean;
var
  Size: Integer;
  Data: PByte;
  SaveCode: Integer;
begin
  Result := False;

  if not FIsLoad then
  begin
    SetError(meProfileNotLoad);
    Exit;
  end;

  Size := tox_size_encrypted(FTox);
  GetMem(Data, Size);
  try
    SaveCode := tox_save_encrypted(FTox, Data, FKey, FKeySize);
    if SaveCode = -1 then
    begin
      SetError(meSaveError);
      Exit;
    end;

    if not BackUp(True) then
    begin
      SetError(meBackUpError);
      Exit;
    end;

    FIni.WriteInteger(SelectedItem.Name, 'fileindex', 2);

    FStream.Position := 0;
    FStream.Size := 0;
    Result := FStream.Write(Data^, Size) = Size;
    if not Result then
    begin
      SetError(meSaveWriteError);
      Exit;
    end;

    FIni.WriteInteger(SelectedItem.Name, 'fileindex', 1);
  finally
    FreeMem(Data);
  end;

  if not Result then
  begin
    SetError(meSaveError);
  end;
end;

{ *  Открывает файл профиля и пытается прочитать его
  *
  * meHasBeenLoaded, meSelectProfileNotFound, meOpenFile, meOpenFileLock,
  * }
function TProfileLoader.Load(Index: Integer; Key: string): Boolean;
var
  StreamLoad: Boolean;
  StreamError: string;
  OpenMode: Cardinal;
  DataInFile: TDataInFile;
begin
  Result := False;

  if FIsLoad then
  begin
    SetError(meHasBeenLoaded);
    // Уже загружен другой профиль. Вначале следует выйти с активного профиля.

    Exit;
  end;

  Selected := Index;
  if Selected = -1 then
  begin
    SetError(meSelectProfileNotFound);
    // Выбранный профиль не существует в списке. Вероятно где-то ошибк4а в коде.

    Exit;
  end;

  // Получение сведений о файле, в котором хранится последняя версия данных
  case FIni.ReadInteger(SelectedItem.Name, 'fileindex', 0) of
    1:
      DataInFile := dfOriginal;
      // Последние данные хранятся в основном файле и загружать нужно их только
      // от туда. В случае ошибки, спросить у пользователя, стоит ли попытаться
      // открыть данные из файла .lock

    2:
      DataInFile := dfLock;
      // Основной файл поврежден. Вероятнее всего программа была аварийно закрыта
      // во время сохранения новых данных. Последняя рабочая версия данных хранится
      // в файле .lock

  else
    DataInFile := dfNone;
    // Нету информации о том, в каком файле хранится информация. Пробовать открыть
    // оригинальный файл, если открыть не вышло, спросить пользователя разрешение
    // на открытие файла .lock

  end;

  // Открытие основного файла
  if FileExists(SelectedItem.FileName) then
    OpenMode := fmOpenReadWrite
  else
    OpenMode := fmCreate or fmOpenReadWrite;

  StreamLoad := False;
  try
    FStream := TFileStream.Create(SelectedItem.FileName, OpenMode);
    StreamLoad := True;
  except on e: Exception do
    StreamError := e.Message;
  end;

  if not StreamLoad then
  begin
    SetError(meOpenFile, StreamError);
    // Открыть или создать файл не удалось. Подробности ошибки в StreamError.
    // Дальнейшая загрузка/создание профиля не имеет смысла, т.к. результат
    // сохранен не будет.

    Exit;
  end;

  try
    // Открытие запасного файла
    if FileExists(SelectedItem.FileName + '.lock') then
      OpenMode := fmOpenReadWrite
    else
      OpenMode := fmCreate or fmOpenReadWrite;

    StreamLoad := False;
    try
      FStreamLock := TFileStream.Create(SelectedItem.FileName + '.lock', OpenMode);
      StreamLoad := True;
    except on e: Exception do
      StreamError := e.Message;
    end;

    if not StreamLoad then
    begin
      SetError(meOpenFileLock, StreamError);
      // Открыть или создать файл не удалось. Подробности ошибки в StreamError.
      // Дальнейшая загрузка/создание профиля не имеет смысла, т.к. результат
      // сохранен не будет.

      Exit;
    end;
    try
      FKeySize := 0;
      if Length(Key) > 0 then
      begin
        FKey := StringToMemory(Key, FKeySize);
        if FKeySize > 0 then
        begin
          // Убирает из памяти конечный символ #0
          // см. Описание к функции StringToMemory
          FKeySize := FKeySize - 1;
        end;
      end;

      Result := LoadTox(DataInFile);
    finally
      if not Result then
      begin
        FStreamLock.Free;
      end;
    end;

  finally
    if not Result then
    begin
      FStream.Free;
    end;
  end;
end;

{ *  Пытается загрузить данные из файла в инициализированный Tox
  * }
function TProfileLoader.LoadTox(DataFile: TDataInFile): Boolean;
var
  LoadResult: Integer;
  Data: PByte;
begin
  Result := False;

  FTox := tox_new(FSettings.UseIPv6Int);
  if not Assigned(FTox) then
  begin
    SetError(meInitTox);
    Exit;
  end;

  try
    case DataFile of
      dfNone:
        begin
          Result := LoadFileNone;
        end;

      dfOriginal:
        begin
          Result := LoadFileOriginal;
        end;

      dfLock:
        begin
          Result := LoadFileLock;
        end;
    end;

    if not Result then
    begin
      Exit;
    end;


    // Загрузка данных в Tox
    if FDataSize > 0 then
    begin
      LoadResult := tox_load_encrypted(FTox, FData, FDataSize, FKey, FKeySize);
      if LoadResult = -1 then
      begin
        SetError(mePassBad);
        Result := False;
        Exit;
      end;

      FIsLoad := True;
    end
    else
    begin
      FIsLoad := True;
      if not SaveData then
      begin
        FIsLoad := False;
        Result := False;
        Exit;
      end;
    end;

    GetMem(Data, TOX_FRIEND_ADDRESS_SIZE);
    try
      tox_get_address(FTox, Data);
      FAddress.DataBin := Data;
    finally
      FreeMem(Data);
    end;
  finally
    if not Result then
    begin
      tox_kill(FTox);
    end;
  end;
end;

function TProfileLoader.LoadFileOriginal: Boolean;
var
  Size: Integer;
  Data: PByte;
begin
  Result := False;

  Data := Read(FStream, Size);
  if Size = -1 then
  begin
    //TODO: Возможно стоит получить разрешение на увеличение ограничения
    Exit;
  end;

  if Size = -2 then
  begin
    if not EventLoadLock then
    begin
      // Пользователь отказался загружать .lock файл
      Exit;
    end;

    Data := Read(FStreamLock, Size);
    if Size < 0 then
    begin
      SetError(meReadError);
      // Загрузить .lock файл также не удалось
      Exit;
    end;

    if not BackUp(False) then
    begin
      SetError(meBackUpError);
      // Ошибка при работе с резервными копиями. Дальнейшая работа с этим
      // профилем не возможна.

      Exit;
    end;
  end;

  FData := Data;
  FDataSize := Size;

  Result := True;
end;

function TProfileLoader.LoadFileLock: Boolean;
var
  Data: PByte;
  Size: Integer;
begin
  Result := False;
  Data := Read(FStreamLock, Size);

  if Size = -1 then
  begin
    // Размер файла слишком большой для загрузки
    Exit;
  end;

  if Size = -2 then
  begin
    // файл .lock загрузить не удалось. Попытка загрузить оригинальный файл
    Data := Read(FStream, Size);
    if Size < 0 then
    begin
      Exit;
    end;
  end
  else
  begin
    if not BackUp(False) then
    begin
      SetError(meBackUpError);
      // Ошибка при работе с резервными копиями. Дальнейшая работа с этим
      // профилем не возможна.

      Exit;
    end;
  end;

  FData := Data;
  FDataSize := Size;
  Result := True;
end;

{ *  Непонятно, как следует загрузить файл. TODO: Пересмотреть способ загрузки
  *  !Возможно стоит спросить у пользователя из какого файла загружаться.
  * }
function TProfileLoader.LoadFileNone: Boolean;
var
  Data: PByte;
  Size: Integer;
begin
  Result := False;

  Data := Read(FStream, Size);
  if Size = -1 then
  begin
    // Размер файла слишком большой для загрузки
    Exit;
  end;

  if Size = -2 then
  begin
    if not EventLoadLock then
    begin
      // Пользователь отказался загружать .lock файл
      Exit;
    end;

    Data := Read(FStreamLock, Size);
    if Size < 0 then
    begin
      SetError(meReadError);
      // Загрузить .lock файл также не удалось
      Exit;
    end;

    if not BackUp(False) then
    begin
      SetError(meBackUpError);
      // Ошибка при работе с резервными копиями. Дальнейшая работа с этим
      // профилем не возможна.

      Exit;
    end;
  end;

  FData := Data;
  FDataSize := Size;

  Result := True;
end;

{ *  Загрузка всех профилей из ini и составление списка
  * }
procedure TProfileLoader.LoadProfiles;
var
  Profiles: TStrings;
  p, FileName: string;
begin
  FProfilePath := FSettings.ConfigPath + 'Profile\';
  CreateDir(FProfilePath);
  FIni := TIniFile.Create(FProfilePath + 'all.ini');

  Profiles := TStringList.Create;
  try
    FIni.ReadSections(Profiles);

    for p in Profiles do
    begin
      FileName := Trim(FIni.ReadString(p, 'file', ''));

      if FileExists(FileName) then
      begin
        Add(p, FileName);
      end
      else if FileExists(FProfilePath + FileName) then
      begin
        Add(p, FProfilePath + FileName);
      end;
    end;
  finally
    Profiles.Free;
  end;
end;

{ *  Создает новый профиль с именем Name и ключом Key.
  *  Если ключ не указывается, то шифрование не сипользуется
  * }
function TProfileLoader.New(Name, Key: string): Boolean;
var
  FileName: string;
  Index: Integer;
begin
  Result := False;

  if FIsLoad then
  begin
    SetError(meHasBeenLoaded);
    // Нельзя выполнять операции со списком профилей во время работы профиля

    Exit;
  end;

  if NameExistInItems(Name) then
  begin
    SetError(meNameExist);
    Exit;
  end;

  FileName := RandomFileName;
  Index := Add(Name, FileName);
  FIni.WriteString(Name, 'file', FileName);

  Result := Load(Index, Key);
end;

function TProfileLoader.RandomFileName: string;
var
  FileName: string;
  i: Integer;
begin
  i := 1;
  repeat
    FileName := FProfilePath + 'data-' + IntToStr(i) + '.dat';
    i := i + 1;
  until not FileExistInItems(FileName);

  Result := FileName;
end;

{ *  Считывает содержимое файла в памяти и возвращает адрес начала считанных
  *  данных. В случае возникновения ошибки вернет Size = -1
  * }
function TProfileLoader.Read(Stream: TStream; var Size: Integer): PByte;
var
  Value: PByte;
begin
  Result := nil;

  Stream.Position := 0;
  Size := Stream.Size;

  if Size > 0 then
  begin
    if Size > FSettings.ProfileSizeMax then
    begin
      SetError(meLongSize);
      Size := -1;
      Exit;
    end;

    GetMem(Value, Stream.Size);
    if Stream.Read(Value^, Size) <> Size then
    begin
      SetError(meReadError);
      Size := -2;
      Exit;
    end;

    Result := Value;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TProfileLoader.SetSelected(const Value: Integer);
begin
  if (Value < 0) or (Value >= Length(FItems)) then
  begin
    FSelected := -1;

    FSelectedItem.Name := '';
    FSelectedItem.FileName := '';
  end
  else
  begin
    FSelected := Value;

    FSelectedItem.Name := FItems[FSelected].Name;
    FSelectedItem.FileName := FItems[FSelected].FileName
  end;
end;

procedure TProfileLoader.Unload;
begin
  if FIsLoad then
  begin
    tox_kill(FTox);

    if Assigned(FStream) then
      FStream.Free;

    if Assigned(FStreamLock) then
      FStreamLock.Free;

    if FDataSize > 0 then
    begin
      FreeMem(FData);
      FDataSize := 0;
    end;

    if FKeySize > 0 then
    begin
      FreeMem(FKey);
      FKeySize := 0;
    end;

    FIsLoad := False;
  end;
end;

end.
