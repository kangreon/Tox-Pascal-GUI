//  ServerList.pas
//
//  Отвечает за загрузку и сохранение DHT серверов и ключей к ним
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ServerList;

interface
{$I tox.inc}

uses
  {$IFDEF FPC}
    Sockets, FileUtil,
  {$ELSE}
    WinSock,
  {$ENDIF}
  Classes, SysUtils, Math;

type
  TServerItem = class
  private
    FOnUpdate: TNotifyEvent;
    FIp: string;
    FName: string;
    FKey: string;
    FPort: Word;
    FNewPort: Word;
    procedure EventUpdate;
    procedure SetIp(const Value: string);
    procedure SetKey(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPort(const Value: Word);
  public
    constructor Create(Name, Key, Ip: string; Port: Word);
    destructor Destroy; override;

    function Clone: TServerItem;

    property Name: string read FName write SetName;
    property Key: string read FKey write SetKey;
    property Ip: string read FIp write SetIp;
    property Port: Word read FPort write SetPort;
    property NewPort: Word read FNewPort;

    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  TServerList = class
  private
    FServerItem: array of TServerItem;
    FServerItemCount: Integer;
    FStream: TFileStream;
    procedure AddServerToList(Name, Key, Ip: string; Port: Word);
    procedure FileSave;
    function FileOpen: Boolean;
    function OpenFile(FileName: string): Boolean;
    procedure LoadDefaultList;
    procedure ServerItemUpdate(Sender: TObject);
    function StreamLoadString(out s: string): Boolean;
    function StreamLoadWord(out w: Word): Boolean;
    procedure StreamSaveString(s: string);
    procedure StreamSaveWord(w: Word);
    function GetItem(Index: Integer): TServerItem;
    function GetRandomItem: TServerItem;

  public
    constructor Create(ConfigPath: string);
    destructor Destroy; override;

    procedure Add(Name, Key, Ip: string; Port: Word);
    procedure Delete(Item: TServerItem); overload;
    procedure Delete(Index: Integer); overload;

    property Count: Integer read FServerItemCount;
    property Item[Index: Integer]: TServerItem read GetItem;
    property RandomItem: TServerItem read GetRandomItem;
  end;

implementation

const
  FILE_TYPE: Word = $2421;

{ TServerList }

procedure TServerList.Add(Name, Key, Ip: string; Port: Word);
begin
  AddServerToList(Name, Key, Ip, Port);
  FileSave;
end;

procedure TServerList.AddServerToList(Name, Key, Ip: string; Port: Word);
var
  Server: TServerItem;
  l: Integer;
begin
  Server := TServerItem.Create(Name, Key, Ip, Port);
  Server.OnUpdate := ServerItemUpdate;
  l := Length(FServerItem);
  if l <= FServerItemCount then
  begin
    SetLength(FServerItem, FServerItemCount + 10);
  end;

  FServerItem[FServerItemCount] := Server;
  FServerItemCount := FServerItemCount + 1;
end;

constructor TServerList.Create(ConfigPath: string);
const
  MAX_TRY_OPEN = 10;
var
  TryOpen: Integer;
  FileName: string;
begin
  TryOpen := 0;

  repeat
    if TryOpen = 0 then
      FileName := 'ServerList.txt'
    else
      FileName := Format('ServerList(%d).txt', [TryOpen]);

    Inc(TryOpen);
  until (TryOpen >= MAX_TRY_OPEN) or OpenFile(ConfigPath + FileName);

  if not (Assigned(FStream) and FileOpen) then
  begin
    LoadDefaultList;
  end;
end;

procedure TServerList.Delete(Item: TServerItem);
var
  i, Index: Integer;
begin
  Index := -1;
  
  for i := 0 to FServerItemCount - 1 do
  begin
    if FServerItem[i] = Item then
    begin
      Index := i;
      Break;
    end;
  end;

  Delete(Index);
end;

procedure TServerList.Delete(Index: Integer);
var
  i: Integer;
begin
  if (Index >= 0) and (Index < FServerItemCount) then
  begin
    for i := Index to FServerItemCount - 2 do
    begin
      FServerItem[i] := FServerItem[i + 1];
    end;

    Dec(FServerItemCount);
  end;
end;

destructor TServerList.Destroy;
begin
  if Assigned(FStream) then
    FStream.Free;
    
  inherited;
end;

function TServerList.FileOpen: Boolean;
var
  w, port: Word;
  i, c: Integer;
  s, key, name, ip: string;
begin
  Result := False;
  FStream.Position := 0;

  // Load and valid FileType
  if (not StreamLoadWord(w)) or (w <> FILE_TYPE) then
    Exit;

  // Load items count
  if (not StreamLoadWord(w)) or (w <= 0) then
    Exit;

  c := w;
  for i := 1 to c do
  begin
    // Load Name Server
    if not StreamLoadString(s) then
      Exit;

    name := s;

    // Load key
    if (not StreamLoadString(s)) or (s = '') then
      Exit;

    key := s;

    // Load ip
    if (not StreamLoadString(s)) or (s = '') then
      Exit;

    ip := s;

    // load port
    if (not StreamLoadWord(w)) or (w = 0) then
      Exit;

    port := w;

    AddServerToList(name, key, ip, port);
  end;

  Result := True;  
end;

procedure TServerList.FileSave;
var
  s: string;
  w: Word;
  i, c: Integer;
begin
  FStream.Size := 0;
  FStream.Position := 0;

  StreamSaveWord(FILE_TYPE);

  w := Word(FServerItemCount);
  StreamSaveWord(w);

  c := w;
  for i := 0 to c - 1 do
  begin
    s := FServerItem[i].Name;
    StreamSaveString(s);

    s := FServerItem[i].Key;
    StreamSaveString(s);

    s := FServerItem[i].Ip;
    StreamSaveString(s);

    w := FServerItem[i].Port;
    StreamSaveWord(w);
  end;
end;

function TServerList.GetItem(Index: Integer): TServerItem;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FServerItem[Index]
  else
    raise Exception.Create(Format('Item ''%d'' not exist', [Index]));
end;

function TServerList.GetRandomItem: TServerItem;
begin
  if FServerItemCount = 0 then
    LoadDefaultList;

  Result := Item[RandomRange(0, FServerItemCount)];
end;

procedure TServerList.LoadDefaultList;
begin
  AddServerToList('stqism',  '5CD7EB176C19A2FD840406CD56177BB8E75587BB366F7BB3004B19E3EDC04143', '192.184.81.118', 33445);
  AddServerToList('JmanGuy', '890D9C546EC2B72476EDFEB86AFEAE229FE8D0686D1ED75E8F7BFC56DCC81C26', '66.74.30.125', 33445);
  AddServerToList('stal',    '728925473812C7AAC482BE7250BCCAD0B8CB9F737BF3D42ABD34459C1768F854', '198.46.136.167', 33445);
  AddServerToList('platos',    'B24E2FB924AE66D023FE1E42A2EE3B432010206F751A2FFD3E297383ACF1572E', '66.175.223.88', 33445);
end;

function TServerList.OpenFile(FileName: string): Boolean;
var
  OpenMode: Word;
begin
  Result := False;

  if FileExists(FileName) { *Converted from FileExists*  } then
  begin
    OpenMode := fmOpenReadWrite or fmShareDenyWrite;
  end
  else
  begin
    OpenMode := fmCreate or fmOpenReadWrite or fmShareDenyWrite;
  end;

  try
    FStream := TFileStream.Create(FileName, OpenMode);
    Result := True;
  except
    FStream := nil;
  end;
end;

procedure TServerList.ServerItemUpdate(Sender: TObject);
begin
  FileSave;
end;

function TServerList.StreamLoadString(out s: string): Boolean;
var
  StringSize: Word;
begin
  Result := False;
  
  if StreamLoadWord(StringSize) then
  begin
    if (FStream.Position + StringSize <= FStream.Size) and (StringSize > 0) then
    begin
      SetLength(s, StringSize);
      Result := FStream.Read(s[1], StringSize) = StringSize;
    end;
  end;
end;

function TServerList.StreamLoadWord(out w: Word): Boolean;
begin
  Result := False;

  if FStream.Position + SizeOf(w) > FStream.Size then
    Exit;

  Result := FStream.Read(w, SizeOf(w)) = SizeOf(w);
end;

procedure TServerList.StreamSaveString(s: string);
var
  StringSize: Integer;
  w: Word;
begin
  StringSize := Length(s);
  w := Word(StringSize);

  StreamSaveWord(w);
  if w > 0 then
  begin
    FStream.Write(s[1], w);
  end;
end;

procedure TServerList.StreamSaveWord(w: Word);
begin
  FStream.Write(w, SizeOf(w));
end;

{ TServerItem }

function TServerItem.Clone: TServerItem;
var
  Item: TServerItem;
begin
  Item := TServerItem.Create(FName, FKey, FIp, FPort);
  Result := Item;
end;

constructor TServerItem.Create(Name, Key, Ip: string; Port: Word);
begin
  FName := Name;
  FKey := Key;
  FIp := Ip;
  FPort := Port;
  FNewPort := htons(FPort);
end;

destructor TServerItem.Destroy;
begin
  FOnUpdate := nil;
  inherited;
end;

procedure TServerItem.EventUpdate;
begin
  FNewPort := htons(Port);
  
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TServerItem.SetIp(const Value: string);
begin
  if FIp <> Value then
  begin
    FIp := Value;
    EventUpdate;
  end;
end;

procedure TServerItem.SetKey(const Value: string);
begin
  if FKey <> Value then
  begin
    FKey := Value;
    EventUpdate;
  end;
end;

procedure TServerItem.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    EventUpdate;
  end;
end;

procedure TServerItem.SetPort(const Value: Word);
begin
  if FPort <> Value then
  begin
    FPort := Value;
    EventUpdate;
  end;
end;

end.

