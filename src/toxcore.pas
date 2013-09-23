//  toxcore.pas
//
//  Класс-обертка над заголовочным файлом для библиотеки libtox
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit toxcore;

interface
{$I tox.inc}

uses
  {$I tox-uses.inc}
  Classes, Settings, ServerList, SysUtils, libtox, ClientAddress, StringUtils,
  FriendList;

type
  TConnectState = (csOnline, csConnecting, csOffline);

  TProcConnecting = procedure(Sender: TObject; Server: TServerItem) of object;
  TProcFriendRequest = procedure(Sender: TObject; ClientAddress: TClientAddress;
    HelloMessage: DataString) of object;
  TProcFriendMessage = procedure(Sender: TObject; FriendNumber: Integer;
    MessageStr: DataString) of object;
  TProcAction = procedure(Sender: TObject; FriendNumber: Integer;
    Action: DataString) of object;
  TProcNameChange = procedure(Sender: TObject; FriendName: Integer;
    NewName: DataString) of object;
  TProcStatusMessage = procedure(Sender: TObject; FriendNumber: Integer;
    NewStatus: DataString) of object;
  TProcUserStatus = procedure(Sender: TObject; FriendNumber: Integer;
    Kind: TToxUserStatus) of object;
  TProcReadReceipt = procedure(Sender: TObject; FriendNumber: Integer;
    Receipt: Integer) of object;
  TProcConnectioStatus = procedure(Sender: TObject; FriendNumber: Integer;
    Status: Byte) of object;

  TToxCore = class(TThread)
  private
    FTox: TTox;
    FConfigPath: string;
    FConnectState: TConnectState;
    FSettings: TSettings;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FSelectedServer: TServerItem;
    FOnConnecting: TProcConnecting;
    FYourAddress: TClientAddress;
    FStartThread: Boolean;
    FStatusOnline: Boolean;
    FTempAddress: TClientAddress;
    FTemtMessage: DataString;
    FTempFriendNumber: Integer;
    FTempUserStatus: TToxUserStatus;
    FTempReceipt: Integer;
    FTempStatus: Byte;
    FOnFriendRequest: TProcFriendRequest;
    FOnFriendMessage: TProcFriendMessage;
    FOnAction: TProcAction;
    FOnNameChange: TProcNameChange;
    FOnStatusMessage: TProcStatusMessage;
    FOnUserStatus: TProcUserStatus;
    FOnReadReceipt: TProcReadReceipt;
    FOnConnectioStatus: TProcConnectioStatus;
    FFriendList: TFriendList;
    FIsLoadLibrary: Boolean;
    procedure EventConnect;
    procedure EventConnectSyn;
    procedure EventConnecting(Server: TServerItem);
    procedure EventConnectingSyn;
    procedure EventDisconnect;
    procedure EventDisconnectSyn;
    procedure EventFriendRequest(ClientAddress: TClientAddress;
      HelloMessage: DataString);
    procedure EventFriendRequestSyn;
    procedure EventFriendMessage(FriendNumber: Integer; MessageStr: DataString);
    procedure EventFriendMessageSyn;
    procedure EventAction(FriendNumber: Integer; Action: DataString);
    procedure EventActionSyn;
    procedure EventNameChange(FriendNumber: Integer; NewName: DataString);
    procedure EventNameChangeSyn;
    procedure EventStatusMessage(FriendNumber: Integer; NewStatus: DataString);
    procedure EventStatusMessageSyn;
    procedure EventUserStatus(FriendNumber: Integer; Kind: TToxUserStatus);
    procedure EventUserStatusSyn;
    procedure EventReadReceipt(FriendNumber: Integer; Receipt: Integer);
    procedure EventReadReceiptSyn;
    procedure EventConnectioStatus(FriendNumber: Integer; Status: Byte);
    procedure EventConnectioStatusSyn;
    procedure InitTox;
    procedure InitConnection;
    procedure SaveData;
    function GetUserName: DataString;
    procedure SetUserName(const Value: DataString);
    procedure DoFriendRequest(ClientAddress: TClientAddress;
      HelloMessage: DataString);
    procedure DoFriendMessage(FriendNumber: Integer; MessageStr: DataString);
    procedure DoAction(FriendNumber: Integer; Action: DataString);
    procedure DoNameChange(FriendNumber: Integer; NewName: DataString);
    procedure DoStatusMessage(FriendNumber: Integer; NewStatus: DataString);
    procedure DoUserStatus(FriendNumber: Integer; Kind: TToxUserStatus);
    procedure DoReadReceipt(FriendNumber: Integer; Receipt: Integer);
    procedure DoConnectionStatus(FriendNumber: Integer; Status: Byte);
  protected
    procedure Execute; override;
  public
    constructor Create(Settings: TSettings);

    function AddFriend(Address: TClientAddress; HelloMessage: DataString;
      out FriendNumber: Integer): TToxFaerr;
    function AddFriendNoRequest(Address: TClientAddress): Boolean;
    procedure SendMessage(FriendNumber: Integer; Text: DataString);
    procedure StartTox;
    procedure StopTox;

    property ConnectState: TConnectState read FConnectState;
    property FriendList: TFriendList read FFriendList;
    property IsLoadLibrary: Boolean read FIsLoadLibrary;
    property SelectedServer: TServerItem read FSelectedServer;
    property UserName: DataString read GetUserName write SetUserName;
    property YourAddress: TClientAddress read FYourAddress;

    property OnAction: TProcAction read FOnAction write FOnAction;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnConnecting: TProcConnecting read FOnConnecting write FOnConnecting;
    property OnConnectioStatus: TProcConnectioStatus read FOnConnectioStatus write FOnConnectioStatus;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnFriendMessage: TProcFriendMessage read FOnFriendMessage write FOnFriendMessage;
    property OnFriendRequest: TProcFriendRequest read FOnFriendRequest write FOnFriendRequest;
    property OnNameChange: TProcNameChange read FOnNameChange write FOnNameChange;
    property OnStatusMessage: TProcStatusMessage read FOnStatusMessage write FOnStatusMessage;
    property OnUserStatus: TProcUserStatus read FOnUserStatus write FOnUserStatus;
    property OnReadReceipt: TProcReadReceipt read FOnReadReceipt write FOnReadReceipt;
  end;

implementation

procedure OnFriendRequest_(public_key: PByte; data: PByte; length: Word;
  UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  ClientAddress: TClientAddress;
  HelloMessage: DataString;
begin
  ToxThread := TToxCore(UserData);

  ClientAddress := TClientAddress.Create(public_key);
  HelloMessage := GetTextFromUTF8Byte(data, length);

  ToxThread.DoFriendRequest(ClientAddress, HelloMessage);
end;

procedure OnFriendMessage_(tox: TTox; FriendNumber: Integer; message: PByte;
  length: Word; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  MessageStr: DataString;
begin
  ToxThread := TToxCore(UserData);

  MessageStr := GetTextFromUTF8Byte(message, length);

  ToxThread.DoFriendMessage(FriendNumber, MessageStr);
end;

procedure OnAction_(tox: TTox; FriendNumber: Integer; Action: PByte;
  length: Word; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  MessageStr: DataString;
begin
  ToxThread := TToxCore(UserData);

  MessageStr := GetTextFromUTF8Byte(Action, length);

  ToxThread.DoAction(FriendNumber, MessageStr);
end;

procedure OnNameChange_(tox: TTox; FriendNumber: Integer; NewName: PByte;
  length: Word; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  NewNameStr: DataString;
begin
  ToxThread := TToxCore(UserData);

  NewNameStr := GetTextFromUTF8Byte(NewName, length);

  ToxThread.DoNameChange(FriendNumber, NewNameStr);
end;

procedure OnStatusMessage_(tox: TTox; FriendNumber: Integer; NewStatus: PByte;
  length: Word; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  NewStatusStr: DataString;
begin
  ToxThread := TToxCore(UserData);

  NewStatusStr := GetTextFromUTF8Byte(NewStatus, length);

  ToxThread.DoStatusMessage(FriendNumber, NewStatusStr);
end;

procedure OnUserStatus_(tox: TTox; FriendNumber: Integer; Kind: TToxUserStatus;
  UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
begin
  ToxThread := TToxCore(UserData);
  ToxThread.DoUserStatus(FriendNumber, Kind);
end;

procedure OnReadReceipt_(tox: TTox; FriendNumber: Integer; Receipt: Integer;
  UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
begin
  ToxThread := TToxCore(UserData);
  ToxThread.DoReadReceipt(FriendNumber, Receipt);
end;

procedure OnConnectionStatus_(tox: TTox; FriendNumber: Integer;
  Status: Byte; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
begin
  ToxThread := TToxCore(UserData);
  ToxThread.DoConnectionStatus(FriendNumber, Status);
end;

{ TToxCore }


// Отправка запроса на дружбу с приветственным сообщением
function TToxCore.AddFriend(Address: TClientAddress; HelloMessage: DataString;
  out FriendNumber: Integer): TToxFaerr;
var
  data_length: Integer;
  data: PByte;
  ret: Integer;
begin
  FriendNumber := -1;
  Result := tfUnknown;

  data := GetUtf8Text(HelloMessage, data_length);
  try
    ret := tox_addfriend(FTox, Address.DataBin, data, data_length);
    if (ret < 0) and (ret >= -8) then
    begin
      Result := TToxFaerr(ret);
    end
    else if ret >= 0 then
    begin
      Result := tfFriendNumber;
      FriendNumber := ret;
    end;

  finally
    FreeMemory(data);
  end;

  SaveData;
end;

function TToxCore.AddFriendNoRequest(Address: TClientAddress): Boolean;
begin
  Result := tox_addfriend_norequest(FTox, Address.DataBin) <> -1;
end;

constructor TToxCore.Create(Settings: TSettings);
begin
  inherited Create(True);

  FSettings := Settings;
  FYourAddress := TClientAddress.Create;
  FFriendList := TFriendList.Create;

  FConnectState := csOffline;
  FStatusOnline := False;
  FStartThread := False;

  // Загрузить Tox только в случае успешной загрузки библиотеки Tox
  FIsLoadLibrary := ToxLoaded;
  if IsLoadLibrary then
    InitTox;
end;

procedure TToxCore.DoAction(FriendNumber: Integer; Action: DataString);
begin
  EventAction(FriendNumber, Action);
end;

// Событие, возникаемое когда пользователь захродит в сеть или
// выходит из нее
procedure TToxCore.DoConnectionStatus(FriendNumber: Integer; Status: Byte);
var
  Item: TFriendItem;
begin
  EventConnectioStatus(FriendNumber, Status);

  Item := FFriendList.Item[FriendNumber];
  if Assigned(Item) then
    Item.Online := Status = 1;
end;

procedure TToxCore.DoFriendMessage(FriendNumber: Integer;
  MessageStr: DataString);
begin
  EventFriendMessage(FriendNumber, MessageStr);
end;

procedure TToxCore.DoFriendRequest(ClientAddress: TClientAddress;
  HelloMessage: DataString);
begin
  if ClientAddress.ValidAddress then
  begin
    EventFriendRequest(ClientAddress, HelloMessage);
    SaveData;
  end
  else
    raise Exception.Create('Error client id');     //TODO: Изменить способ уведомления об ошибке
end;

// Событие, вызываемое при изменении имени пользоватея одного из
// списка друзей.
procedure TToxCore.DoNameChange(FriendNumber: Integer; NewName: DataString);
var
  Item: TFriendItem;
begin
  EventNameChange(FriendNumber, NewName);

  // Изменение имени пользователя в основном списке
  Item := FFriendList.Item[FriendNumber];
  if Assigned(Item) then
    Item.UserName := NewName;

  SaveData;
end;

procedure TToxCore.DoReadReceipt(FriendNumber, Receipt: Integer);
begin
  EventReadReceipt(FriendNumber, Receipt);
end;

// Событие изменения пользователем статусного сообщения
procedure TToxCore.DoStatusMessage(FriendNumber: Integer;
  NewStatus: DataString);
var
  Item: TFriendItem;
begin
  EventStatusMessage(FriendNumber, NewStatus);

  Item := FFriendList.Item[FriendNumber];
  if Assigned(Item) then
    Item.StatusMessage := NewStatus;
end;

procedure TToxCore.DoUserStatus(FriendNumber: Integer; Kind: TToxUserStatus);
var
  Item: TFriendItem;
begin
  EventUserStatus(FriendNumber, Kind);

  Item := FFriendList.Item[FriendNumber];
  if Assigned(Item) then
    Item.UserStatus := Kind;
end;

procedure TToxCore.EventAction(FriendNumber: Integer; Action: DataString);
begin
  FTempFriendNumber := FriendNumber;
  FTemtMessage := Action;
  Synchronize(EventActionSyn);
end;

procedure TToxCore.EventActionSyn;
begin
  if Assigned(FOnAction) then
    FOnAction(Self, FTempFriendNumber, FTemtMessage);
end;

procedure TToxCore.EventConnect;
begin
  Synchronize(EventConnectSyn);
end;

procedure TToxCore.EventConnecting(Server: TServerItem);
begin
  if Assigned(Server) then
    FSelectedServer := Server.Clone;

  Synchronize(EventConnectingSyn);
end;

procedure TToxCore.EventConnectingSyn;
begin
  if Assigned(FOnConnecting) then
    FOnConnecting(Self, FSelectedServer);
end;

procedure TToxCore.EventConnectioStatus(FriendNumber: Integer; Status: Byte);
begin
  FTempFriendNumber := FriendNumber;
  FTempStatus := Status;

  Synchronize(EventConnectioStatusSyn);
end;

procedure TToxCore.EventConnectioStatusSyn;
begin
  if Assigned(FOnConnectioStatus) then
    FOnConnectioStatus(Self, FTempFriendNumber, FTempStatus);
end;

procedure TToxCore.EventConnectSyn;
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TToxCore.EventDisconnect;
begin
  Synchronize(EventDisconnectSyn);
end;

procedure TToxCore.EventDisconnectSyn;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TToxCore.EventFriendMessage(FriendNumber: Integer;
  MessageStr: DataString);
begin
  FTempFriendNumber := FriendNumber;
  FTemtMessage := MessageStr;
  Synchronize(EventFriendMessageSyn);
end;

procedure TToxCore.EventFriendMessageSyn;
begin
  if Assigned(FOnFriendMessage) then
    FOnFriendMessage(Self, FTempFriendNumber, FTemtMessage);
end;

procedure TToxCore.EventFriendRequest(ClientAddress: TClientAddress;
  HelloMessage: DataString);
begin
  FTempAddress := ClientAddress;
  FTemtMessage := HelloMessage;

  Synchronize(EventFriendRequestSyn);
end;

procedure TToxCore.EventFriendRequestSyn;
begin
  if Assigned(FOnFriendRequest) then
    FOnFriendRequest(Self, FTempAddress, FTemtMessage);
end;

procedure TToxCore.EventNameChange(FriendNumber: Integer; NewName: DataString);
begin
  FTempFriendNumber := FriendNumber;
  FTemtMessage := NewName;

  Synchronize(EventNameChangeSyn);
end;

procedure TToxCore.EventNameChangeSyn;
begin
  if Assigned(FOnNameChange) then
    FOnNameChange(Self, FTempFriendNumber, FTemtMessage);
end;

procedure TToxCore.EventReadReceipt(FriendNumber, Receipt: Integer);
begin
  FTempFriendNumber := FriendNumber;
  FTempReceipt := Receipt;

  Synchronize(EventReadReceiptSyn);
end;

procedure TToxCore.EventReadReceiptSyn;
begin
  if Assigned(FOnReadReceipt) then
    FOnReadReceipt(Self, FTempFriendNumber, FTempReceipt);
end;

procedure TToxCore.EventStatusMessage(FriendNumber: Integer;
  NewStatus: DataString);
begin
  FTempFriendNumber := FriendNumber;
  FTemtMessage := NewStatus;

  Synchronize(EventStatusMessageSyn);
end;

procedure TToxCore.EventStatusMessageSyn;
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, FTempFriendNumber, FTemtMessage);
end;

procedure TToxCore.EventUserStatus(FriendNumber: Integer; Kind: TToxUserStatus);
begin
  FTempFriendNumber := FriendNumber;
  FTempUserStatus := Kind;

  Synchronize(EventUserStatusSyn);
end;

procedure TToxCore.EventUserStatusSyn;
begin
  if Assigned(FOnUserStatus) then
    FOnUserStatus(Self, FTempFriendNumber, FTempUserStatus);
end;

procedure TToxCore.Execute;
var
  dht_on, conn_err: Boolean;
  conn_try: Integer;
begin
  inherited;
  FStartThread := True;

  dht_on := False;
  conn_err := False;
  conn_try := -1;
  while not Self.Terminated do
  begin
    if not FStatusOnline then
    begin
      // Остановка работы TOX
      tox_kill(FTox);

      // Создание события остановки
      dht_on := False;
      FConnectState := csOffline;
      EventDisconnect;

      // Ожидание действий пользователя
      while (not Self.Terminated) and (not FStatusOnline) do
      begin
        Sleep(50);
      end;

      if Self.Terminated then
        Exit;

      EventConnecting(nil);
      InitTox;
    end;


    Inc(conn_try);
    if (not dht_on) and (tox_isconnected(FTox) <> 1) and (conn_try mod 100 = 0) then
    begin
      if not conn_err then
      begin
        InitConnection;
      end;
    end
    else if (not dht_on) and (tox_isconnected(FTox) = 1) then
    begin
      dht_on := True;
      FConnectState := csOnline;
      EventConnect;
    end
    else if (dht_on) and (tox_isconnected(FTox) = 0) then
    begin
      dht_on := False;
      FConnectState := csOffline;
      EventDisconnect;
    end;

    tox_do(FTox);
    Sleep(60);
  end;          
end;

function TToxCore.GetUserName: DataString;
begin
  Result := FSettings.UserName;
end;

procedure TToxCore.InitConnection;
var
  Item: TServerItem;
  Data: Pointer;
  count: Integer;
  ret: Integer;
begin
  count := 0;
  repeat
    Inc(count);
    Item := FSettings.ServerList.RandomItem;

    Data := hex_string_to_bin(Item.Key);
    try
      ret := tox_bootstrap_from_address(FTox, PAnsiChar(AnsiString(Item.Ip)), 0, Item.NewPort, Data);
    finally
      FreeMemory(Data);
    end;
  until (count > 10) or (ret <> 0);

  // Событие начала соединения с выбранным сервером
  FConnectState := csConnecting;
  EventConnecting(Item);
end;

procedure TToxCore.InitTox;
var
  data, name: PByte;
  size: Integer;
  UserName: DataString;
  i: Integer;
begin
  FConfigPath := FSettings.ConfigPath;

  FTox := tox_new();

  // Start callback function
  tox_callback_friendrequest(FTox, OnFriendRequest_, Self);
  tox_callback_friendmessage(FTox, OnFriendMessage_, Self);
  tox_callback_action(FTox, OnAction_, Self);
  tox_callback_namechange(FTox, OnNameChange_, Self);
  tox_callback_statusmessage(FTox, OnStatusMessage_, Self);
  tox_callback_userstatus(FTox, OnUserStatus_, Self);
  tox_callback_read_receipt(FTox, OnReadReceipt_, Self);
  tox_callback_connectionstatus(FTox, OnConnectionStatus_, Self);

  // Load/save client data
  data := FSettings.LoadData(size);
  try
    if size > 0 then
      tox_load(FTox, data, size)
    else
    begin
      Self.SaveData;
    end;
  finally
    FreeMemory(data);
  end;

  // Getting your address
  data := GetMemory(FRIEND_ADDRESS_SIZE);
  try
    tox_getaddress(FTox, data);
    FYourAddress.DataBin := data;
  finally
    FreeMemory(data);
  end;

  // Get user list
  data := GetMemory(TOX_CLIENT_ID_SIZE);
  name := GetMemory(TOX_MAX_NAME_LENGTH);
  try
    i := 0;
    while tox_getclient_id(FTox, i, data) = 0 do
    begin
      try
        FriendList.Add(i, data);

        size := tox_getname(FTox, i, name);
        if size > 1 then
        begin
          FFriendList.Item[i].UserName := GetTextFromUTF8Byte(name, size);
        end;

      finally
        Inc(i);
      end;
    end;
  finally
    FreeMem(name);
    FreeMemory(data);
  end;

  UserName := FSettings.UserName;
end;

procedure TToxCore.SaveData;
var
  size: Integer;
  savedata: PByte;
begin
  size := tox_size(FTox);
  savedata := GetMemory(size);
  try
    if size > 0 then
    begin
      tox_save(FTox, savedata);
      FSettings.SaveData(savedata, size);
    end;
  finally
    FreeMemory(savedata);
  end;
end;

procedure TToxCore.SendMessage(FriendNumber: Integer; Text: DataString);
var
  Data: PByte;
  DataLength: Integer;
begin
  Data := GetUtf8Text(Text, DataLength);
  try
    tox_sendmessage(FTox, FriendNumber, Data, DataLength);
  finally
    FreeMem(Data);
  end;
end;

procedure TToxCore.SetUserName(const Value: DataString);
var
  Data: PByte;
  DataLength: Integer;
begin
  FSettings.UserName := Value;

  Data := GetUtf8Text(Value, DataLength);
  try
    tox_setname(FTox, Data, DataLength);
  finally
    FreeMemory(Data);
  end;

  SaveData;
end;

procedure TToxCore.StartTox;
begin
  FStatusOnline := True;

  if not FStartThread then
  begin
    {$IFDEF NEW_DELPHI}
    Start;
    {$ELSE}
    Resume;
    {$ENDIF}
  end;
end;

procedure TToxCore.StopTox;
begin
  FStatusOnline := False;
end;

end.
