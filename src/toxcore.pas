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
  FriendList, MessageList, FriendItem, DataBase;

type
  TConnectState = (csOnline, csConnecting, csOffline);

  TProcConnecting = procedure(Sender: TObject; Server: TServerItem) of object;
  TProcFriendRequest = procedure(Sender: TObject; ClientAddress: TFriendAddress;
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
    FDataBase: TDataBase;
    FTox: TTox;
    FConfigPath: string;
    FConnectState: TConnectState;
    FSettings: TSettings;
    FMessageList: TMessageList;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FSelectedServer: TServerItem;
    FOnConnecting: TProcConnecting;
    FYourAddress: TFriendAddress;
    FStartThread: Boolean;
    FStatusOnline: Boolean;
    FTempAddress: TFriendAddress;
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
    FUserName: DataString;
    FStatusMessage: DataString;
    procedure EventConnect;
    procedure EventConnectSyn;
    procedure EventConnecting(Server: TServerItem);
    procedure EventConnectingSyn;
    procedure EventDisconnect;
    procedure EventDisconnectSyn;
    procedure EventFriendRequest(ClientAddress: TFriendAddress;
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
    procedure SetUserName(const Value: DataString);
    procedure DoFriendRequest(FriendAddress: TFriendAddress;
      HelloMessage: DataString);
    procedure DoFriendMessage(FriendNumber: Integer; MessageStr: DataString);
    procedure DoAction(FriendNumber: Integer; Action: DataString);
    procedure DoNameChange(FriendNumber: Integer; NewName: DataString);
    procedure DoStatusMessage(FriendNumber: Integer; NewStatus: DataString);
    procedure DoUserStatus(FriendNumber: Integer; Kind: TToxUserStatus);
    procedure DoReadReceipt(FriendNumber: Integer; Receipt: Integer);
    procedure DoConnectionStatus(FriendNumber: Integer; Status: Byte);
    procedure UpdateFriends(const Clear: Boolean = True);
    procedure SetStatusMessage(const Value: DataString);
  protected
    procedure Execute; override;
  public
    constructor Create(Settings: TSettings);

    function AddFriend(Address: TFriendAddress; HelloMessage: DataString;
      out FriendNumber: Integer): TToxFaerr;
    function AddFriendNoRequest(Address: TFriendAddress): Boolean;
    procedure SendMessage(FriendNumber: Integer; Text: DataString);
    procedure SetUserStatus(Status: TToxUserStatus);
    procedure StartTox;
    procedure StopTox;

    property ConnectState: TConnectState read FConnectState;
    property FriendList: TFriendList read FFriendList;
    property IsLoadLibrary: Boolean read FIsLoadLibrary;
    property MessageList: TMessageList read FMessageList;
    property SelectedServer: TServerItem read FSelectedServer;
    property StatusMessage: DataString read FStatusMessage write SetStatusMessage;
    property UserName: DataString read FUserName write SetUserName;
    property YourAddress: TFriendAddress read FYourAddress;

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
  ClientAddress: TFriendAddress;
  HelloMessage: DataString;
begin
  ToxThread := TToxCore(UserData);

  ClientAddress := TFriendAddress.Create(public_key);
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


{ *  Отправка пользователю запроса на добавление в список друзей.
  *
  *  Address - Идентификатор пользователя, добавляемого в друзья
  *  HelloMessage - Приветственное сообщение. Обязательно должно присутствовать.
  *  out FriendNumber - Номер добавленного пользователя.
  *
  *  Возвращает результат добавления пользователя в друзья.
  * }
function TToxCore.AddFriend(Address: TFriendAddress; HelloMessage: DataString;
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

      FFriendList.Add(Address, FriendNumber);
    end;

  finally
    FreeMemory(data);
  end;

  SaveData;
end;

function TToxCore.AddFriendNoRequest(Address: TFriendAddress): Boolean;
begin
  Result := tox_addfriend_norequest(FTox, Address.DataBin) <> -1;
end;

{ *  Инициализация библиотеки Tox, загрузка начальных данных, соединение
  *  с базой данных, загрузка пользователей.
  * }
constructor TToxCore.Create(Settings: TSettings);
begin
  inherited Create(True);

  FSettings := Settings;
  FYourAddress := TFriendAddress.Create;

  FConnectState := csOffline;
  FStatusOnline := False;
  FStartThread := False;

  // Загрузить Tox только в случае успешной загрузки библиотеки Tox
  if True then
  begin
    InitTox;

    FDataBase := TDataBase.Create(FSettings);
    FDataBase.LoadBase;

    FFriendList := TFriendList.Create(FDataBase.FriendBase, FYourAddress, FUserName, FStatusMessage);
    FMessageList := TMessageList.Create(FDataBase, FFriendList);

    UpdateFriends(True);
  end;
end;

procedure TToxCore.DoAction(FriendNumber: Integer; Action: DataString);
begin
  EventAction(FriendNumber, Action);
end;

{ *  Событие возникает при подключении или отключении пользователя
  *  из списка.
  * }
procedure TToxCore.DoConnectionStatus(FriendNumber: Integer; Status: Byte);
var
  Item: TFriendItem;
begin
  EventConnectioStatus(FriendNumber, Status);

  Item := FFriendList.ItemFriend[FriendNumber];
  if Assigned(Item) then
  begin
    Item.Online := Status = 1
  end
  else
  begin
    UpdateFriends(False);
  end;
end;

{ *  Событие возникает при получении сообщения от пользователя
  * }
procedure TToxCore.DoFriendMessage(FriendNumber: Integer;
  MessageStr: DataString);
begin
  EventFriendMessage(FriendNumber, MessageStr);
end;

procedure TToxCore.DoFriendRequest(FriendAddress: TFriendAddress;
  HelloMessage: DataString);
begin
  if FriendAddress.ValidAddress then
  begin
    EventFriendRequest(FriendAddress, HelloMessage);
    SaveData;
  end;
end;

{ *  Событие возникает при изменении имени пользователя из списка друзей
  * }
procedure TToxCore.DoNameChange(FriendNumber: Integer; NewName: DataString);
var
  Item: TFriendItem;
begin
  EventNameChange(FriendNumber, NewName);

  Item := FFriendList.ItemFriend[FriendNumber];
  if Assigned(Item) then
    Item.UserName := NewName
  else
    UpdateFriends(False);

  SaveData;
end;

{ *  Событие возникает после доставки отправленного сообщения пользователю
  * }
procedure TToxCore.DoReadReceipt(FriendNumber, Receipt: Integer);
begin
  EventReadReceipt(FriendNumber, Receipt);
end;

{ *  Событие возникает после изменения пользователем из списка сдрузей
  *  своего статуса.
  * }
procedure TToxCore.DoStatusMessage(FriendNumber: Integer;
  NewStatus: DataString);
var
  Item: TFriendItem;
begin
  EventStatusMessage(FriendNumber, NewStatus);

  Item := FFriendList.ItemFriend[FriendNumber];
  if Assigned(Item) then
    Item.StatusMessage := NewStatus
  else
    UpdateFriends(False);
end;

procedure TToxCore.DoUserStatus(FriendNumber: Integer; Kind: TToxUserStatus);
var
  Item: TFriendItem;
begin
  EventUserStatus(FriendNumber, Kind);

  Item := FFriendList.ItemFriend[FriendNumber];
  if Assigned(Item) then
    Item.UserStatus := Kind
  else
    UpdateFriends(False);
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
var
  Item: TFriendItem;
begin
  FTempFriendNumber := FriendNumber;
  FTempStatus := Status;

  Item := FFriendList.ItemFriend[FriendNumber];
  if Assigned(Item) and (Status = 0) then
    Item.UserStatus := usInvalid;

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

procedure TToxCore.EventFriendRequest(ClientAddress: TFriendAddress;
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
  {$IFNDEF FPC}inherited;{$ENDIF}
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
    Sleep(20);
  end;          
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

{ *   Инициальзация библиотеки Tox, загрузка настроек из файла, получение
  *   собственного адреса и загрузка списка друзей.
  *
  * }
procedure TToxCore.InitTox;
var
  data: PByte;
  size: Integer;
begin
  FConfigPath := FSettings.ConfigPath;

  FTox := tox_new(0);
  if not Assigned(FTox) then
  begin
    FIsLoadLibrary := False;
    Exit;
  end
  else
    FIsLoadLibrary := True;

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

  // Получение собственного имени
  data := GetMemory(TOX_MAX_NAME_LENGTH);
  try
    size := tox_getselfname(FTox, data, TOX_MAX_NAME_LENGTH);
    if size >= 1 then
      FUserName := GetTextFromUTF8Byte(data, size)
    else
      FUserName := '';
  finally
    FreeMemory(data);
  end;

  // Получение собственного статуса
  data := GetMemory(TOX_MAX_STATUSMESSAGE_LENGTH);
  try
    size := tox_copy_self_statusmessage(FTox, data, TOX_MAX_STATUSMESSAGE_LENGTH);
    if size >= 1 then
      FStatusMessage := GetTextFromUTF8Byte(data, size)
    else
      FStatusMessage := '';
  finally
    FreeMemory(data);
  end;
end;

{ *  Обновляет список друзей путем загрузки всего списка и сверкой с уже
  *  загруженным
  * }
procedure TToxCore.UpdateFriends(const Clear: Boolean);
var
  Data, Name, Status: Pointer;
  size: Integer;
  i: Integer;
  ClientId: TClientId;
  Item: TFriendItem;
begin
  // Get user list
  data := GetMemory(TOX_CLIENT_ID_SIZE);
  name := GetMemory(TOX_MAX_NAME_LENGTH);
  FriendList.BeginUpdate;
  try
    if Clear then
      FriendList.ClearFriend;

    i := 0;
    while tox_getclient_id(FTox, i, data) = 0 do
    begin
      ClientId := TClientId.Create(Data);
      try
        Item := FriendList.Add(ClientId, i);

        // Получение имени пользователя
        size := tox_getname(FTox, i, name);
        if size > 0 then
          Item.UserName := GetTextFromUTF8Byte(name, size);

        // Копирование статуса
        size := tox_get_statusmessage_size(FTox, i);
        Status := GetMemory(size);
        try
          tox_copy_statusmessage(FTox, i, Status, size);
          Item.StatusMessage := GetTextFromUTF8Byte(Status, size);
        finally
          FreeMemory(Status);
        end;
      finally
        ClientId.Free;
        Inc(i);
      end;
    end;
  finally
    FriendList.EndUpdate;
    FreeMem(name);
    FreeMemory(data);
  end;
end;

procedure TToxCore.SaveData;
var
  size: Integer;
  savedata: PByte;
begin
  if FConnectState <> csOnline then
    Exit;


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

procedure TToxCore.SetStatusMessage(const Value: DataString);
var
  Data: PByte;
  DataLength: Integer;
begin
  FStatusMessage := Value;
  FFriendList.MyItem.StatusMessage := Value;
  Data := GetUtf8Text(Value, DataLength);
  try
    tox_set_statusmessage(FTox, Data, DataLength)
  finally
    FreeMemory(Data);
  end;

  SaveData;
end;

procedure TToxCore.SetUserName(const Value: DataString);
var
  Data: PByte;
  DataLength: Integer;
begin
  FUserName := Value;
  FFriendList.MyItem.UserName := Value;
  Data := GetUtf8Text(Value, DataLength);
  try
    if FConnectState = csOnline then
      tox_setname(FTox, Data, DataLength);
  finally
    FreeMemory(Data);
  end;

  SaveData;
end;

procedure TToxCore.SetUserStatus(Status: TToxUserStatus);
begin
  if FConnectState = csOnline then
    tox_set_userstatus(FTox, Integer(Status));
end;

procedure TToxCore.StartTox;
begin
  FStatusOnline := True;

  if not FStartThread then
  begin
    Start;
  end;
end;

procedure TToxCore.StopTox;
begin
  FStatusOnline := False;
end;

end.
