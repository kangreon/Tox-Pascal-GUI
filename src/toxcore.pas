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
  TConnectionStatus = (csOnline, csConnecting, csOffline);

  TProcConnecting = procedure(Sender: TObject; ServerCount: Integer) of object;
  TProcFriendRequest = procedure(Sender: TObject; ClientAddress: TFriendAddress;
    HelloMessage: DataString) of object;
  TProcFriendMessage = procedure(Sender: TObject; Friend: TFriendItem;
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
    FConnectionStatus: TConnectionStatus;
    FSettings: TSettings;
    FMessageList: TMessageList;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnConnecting: TProcConnecting;
    FYourAddress: TFriendAddress;
    FStartThread: Boolean;
    FTempAddress: TFriendAddress;
    FTemtMessage: DataString;
    FTempFriend: TFriendItem;
    FTempFriendNumber: Integer;
    FTempUserStatus: TToxUserStatus;
    FTempReceipt: Integer;
    FTempServerCount: Integer;
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

    procedure DoFriendRequest(FriendAddress: TFriendAddress;
      HelloMessage: DataString);
    procedure DoFriendMessage(FriendNumber: Integer; MessageStr: DataString);
    procedure DoAction(FriendNumber: Integer; Action: DataString);
    procedure DoNameChange(FriendNumber: Integer; NewName: DataString);
    procedure DoStatusMessage(FriendNumber: Integer; NewStatus: DataString);
    procedure DoUserStatus(FriendNumber: Integer; Kind: TToxUserStatus);
    procedure DoReadReceipt(FriendNumber: Integer; Receipt: Integer);
    procedure DoConnectionStatus(FriendNumber: Integer; Status: Byte);

    procedure EventConnect;
    procedure EventConnectSyn;
    procedure EventConnecting(ServerCount: Integer);
    procedure EventConnectingSyn;
    procedure EventDisconnect;
    procedure EventDisconnectSyn;
    procedure EventFriendRequest(ClientAddress: TFriendAddress;
      HelloMessage: DataString);
    procedure EventFriendRequestSyn;
    procedure EventFriendMessage(Friend: TFriendItem; MessageStr: DataString);
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

    function IsToxConnect: Boolean;
    procedure InitTox;
    procedure InitConnection;
    procedure SaveData;
    procedure SetUserName(const Value: DataString);
    function GetUserName(FriendIndex: Integer): DataString;
    function GetUserStatus(FriendIndex: Integer): DataString;

    procedure UpdateFriends(const Clear: Boolean = True);
    procedure SetStatusMessage(const Value: DataString);
    procedure FriendListNewFriend(Sender: TObject; Friend: TFriendItem);
    function IsConnected: Boolean;
    function BootstrapFromAddress(Address: string; Port: Word;
      IsUseIpV6: Boolean; PublicKeyHex: string): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Settings: TSettings);

    function AddFriend(Address: TFriendAddress; HelloMessage: DataString;
      out FriendNumber: Integer): TToxFaerr;
    function AddFriendNoRequest(Address: TFriendAddress): Boolean;
    procedure SendMessage(Friend: TFriendItem; Text: DataString);
    procedure SetUserStatus(Status: TToxUserStatus);

    procedure StartTox;
    procedure StopTox;

    property ConnectState: TConnectionStatus read FConnectionStatus;
    property FriendList: TFriendList read FFriendList;
    property IsLoadLibrary: Boolean read FIsLoadLibrary;
    property MessageList: TMessageList read FMessageList;
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

{ TToxCore }

{ *  Обработчики событий Tox
  * }
{$I ToxEvents.inc}

{ *  Инициализация библиотеки Tox, загрузка начальных данных, соединение
  *  с базой данных, загрузка пользователей.
  * }
constructor TToxCore.Create(Settings: TSettings);
begin
  inherited Create(True);

  FSettings := Settings;
  FYourAddress := TFriendAddress.Create;

  FConnectionStatus := csOffline;
  FStartThread := False;

  InitTox;

  FDataBase := TDataBase.Create(FSettings);
  FDataBase.LoadBase;

  FFriendList := TFriendList.Create(FDataBase.FriendBase, FYourAddress,
    FUserName, FStatusMessage);
  FFriendList.OnNewFriend := FriendListNewFriend;

  FMessageList := TMessageList.Create(FDataBase, FFriendList);

  UpdateFriends(True);

  FUserName := FFriendList.MyItem.UserName;
  FStatusMessage := FFriendList.MyItem.StatusMessage;
end;

procedure TToxCore.StartTox;
begin
  if FConnectionStatus = csOffline then
    FConnectionStatus := csConnecting;

  if not FStartThread then
  begin
    Start;
  end;
end;

procedure TToxCore.StopTox;
begin
  FConnectionStatus := csOffline;
end;

procedure TToxCore.Execute;
var
  conn_try: Integer;
begin
  {$IFNDEF FPC}inherited;{$ENDIF}
  FStartThread := True;

  conn_try := -1;
  while not Self.Terminated do
  begin
    if (FConnectionStatus = csOffline) then
    begin
      // Остановка работы TOX
      tox_kill(FTox);
      FTox := nil;

      EventDisconnect;

      // Ожидание действий пользователя
      while (not Self.Terminated) and (FConnectionStatus = csOffline) do
      begin
        Sleep(50);
      end;

      if Self.Terminated then
        Exit;

      InitTox;
    end;

    Inc(conn_try);
    if (FConnectionStatus <> csOnline) and (not IsConnected) and (conn_try mod 100 = 0) then
    begin
      InitConnection;
    end
    else if (FConnectionStatus <> csOnline) and IsConnected then
    begin
      FConnectionStatus := csOnline;

      // Изменение имени пользователя и статуса
      UserName := FUserName;
      StatusMessage := FStatusMessage;

      EventConnect;
    end
    else if (FConnectionStatus = csOnline) and not IsConnected then
    begin
      FConnectionStatus := csOffline;
      EventDisconnect;
    end;

    tox_do(FTox);
    Sleep(20);
  end;
end;

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

  if not IsToxConnect then
    Exit;

  data := GetUtf8Text(HelloMessage, data_length);
  try
    ret := tox_add_friend(FTox, Address.DataBin, data, data_length);
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

{ *  Подтверждение добавления человка в друзья
  * }
function TToxCore.AddFriendNoRequest(Address: TFriendAddress): Boolean;
begin
  if IsToxConnect then
    Result := tox_add_friend_norequest(FTox, Address.DataBin) <> -1
  else
    Result := False;
end;

{ *  Отправка сообщения Text другу Friend
  * }
procedure TToxCore.SendMessage(Friend: TFriendItem; Text: DataString);
var
  Data: PByte;
  DataLength: Integer;
begin
  if IsToxConnect and Assigned(Friend) and Friend.IsFriend then
  begin
    FMessageList.SetMessage(Friend, Text, True);

    Data := GetUtf8Text(Text, DataLength);
    try
      tox_send_message(FTox, Friend.Number, Data, DataLength);
    finally
      FreeMem(Data);
    end;
  end;
end;

{ *  Устанавливает статус для себя
  * }
procedure TToxCore.SetUserStatus(Status: TToxUserStatus);
begin
  if IsToxConnect then
    tox_set_user_status(FTox, Integer(Status));
end;

{ *  Выбор серверов из списка и подключение к ним
  * }
procedure TToxCore.InitConnection;
var
  Item: TServerItem;
  ServerCount: Integer;
  i, c: Integer;
  IsAdd: Boolean;
begin
  ServerCount := 0;

  c := FSettings.ServerList.Count;
  for i := 0 to c - 1 do
  begin
    Item := FSettings.ServerList.Item[i];

    IsAdd := BootstrapFromAddress(Item.Ip, Item.NewPort, FSettings.UseIPv6,
      Item.Key);

    if IsAdd then
      Inc(ServerCount);
  end;

  // Событие начала соединения с выбранными серверами
  EventConnecting(ServerCount);
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

  FTox := tox_new(FSettings.UseIPv6Int);
  if not Assigned(FTox) then
  begin
    FIsLoadLibrary := False;
    Exit;
  end
  else
    FIsLoadLibrary := True;

  // Start callback function
  tox_callback_friend_request(FTox, OnFriendRequest_, Self);
  tox_callback_friend_message(FTox, OnFriendMessage_, Self);
  tox_callback_friend_action(FTox, OnAction_, Self);
  tox_callback_name_change(FTox, OnNameChange_, Self);
  tox_callback_status_message(FTox, OnStatusMessage_, Self);
  tox_callback_user_status(FTox, OnUserStatus_, Self);
  tox_callback_read_receipt(FTox, OnReadReceipt_, Self);
  tox_callback_connection_status(FTox, OnConnectionStatus_, Self);

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
  data := GetMemory(TOX_FRIEND_ADDRESS_SIZE);
  try
    tox_get_address(FTox, data);
    FYourAddress.DataBin := data;
  finally
    FreeMemory(data);
  end;

  // Получение собственного имени
  if Trim(FUserName) = '' then
  begin
    data := GetMemory(TOX_MAX_NAME_LENGTH);
    try
      size := tox_get_self_name(FTox, data, TOX_MAX_NAME_LENGTH);
      if size > 0 then
        FUserName := GetTextFromUTF8Byte(data, size)
      else
        FUserName := '';
    finally
      FreeMemory(data);
    end;
  end;

  if Trim(FUserName) = '' then
    FUserName := FSettings.DefUserName;

  // Получение собственного статуса
  if Trim(FStatusMessage) = '' then
  begin
    data := GetMemory(TOX_MAX_STATUSMESSAGE_LENGTH);
    try
      size := tox_get_self_status_message(FTox, data, TOX_MAX_STATUSMESSAGE_LENGTH);
      if size > 0 then
        FStatusMessage := GetTextFromUTF8Byte(data, size)
      else
        FStatusMessage := '';
    finally
      FreeMemory(data);
    end;
  end;

  if Trim(FUserName) = '' then
    FUserName := FSettings.DefUserName;
end;

{ *  Обновляет список друзей путем загрузки всего списка и сверкой с уже
  *  загруженным
  * }
procedure TToxCore.UpdateFriends(const Clear: Boolean);
var
  Data: Pointer;
  i: Integer;
  ClientId: TClientId;
  Item: TFriendItem;
  StatusMessage: DataString;
begin
  // Get user list
  Data := GetMemory(TOX_CLIENT_ID_SIZE);
  FriendList.BeginUpdate;
  try
    if Clear then
      FriendList.ClearFriend;

    i := 0;
    while tox_get_client_id(FTox, i, data) = 0 do
    begin
      ClientId := TClientId.Create(Data);
      try
        Item := FriendList.Add(ClientId, i);
        Item.UserName := GetUserName(i);

        StatusMessage := GetUserStatus(i);
        if StatusMessage <> '' then
          Item.StatusMessage := StatusMessage;

        Item.StatusMessage := GetUserStatus(i);

      finally
        ClientId.Free;
        Inc(i);
      end;
    end;
  finally
    FriendList.EndUpdate;
    FreeMemory(data);
  end;

  FFriendList.MyItem.Addressg.DataHex := FYourAddress.DataHex;
end;

procedure TToxCore.SaveData;
var
  size: Integer;
  savedata: PByte;
begin
  if not Assigned(FTox) then
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

procedure TToxCore.SetStatusMessage(const Value: DataString);
var
  Data: PByte;
  DataLength: Integer;
begin
  FStatusMessage := Value;
  FFriendList.MyItem.StatusMessage := Value;

  if IsToxConnect then
  begin
    Data := GetUtf8Text(Value, DataLength);
    try
      tox_set_status_message(FTox, Data, DataLength)
    finally
      FreeMemory(Data);
    end;

    SaveData;
  end;
end;

procedure TToxCore.SetUserName(const Value: DataString);
var
  Data: PByte;
  DataLength: Integer;
begin
  FUserName := Value;
  FFriendList.MyItem.UserName := Value;

  if IsToxConnect then
  begin
    Data := GetUtf8Text(Value, DataLength);
    try
      tox_set_name(FTox, Data, DataLength);
    finally
      FreeMemory(Data);
    end;

    SaveData;
  end;
end;

{ *  Добавление адреса начальной загрузки в Tox
  * }
function TToxCore.BootstrapFromAddress(Address: string; Port: Word;
  IsUseIpV6: Boolean; PublicKeyHex: string): Boolean;
var
  UseIpV6: Byte;
  PublicKey: PByte;
begin
  UseIpV6 := 0;
  if IsUseIpV6 then
    UseIpV6 := 1;

  PublicKey := hex_string_to_bin(PublicKeyHex);
  try
    Result := tox_bootstrap_from_address(FTox, PAnsiChar(AnsiString(Address)),
      UseIpV6, Port, PublicKey) = 1;
  finally
    FreeMemory(PublicKey);
  end;
end;

{ *  Собтие возникает при изменении количества пользователей в списке.
  * }
procedure TToxCore.FriendListNewFriend(Sender: TObject; Friend: TFriendItem);
begin
  FMessageList.InsertFriend(Friend);
end;

{ *  Возврашает имя друга FriendIndex
  * }
function TToxCore.GetUserName(FriendIndex: Integer): DataString;
var
  DataSize: Integer;
  NameData: PByte;
begin
  Result := '';

  NameData := GetMemory(TOX_MAX_NAME_LENGTH);
  try
    DataSize := tox_get_name(FTox, FriendIndex, NameData);
    if DataSize > 0 then
    begin
      Result := GetTextFromUTF8Byte(NameData, DataSize);
    end;
  finally
    FreeMemory(NameData);
  end;
end;

{ *  Возвращает текст статуса друга FriendIndex
  * }
function TToxCore.GetUserStatus(FriendIndex: Integer): DataString;
var
  DataSize, MessageSize: Integer;
  StatusData: PByte;
begin
  Result := '';

  DataSize := tox_get_status_message_size(FTox, FriendIndex);
  if DataSize > 0 then
  begin
    StatusData := GetMemory(DataSize);
    try
      MessageSize := tox_get_status_message(FTox, FriendIndex, StatusData, DataSize);
      if MessageSize > 0 then
      begin
        Result := GetTextFromUTF8Byte(StatusData, MessageSize);
      end;
    finally
      FreeMemory(StatusData)
    end;
  end;
end;

{ *  Возвращает состояние подключения к сети
  * }
function TToxCore.IsConnected: Boolean;
begin
  if FStartThread and Assigned(FTox) then
    Result := (tox_isconnected(FTox) = 1)
  else
    Result := False;
end;

function TToxCore.IsToxConnect: Boolean;
begin
  Result := IsConnected and (FConnectionStatus = csOnline);
end;



{ *  ВЫЗОВЫ СОБЫТИЙ ЭТОГО КЛАССА
  * }

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

procedure TToxCore.EventConnecting(ServerCount: Integer);
begin
  FTempServerCount := ServerCount;
  Synchronize(EventConnectingSyn);
end;

procedure TToxCore.EventConnectingSyn;
begin
  if Assigned(FOnConnecting) then
    FOnConnecting(Self, FTempServerCount);
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

procedure TToxCore.EventFriendMessage(Friend: TFriendItem;
  MessageStr: DataString);
begin
  FTempFriend := Friend;
  FTemtMessage := MessageStr;
  Synchronize(EventFriendMessageSyn);
end;

procedure TToxCore.EventFriendMessageSyn;
begin
  if Assigned(FOnFriendMessage) then
    FOnFriendMessage(Self, FTempFriend, FTemtMessage);
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


{ *  ПОЛОУЧЕНИЕ СОБЫТИЙ ОТ TOX
  *}

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
var
  Friend: TFriendItem;
begin
  Friend := FFriendList.ItemFriend[FriendNumber];
  if Assigned(Friend) then
  begin
    FMessageList.SetMessage(Friend, MessageStr, False);
    EventFriendMessage(Friend, MessageStr);
  end;
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

end.

