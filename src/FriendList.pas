//  FriendList.pas
//
//  Осуществляет хранение и изменение списка друзей. Создает события на
//  изменения состояний.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit FriendList;

interface
  {$I tox.inc}

uses
  Classes, SysUtils, ClientAddress, StringUtils, libtox;

type
  TProcUpdateItem = procedure(Sender: TObject; Item: Integer) of object;

  TFriendItem = class
  private
    FUserName: DataString;
    FClientId: TClientId;
    FStatusMessage: DataString;
    FOnline: Boolean;
    FUserStatus: TToxUserStatus;
    FOnUpdate: TNotifyEvent;
    FNumber: Integer;
    FData: Pointer;
    FIsFriend: Boolean;
    FLocaleName: DataString;
    FAddress: TFriendAddress;
    FIsAddressExist: Boolean;
    FBaseId: Integer;
    procedure EventUpdate;
    procedure SetUserName(const Value: DataString);
    procedure SetStatusMessage(const Value: DataString);
    procedure SetOnline(const Value: Boolean);
    procedure SetUserStatus(const Value: TToxUserStatus);
  public
    constructor Create(IsFriend: Boolean; Client: TClientId;
      Number: Integer = -1); overload;
    constructor Create(IsFriend: Boolean; Address: TFriendAddress;
      Number: Integer = -1); overload;
    destructor Destroy; override;

    property Addressg: TFriendAddress read FAddress;
    property BaseId: Integer read FBaseId write FBaseId;
    property ClientId: TClientId read FClientId;
    property Data: Pointer read FData write FData;
    property IsAddressExist: Boolean read FIsAddressExist;
    property IsFriend: Boolean read FIsFriend write FIsFriend;
    property LocaleName: DataString read FLocaleName write FLocaleName;
    property Number: Integer read FNumber write FNumber;
    property Online: Boolean read FOnline write SetOnline;
    property StatusMessage: DataString read FStatusMessage write SetStatusMessage;
    property UserName: DataString read FUserName write SetUserName;
    property UserStatus: TToxUserStatus read FUserStatus write SetUserStatus;

    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  TFriendList = class
  private type
    TFriendItemList = TList;
  private
    FItems: TFriendItemList;
    FStopUpdate: Boolean;
    FOnUpdateItem: TProcUpdateItem;
    FOnNewItem: TNotifyEvent;
    FMyItem: TFriendItem;
    function GetCount: Integer;
    function GetItemFriend(Index: Integer): TFriendItem;
    procedure ItemOnUpdate(Sender: TObject);
    procedure LoadFromDatabase(MyId: TFriendAddress; Name, Status: DataString);
    function AddWithoutFriend(Address: TFriendAddress; UserName,
      LocaleName: DataString): TFriendItem;
    function GetItemWithAddress(Address: TFriendAddress): TFriendItem;
    procedure UnfriendNumber(Number: Integer);
    procedure UnfriendClients;
    function GetItemWithNumber(Number: Integer): TFriendItem;
    function GetItemWithClientId(ClientId: TClientId): TFriendItem;
  public
    constructor Create(MyId: TFriendAddress; Name, Status: DataString);
    destructor Destroy; override;

    function Add(Address: TFriendAddress; Number: Integer): TFriendItem; overload;
    function Add(Client: TClientId; Number: Integer): TFriendItem; overload;
    procedure BeginUpdate;
    procedure ClearFriend;
    procedure EndUpdate;
    function FindByAddress(FindValue: DataString): TFriendItem;
    function FindByClient(FindValue: DataString): TFriendItem;

    property Count: Integer read GetCount;
    property ItemFriend[Index: Integer]: TFriendItem read GetItemFriend;
    property MyItem: TFriendItem read FMyItem;

    property OnNewItem: TNotifyEvent read FOnNewItem write FOnNewItem;
    property OnUpdateItem: TProcUpdateItem read FOnUpdateItem write FOnUpdateItem;
  end;

implementation

{ TFriendList }

{ *  Создает новую запись для пользователя. Определяет тип создаваемой записи.
  *
  *  IsFriend - Принадлежит ли создаваемый пользователь к списку друзей
  *  Address - Адрес пользователя
  *  Number - Используется в случае принадлежности пользователя к списку
  *           друзей для ассоциации с Core.
  * }
constructor TFriendItem.Create(IsFriend: Boolean; Address: TFriendAddress;
  Number: Integer);
begin
  if not Assigned(Address) then
    raise Exception.Create('Не использовать эту процедуру для создания пользователя без Address');

  FClientId := TClientId.Create(Address);
  FAddress := Address.Clone;

  FIsFriend := IsFriend;
  FNumber := Number;
  FUserStatus := usInvalid;
end;

constructor TFriendItem.Create(IsFriend: Boolean; Client: TClientId;
  Number: Integer);
begin
  FIsAddressExist := Assigned(Addressg);

  FClientId := Client.Clone;

  FIsFriend := IsFriend;
  FNumber := Number;
  FUserStatus := usInvalid;
end;

destructor TFriendItem.Destroy;
begin
  FClientId.Free;
  FAddress.Free;
  inherited;
end;

procedure TFriendItem.EventUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TFriendItem.SetOnline(const Value: Boolean);
begin
  if FOnline <> Value then
  begin
    FOnline := Value;
    EventUpdate;
  end;
end;

procedure TFriendItem.SetStatusMessage(const Value: DataString);
begin
  if FStatusMessage <> Value then
  begin
    FStatusMessage := Value;
    EventUpdate;
  end;
end;

procedure TFriendItem.SetUserName(const Value: DataString);
begin
  if FUserName <> Value then
  begin
    FUserName := Value;
    EventUpdate;
  end;
end;

procedure TFriendItem.SetUserStatus(const Value: TToxUserStatus);
begin
  if FUserStatus <> Value then
  begin
    FUserStatus := Value;
    EventUpdate;
  end;
end;

{ TFiendList }

function TFriendList.Add(Address: TFriendAddress; Number: Integer): TFriendItem;
var
  Item: TFriendItem;
begin
  UnfriendNumber(Number);

  Item := GetItemWithAddress(Address);
  try
    Item.Number := Number;
    Item.IsFriend := True;

    if Assigned(FOnNewItem) then
      FOnNewItem(Self);
  finally
    Result := Item;
  end;
end;

function TFriendList.Add(Client: TClientId; Number: Integer): TFriendItem;
var
  Item: TFriendItem;
begin
  UnfriendNumber(Number);

  Item := GetItemWithClientId(Client);
  if Assigned(Item) then
  begin
    try
      Item.Number := Number;
      Item.IsFriend := True;

      if Assigned(FOnNewItem) then
        FOnNewItem(Self);
    finally
      Result := Item;
    end;
  end
  else
    //TODO: Адреса для пользователя не найдено. Создать пользователя без адреса
  begin
    Item := TFriendItem.Create(True, Client, Number);
    Item.OnUpdate := ItemOnUpdate;

    FItems.Add(Item);

    Result := Item;
  end;
end;

procedure TFriendList.BeginUpdate;
begin
  FStopUpdate := True;
end;

procedure TFriendList.ClearFriend;
begin
  UnfriendClients;

  if (not FStopUpdate) and Assigned(FOnNewItem) then
    FOnNewItem(Self);
end;

constructor TFriendList.Create(MyId: TFriendAddress; Name, Status: DataString);
begin
  FItems := TFriendItemList.Create;
  FStopUpdate := False;

  LoadFromDatabase(MyId, Name, Status);
end;

destructor TFriendList.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TFriendList.EndUpdate;
begin
  if FStopUpdate then
  begin
    FStopUpdate := False;

    if Assigned(FOnNewItem) then
      FOnNewItem(Self);
  end;
end;

function TFriendList.FindByAddress(FindValue: DataString): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
  Address: TFriendAddress;
  Client: TClientId;
begin
  Result := nil;

  Address := TFriendAddress.Create(FindValue);
  Client := TClientId.Create(Address);
  try
    if Address.ValidAddress and Client.ValidAddress then
      for PItem in FItems do
      begin
        Item := TFriendItem(PItem);
        if Item.ClientId.IsCompare(Client) then
        begin
          Result := Item;
          Break;
        end;
      end;
  finally
    Client.Free;
    Address.Free;
  end;
end;

function TFriendList.FindByClient(FindValue: DataString): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  Result := nil;

  for PItem in FItems do
  begin
    Item := TFriendItem(PItem);
    if Item.ClientId.IsCompare(FindValue) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TFriendList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFriendList.GetItemFriend(Index: Integer): TFriendItem;
begin
  Result := GetItemWithNumber(Index);
end;

procedure TFriendList.ItemOnUpdate(Sender: TObject);
begin
  if Assigned(Sender) and Assigned(FOnUpdateItem) then
  begin
    if TFriendItem(Sender).IsFriend then
    begin
      FOnUpdateItem(Self, TFriendItem(Sender).Number);
    end
    else
    begin
      //TODO: Реализовать
    end;
  end;
end;

{ *  Убирает всех пользователей из списка друзей
  * }
procedure TFriendList.UnfriendClients;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  for PItem in FItems do
  begin
    Item := TFriendItem(PItem);

    if (Item.IsFriend) then
    begin
      Item.IsFriend := False;
      Item.Number := -1;
    end;
  end;
end;

function TFriendList.GetItemWithNumber(Number: Integer): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  Result := nil;

  for PItem in FItems do
  begin
    Item := TFriendItem(PItem);

    if (Item.Number = Number) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

{ *  Убирает пользователя из списка друзей
  * }
procedure TFriendList.UnfriendNumber(Number: Integer);
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  for PItem in FItems do
  begin
    Item := TFriendItem(PItem);

    if (Item.Number = Number) then
    begin
      Item.IsFriend := False;
      Item.Number := -1;
    end;
  end;
end;

{ *  Находит в списке пользователя с идентификатором или создает его.
  * }
function TFriendList.GetItemWithAddress(Address: TFriendAddress): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
  IsExistFriend: Boolean;
  Client: TClientId;
begin
  IsExistFriend := False;
  Item := nil;

  Client := TClientId.Create(Address);
  try
    if Client.ValidAddress then
    begin
      for PItem in FItems do
      begin
        Item := TFriendItem(PItem);

        if Assigned(Item) and (Item.ClientId.IsCompare(Client)) then
        begin
          IsExistFriend := True;
          Break;
        end;
      end;
    end;
  finally
    Client.Free;
  end;

  if not IsExistFriend then
  begin
    Item := TFriendItem.Create(False, Address);
    FItems.Add(Item);
    Result := Item;
  end
  else
    Result := Item;
end;

{ *  Находит в списке пользователя с идентификатором или создает его.
  * }
function TFriendList.GetItemWithClientId(ClientId: TClientId): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  Result := nil;

  for PItem in FItems do
  begin
    Item := TFriendItem(PItem);

    if Assigned(Item) and (Item.ClientId.IsCompare(ClientId)) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

{ *  Добавляет пользователя в список без пометки активности. Эта пометка
  *  дает понять, добавлен ли пользователь в список друзей.
  *
  *  ClientId - уникальный идентификатор пользователя
  *  UserName - Имя добавляемого пользователя
  *  LocaleName - Имя, которое устанавливается самостоятельно
  * }
function TFriendList.AddWithoutFriend(Address: TFriendAddress;
  UserName, LocaleName: DataString): TFriendItem;
var
  Item: TFriendItem;
begin
  Item := GetItemWithAddress(Address);
  Item.UserName := UserName;
  Item.LocaleName := LocaleName;
  Item.OnUpdate := ItemOnUpdate;

  Result := Item;
end;

{ *  Загружает список пользователей из базы данных
  * }
procedure TFriendList.LoadFromDatabase(MyId: TFriendAddress; Name, Status: DataString);
var
  Address: TFriendAddress;
begin
  //TODO: Реализовать настоящую загрузку из базы

  Address := TFriendAddress.Create;
  try
    Address.DataHex := '1111111111111111111111111111111111111111111111111111111111111111111111111111';
    AddWithoutFriend(Address, 'Dima', 'Kangreon');
  finally
    Address.Free;
  end;

  Address := TFriendAddress.Create;
  try
    Address.DataHex := '2222222222222222222222222222222222222222222222222222222222222222222222222222';
    AddWithoutFriend(Address, 'Andrey', 'A2');
  finally
    Address.Free;
  end;
end;

end.
