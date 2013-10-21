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
  Classes, SysUtils, ClientAddress, StringUtils, libtox, SQliteTable3,
  FriendItem, FriendBase;

type
  TProcUpdateItem = procedure(Sender: TObject; Item: Integer) of object;

  TFriendList = class
  private
    FBase: TFriendBase;
    FStopUpdate: Boolean;
    FOnUpdateItem: TProcUpdateItem;
    FMyItem: TFriendItem;
    FOnNewFriend: TProcNewFriend;
    FOnUpdateList: TNotifyEvent;
    function GetCount: Integer;
    function GetItemFriend(Index: Integer): TFriendItem;
    function GetItemWithAddress(Address: TFriendAddress): TFriendItem;
    procedure UnfriendNumber(Number: Integer);
    procedure UnfriendClients;
    function GetItemWithNumber(Number: Integer): TFriendItem;
    function GetItemWithClientId(ClientId: TClientId): TFriendItem;
    function GetItem: TFriendItemList;
    procedure BaseItemUpdate(Sender: TObject);
    procedure BaseNewItem(Sender: TObject; Item: TFriendItem);
    procedure EventUpdateList;
  public
    constructor Create(Base: TFriendBase; MyId: TFriendAddress; Name,
      Status: DataString);
    destructor Destroy; override;

    function Add(Address: TFriendAddress; Number: Integer): TFriendItem; overload;
    function Add(Client: TClientId; Number: Integer): TFriendItem; overload;
    procedure BeginUpdate;
    procedure ClearFriend;
    procedure EndUpdate;
    function FindByAddress(FindValue: TFriendAddress): TFriendItem;
    function FindByClient(FindValue: TClientId): TFriendItem;

    property Count: Integer read GetCount;
    property Item: TFriendItemList read GetItem;
    property ItemFriend[Index: Integer]: TFriendItem read GetItemFriend;
    property MyItem: TFriendItem read FMyItem;

    property OnNewFriend: TProcNewFriend read FOnNewFriend write FOnNewFriend;
    property OnUpdateItem: TProcUpdateItem read FOnUpdateItem write FOnUpdateItem;
    property OnUpdateList: TNotifyEvent read FOnUpdateList write FOnUpdateList;
  end;

implementation

const
  EVENT_NEW_FRIEND  = 1;

{ TFiendList }

constructor TFriendList.Create(Base: TFriendBase; MyId: TFriendAddress;
  Name, Status: DataString);
begin
  FBase := Base;
  FBase.OnNewItem := BaseNewItem;
  FBase.OnUpdateItem := BaseItemUpdate;

  FStopUpdate := False;

  FMyItem := GetItemWithAddress(MyId);
  if not Assigned(FMyItem) then
    FMyItem := FBase.Add(Name, '', Status, MyId);

  FMyItem.IsMy := True;
end;

destructor TFriendList.Destroy;
begin

  inherited;
end;

procedure TFriendList.BaseItemUpdate(Sender: TObject);
begin
  if Assigned(FOnUpdateItem) then
    FOnUpdateItem(Sender, -1);
end;

{ *  Событие возникает при добавлении в базу нового пользователя Sender.
  * }
procedure TFriendList.BaseNewItem(Sender: TObject; Item: TFriendItem);
begin
  if Assigned(FOnNewFriend) and Assigned(Sender) then
    FOnNewFriend(Self, Item);

  EventUpdateList;
end;

function TFriendList.Add(Address: TFriendAddress; Number: Integer): TFriendItem;
var
  Client: TClientId;
begin
  Client := TClientId.Create(Address);
  try
    Result := Add(Client, Number);
  finally
    Client.Free;
  end;
end;

function TFriendList.Add(Client: TClientId; Number: Integer): TFriendItem;
var
  Item: TFriendItem;
  IsNewFriend: Boolean;
begin
  UnfriendNumber(Number);

  Item := GetItemWithClientId(Client);

  IsNewFriend := not Assigned(Item);
  if IsNewFriend then
    Item := FBase.Add('', '', '', Client);

  Item.IsFriend := True;
  Item.Number := Number;
  Result := Item;

  EventUpdateList;
end;

procedure TFriendList.BeginUpdate;
begin
  FStopUpdate := True;
  FBase.BeginUpdate;
end;

procedure TFriendList.ClearFriend;
begin
  UnfriendClients;
end;

procedure TFriendList.EndUpdate;
begin
  if FStopUpdate then
  begin
    FStopUpdate := False;
    FBase.EndUpdate;
  end;
end;

procedure TFriendList.EventUpdateList;
begin
  if Assigned(FOnUpdateList) then
    FOnUpdateList(Self);
end;

function TFriendList.FindByAddress(FindValue: TFriendAddress): TFriendItem;
var
  Client: TClientId;
begin
  Client := TClientId.Create(FindValue);
  try
    Result := FindByClient(Client);
  finally
    Client.Free;
  end;
end;

function TFriendList.FindByClient(FindValue: TClientId): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  Result := nil;

  for PItem in FBase.Friends do
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
  Result := FBase.Friends.Count;
end;

function TFriendList.GetItem: TFriendItemList;
begin
  Result := FBase.Friends;
end;

function TFriendList.GetItemFriend(Index: Integer): TFriendItem;
begin
  Result := GetItemWithNumber(Index);
end;

{ *  Убирает всех пользователей из списка друзей
  * }
procedure TFriendList.UnfriendClients;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  for PItem in FBase.Friends do
  begin
    Item := TFriendItem(PItem);

    if (Item.IsFriend) then
    begin
      Item.IsFriend := False;
      Item.Number := -1;
    end;
  end;

  EventUpdateList;
end;

function TFriendList.GetItemWithNumber(Number: Integer): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  Result := nil;

  for PItem in FBase.Friends do
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
  for PItem in FBase.Friends do
  begin
    Item := TFriendItem(PItem);

    if (Item.Number = Number) then
    begin
      Item.IsFriend := False;
      Item.Number := -1;
    end;
  end;

  EventUpdateList;
end;

{ *  Находит в списке пользователя с идентификатором
  * }
function TFriendList.GetItemWithAddress(Address: TFriendAddress): TFriendItem;
var
  Client: TClientId;
begin
  Client := TClientId.Create(Address);
  try
    Result := GetItemWithClientId(Client);
  finally
    Client.Free;
  end;
end;

{ *  Находит в списке пользователя с идентификатором.
  * }
function TFriendList.GetItemWithClientId(ClientId: TClientId): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  Result := nil;

  for PItem in FBase.Friends do
  begin
    Item := TFriendItem(PItem);

    if Assigned(Item) and (Item.ClientId.IsCompare(ClientId)) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

end.
