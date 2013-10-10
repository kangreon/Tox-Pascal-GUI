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
  Classes, ClientAddress, StringUtils, libtox;

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
    procedure EventUpdate;
    procedure SetUserName(const Value: DataString);
    procedure SetStatusMessage(const Value: DataString);
    procedure SetOnline(const Value: Boolean);
    procedure SetUserStatus(const Value: TToxUserStatus);
  public
    constructor Create(IsFriend: Boolean; ClientId: TClientId; Number: Integer = -1);
    destructor Destroy; override;

    property ClientId: TClientId read FClientId;
    property Data: Pointer read FData write FData;
    property Number: Integer read FNumber write FNumber;
    property IsFriend: Boolean read FIsFriend write FIsFriend;
    property LocaleName: DataString read FLocaleName write FLocaleName;
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
    function GetCount: Integer;
    function GetItem(Index: Integer): TFriendItem;
    procedure ItemOnUpdate(Sender: TObject);
    procedure LoadFromDatabase;
    function AddWithoutFriend(ClientId: TClientId; UserName,
      LocaleName: DataString): TFriendItem;
    function GetItemWithClientId(ClientId: TClientId): TFriendItem;
    procedure UnfriendNumber(Number: Integer);
    procedure UnfriendClients;
    function GetItemWithNumber(Number: Integer): TFriendItem;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(ClientId: TClientId; Number: Integer): TFriendItem;
    procedure BeginUpdate;
    procedure ClearFriend;
    procedure EndUpdate;

    property Count: Integer read GetCount;
    property Item[Index: Integer]: TFriendItem read GetItem;

    property OnNewItem: TNotifyEvent read FOnNewItem write FOnNewItem;
    property OnUpdateItem: TProcUpdateItem read FOnUpdateItem write FOnUpdateItem;
  end;

implementation

{ TFriendList }

{ *  Создает новую запись для пользователя. Определяет тип создаваемой записи.
  *
  *  IsFriend - Принадлежит ли создаваемый пользователь к списку друзей
  *  ClientId - Уникальный идентификатор пользователя
  *  Number - Используется в случае принадлежности пользователя к списку
  *           друзей для ассоциации с Core.
  * }
constructor TFriendItem.Create(IsFriend: Boolean; ClientId: TClientId;
  Number: Integer);
begin
  FIsFriend := IsFriend;
  FClientId := ClientId.Clone;
  FNumber := Number;
  FUserStatus := usInvalid;
end;

destructor TFriendItem.Destroy;
begin
  FClientId.Free;
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

function TFriendList.Add(ClientId: TClientId; Number: Integer): TFriendItem;
var
  Item: TFriendItem;
begin
  UnfriendNumber(Number);

  Item := GetItemWithClientId(ClientId);
  try
    Item.Number := Number;
    Item.IsFriend := True;

    if Assigned(FOnNewItem) then
      FOnNewItem(Self);
  finally
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

constructor TFriendList.Create;
begin
  FItems := TFriendItemList.Create;
  FStopUpdate := False;
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

function TFriendList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFriendList.GetItem(Index: Integer): TFriendItem;
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
function TFriendList.GetItemWithClientId(ClientId: TClientId): TFriendItem;
var
  PItem: Pointer;
  Item: TFriendItem;
  IsExistFriend: Boolean;
begin
  IsExistFriend := False;
  Item := nil;

  for PItem in FItems do
  begin
    Item := TFriendItem(PItem);

    if Assigned(Item) and (Item.ClientId.DataHex = ClientId.DataHex) then
    begin
      IsExistFriend := True;
      Break;
    end;
  end;

  if not IsExistFriend then
  begin
    Item := TFriendItem.Create(False, ClientId);
    FItems.Add(Item);
    Result := Item;
  end
  else
    Result := Item;
end;

{ *  Добавляет пользователя в список без пометки активности. Эта пометка
  *  дает понять, добавлен ли пользователь в список друзей.
  *
  *  ClientId - уникальный идентификатор пользователя
  *  UserName - Имя добавляемого пользователя
  *  LocaleName - Имя, которое устанавливается самостоятельно
  * }
function TFriendList.AddWithoutFriend(ClientId: TClientId;
  UserName, LocaleName: DataString): TFriendItem;
var
  Item: TFriendItem;
begin
  Item := GetItemWithClientId(ClientId);
  Item.UserName := UserName;
  Item.LocaleName := LocaleName;
  Item.OnUpdate := ItemOnUpdate;

  Result := Item;
end;

{ *  Загружает список пользователей из базы данных
  * }
procedure TFriendList.LoadFromDatabase;
begin
  //TODO: Реализовать настоящую загрузку из базы

end;

end.
