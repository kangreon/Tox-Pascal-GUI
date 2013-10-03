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
    FNumber: Integer;
    FUserName: DataString;
    FClientId: TClientId;
    FStatusMessage: DataString;
    FOnline: Boolean;
    FUserStatus: TToxUserStatus;
    FOnUpdate: TNotifyEvent;
    FIndex: Integer;
    procedure EventUpdate;
    procedure SetUserName(const Value: DataString);
    procedure SetStatusMessage(const Value: DataString);
    procedure SetOnline(const Value: Boolean);
    procedure SetUserStatus(const Value: TToxUserStatus);
  public
    constructor Create(Number: Integer; ClientId: PByte);
    destructor Destroy; override;

    property Index: Integer read FIndex write FIndex;
    property Number: Integer read FNumber;
    property UserName: DataString read FUserName write SetUserName;
    property ClientId: TClientId read FClientId;
    property Online: Boolean read FOnline write SetOnline;
    property StatusMessage: DataString read FStatusMessage write SetStatusMessage;
    property UserStatus: TToxUserStatus read FUserStatus write SetUserStatus;

    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  TFriendList = class
  private
    FItems: array of TFriendItem;
    FStopUpdate: Boolean;
    FOnUpdateItem: TProcUpdateItem;
    FOnNewItem: TNotifyEvent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TFriendItem;
    procedure ItemOnUpdate(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Number: Integer; ClientId: PByte): TFriendItem;
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    function Replcae(Number: Integer; ClientId: PByte): TFriendItem;

    property Count: Integer read GetCount;
    property Item[Index: Integer]: TFriendItem read GetItem;

    property OnNewItem: TNotifyEvent read FOnNewItem write FOnNewItem;
    property OnUpdateItem: TProcUpdateItem read FOnUpdateItem write FOnUpdateItem;
  end;

implementation

{ TFriendList }

constructor TFriendItem.Create(Number: Integer; ClientId: PByte);
begin
  FNumber := Number;
  FClientId := TClientId.Create(ClientId);
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

function TFriendList.Add(Number: Integer; ClientId: PByte): TFriendItem;
var
  Item: TFriendItem;
  c: Integer;
begin
  Item := TFriendItem.Create(Number, ClientId);
  Item.OnUpdate := ItemOnUpdate;
  Result := Item;

  c := Length(FItems);
  SetLength(FItems, c + 1);
  Item.Index := c;
  FItems[c] := Item;

  if Assigned(FOnNewItem) then
    FOnNewItem(Self);
end;


function TFriendList.Replcae(Number: Integer; ClientId: PByte): TFriendItem;
var
  Item: TFriendItem;
begin
  if (Number >= 0) and (Number < Count) then
  begin
    Item := TFriendItem.Create(Number, ClientId);
    Item.OnUpdate := ItemOnUpdate;
    Result := Item;

    FItems[Number] := Item;

    if Assigned(FOnNewItem) then
      FOnNewItem(Self);
  end
  else
    Result := nil;
end;

procedure TFriendList.BeginUpdate;
begin
  FStopUpdate := True;
end;

procedure TFriendList.Clear;
begin
  SetLength(FItems, 0);

  if (not FStopUpdate) and Assigned(FOnNewItem) then
    FOnNewItem(Self);
end;

constructor TFriendList.Create;
begin
  SetLength(FItems, 0);
  FStopUpdate := False;
end;

destructor TFriendList.Destroy;
begin
  SetLength(FItems, 0);
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
  Result := Length(FItems);
end;

function TFriendList.GetItem(Index: Integer): TFriendItem;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FItems[Index]
  else
    Result := nil;
end;

procedure TFriendList.ItemOnUpdate(Sender: TObject);
begin
  if Assigned(Sender) and Assigned(FOnUpdateItem) then
  begin
    FOnUpdateItem(Self, TFriendItem(Sender).Index);
  end;
end;


end.
