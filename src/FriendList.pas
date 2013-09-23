//  FriendList.pas
//
//  ќсуществл€ет хранение и изменение списка друзей. —оздает событи€ на
//  изменени€ состо€ний.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit FriendList;

interface
  {$I tox.inc}

uses
  ClientAddress, StringUtils, libtox;

type
  TFriendItem = class
  private
    FNumber: Integer;
    FUserName: DataString;
    FClientId: TClientId;
    FStatusMessage: DataString;
    FOnline: Boolean;
    FUserStatus: TToxUserStatus;
    procedure SetUserName(const Value: DataString);
    procedure SetStatusMessage(const Value: DataString);
    procedure SetOnline(const Value: Boolean);
    procedure SetUserStatus(const Value: TToxUserStatus);
  public
    constructor Create(Number: Integer; ClientId: PByte);
    destructor Destroy; override;

    property Number: Integer read FNumber;
    property UserName: DataString read FUserName write SetUserName;
    property ClientId: TClientId read FClientId;
    property Online: Boolean read FOnline write SetOnline;
    property StatusMessage: DataString read FStatusMessage write SetStatusMessage;
    property UserStatus: TToxUserStatus read FUserStatus write SetUserStatus;
  end;

  TFriendList = class
  private
    FItems: array of TFriendItem;
    function GetCount: Integer;
    function GetItem(Index: Integer): TFriendItem;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Number: Integer; ClientId: PByte): TFriendItem;

    property Count: Integer read GetCount;
    property Item[Index: Integer]: TFriendItem read GetItem;
  end;

implementation

{ TFriendList }

constructor TFriendItem.Create(Number: Integer; ClientId: PByte);
begin
  FNumber := Number;
  FClientId := TClientId.Create(ClientId);
end;

destructor TFriendItem.Destroy;
begin
  FClientId.Free;
  inherited;
end;

procedure TFriendItem.SetOnline(const Value: Boolean);
begin
  FOnline := Value;
end;

procedure TFriendItem.SetStatusMessage(const Value: DataString);
begin
  FStatusMessage := Value;
end;

procedure TFriendItem.SetUserName(const Value: DataString);
begin
  FUserName := Value;
end;

procedure TFriendItem.SetUserStatus(const Value: TToxUserStatus);
begin
  FUserStatus := Value;
end;

{ TFiendList }

function TFriendList.Add(Number: Integer; ClientId: PByte): TFriendItem;
var
  Item: TFriendItem;
  c: Integer;
begin
  Item := TFriendItem.Create(Number, ClientId);
  Result := Item;

  c := Length(FItems);
  SetLength(FItems, c + 1);
  FItems[c] := Item;
end;

constructor TFriendList.Create;
begin
  SetLength(FItems, 0);
end;

destructor TFriendList.Destroy;
begin
  SetLength(FItems, 0);
  inherited;
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

end.
