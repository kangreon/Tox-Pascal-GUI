unit FriendItem;

interface

uses
  SysUtils, StringUtils, libtox, ClientAddress, Classes;

type
  TFriendItem = class
  private
    FUserName: DataString;
    FClientId: TClientId;
    FStatusMessage: DataString;
    FOnline: Boolean;
    FUserStatus: TToxUserStatus;
    FNumber: Integer;
    FData: Pointer;
    FIsFriend: Boolean;
    FLocaleName: DataString;
    FAddress: TFriendAddress;
    FIsAddressExist: Boolean;
    FBaseId: Integer;
    FOnUpdate: array of TNotifyEvent;
    FOnUpdateBase: TNotifyEvent;
    procedure EventUpdate;
    procedure EventUpdateBase;
    procedure SetUserName(const Value: DataString);
    procedure SetStatusMessage(const Value: DataString);
    procedure SetOnline(const Value: Boolean);
    procedure SetUserStatus(const Value: TToxUserStatus);
    procedure SetLocaleName(const Value: DataString);
    procedure SetOnUpdate(const Value: TNotifyEvent);
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
    property LocaleName: DataString read FLocaleName write SetLocaleName;
    property Number: Integer read FNumber write FNumber;
    property Online: Boolean read FOnline write SetOnline;
    property StatusMessage: DataString read FStatusMessage write SetStatusMessage;
    property UserName: DataString read FUserName write SetUserName;
    property UserStatus: TToxUserStatus read FUserStatus write SetUserStatus;

    property OnUpdate: TNotifyEvent write SetOnUpdate;
    property OnUpdateBase: TNotifyEvent read FOnUpdateBase write FOnUpdateBase;
  end;

  TFriendItemList = TList;

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
var
  Update: TNotifyEvent;
begin
  for Update in FOnUpdate do
  begin
    if Assigned(Update) then
      TNotifyEvent(Update)(Self);
  end;
end;

procedure TFriendItem.EventUpdateBase;
begin
  if Assigned(FOnUpdateBase) then
    FOnUpdateBase(Self);
end;

procedure TFriendItem.SetLocaleName(const Value: DataString);
begin
  if FLocaleName <> Value then
  begin
    FLocaleName := Value;
    EventUpdateBase;
    EventUpdate;
  end;
end;

procedure TFriendItem.SetOnline(const Value: Boolean);
begin
  if FOnline <> Value then
  begin
    FOnline := Value;
    EventUpdate;
  end;
end;

procedure TFriendItem.SetOnUpdate(const Value: TNotifyEvent);
var
  i: Integer;
begin
  i := Length(FOnUpdate);
  SetLength(FOnUpdate, i + 1);
  FOnUpdate[i] := Value;
end;

procedure TFriendItem.SetStatusMessage(const Value: DataString);
begin
  if FStatusMessage <> Value then
  begin
    FStatusMessage := Value;
    EventUpdateBase;
    EventUpdate;
  end;
end;

procedure TFriendItem.SetUserName(const Value: DataString);
begin
  if FUserName <> Value then
  begin
    FUserName := Value;
    EventUpdate;
    EventUpdateBase;
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

end.
