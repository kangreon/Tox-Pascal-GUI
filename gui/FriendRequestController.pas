//  ActiveRegion.pas
//
//  Управляет окнами уведомления с запросом о добавлении в друзья другого
//  пользователя
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit FriendRequestController;

interface
  {$I tox.inc}

uses
{$I tox-uses.inc}
  fmFriendRequest, Classes, ClientAddress, StringUtils, Forms, ExtCtrls;

type
  PRequestItem = ^TRequestItem;
  TRequestItem = record
    Address: TFriendAddress;
    HelloMessage: DataString;
  end;
  TRequestList = array of PRequestItem;

  TProcAddFriend = procedure(Sender: TObject; ClientAddress: TFriendAddress) of object;
  TFriendRequestController = class
  private
    FForm: TFormFriendRequest;
    FFormPosition: TPoint;
    FIsActiveRequest: Boolean;
    FIsFirstForm: Boolean;
    FRequestList: TRequestList;
    FRunNextWindow: TTimer;
    FOwner: TComponent;
    FOnAddFriend: TProcAddFriend;
  procedure ShowLastRequest;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OpenNextWindow(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure InsertRequest(ClientAddress: TFriendAddress;
      HelloMessage: DataString);

    property OnAddFriend: TProcAddFriend read FOnAddFriend write FOnAddFriend;
  end;

implementation

{ TFriendRequestController }

constructor TFriendRequestController.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FIsActiveRequest := False;
  FIsFirstForm := True;

  FRunNextWindow := TTimer.Create(AOwner);
  FRunNextWindow.Enabled := False;
  FRunNextWindow.Interval := 10;
  FRunNextWindow.OnTimer := OpenNextWindow;
end;

destructor TFriendRequestController.Destroy;
begin
  FForm.Free;
  inherited;
end;

procedure TFriendRequestController.InsertRequest(ClientAddress: TFriendAddress;
  HelloMessage: DataString);
var
  item: Integer;
begin
  item := Length(FRequestList);
  SetLength(FRequestList, item + 1);
  New(FRequestList[item]);
  FRequestList[item].Address := ClientAddress;
  FRequestList[item].HelloMessage := HelloMessage;

  if not FIsActiveRequest then
  begin
    ShowLastRequest;
  end;
end;

procedure TFriendRequestController.ShowLastRequest;
var
  LastItemIndex: Integer;
  item: TRequestItem;
begin
  LastItemIndex := Length(FRequestList);
  if LastItemIndex <= 0 then
  begin
    FIsActiveRequest := False;
    Exit;
  end;

  FIsActiveRequest := True;
  if Assigned(FForm) then
    FForm.Free;

  item := FRequestList[LastItemIndex - 1]^;
  SetLength(FRequestList, LastItemIndex - 1);
  FForm := TFormFriendRequest.Create(FOwner, item.Address, Item.HelloMessage);
  FForm.OnClose := FormClose;
  if FIsFirstForm then
  begin
    FIsFirstForm := False;
    FForm.Position := poOwnerFormCenter;
  end
  else
  begin
    FForm.Left := FFormPosition.X;
    FForm.Top := FFormPosition.Y;
  end;
  FForm.Show;
end;

procedure TFriendRequestController.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Запуск следующего окна от сообщения системы.

  case TFormFriendRequest(Sender).ReturnMessage of
    rtAdd:
      if Assigned(FOnAddFriend) then
        FOnAddFriend(Self, TFormFriendRequest(Sender).ClientAddress);

    rtIgnore:;
  end;

  FRunNextWindow.Enabled := True;
  FFormPosition := Point(TForm(Sender).Left, TForm(Sender).Top);
end;

procedure TFriendRequestController.OpenNextWindow(Sender: TObject);
begin
  TTimer(Sender).Enabled := False;
  ShowLastRequest;
end;

end.
