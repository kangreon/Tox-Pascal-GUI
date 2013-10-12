// UserList.pas
//
// Виджет, отображающий прокручивающийся список пользователей. Здесь
// содержится 3 компонента: список пользователей, полоса прокрутки и
// панель переключения активного списка
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit UserList;

interface

{$I tox.inc}

uses
{$I tox-uses.inc}
  Controls, Classes, SysUtils, UserListStyle, ScrollBarNormal, Messages,
  ActiveRegion, UserListDraw, FriendList;

type
  { TUserList }

  TUserList = class(TCustomControl)
  private
    FActiveRegion: TActiveRegion;
    FScroll: TScrollBarNormal;
    FList: TUserListDraw;
    FFriends: TFriendList;
    procedure ScrollOnScroll(Sender: TObject);
    procedure ListOnChangeSize(Sender: TObject);
    procedure FriendsUpdate(Sender: TObject; Index: Integer);
    procedure LoadAllUsers;
    procedure FriendsNewItem(Sender: TObject);
  protected
    procedure CreateWnd; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent; FriendList: TFriendList);
      reintroduce;

    property Scroll: TScrollBarNormal read FScroll;
  end;

implementation

{ TUserList }

constructor TUserList.Create(AOwner: TComponent; FriendList: TFriendList);
begin
  inherited Create(AOwner);
  FFriends := FriendList;
end;

{ *  Процедура вызывается сразу после создания нового окна. Здесь проходит
  *  инициализация всех дочерних компонентов
  * }
procedure TUserList.CreateWnd;
begin
  inherited;
  DoubleBuffered := True;
  Color := TUserListStyle.BackgroundColor;

  FScroll := TScrollBarNormal.Create(Self);
  FScroll.Parent := Self;
  FScroll.Align := alRight;
  FScroll.Width := TUserListStyle.ScrollWidth;
  FScroll.PageSize := Height;
  FScroll.OnScroll := ScrollOnScroll;

  FList := TUserListDraw.Create(Self);
  FList.Align := alClient;
  FList.Parent := Self;
  FList.OnChangeSize := ListOnChangeSize;

  FFriends.OnUpdateItem := FriendsUpdate;
  FFriends.OnNewItem := FriendsNewItem;
  LoadAllUsers;

  FActiveRegion := TActiveRegion.Create(Self);
  FActiveRegion.Align := alClient;
  FActiveRegion.Parent := Self;
  FList.ActiveRigion := FActiveRegion;
end;

procedure TUserList.FriendsNewItem(Sender: TObject);
begin
  LoadAllUsers;
end;

{*  Событие на изменение любого параметра пользователя из списка друзей
 *}
procedure TUserList.FriendsUpdate(Sender: TObject; Index: Integer);
begin
  FList.UpdateItem(Index);
end;

procedure TUserList.LoadAllUsers;
var
  i: Integer;
  Item: TFriendItem;
begin
  FList.BeginUpdate;
  try
    FList.Clear;

    i := 0;
    repeat
      Item := FFriends.ItemFriend[i];
      Inc(i);
      if Assigned(Item) then
        FList.AddItem(Item);
    until not Assigned(Item);

  finally
    FList.EndUpdate;
  end;
end;

function TUserList.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
{$IFNDEF FPC} inherited; {$ENDIF}
  FScroll.Position := FScroll.Position + 10;
  Result := True;
end;

function TUserList.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
{$IFNDEF FPC} inherited; {$ENDIF}
  FScroll.Position := FScroll.Position - 10;
  Result := True;
end;

procedure TUserList.Resize;
begin
  inherited;
  if Assigned(FScroll) then
    FScroll.PageSize := ClientHeight;
end;

procedure TUserList.WndProc(var Message: TMessage);
  function GET_WHEEL_DELTA_WPARAM(wp: longint): smallint;
  begin
    Result := smallint(wp shr 16);
  end;

begin
  inherited;
  case Message.Msg of
    CM_MOUSEENTER:
      SetFocus;

{$IFDEF FPC}
    LM_MOUSEWHEEL:
      begin
        if GET_WHEEL_DELTA_WPARAM(Message.wParam) > 0 then
          DoMouseWheelUp([], Point(0, 0))
        else
          DoMouseWheelDown([], Point(0, 0));

        Message.Result := 1;
      end;
{$ENDIF}
  end;
end;

procedure TUserList.ListOnChangeSize(Sender: TObject);
begin
  FScroll.ListSize := FList.Size;
  FScroll.PageSize := FList.ClientHeight;
end;

procedure TUserList.ScrollOnScroll(Sender: TObject);
begin
  FList.Position := FScroll.Position;
end;

end.
