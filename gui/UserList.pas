﻿// UserList.pas
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
  Controls, Classes, SysUtils, ScrollBarNormal, Messages, ActiveRegion,
  UserListDraw, FriendList, FriendItem, libtox, SkinUserList, UserListSelect;

type
  { TUserList }
  TSortStatus = (ssUp, ssDown);
  TSortName = (snUp, snDown);

  TUserList = class(TCustomControl)
  private
    FActiveRegion: TActiveRegion;
    FFriends: TFriendList;
    FList: TUserListDraw;
    FScroll: TScrollBarNormal;
    FSkin: TSkinUserList;
    FSortName: TSortName;
    FSortStatus: TSortStatus;
    FOnSelectItem: TProcSelectItem;
    FListSelect: TUserListSelect;
    procedure ScrollOnScroll(Sender: TObject);
    procedure ListChangeSize(Sender: TObject);
    procedure FriendsUpdate(Sender: TObject; Index: Integer);
    procedure LoadAllUsers;
    procedure FriendsUpdateList(Sender: TObject);
    procedure SortList(UseBeginUpdate: Boolean);
    function StatusCmp(Status1, Status2: TToxUserStatus): SmallInt;
    procedure ListSelectItem(Sender: TObject; Item: TFriendItem);
  protected
    procedure CreateWnd; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent; FriendList: TFriendList;
      Skin: TSkinUserList); reintroduce;
    destructor Destroy; override;

    property Scroll: TScrollBarNormal read FScroll;
    property ListSelect: TUserListSelect read FListSelect;

    property OnSelectItem: TProcSelectItem read FOnSelectItem
      write FOnSelectItem;
  end;

implementation

{ TUserList }

constructor TUserList.Create(AOwner: TComponent; FriendList: TFriendList;
  Skin: TSkinUserList);
begin
  inherited Create(AOwner);
  FFriends := FriendList;
  FSkin := Skin;

  FListSelect := TUserListSelect.Create(ltFriend);
  FListSelect.OnSelectItem := FriendsUpdateList;

  FSortName := snDown;
  FSortStatus := ssDown;
end;

destructor TUserList.Destroy;
begin
  FListSelect.Free;
  inherited;
end;

{ *  Процедура вызывается сразу после создания нового окна. Здесь проходит
  *  инициализация всех дочерних компонентов
  * }
procedure TUserList.CreateWnd;
begin
  inherited;
  DoubleBuffered := True;
  ControlStyle := ControlStyle - [csParentBackground];

  FScroll := TScrollBarNormal.Create(Self, FSkin);
  FScroll.Parent := Self;
  FScroll.Align := alRight;
  FScroll.Width := FSkin.ScrollBarWidth;
  FScroll.PageSize := Height;
  FScroll.OnScroll := ScrollOnScroll;

  FList := TUserListDraw.Create(Self, FSkin);
  FList.Align := alClient;
  FList.Parent := Self;
  FList.OnChangeSize := ListChangeSize;
  FList.OnSelectItem := ListSelectItem;

  FFriends.OnUpdateItem := FriendsUpdate;
  FFriends.OnUpdateList := FriendsUpdateList;
  LoadAllUsers;

  FActiveRegion := TActiveRegion.Create(Self);
  FActiveRegion.Align := alClient;
  FActiveRegion.Parent := Self;
  FList.ActiveRigion := FActiveRegion;
end;

procedure TUserList.FriendsUpdateList(Sender: TObject);
begin
  LoadAllUsers;
end;

{*  Событие на изменение любого параметра пользователя из списка друзей
 *}
procedure TUserList.FriendsUpdate(Sender: TObject; Index: Integer);
begin
  //TODO: В случае изменения состояния пользователя, перезагрузить весь активный
  //  список
  FList.UpdateItem(Index);
  SortList(True);
end;

{ *  Перезагружает пользователей в список друзей в соответствии с выбранными
  *  параметрами отображения списка и выбранной сортировкой.
  * }
procedure TUserList.LoadAllUsers;
var
  Item: TFriendItem;
  PItem: Pointer;
begin
  // TODO: Восстанавливать выделенный элемент после перезагрузки

  FList.BeginUpdate;
  try
    FList.Clear;

    for PItem in FFriends.Item do
    begin
      Item := TFriendItem(PItem);
      case FListSelect.ActiveItem of
        ltOnline:
          begin
            if Item.IsFriend and (Item.UserStatus <> usInvalid) then
            begin
              FList.AddItem(Item);
            end;
          end;
        ltFriend:
          begin
            if Item.IsFriend then
            begin
              FList.AddItem(Item);
            end;
          end;
      else
        FList.AddItem(Item);
      end;
    end;

    SortList(False);
  finally
    FList.EndUpdate;
  end;
end;

{ *  Сравнивает двух статусов. Больший статус - в сети, младший - отключен.
  *  Возвращает больше нуля, если первый статус больше второго.
  * }
function TUserList.StatusCmp(Status1, Status2: TToxUserStatus): SmallInt;
begin
  Result := Integer(Status2) - Integer(Status1);
end;

{ *  Сортирует список пользователей с заданными в классе параметрами.
  * }
procedure TUserList.SortList(UseBeginUpdate: Boolean);
var
  i, j, c: Integer;
  FirstItem, LastItem: TFriendItem;
  CmpRes: Integer;
begin
  if UseBeginUpdate then
    FList.BeginUpdate;
  try
    c := FList.ItemsCount;

    for i := 0 to c - 2 do
      for j := i + 1 to c - 1 do
      begin
        FirstItem := FList.Items[i].Item;
        LastItem := FList.Items[j].Item;

        CmpRes := StatusCmp(FirstItem.UserStatus, LastItem.UserStatus);
        if CmpRes > 0 then
        begin
          if FSortStatus = TSortStatus.ssUp then
            FList.Swap(i, j);
        end
        else if CmpRes < 0 then
        begin
          if FSortStatus = TSortStatus.ssDown then
            FList.Swap(i, j);
        end;
      end;

    for i := 0 to c - 2 do
      for j := i + 1 to c - 1 do
      begin
        FirstItem := FList.Items[i].Item;
        LastItem := FList.Items[j].Item;

        if FirstItem.UserStatus <> LastItem.UserStatus then
          Continue;

        if FirstItem.UserName = '' then
        begin
          FList.Swap(i, j);
          Continue;
        end;

        if LastItem.UserName = '' then
          Continue;

        CmpRes := AnsiCompareText(FirstItem.UserName, LastItem.UserName);
        if CmpRes > 0 then
        begin
          if FSortName = TSortName.snDown then
            FList.Swap(i, j);
        end
        else if CmpRes < 0 then
        begin
          if FSortName = TSortName.snUp then
            FList.Swap(i, j);
        end;
      end;

  finally
    if UseBeginUpdate then
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
{$IFDEF FPC}
  function GET_WHEEL_DELTA_WPARAM(wp: WPARAM): SmallInt;
  begin
    Result := SmallInt(wp shr 16);
  end;
{$ENDIF}

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

procedure TUserList.ListChangeSize(Sender: TObject);
begin
  FScroll.ListSize := FList.Size;
  FScroll.PageSize := FList.ClientHeight;
end;

procedure TUserList.ListSelectItem(Sender: TObject; Item: TFriendItem);
begin
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self, Item);
end;

procedure TUserList.ScrollOnScroll(Sender: TObject);
begin
  FList.Position := FScroll.Position;
end;

end.
