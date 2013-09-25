//  UserList.pas
//
//  Виджет, отображающий прокручивающийся список пользователей. Здесь
//  содержится 3 компонента: список пользователей, полоса прокрутки и
//  панель переключения активного списка
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit UserList;

interface
  {$I tox.inc}

uses
  Controls, Classes, Types, SysUtils, UserListStyle, ScrollBarNormal, Messages,
  ActiveRegion;

type

  TUserList = class(TCustomControl)
  private
    FScroll: TScrollBarNormal;
    procedure ScrollOnScroll(Sender: TObject);
  protected
    procedure CreateWnd; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Resize; override;
    procedure WndProc(var Message: TMessage); override;
  public
    property Scroll: TScrollBarNormal read FScroll;

  end;

implementation

{ TUserList }

{*  Процедура вызывается сразу после создания нового окна. Здесь проходит
 *  инициализация всех дочерних компонентов
 *}
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
  FScroll.ListSize := 500;
  FScroll.Position := 20;
  FScroll.OnScroll := ScrollOnScroll;
end;

function TUserList.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  FScroll.Position := FScroll.Position + 5;
end;

function TUserList.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  FScroll.Position := FScroll.Position - 5;
end;

procedure TUserList.Resize;
begin
  inherited;
  if Assigned(FScroll) then
    FScroll.PageSize := ClientHeight;
end;

procedure TUserList.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_MOUSEENTER:
      SetFocus;
  end;
end;

procedure TUserList.ScrollOnScroll(Sender: TObject);
begin

end;

end.
