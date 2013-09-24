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
  Controls, UserListStyle, ScrollBarNormal;

type
  TUserList = class(TWinControl)
  private
    FScroll: TScrollBarNormal;
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
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
  FScroll.ListSize := Height * 3;
  FScroll.Position := 20;
end;

procedure TUserList.Resize;
begin
  inherited;
  if Assigned(FScroll) then
    FScroll.PageSize := ClientHeight;
end;

end.
