//  UserStatusStyle.pas
//
//  Описывает постоянные значения GUI для виджета статуса пользователя
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit UserStatusStyle;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  Graphics;

type
  // TODO: ЗАМЕНИТЬ!!!!
  TTUserStatusStyle = class
  private
    function GetBackgroundColor: TColor;
    function GetHeight: Integer;
    function GetMinWidth: Integer;
    function GetRightButtonBackgroundNormal: TColor;
    function GetRightButtonBackgroundActive: TColor;
    function GetRightButtonBackgroundDown: TColor;
    function GetRightButtonWidth: Integer;
    function GetIconPositionLeft: Integer;
    function GetIconWidth: Integer;
    function GetIconHeight: Integer;
    function GetUserNameMarginLeft: Integer;
    function GetUserNameMarginTop: Integer;
    function GetUserNameHeight: Integer;
  public
    // Основной цвет фона
    property BackgroundColor:TColor read GetBackgroundColor;
    // Постоянная высота панели
    property Height: Integer read GetHeight;
    // Минимальная длина панели
    property MinWidth: Integer read GetMinWidth;
    // Цвет фона правой кнопки в обычном состоянии
    property RightButtonBackgroundNormal: TColor read GetRightButtonBackgroundNormal;
    // Цвет фона правой кнопки в состоянии наведения мыши
    property RightButtonBackgroundActive: TColor read GetRightButtonBackgroundActive;
    // Цвет фона правой кнопки в состоянии нажатия мыши
    property RightButtonBackgroundDown: TColor read GetRightButtonBackgroundDown;
    // Ширина правой кнопки
    property RightButtonWidth: Integer read GetRightButtonWidth;
    // Отступ аваторки пользователя от левого края
    property IconPositionLeft: Integer read GetIconPositionLeft;
    // Длина иконки
    property IconWidth: Integer read GetIconWidth;
    // Ширина иконки
    property IconHeight: Integer read GetIconHeight;
    // Отступ для имени с левой стороны
    property UserNameMarginLeft: Integer read GetUserNameMarginLeft;
    // Отступ для имент сверху
    property UserNameMarginTop: Integer read GetUserNameMarginTop;
    // Высота имени
    property UserNameHeight: Integer read GetUserNameHeight;
  public const
    // Отступ аваторки пользователя справа
    IconMarginRight: Integer          = 5;
    // Цвет имени пользователя
    UserNameColor: TColor             = clWhite;
    //
    StatusMessageColor: TColor        = $EEEEEE;
    // Высота текста со статусом пользователя
    StatusMessageHeight: Integer      = 13;
  end;

function TUserStatusStyle: TTUserStatusStyle;

implementation

var
  a: TTUserStatusStyle;

function TUserStatusStyle: TTUserStatusStyle;
begin
  if not Assigned(a) then
    a := TTUserStatusStyle.Create;
  Result := a;
end;

{ TUserStatusStyle }

function TTUserStatusStyle.GetBackgroundColor: TColor;
begin
  Result := RGB(35, 31, 32);
end;

function TTUserStatusStyle.GetHeight: Integer;
begin
  Result := 59
end;

function TTUserStatusStyle.GetIconHeight: Integer;
begin
  Result := 41;
end;

function TTUserStatusStyle.GetIconPositionLeft: Integer;
begin
  Result := 8;
end;

function TTUserStatusStyle.GetIconWidth: Integer;
begin
  Result := 44;
end;

function TTUserStatusStyle.GetMinWidth: Integer;
begin
  Result := 223;
end;

function TTUserStatusStyle.GetRightButtonBackgroundActive: TColor;
begin
  Result := $525051;
end;

function TTUserStatusStyle.GetRightButtonBackgroundDown: TColor;
begin
  Result := $323031;
end;

function TTUserStatusStyle.GetRightButtonBackgroundNormal: TColor;
begin
  Result := $424041;
end;

function TTUserStatusStyle.GetRightButtonWidth: Integer;
begin
  Result := 13;
end;

function TTUserStatusStyle.GetUserNameHeight: Integer;
begin
  Result := 16;
end;

function TTUserStatusStyle.GetUserNameMarginLeft: Integer;
begin
  Result := 5;
end;

function TTUserStatusStyle.GetUserNameMarginTop: Integer;
begin
  Result := 15;
end;

end.
