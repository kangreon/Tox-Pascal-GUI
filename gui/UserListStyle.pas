//  UserListStyle.pas
//
//  Описывает постоянные значения GUI для виджета статуса пользователя
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit UserListStyle;

interface
  {$I tox.inc}

uses
//  {$I tox-uses.inc}
  Graphics;

type
  TUserListStyle = class
  public const
    BackgroundColor: TColor = $424041;
    ScrollWidth: Integer = 13;
  end;

implementation

end.
