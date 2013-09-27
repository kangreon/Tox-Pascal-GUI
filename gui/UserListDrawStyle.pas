//  UserListDrawStyle.pas
//
//  Описывает постоянные значения GUI для виджета списка пользователей
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit UserListDrawStyle;

interface
  {$I tox.inc}

uses
  Graphics;

type
  TULDStyle = class
  public const
    BackgroundNormal: TColor        = $424041;
    BackgroundActive: TColor        = $555353;
    BackgroundSelect: TColor        = $666463;
    ItemHeight: Integer             = 59;

    IconLeft: Integer               = 8;
    IconMarginRight: Integer        = 8;

    StatusIconMarginLeft: Integer   = 0;
    StatusIconMarginRight: Integer  = 0;

    NameHeight: Integer             = 14;
    StatusHeight: Integer           = 13;
    NameColor: TColor               = $FFFFFF;
    StatusColor: TColor             = $CAC8C7;
  end;

implementation

end.
