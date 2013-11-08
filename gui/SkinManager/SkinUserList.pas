// SkinUserList.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit SkinUserList;

interface

uses
  Classes, Graphics, StringUtils, SkinTypes;

type
  TSkinUserList = class
  private
    FBackgroundColor: TColor;
    FItemColorActive: TColor;
    FItemColorDown: TColor;
    FItemHeight: Integer;
    FIconLeft: Integer;
    FIconMarginRight: Integer;
    FStatusIconMarginLeft: Integer;
    FStatusIconMarginRight: Integer;
    FImgStateOnline: TStateImage;
    FImgStateAway: TStateImage;
    FImgBusy: TStateImage;
    FImgStateOffline: TStateImage;
    FImgStateOnlineNew: TStateImage;
    FImgStateAwayNew: TStateImage;
    FImgBusyNew: TStateImage;
    FImgStateOfflineNew: TStateImage;
    FImgScrollBarTop: TStateImage;
    FImgScrollBarCenter: TStateImage;
    FImgScrollBarBottom: TStateImage;
    FScrollBarWidth: Integer;
    FScrollBarBack: TColor;
    FScrollBarMinCenterHeight: Integer;
    FScrollbarCenterColor: TColor;
    FScrollbarCenterColorDown: TColor;
    FScrollbarCenterColorActive: TColor;
    FNameFont: TFontSkin;
    FStatusFont: TFontSkin;
  public
    constructor Create;
    destructor Destroy; override;

    // Цвет фона компонента
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;

    // Цвет выделенного элемента+
    property ItemColorActive: TColor read FItemColorActive write FItemColorActive;

    // Цвет нажатого элемента
    property ItemColorDown: TColor read FItemColorDown write FItemColorDown;

    // Высота элемента списка
    property ItemHeight: Integer read FItemHeight write FItemHeight;

    // Расстояние от края элемента до иконки пользователя
    property IconLeft: Integer read FIconLeft write FIconLeft;

    // Отступ от иконки пользователя с правой стороны
    property IconMarginRight: Integer read FIconMarginRight write FIconMarginRight;

    // Отступ от иконки статуса с правой стороны
    property StatusIconMarginLeft: Integer read FStatusIconMarginLeft write FStatusIconMarginLeft;

    // Отступ от иконки с левой стороны
    property StatusIconMarginRight: Integer read FStatusIconMarginRight write FStatusIconMarginRight;

    // Шрифт для имени пользователя
    property NameFont: TFontSkin read FNameFont;
    // Шрифт для статуса пользователя
    property StatusFont: TFontSkin read FStatusFont;

    // Состояние пользователя онлайн
    property ImgStateOnline: TStateImage read FImgStateOnline write FImgStateOnline;
    property ImgStateOnlineNew: TStateImage read FImgStateOnlineNew write FImgStateOnlineNew;

    // Состояние пользователя отошел
    property ImgStateAway: TStateImage read FImgStateAway write FImgStateAway;
    property ImgStateAwayNew: TStateImage read FImgStateAwayNew write FImgStateAwayNew;

    // Состояние пользователя занят
    property ImgStateBusy: TStateImage read FImgBusy write FImgBusy;
    property ImgStateBusyNew: TStateImage read FImgBusyNew write FImgBusyNew;

    // Состояние пользователя отключен
    property ImgStateOffline: TStateImage read FImgStateOffline write FImgStateOffline;
    property ImgStateOfflineNew: TStateImage read FImgStateOfflineNew write FImgStateOfflineNew;

    // Верхняя часть полосы прокрутки
    property ImgScrollBarTop: TStateImage read FImgScrollBarTop write FImgScrollBarTop;
    //TODO: Не используется
    property ImgScrollBarCenter: TStateImage read FImgScrollBarCenter write FImgScrollBarCenter;
    property ImgScrollBarBottom: TStateImage read FImgScrollBarBottom write FImgScrollBarBottom;

    // Цвет заднего фона полосы прокрутки
    property ScrollBarBack: TColor read FScrollBarBack write FScrollBarBack;

    // Цвет центральной части полосы прокрутки
    property ScrollbarCenterColor: TColor read FScrollbarCenterColor write FScrollbarCenterColor;
    property ScrollbarCenterColorActive: TColor read FScrollbarCenterColorActive write FScrollbarCenterColorActive;
    property ScrollbarCenterColorDown: TColor read FScrollbarCenterColorDown write FScrollbarCenterColorDown;

    // Минимальная высода центральной части полосы прокрутки
    property ScrollBarMinCenterHeight: Integer read FScrollBarMinCenterHeight write FScrollBarMinCenterHeight;
    // Ширина полосы прокрутки
    property ScrollBarWidth: Integer read FScrollBarWidth write FScrollBarWidth;
  end;

implementation

{ TSkinUserList }

constructor TSkinUserList.Create;
begin
  FNameFont := TFontSkin.Create;
  FStatusFont := TFontSkin.Create;
end;

destructor TSkinUserList.Destroy;
begin
  FNameFont.Free;
  FStatusFont.Free;
  inherited;
end;

end.
