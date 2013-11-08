// SkinUserStatus.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit SkinUserStatus;

interface

uses
  Graphics, StringUtils, SkinTypes;

type
  TSkinUserStatus = class
  private
    FBackColor: TColor;
    FHeight: Integer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FButtonBack: TColor;
    FButtonBackActive: TColor;
    FButtonBackDown: TColor;
    FButtonWidth: Integer;
    FIconLeft: Integer;
    FIconWidth: Integer;
    FIconHeight: Integer;
    FNameFontSize: Integer;
    FNameFontName: DataString;
    FNameFontColor: TColor;
    FNameFontStyle: TFontStyles;
    FStatusFontName: DataString;
    FStatusFontColor: TColor;
    FStatusFontSize: Integer;
    FStatusFontStyle: TFontStyles;
    FIconMarginRight: Integer;
    FNameMarginBottom: Integer;
    FStatusMarginTop: Integer;
    FImgLoading: TBitmap;
    FImgStateOnline: TBitmap;
    FImgStateAway: TBitmap;
    FImgBusy: TBitmap;
    FImgStateOffline: TBitmap;
  public
    // Цвет фона
    property BackColor: TColor read FBackColor write FBackColor;

    // Высота элемента
    property Height: Integer read FHeight write FHeight;

    // Минимальная ширина элемента
    property MinWidth: Integer read FMinWidth write FMinWidth;

    // Максимальная ширина элемента
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;

    // Цвет кнопки изменения статуса в нормальном состоянии
    property ButtonBack: TColor read FButtonBack write FButtonBack;

    // В активном состоянии
    property ButtonBackActive: TColor read FButtonBackActive write FButtonBackActive;

    // В нажатом состоянии
    property ButtonBackDown: TColor read FButtonBackDown write FButtonBackDown;

    // Ширина кнопки
    property ButtonWidth: Integer read FButtonWidth write FButtonWidth;

    // Отступ от края до иконки пользователя.
    property IconLeft: Integer read FIconLeft write FIconLeft;

    // Отступ от иконки пользователя с правой стороны
    property IconMarginRight: Integer read FIconMarginRight write FIconMarginRight;

    // Ширина иконки пользователя
    property IconWidth: Integer read FIconWidth write FIconWidth;

    // Высота иконки пользователя
    property IconHeight: Integer read FIconHeight write FIconHeight;

    // Параметры шрифта для имени пользователя
    property NameFontSize: Integer read FNameFontSize write FNameFontSize;
    property NameFontName: DataString read FNameFontName write FNameFontName;
    property NameFontColor: TColor read FNameFontColor write FNameFontColor;
    property NameFontStyle: TFontStyles read FNameFontStyle write FNameFontStyle;

    // Параметры шрифта для статуса
    property StatusFontSize: Integer read FStatusFontSize write FStatusFontSize;
    property StatusFontName: DataString read FStatusFontName write FStatusFontName;
    property StatusFontColor: TColor read FStatusFontColor write FStatusFontColor;
    property StatusFontStyle: TFontStyles read FStatusFontStyle write FStatusFontStyle;

    // Отступ от центра виджета до нижней части имени пользователя
    property NameMarginBottom: Integer read FNameMarginBottom write FNameMarginBottom;

    // Отступ от центра виджета до верхней части статуса пользователя
    property StatusMarginTop: Integer read FStatusMarginTop write FStatusMarginTop;

    // Изображение с анимацией загрузки
    property ImgLoading: TBitmap read FImgLoading write FImgLoading;

    // Состояние пользователя онлайн
    property ImgStateOnline: TBitmap read FImgStateOnline write FImgStateOnline;

    // Состояние пользователя отошел
    property ImgStateAway: TBitmap read FImgStateAway write FImgStateAway;

    // Состояние пользователя занят
    property ImgStateBusy: TBitmap read FImgBusy write FImgBusy;

    // Состояние пользователя отключен
    property ImgStateOffline: TBitmap read FImgStateOffline write FImgStateOffline;

    function SetCanvasForName(Canvas: TCanvas): Integer;
    function SetCanvasForStatus(Canvas: TCanvas): Integer;
  end;

implementation

{ TSkinUserStatus }

function TSkinUserStatus.SetCanvasForName(Canvas: TCanvas): Integer;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := NameFontName;
  Canvas.Font.Style := NameFontStyle;
  Canvas.Font.Size := NameFontSize;
  Canvas.Font.Color := NameFontColor;

  Result := Canvas.TextHeight('Q');
end;

function TSkinUserStatus.SetCanvasForStatus(Canvas: TCanvas): Integer;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := StatusFontName;
  Canvas.Font.Style := StatusFontStyle;
  Canvas.Font.Size := StatusFontSize;
  Canvas.Font.Color := StatusFontColor;

  Result := Canvas.TextHeight('Q');
end;

end.
