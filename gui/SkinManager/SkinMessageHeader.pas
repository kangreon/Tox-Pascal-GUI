// SkinMessageHeader.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit SkinMessageHeader;

interface

uses
  SkinTypes, StringUtils, Graphics;

type
  TSkinMessageHeader = class
  private
    FBackColor: TColor;
    FHeight: Integer;
    FIconMarginLeft: Integer;
    FIconMarginRight: Integer;
    FAudioMarginLeft: Integer;
    FVideoMarginLeft: Integer;
    FVideoMarginRight: Integer;
    FImgAudioButton: TStateImage;
    FImgVideoButton: TStateImage;
    FImgDefIcon: TBitmap;
    FIconWidth: Integer;
    FIconHeight: Integer;
    FDivLineColor: TColor;
    FDivLineStyle: TPenStyle;
    FNameFont: TFontSkin;
    FStatusFont: TFontSkin;
  public
    constructor Create;
    destructor Destroy; override;

    // Цвет фона
    property BackColor: TColor read FBackColor write FBackColor;
    // Высота компонента
    property Height: Integer read FHeight write FHeight;

    // Настройки шрифта для имени пользователя
    property NameFont: TFontSkin read FNameFont;

    // Параметры шрифта для статуса
    property StatusFont: TFontSkin read FStatusFont;

    // Расстояния от краев иконки пользователя до др. компонентов
    property IconMarginLeft: Integer read FIconMarginLeft write FIconMarginLeft;
    property IconMarginRight: Integer read FIconMarginRight write FIconMarginRight;

    // Размеры иконки пользователя
    property IconWidth: Integer read FIconWidth write FIconWidth;
    property IconHeight: Integer read FIconHeight write FIconHeight;

    // Отступы у кнопок видео и аудио звонков
    property AudioMarginLeft: Integer read FAudioMarginLeft write FAudioMarginLeft;
    property VideoMarginLeft: Integer read FVideoMarginLeft write FVideoMarginLeft;
    property VideoMarginRight: Integer read FVideoMarginRight write FVideoMarginRight;

    // Параметры разделительной линии в низу компонента
    property DivLineColor: TColor read FDivLineColor write FDivLineColor;
    property DivLineStyle: TPenStyle read FDivLineStyle write FDivLineStyle;

    // Кнопка аудио-звонка
    property ImgAudioButton: TStateImage read FImgAudioButton write FImgAudioButton;
    // Кнопка видео-звонка
    property ImgVideoButton: TStateImage read FImgVideoButton write FImgVideoButton;
    // Стандартная иконка пользователя
    property ImgDefIcon: TBitmap read FImgDefIcon write FImgDefIcon;

    procedure SetCanvasForDivLine(Canvas: TCanvas);
  end;

implementation

{ TSkinMessageHeader }

constructor TSkinMessageHeader.Create;
begin
  FNameFont := TFontSkin.Create;
  FStatusFont := TFontSkin.Create;
end;

destructor TSkinMessageHeader.Destroy;
begin
  FStatusFont.Free;
  FNameFont.Free;
  inherited;
end;

procedure TSkinMessageHeader.SetCanvasForDivLine(Canvas: TCanvas);
begin
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := DivLineStyle;
  Canvas.Pen.Color := DivLineColor;
end;

end.
