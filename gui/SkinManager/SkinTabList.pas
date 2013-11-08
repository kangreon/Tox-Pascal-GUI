// SkinTabList.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit SkinTabList;

interface

uses
  SkinTypes;

type
  TSkinTabList = class
  private
    FItemFont: TFontSkin;
    FItemHeight: Integer;
    FItemColor: TStateColor;
    FCaptionMarginLeft: Integer;
    FCaptionMarginRight: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Шрифт для элемента списка
    property ItemFont: TFontSkin read FItemFont write FItemFont;
    // Высота одного элемента
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    // Цвет фона в зависимости от состояния мыши
    property ItemColor: TStateColor read FItemColor write FItemColor;
    // Отступ от текста с левой стороны
    property CaptionMarginLeft: Integer read FCaptionMarginLeft write FCaptionMarginLeft;
    // Отступ от текста с правой стороны
    property CaptionMarginRight: Integer read FCaptionMarginRight write FCaptionMarginRight;
  end;

implementation

{ TSkinTabList }

constructor TSkinTabList.Create;
begin
  FItemFont := TFontSkin.Create;
end;

destructor TSkinTabList.Destroy;
begin
  FItemFont.Free;
  inherited;
end;

end.
