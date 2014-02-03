// SkinProfileSelect.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2014 Dmitry
//
unit SkinProfileSelect;

interface

uses
  Graphics, SkinTypes;

type
  TSkinProfileSelect = class
  private
    FLeftBarColor: TColor;
    FLeftBarWidth: Integer;
    FLogoTox: TBitmap;
    FLeftButtonHeight: Integer;
    FDivisionLineMargin: Integer;
    FIconSelection: TBitmap;
    FBackColor: TColor;
    FMenuFont: TFontSkin;
    FLabelFont: TFontSkin;
    FEditFont: TFontSkin;
  public
    constructor Create;
    destructor Destroy; override;

    property BackColor: TColor read FBackColor write FBackColor;

    property LeftBarColor: TColor read FLeftBarColor write FLeftBarColor;
    property LeftBarWidth: Integer read FLeftBarWidth write FLeftBarWidth;
    property LeftButtonHeight: Integer read FLeftButtonHeight write FLeftButtonHeight;
    property DivisionLineMargin: Integer read FDivisionLineMargin write FDivisionLineMargin;

    property LogoTox: TBitmap read FLogoTox write FLogoTox;
    property IconSelection: TBitmap read FIconSelection write FIconSelection;

    property MenuFont: TFontSkin read FMenuFont;
    property LabelFont: TFontSkin read FLabelFont;
    property EditFont: TFontSkin read FEditFont;
  end;

implementation

{ TSkinProfileSelect }

constructor TSkinProfileSelect.Create;
begin
  FMenuFont := TFontSkin.Create;
  FLabelFont := TFontSkin.Create;
  FEditFont := TFontSkin.Create;
end;

destructor TSkinProfileSelect.Destroy;
begin
  FMenuFont.Free;
  FLabelFont.Free;
  FEditFont.Free;
  inherited;
end;

end.
