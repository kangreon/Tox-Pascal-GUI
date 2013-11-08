// SkinTabControl.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit SkinTabControl;

interface

uses
  SkinTypes, Graphics, StringUtils, SkinTabList;

type
  TSkinTabControl = class
  private
    FBackColor: TColor;
    FButtonHeight: Integer;
    FButtonMarginTop: Integer;
    FButtonMargintBottom: Integer;
    FButtonMarginLeft: Integer;
    FButtonMarginRight: Integer;
    FButtonListColor: TStateColor;
    FButtonRequestColor: TStateColor;
    FImgListTopLeft: TStateImage;
    FImgListTopRight: TStateImage;
    FImgListBottomLeft: TStateImage;
    FImgListBottomRight: TStateImage;
    FImgRequestBottomRight: TStateImage;
    FImgRequestTopLeft: TStateImage;
    FImgRequestTopRight: TStateImage;
    FImgRequestBottomLeft: TStateImage;
    FCaptionMarginLeft: Integer;
    FcaptionMarginRight: Integer;
    FTabListItemHeight: Integer;
    FTabListMarginLeft: Integer;
    FTabListMarginRight: Integer;
    FTabListMarginTop: Integer;
    FTabListMarginBottom: Integer;
    FImgListExtTopLeft: TStateImage;
    FImgListExtTopRight: TStateImage;
    FImgListExtBottomLeft: TStateImage;
    FImgListExtBottomRight: TStateImage;
    FImgRequestExtBottomRight: TStateImage;
    FImgRequestExtTopLeft: TStateImage;
    FImgRequestExtTopRight: TStateImage;
    FImgRequestExtBottomLeft: TStateImage;
    FButton: TFontSkin;
    FSkinTabList: TSkinTabList;
  public
    constructor Create;
    destructor Destroy; override;

    // Цвет фона
    property BackColor: TColor read FBackColor write FBackColor;

    // Высота кнопки
    property ButtonHeight: Integer read FButtonHeight write FButtonHeight;

    // Отступы от кнопки до других компонентов и краев
    property ButtonMarginTop: Integer read FButtonMarginTop write FButtonMarginTop;
    property ButtonMargintBottom: Integer read FButtonMargintBottom write FButtonMargintBottom;
    property ButtonMarginLeft: Integer read FButtonMarginLeft write FButtonMarginLeft;
    property ButtonMarginRight: Integer read FButtonMarginRight write FButtonMarginRight;

    // Цвета кнопок в зависимости от состояния
    property ButtonListColor: TStateColor read FButtonListColor write FButtonListColor;
    property ButtonRequestColor: TStateColor read FButtonRequestColor write FButtonRequestColor;

    //Насйтройки шрифта
    property Button: TFontSkin read FButton;

    // Отступы от краев кнопки до текста в ней
    property CaptionMarginLeft: Integer read FCaptionMarginLeft write FCaptionMarginLeft;
    property CaptionMarginRight: Integer read FcaptionMarginRight write FcaptionMarginRight;

    // Размеры окна списков
    property TabListItemHeight: Integer read FTabListItemHeight write FTabListItemHeight;
    property TabListMarginLeft: Integer read FTabListMarginLeft write FTabListMarginLeft;
    property TabListMarginRight: Integer read FTabListMarginRight write FTabListMarginRight;
    property TabListMarginTop: Integer read FTabListMarginTop write FTabListMarginTop;
    property TabListMarginBottom: Integer read FTabListMarginBottom write FTabListMarginBottom;

    // Края списка
    property ImgListTopLeft: TStateImage read FImgListTopLeft write FImgListTopLeft;
    property ImgListTopRight: TStateImage read FImgListTopRight write FImgListTopRight;
    property ImgListBottomLeft: TStateImage read FImgListBottomLeft write FImgListBottomLeft;
    property ImgListBottomRight: TStateImage read FImgListBottomRight write FImgListBottomRight;
    //Внешние края
    property ImgListExtTopLeft: TStateImage read FImgListExtTopLeft write FImgListExtTopLeft;
    property ImgListExtTopRight: TStateImage read FImgListExtTopRight write FImgListExtTopRight;
    property ImgListExtBottomLeft: TStateImage read FImgListExtBottomLeft write FImgListExtBottomLeft;
    property ImgListExtBottomRight: TStateImage read FImgListExtBottomRight write FImgListExtBottomRight;

    // Края запроса пользователя
    property ImgRequestTopLeft: TStateImage read FImgRequestTopLeft write FImgRequestTopLeft;
    property ImgRequestTopRight: TStateImage read FImgRequestTopRight write FImgRequestTopRight;
    property ImgRequestBottomLeft: TStateImage read FImgRequestBottomLeft write FImgRequestBottomLeft;
    property ImgRequestBottomRight: TStateImage read FImgRequestBottomRight write FImgRequestBottomRight;
    // Внешние края
    property ImgRequestExtTopLeft: TStateImage read FImgRequestExtTopLeft write FImgRequestExtTopLeft;
    property ImgRequestExtTopRight: TStateImage read FImgRequestExtTopRight write FImgRequestExtTopRight;
    property ImgRequestExtBottomLeft: TStateImage read FImgRequestExtBottomLeft write FImgRequestExtBottomLeft;
    property ImgRequestExtBottomRight: TStateImage read FImgRequestExtBottomRight write FImgRequestExtBottomRight;

    property SkinTabList: TSkinTabList read FSkinTabList write FSkinTabList;
  end;

implementation

{ TSkinTabControl }

constructor TSkinTabControl.Create;
begin
  FButton := TFontSkin.Create;
end;

destructor TSkinTabControl.Destroy;
begin
  FButton.Free;
  inherited;
end;


end.
