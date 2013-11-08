// SkinMessageList.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit SkinMessageList;

interface

uses
  SkinTypes, Graphics, StringUtils;

type
  TSkinMessageList = class
  private
    FBackColor: TColor;
    FSelectBackColor: TColor;
    FColNameWidth: Integer;
    FDateFormat: DataString;
    FTimeFormat: DataString;
    FNamePositionLeft: Integer;
    FNameMarginRight: Integer;
    FMessageHeaderHeight: Integer;
    FMessageBottomMargin: Integer;
    FTextFont: TFontSkin;
    FNameFont: TFontSkin;
    FDateFont: TFontSkin;
  public
    constructor Create;
    destructor Destroy; override;

    // Цвет фона
    property BackColor: TColor read FBackColor write FBackColor;

    // Параметры шрифта пользовательского тектста
    property TextFont: TFontSkin read FTextFont;

    // Параметры шрифта имени пользователя
    property NameFont: TFontSkin read FNameFont;

    // Параметры шрифта даты и времени
    property DateFont: TFontSkin read FDateFont;

    // Расстояние от левого края до имении пользователя
    property NamePositionLeft: Integer read FNamePositionLeft write FNamePositionLeft;
    // Расстояние от правого края текста до следующего элемента
    property NameMarginRight: Integer read FNameMarginRight write FNameMarginRight;

    // Формат вывода даты
    property DateFormat: DataString read FDateFormat write FDateFormat;

    // Формат вывода времени
    property TimeFormat: DataString read FTimeFormat write FTimeFormat;

    // Цвет выделения
    property SelectBackColor: TColor read FSelectBackColor write FSelectBackColor;

    // Ширина колонак с именами и датами (временем)
    property ColNameWidth: Integer read FColNameWidth write FColNameWidth;

    // Высота заголовка сообщения (Отступ и разделительная линия)
    property MessageHeaderHeight: Integer read FMessageHeaderHeight write FMessageHeaderHeight;
    // Отступ от нижней частисообщения до следующего
    property MessageBottomMargin: Integer read FMessageBottomMargin write FMessageBottomMargin;

    procedure SetCanvasForText(Canvas: TCanvas; Select: Boolean = False;
      IsMy: Boolean = False);
    procedure SetCanvasForName(Canvas: TCanvas; Select: Boolean = False;
      IsMy: Boolean = False);
    procedure SetCanvasForDate(Canvas: TCanvas; Select: Boolean = False;
      IsMy: Boolean = False);
  end;

implementation

{ TSkinMessageList }

constructor TSkinMessageList.Create;
begin
  FTextFont := TFontSkin.Create;
  FNameFont := TFontSkin.Create;
  FDateFont := TFontSkin.Create;
end;

destructor TSkinMessageList.Destroy;
begin
  FNameFont.Free;
  FTextFont.Free;
  FDateFont.Free;
  inherited;
end;

procedure TSkinMessageList.SetCanvasForDate(Canvas: TCanvas; Select: Boolean;
  IsMy: Boolean);
begin
  if Select then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := SelectBackColor;
  end
  else
    Canvas.Brush.Style := bsClear;

  Canvas.Font.Name := DateFont.Name;
  Canvas.Font.Size := DateFont.Size;
  Canvas.Font.Style := DateFont.Styles;

  if Select then
    Canvas.Font.Color := DateFont.Color[1]
  else if IsMy then
    Canvas.Font.Color := DateFont.Color[2]
  else
    Canvas.Font.Color := DateFont.Color[0];
end;

procedure TSkinMessageList.SetCanvasForName(Canvas: TCanvas; Select: Boolean;
  IsMy: Boolean);
begin
  if Select then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := SelectBackColor;
  end
  else
    Canvas.Brush.Style := bsClear;

  Canvas.Font.Name := NameFont.Name;
  Canvas.Font.Size := NameFont.Size;
  Canvas.Font.Style := NameFont.Styles;

  if Select then
    Canvas.Font.Color := NameFont.Color[1]
  else if IsMy then
    Canvas.Font.Color := NameFont.Color[2]
  else
    Canvas.Font.Color := NameFont.Color[0];
end;

procedure TSkinMessageList.SetCanvasForText(Canvas: TCanvas; Select: Boolean;
  IsMy: Boolean);
begin
  if Select then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := SelectBackColor;
  end
  else
    Canvas.Brush.Style := bsClear;

  Canvas.Font.Name := TextFont.Name;
  Canvas.Font.Size := TextFont.Size;
  Canvas.Font.Style := TextFont.Styles;

  if Select then
    Canvas.Font.Color := TextFont.Color[1]
  else if IsMy then
    Canvas.Font.Color := TextFont.Color[2]
  else
    Canvas.Font.Color := TextFont.Color[0];
end;

end.
