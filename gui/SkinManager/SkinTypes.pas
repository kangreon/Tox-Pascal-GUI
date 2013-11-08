// SkinTypes.pas
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit SkinTypes;

interface

uses
  Graphics, SysUtils, StringUtils;

type
  TMouseState = (msNone, msActive, msDown);
  TStateImage = array[0..2] of TBitmap;
  TStateColor = array[0..2] of TColor;

  TFontSkin = class
  private
    FName: DataString;
    FSize: Integer;
    FColor: TStateColor;
    FStyles: TFontStyles;
  public
    constructor Create;

    procedure SetCanvas(Canvas: TCanvas); overload;
    procedure SetCanvas(Canvas: TCanvas; State: TMouseState); overload;

    property Name: DataString read FName write FName;
    property Size: Integer read FSize write FSize;
    property Color: TStateColor read FColor write FColor;
    property Styles: TFontStyles read FStyles write FStyles;
  end;

function InsertColor(Value: array of Byte): TStateColor; overload;
function InsertColor(Value: array of TColor): TStateColor; overload;
function RGB(r, g, b: Byte): TColor;

function FontStylesToInt(Styles: TFontStyles): Integer;
function IntToFontStyles(Value: Integer): TFontStyles;

implementation

function FontStylesToInt(Styles: TFontStyles): Integer;
begin
  {$IFDEF FPC}
  Result := Integer(Styles);
  {$ELSE}
  Result := Integer(Byte(Styles));
  {$ENDIF}
end;

function IntToFontStyles(Value: Integer): TFontStyles;
begin
  {$IFDEF FPC}
  Result := TFontStyles(Value);
  {$ELSE}
  Byte(Result) := Byte(Value);
  {$ENDIF}
end;


function InsertColor(Value: array of Byte): TStateColor;
begin
  if Length(Value) <> 9 then
    raise Exception.Create('Value in InsertColor 1');

  Result := InsertColor([
    RGB(Value[0], Value[1], Value[2]),
    RGB(Value[3], Value[4], Value[5]),
    RGB(Value[6], Value[7], Value[8])
  ]);
end;

function InsertColor(Value: array of TColor): TStateColor;
var
  i: Integer;
begin
  if Length(Value) <> 3 then
    raise Exception.Create('Value in InsertColor 2');

  for i := Low(Value) to High(Value) do
    Result[i] := Value[i];
end;

function RGB(r, g, b: Byte): TColor;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;

{ TFontData }

constructor TFontSkin.Create;
begin
  FName := 'DejaVu Sans';
  FSize := 10;
  FColor[0] := clGray;
  FColor[1] := clGray;
  FColor[2] := clGray;

  FStyles := [];
end;

procedure TFontSkin.SetCanvas(Canvas: TCanvas);
begin
  SetCanvas(Canvas, TMouseState.msNone);
end;

procedure TFontSkin.SetCanvas(Canvas: TCanvas; State: TMouseState);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := Name;
  Canvas.Font.Size := Size;
  Canvas.Font.Color := Color[Integer(State)];
  Canvas.Font.Style := Styles;
end;

end.
