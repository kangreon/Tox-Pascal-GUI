//  ImageUtils.pas
//
//  Реализует кросскомпиляторные функции для вывода текста с поддержкой
//  Unicode.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
//TODO: Исправить все проблемы и почистить код
unit ImageUtils;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  Graphics, StringUtils;

{$IFNDEF NEW_DELPHI}
type
  TTextFormats = (tfBottom, tfCalcRect, tfCenter, tfEditControl, tfEndEllipsis,
    tfPathEllipsis, tfExpandTabs, tfExternalLeading, tfLeft, tfModifyString,
    tfNoClip, tfNoPrefix, tfRight, tfRtlReading, tfSingleLine, tfTop,
    tfVerticalCenter, tfWordBreak, tfHidePrefix, tfNoFullWidthCharBreak,
    tfPrefixOnly, tfTabStop, tfWordEllipsis, tfComposited);

  TTextFormat = set of TTextFormats;
{$ENDIF}

function TextExtentW(Canvas: TCanvas; const Text: DataString): TSize;
procedure TextOutW(Canvas: TCanvas; X, Y: Integer; const Text: DataString);
procedure TextRectW(Canvas: TCanvas; var Rect: TRect; var Text: DataString;
  TextFormat: TTextFormat = []);

implementation

type
  TCanvasEx = class(TCanvas)
  end;

procedure TextRectW(Canvas: TCanvas; var Rect: TRect; var Text: DataString;
  TextFormat: TTextFormat = []);
  {$IFDEF FPC}
var
  TextStyle: TTextStyle;
  {$ENDIF}
begin
  {$IFDEF NEW_DELPHI}
  Canvas.TextRect(Rect, Text, TextFormat);
  {$ELSE}
    {$IFDEF FPC}
    TextStyle := Canvas.TextStyle;
    TextStyle.EndEllipsis := tfEndEllipsis in TextFormat;
    TextStyle.Wordbreak := False;
    TextStyle.SingleLine := True;

    Canvas.TextRect(Rect, Rect.Left, Rect.Top, Text, TextStyle);
    {$ENDIF}
  {$ENDIF}
end;

function TextExtentW(Canvas: TCanvas; const Text: DataString): TSize;
begin
  {$IFDEF NEW_DELPHI}
  Result := Canvas.TextExtent(Text);
  {$ELSE}
    {$IFDEF FPC}
      Result := Canvas.TextExtent(Text);
    {$ELSE}
      TCanvasEx(Canvas).RequiredState([csHandleValid, csFontValid]);
      Result.cX := 0;
      Result.cY := 0;
      Windows.GetTextExtentPoint32W(TCanvasEx(Canvas).Handle, PWideChar(Text), Length(Text), Result);
    {$ENDIF}
  {$ENDIF}
end;

procedure TextOutW(Canvas: TCanvas; X, Y: Integer; const Text: DataString);
begin
  {$IFDEF NEW_DELPHI}
  Canvas.TextOut(X, Y, Text);
  {$ELSE}
    {$IFDEF FPC}
      Canvas.TextOut(X, Y, Text);
    {$ELSE}
      TCanvasEx(Canvas).Changing;
      TCanvasEx(Canvas).RequiredState([csHandleValid, csFontValid, csBrushValid]);
      if TCanvasEx(Canvas).CanvasOrientation = coRightToLeft then Inc(X, TextExtentW(Canvas, Text).cx + 1);
      Windows.ExtTextOutW(TCanvasEx(Canvas).Handle, X, Y, TCanvasEx(Canvas).TextFlags, nil, PWideChar(Text),
       Length(Text), nil);
      Canvas.MoveTo(X + TextExtentW(Canvas, Text).cx, Y);
      TCanvasEx(Canvas).Changed;
    {$ENDIF}
  {$ENDIF}
end;

end.
