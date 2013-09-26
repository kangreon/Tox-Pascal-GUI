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
  TDrawTextFlags = Cardinal;
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
  {$IFNDEF NEW_DELPHI}
var
  Format: TDrawTextFlags;
  i: TTextFormats;
  {$ENDIF}
  {$IFDEF FPC}
  TextStyle: TTextStyle;
  {$ENDIF}
begin
  {$IFDEF NEW_DELPHI}
  Canvas.TextRect(Rect, Text, TextFormat);
  {$ELSE}
    {$IFDEF FPC}
  //Format := 0;
  //for i := Low(TTextFormats) to High(TTextFormats) do
  //begin
  //  if i in TextFormat then
  //  case i of
  //    tfEndEllipsis:
  //      Format := Format or DT_END_ELLIPSIS;
  //  end
  //end;
  TextStyle := Canvas.TextStyle;
  TextStyle.EndEllipsis := tfEndEllipsis in TextFormat;

  Canvas.TextRect(Rect, Rect.Left, Rect.Top, Text, TextStyle);

  //DrawText(TCanvasEx(Canvas).Handle, PChar(Text), Length(Text), Rect, Format);

    {$ELSE}
      TCanvasEx(Canvas).RequiredState([csHandleValid, csFontValid]);

      Format := 0;
      for i := Low(TTextFormats) to High(TTextFormats) do
      begin
        if i in TextFormat then
        case i of
          tfEndEllipsis:
            Format := Format or DT_END_ELLIPSIS;
        end
      end;
      
      Windows.DrawTextExW(TCanvasEx(Canvas).Handle, PWideChar(Text), Length(Text), Rect, Format, nil);
//      if tfModifyString in TextFormat then
//        SetLength(Text, StrLen(PWideChar(Text)));
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
