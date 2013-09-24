//  ScrollBarNormal.pas
//
//  ќбычна€ полоса прокрутки с настраиваемым стилем
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ScrollBarNormal;

interface
  {$I tox.inc}

uses
  Graphics, Types, Controls, ScrollBarNormalStyle;

type
  TScrollBarNormal = class(TCustomControl)
  private
    FPageSize: Integer;
    FPosition: Integer;
    FListSize: Integer;
    procedure SetPageSize(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetListSize(const Value: Integer);
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
  public
    property ListSize: Integer read FListSize write SetListSize;
    property PageSize: Integer read FPageSize write SetPageSize;
    property Position: Integer read FPosition write SetPosition;
  end;

implementation

{ TScrollBarNormal }

procedure TScrollBarNormal.CreateWnd;
begin
  inherited;
  DoubleBuffered := True;

  Constraints.MinWidth := TSBNStyle.MinWidth;
  Color := TSBNStyle.BackgroundColor;
end;

{*  –исование полоски прокрутки с активным стилем
 *
 *}
procedure TScrollBarNormal.Paint;
var
  DrawColor: TColor;
  HeightMaximal: Integer;
  OtherListSize: Integer;
  SliderSize: Integer;
  SlideMaxHeight: Integer;
  DrawTop: Integer;
  DrawBottom: Integer;
  SliderRect: TRect;
begin
  inherited;

  if (ListSize <= PageSize) or (FListSize <= 0) or (FPageSize <= 0) then
  begin
    if Position <> 0 then
      Position := 0;

    Exit;
  end;

  DrawColor := TSBNStyle.ColorNormal;
  HeightMaximal := ClientHeight - 2;
  OtherListSize := ListSize - PageSize;

  SliderSize := HeightMaximal - OtherListSize;
  if SliderSize < 20 then
    SliderSize := 20;

  SlideMaxHeight := HeightMaximal - SliderSize;
  DrawTop := FPosition * SlideMaxHeight div FListSize + 1;
  DrawBottom := DrawTop + SliderSize;

  SliderRect.Left := 1;
  SliderRect.Top := DrawTop + 2;
  SliderRect.Right := ClientWidth - 1;
  SliderRect.Bottom := SliderRect.Top + SliderSize - 2;
  Canvas.Brush.Color := DrawColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := DrawColor;
  Canvas.FillRect(SliderRect);

  Canvas.MoveTo(2, DrawTop + 1);
  Canvas.LineTo(ClientWidth - 2, DrawTop + 1);
  Canvas.MoveTo(3, DrawTop);
  Canvas.LineTo(ClientWidth - 3, DrawTop);

  Canvas.MoveTo(2, DrawBottom);
  Canvas.LineTo(ClientWidth - 2, DrawBottom);
  Canvas.MoveTo(3, DrawBottom + 1);
  Canvas.LineTo(ClientWidth - 3, DrawBottom + 1);

  Canvas.Pen.Color := $302F30;
  Canvas.Pixels[1, DrawTop + 1] := Canvas.Pen.Color;
  Canvas.Pixels[ClientWidth - 2, DrawTop + 1] := Canvas.Pen.Color;
  Canvas.Pixels[2, DrawTop] := Canvas.Pen.Color;
  Canvas.Pixels[ClientWidth - 3, DrawTop] := Canvas.Pen.Color;

  Canvas.Pixels[1, DrawBottom] := Canvas.Pen.Color;
  Canvas.Pixels[ClientWidth - 2, DrawBottom] := Canvas.Pen.Color;
  Canvas.Pixels[2, DrawBottom + 1] := Canvas.Pen.Color;
  Canvas.Pixels[ClientWidth - 3, DrawBottom + 1] := Canvas.Pen.Color;
end;

procedure TScrollBarNormal.SetListSize(const Value: Integer);
begin
  FListSize := Value;
  if FPosition > FListSize then
    FPosition := FListSize;
  if FListSize < 0 then
    FListSize := 0;
  Invalidate;
end;

procedure TScrollBarNormal.SetPageSize(const Value: Integer);
begin
  FPageSize := Value;
  Invalidate;
end;

procedure TScrollBarNormal.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  if FPosition < 0 then
    FPosition := 0;

  if FPosition > FListSize then
    FPosition := FListSize;
  Invalidate;
end;

end.
