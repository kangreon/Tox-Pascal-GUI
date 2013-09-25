//  ScrollBarNormal.pas
//
//  Обычная полоса прокрутки с настраиваемым стилем
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ScrollBarNormal;

interface
  {$I tox.inc}

uses
  Classes, Graphics, SysUtils, Types, Controls, ScrollBarNormalStyle,
  ActiveRegion, Messages;

type
  TScrollBarNormal = class(TGraphicControl)
  private
    FPageSize: Integer;
    FPosition: Integer;
    FListSize: Integer;
    FMouseDownY: Integer;
    // Позиция слайдера без учета одного пикселя сверху
    FSliderTop: Integer;
    FSliderTopDown: Integer;
    FSliderHeight: Integer;
    FSliderState: TDownState;
    FOnScroll: TNotifyEvent;
    procedure SetPageSize(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetListSize(const Value: Integer);
    procedure DrawSliderTop(Top, Margin: Integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ListSize: Integer read FListSize write SetListSize;
    property PageSize: Integer read FPageSize write SetPageSize;
    property Position: Integer read FPosition write SetPosition;

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;

implementation

{ TScrollBarNormal }

constructor TScrollBarNormal.Create(AOwner: TComponent);
begin
  inherited;
  Constraints.MinWidth := TSBNStyle.MinWidth;

  FSliderState := dsNone;
end;


destructor TScrollBarNormal.Destroy;
begin

  inherited;
end;

{*  Рисование полоски прокрутки с активным стилем
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

  // Зарисовка фона полосы прокрутки
  Canvas.Brush.Color := TSBNStyle.BackgroundColor;
  Canvas.FillRect(Canvas.ClipRect);

  if (ListSize <= PageSize) or (FListSize <= 0) or (FPageSize <= 0) then
  begin
    if Position <> 0 then
      Position := 0;

    Exit;
  end;

  case FSliderState of
    dsNone:
      DrawColor := TSBNStyle.SliderColorNormal;
    dsActive:
      DrawColor := TSBNStyle.SliderColorActive;
    dsDown:
      DrawColor := TSBNStyle.SliderColorDown;
  end;

  HeightMaximal := ClientHeight - 2;
  OtherListSize := ListSize - PageSize;

  SliderSize := HeightMaximal - OtherListSize;
  if SliderSize < TSBNStyle.SliderMinHeight then
    SliderSize := TSBNStyle.SliderMinHeight;

  SlideMaxHeight := HeightMaximal - SliderSize;
  DrawTop := FPosition * SlideMaxHeight div FListSize + 1;
  DrawBottom := DrawTop + SliderSize;
  FSliderTop := DrawTop;
  FSliderHeight := SliderSize;

  SliderRect := Bounds(1, DrawTop + 2, ClientWidth - 2, SliderSize - 4);

  // Рисование тела слайдера
  Canvas.Brush.Color := DrawColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := DrawColor;
  Canvas.FillRect(SliderRect);

  // Рисование верхних и нижних краев слайдера
  DrawSliderTop(DrawTop, 1);
  DrawSliderTop(DrawTop + 1, 0);
  DrawSliderTop(DrawBottom - 1, 1);
  DrawSliderTop(DrawBottom - 2, 0);
end;

procedure TScrollBarNormal.DrawSliderTop(Top, Margin: Integer);
var
  LeftDraw, RightDraw: Integer;
  Color, ColorExt: TColor;
begin
  case FSliderState of
    dsNone:
      Color := TSBNStyle.SliderColorNormal;
    dsActive:
      Color := TSBNStyle.SliderColorActive;
    dsDown:
      Color := TSBNStyle.SliderColorDown;
  end;

  case FSliderState of
    dsNone:
      ColorExt := TSBNStyle.SliderColorExtNormal;
    dsActive:
      ColorExt := TSBNStyle.SliderColorExtActive;
    dsDown:
      ColorExt := TSBNStyle.SliderColorExtDown;
  end;

  LeftDraw := Margin + 1;
  RightDraw := ClientWidth - Margin - 2;

  Canvas.Pixels[LeftDraw, Top] := ColorExt;
  Canvas.Pixels[RightDraw, Top] := ColorExt;

  Canvas.Pen.Color := Color;
  Canvas.MoveTo(LeftDraw + 1, Top);
  Canvas.LineTo(RightDraw, Top);
end;

procedure TScrollBarNormal.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (Y >= FSliderTop) and (Y <= FSliderTop + FSliderHeight) then
  begin
    FMouseDownY := Y;
    FSliderTopDown := FSliderTop;

    FSliderState := dsDown;
    Invalidate;
  end;
end;

procedure TScrollBarNormal.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  SeekSize: Integer;
  BarHeight: Integer;
begin
  inherited;
  if FSliderState = dsDown then
  begin
    SeekSize := y - FMouseDownY;
    FSliderTop := FSliderTopDown + SeekSize;
    BarHeight := ClientHeight - 2;
    if FSliderTop < 0 then
      FSliderTop := 0;

    if FSliderTop + FSliderHeight > BarHeight then
      FSliderTop := BarHeight - FSliderHeight;

    Position := FListSize * FSliderTop div (BarHeight - FSliderHeight);
  end
  else
  begin
    if (Y >= FSliderTop) and (Y <= FSliderTop + FSliderHeight) then
    begin
      if FSliderState <> dsActive then
      begin
        FSliderState := dsActive;
        Invalidate;
      end;
    end
    else
    begin
      if FSliderState <> dsNone then
      begin
        FSliderState := dsNone;
        Invalidate;
      end;
    end;

  end;
end;

procedure TScrollBarNormal.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    if FSliderState = dsDown then
    begin
      if (Y >= FSliderTop) and (Y <= FSliderTop + FSliderHeight) and
        (X >= 0) and (X <= ClientWidth) then
      begin
        FSliderState := dsActive;
      end
      else
      begin
        FSliderState := dsNone;
      end;

      Invalidate;
    end;
  end;
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

  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TScrollBarNormal.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_MOUSELEAVE:
      begin
        if FSliderState <> dsNone then
        begin
          FSliderState := dsNone;
          Invalidate;
        end;
      end;
  end;
end;

end.
