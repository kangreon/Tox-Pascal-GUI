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
  Classes, Graphics, SysUtils, Types, Controls, ActiveRegion, Messages,
  SkinUserList;

type
  TScrollBarNormal = class(TGraphicControl)
  private
    FPageSize: Integer;
    FPosition: Integer;
    FListSize: Integer;
    FMouseDownY: Integer;
    FSkin: TSkinUserList;
    // Позиция слайдера без учета одного пикселя сверху
    FSliderTop: Integer;
    FSliderTopDown: Integer;
    FSliderHeight: Integer;
    FSliderState: TDownState;
    FOnScroll: TNotifyEvent;
    procedure SetPageSize(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetListSize(const Value: Integer);
    procedure DrawSlider(Left, Top, BottomTop: Integer; Center: TRect);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent; Skin: TSkinUserList); reintroduce;
    destructor Destroy; override;

    property ListSize: Integer read FListSize write SetListSize;
    property PageSize: Integer read FPageSize write SetPageSize;
    property Position: Integer read FPosition write SetPosition;

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;

implementation

{ TScrollBarNormal }

constructor TScrollBarNormal.Create(AOwner: TComponent; Skin: TSkinUserList);
begin
  inherited Create(AOwner);
  FSkin := Skin;

  Constraints.MinWidth := FSkin.ScrollBarWidth;
  Constraints.MaxWidth := Constraints.MinWidth;
  Width := Constraints.MinWidth;

  FPosition := 0;
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
  HeightMaximal: Integer;
  OtherListSize: Integer;
  SliderHeight: Integer;
  SlideMaxHeight: Integer;
  DrawTop: Integer;
  SliderRect: TRect;
  SliderMinHeight, CenterHeight: Integer;
  SliderWidth: Integer;
  SliderLeft: Integer;
  SliderTopHeight: Integer;
begin
  inherited;

  // Зарисовка фона полосы прокрутки
  Canvas.Brush.Color := FSkin.ScrollBarBack;
  Canvas.FillRect(Canvas.ClipRect);

  if (ListSize <= PageSize) or (FListSize <= 0) or (FPageSize <= 0) then
  begin
    if Position <> 0 then
      FPosition := 0;
    Exit;
  end;

  // Отступы от верхнего и нижнего края. Всегда постоянны
  HeightMaximal := ClientHeight - 2;
  OtherListSize := ListSize - PageSize;

  CenterHeight := FSkin.ScrollBarMinCenterHeight;
  SliderTopHeight := FSkin.ImgScrollBarTop[0].Height;

  SliderMinHeight := SliderTopHeight * 2 + CenterHeight;
  SliderWidth := FSkin.ImgScrollBarTop[0].Width;

  SliderHeight := HeightMaximal - OtherListSize;
  if SliderHeight < SliderMinHeight then
    SliderHeight := SliderMinHeight;

  SlideMaxHeight := HeightMaximal - SliderHeight;
  DrawTop := FPosition * SlideMaxHeight div (FListSize - FPageSize) + 1;

  FSliderTop := DrawTop;
  FSliderHeight := SliderHeight;

  SliderLeft := (ClientWidth - SliderWidth) div 2;
  SliderRect := Bounds(
    SliderLeft,
    DrawTop + SliderTopHeight,
    SliderWidth,
    SliderHeight - SliderTopHeight * 2
  );

  DrawSlider(SliderLeft, DrawTop, DrawTop + SliderHeight - SliderTopHeight,
    SliderRect);
end;

{ *  Рисует верхнюю нижнюю и центрольную части слайдера в заданной
  *  позиции.
  * }
procedure TScrollBarNormal.DrawSlider(Left, Top, BottomTop: Integer;
  Center: TRect);
var
  DrawColor: TColor;
begin
  case FSliderState of
    dsActive:
      DrawColor := FSkin.ScrollbarCenterColorActive;

    dsDown:
      DrawColor := FSkin.ScrollbarCenterColorDown;
  else
    DrawColor := FSkin.ScrollbarCenterColor;
  end;

  // Рисование центральной части слайдера
  Canvas.Brush.Color := DrawColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := DrawColor;
  Canvas.FillRect(Center);

  // Рисование краев
  if Length(FSkin.ImgScrollBarTop) = 3 then
    Canvas.Draw(Left, Top, FSkin.ImgScrollBarTop[Integer(FSliderState)]);

  if Length(FSkin.ImgScrollBarBottom) = 3 then
    Canvas.Draw(Left, BottomTop, FSkin.ImgScrollBarBottom[Integer(FSliderState)]);
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

    Position := (FListSize - FPageSize) * FSliderTop div (BarHeight - FSliderHeight);
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

  if FListSize <= FPageSize then
    FPosition := 0;
  Invalidate;
end;

procedure TScrollBarNormal.SetPageSize(const Value: Integer);
begin
  FPageSize := Value;
  if (FListSize - FPosition <= FPageSize) and (FPosition <> 0) then
  begin
    Position := FListSize - FPageSize;
  end;
  Invalidate;
end;

procedure TScrollBarNormal.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  if FPosition < 0 then
    FPosition := 0;

  if FPosition > FListSize - FPageSize then
    FPosition := FListSize - FPageSize;

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
