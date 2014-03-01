unit TabView;

interface

uses
  Classes, Controls, SysUtils, Types, Graphics, SkinTabControl, StringUtils,
  ActiveRegion, ImageUtils;

type
  TTabPosition = (tpFirst, tpCenter, tpLast);
  TGetImage = (giTopLeft, giTopRight, giBottomLeft, giBottomRight, giExtTopLeft,
    giExtTopRight, giExtBottomLeft, giExtBottomRight);

  TColorTheme = (ct1, ct2);

  TTabView = class(TGraphicControl)
  private
    FShowContent: Boolean;
    FBorderLeft: Integer;
    FBorderRight: Integer;
    FHeaderWidth: Integer;
    FSkin: TSkinTabControl;
    FOnChangeHeader: TNotifyEvent;
    FTabPosition: TTabPosition;
    FTextHeader: DataString;
    FTabColor: TColor;
    FColorTheme: TColorTheme;

    procedure EventChangeHeader;

    procedure DrawRectangleSkin(Value: TRect; Color: TColor; ImgTopLeft,
      ImgTopRight, ImgBottomLeft, ImgBottomRight: TBitmap);
    procedure PaintHeader;
    procedure CalcHeaderWidth(ALeft, AWidth: Integer);
    procedure SetTextHeader(const Value: DataString);
    function GetSkinImage(Image: TGetImage; DownState: TDownState;
      IsList: Boolean): TBitmap;
    procedure SetTabColor(const Value: TColor);
    function GetTabHeight: Integer;
    procedure PaintContent;
  protected
    procedure Paint; override;
    procedure DoCreate; virtual; abstract;
  public
    constructor Create(AOwner: TComponent; ASkin: TSkinTabControl); reintroduce;
    destructor Destroy; override;

    procedure CursorMessage(RegionMessage: TRegionMessage; x, y: Integer;
      Button: TMouseButton; Shift: TShiftState); virtual;

    function GetHeight: Integer; virtual; abstract;
    procedure Repaint(ALeft, AWidth: Integer; TabPosition: TTabPosition);
    procedure SetParent(AOwner: TWinControl);

    procedure ShowContent;
    procedure HideContent;

    property ColorTheme: TColorTheme read FColorTheme write FColorTheme;
    property HeaderWidth: Integer read FHeaderWidth;
    property Skin: TSkinTabControl read FSkin write FSkin;
    property TabColor: TColor read FTabColor write SetTabColor;
    property TextHeader: DataString read FTextHeader write SetTextHeader;
    property TabHeight: Integer read GetTabHeight;

    property OnChangeHeader: TNotifyEvent read FOnChangeHeader write FOnChangeHeader;
  end;

implementation

const
  DOWN_ARROW: DataString = #$25BE;
  UP_ARROW: DataString = #$25B4;


{ TTabView }

constructor TTabView.Create(AOwner: TComponent; ASkin: TSkinTabControl);
begin
  inherited Create(AOwner);
  FShowContent := False;
  FSkin := ASkin;
end;

procedure TTabView.CursorMessage(RegionMessage: TRegionMessage; x, y: Integer;
  Button: TMouseButton; Shift: TShiftState);
begin
  case RegionMessage of
    rmMouseEnter: ;
    rmMouseLeave: ;
    rmMouseMove: ;
    rmMouseDown:
      begin
        if (x >= FBorderLeft) and (x <= FBorderLeft + FHeaderWidth) and
          (y >= Skin.ButtonMarginTop) and (y <= Skin.ButtonMarginTop + Skin.ButtonHeight) then
        begin
          if FShowContent then
            HideContent
          else
            ShowContent;
        end;
      end;
    rmMouseUp: ;
    rmMouseClick: ;
    rmMouseDblClick: ;
  end;
end;

destructor TTabView.Destroy;
begin
  inherited;
end;

{ *  Выводит прямоугольник Value с цветом фона Color, у которого вместо
  *  углов используются изображения
  * }
procedure TTabView.DrawRectangleSkin(Value: TRect; Color: TColor; ImgTopLeft,
  ImgTopRight, ImgBottomLeft, ImgBottomRight: TBitmap);
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;

  Canvas.FillRect(Value);
  Canvas.Draw(Value.Left, Value.Top, ImgTopLeft);
  Canvas.Draw(Value.Right - ImgTopRight.Width, Value.Top, ImgTopRight);
  Canvas.Draw(Value.Right - ImgBottomRight.Width, Value.Bottom -
    ImgBottomRight.Height, ImgBottomRight);
  Canvas.Draw(Value.Left, Value.Bottom - ImgBottomLeft.Height, ImgBottomLeft);
end;

procedure TTabView.EventChangeHeader;
begin
  if Assigned(FOnChangeHeader) then
  begin
    FOnChangeHeader(Self);
  end;
end;

{ *  Возвращает изображение с заданными параметрами
  * }
function TTabView.GetSkinImage(Image: TGetImage; DownState: TDownState;
  IsList: Boolean): TBitmap;
var
  i: Integer;
begin
  i := Integer(DownState);

  case Image of
    giTopLeft:
      if IsList then
        Result := FSkin.ImgListTopLeft[i]
      else
        Result := FSkin.ImgRequestTopLeft[i];

    giTopRight:
      if IsList then
        Result := FSkin.ImgListTopRight[i]
      else
        Result := FSkin.ImgRequestTopRight[i];

    giBottomLeft:
      if IsList then
        Result := FSkin.ImgListBottomLeft[i]
      else
        Result := FSkin.ImgRequestBottomLeft[i];

    giBottomRight:
      if IsList then
        Result := FSkin.ImgListBottomRight[i]
      else
        Result := FSkin.ImgRequestBottomRight[i];

    giExtTopLeft:
      if IsList then
        Result := FSkin.ImgListExtTopLeft[i]
      else
        Result := FSkin.ImgRequestExtTopLeft[i];

    giExtTopRight:
      if IsList then
        Result := FSkin.ImgListExtTopRight[i]
      else
        Result := FSkin.ImgRequestExtTopRight[i];

    giExtBottomLeft:
      if IsList then
        Result := FSkin.ImgListExtBottomLeft[i]
      else
        Result := FSkin.ImgRequestExtBottomLeft[i];

    giExtBottomRight:
      if IsList then
        Result := FSkin.ImgListExtBottomRight[i]
      else
        Result := FSkin.ImgRequestExtBottomRight[i];
  else
    //TODO: Чтобы избежать ошибки
    Result := FSkin.ImgRequestExtBottomRight[i];
  end;
end;

function TTabView.GetTabHeight: Integer;
begin
  if FShowContent then
  begin
    Result := Skin.ButtonMarginTop * 2 + Skin.ButtonMargintBottom * 2 + Skin.ButtonHeight +
      GetHeight;
  end
  else
  begin
    Result := Skin.ButtonMarginTop + Skin.ButtonMargintBottom + Skin.ButtonHeight;
  end;
end;

procedure TTabView.Paint;
begin
  inherited;
  PaintHeader;
end;

{ *  Рисует фон заголовка и текст вкладки
  * }
procedure TTabView.PaintHeader;
var
  BLeft, BRight: Integer;
  BTop, BBottom: Integer;
  ElementLeft, ElementTop: Integer;
  PaintRect: TRect;
  IconSize: TSize;
  IconText: DataString;
  CaptionText: DataString;

  TmpImageList: Boolean;
  DownState: TDownState;
begin
  BTop := Skin.ButtonMarginTop;
  BBottom := BTop + Skin.ButtonHeight;
  BRight := FBorderRight - Skin.ButtonMarginRight;
  if FTabPosition = tpFirst then
  begin
    BLeft := FBorderLeft + Skin.ButtonMarginLeft;
  end
  else
  begin
    BLeft := FBorderLeft;
  end;

  TmpImageList := ColorTheme = ct1;
  DownState := dsNone;

  PaintRect := Rect(BLeft, BTop, BRight, BBottom);
  DrawRectangleSkin(PaintRect, FTabColor,
    GetSkinImage(giTopLeft, DownState, TmpImageList),
    GetSkinImage(giTopRight, DownState, TmpImageList),
    GetSkinImage(giBottomLeft, DownState, TmpImageList),
    GetSkinImage(giBottomRight, DownState, TmpImageList)
  );

  // Рисование фона для содержимого вкладки
  if FShowContent then
  begin
    PaintContent;
  end;

  // Вывод значка
  Skin.Button.SetCanvas(Canvas);

  if FShowContent then
  begin
    IconText := ' ' + UP_ARROW;
  end
  else
  begin
    IconText := ' ' + DOWN_ARROW;
  end;

  IconSize := Canvas.TextExtent(IconText);
  ElementLeft := BRight - Skin.CaptionMarginRight - IconSize.cx;
  ElementTop := BTop + (Skin.ButtonHeight - IconSize.cy) div 2;
  Canvas.TextOut(ElementLeft, ElementTop, IconText);

  CaptionText := FTextHeader;
  PaintRect := Rect(
    BLeft + Skin.CaptionMarginLeft,
    ElementTop,
    ElementLeft,
    ElementTop + IconSize.cy
  );
  TextRectEndEllipsis(Canvas, PaintRect, CaptionText);
end;

{ *  Рисует фон для содержимого вкладки
  * }
procedure TTabView.PaintContent;
var
  ContentHeight: Integer;
  CLeft, CTop, CRight, CBottom: Integer;
  PaintRect: TRect;

  TmpImageList: Boolean;
  DownState: TDownState;
begin
  ContentHeight := GetHeight;

  CLeft := Skin.ButtonMarginLeft;
  CTop := Skin.ButtonMarginTop{ * 2} + Skin.ButtonMargintBottom + Skin.ButtonHeight;
  CRight := ClientWidth - Skin.ButtonMarginRight;
  CBottom := CTop + ContentHeight;

  TmpImageList := ColorTheme = ct1;
  DownState := dsNone;

  PaintRect := Rect(CLeft, CTop, CRight, CBottom);
  DrawRectangleSkin(PaintRect, FTabColor,
    GetSkinImage(giTopLeft, DownState, TmpImageList),
    GetSkinImage(giTopRight, DownState, TmpImageList),
    GetSkinImage(giBottomLeft, DownState, TmpImageList),
    GetSkinImage(giBottomRight, DownState, TmpImageList)
  );

  if FTabPosition = tpFirst then
  begin
    CLeft := FBorderLeft + Skin.ButtonMarginLeft;
  end
  else
  begin
    CLeft := FBorderLeft;
  end;

  PaintRect := Rect(
    CLeft,
    CTop - Skin.ButtonMarginTop{ * 3} * 2,
    FBorderRight - Skin.ButtonMarginRight,
    CTop + Skin.ButtonMarginTop
  );

  Canvas.FillRect(PaintRect);
end;

{ *  Вызывает перерисовку компонента с установленной границей рисования заголовка
  * }
procedure TTabView.Repaint(ALeft, AWidth: Integer; TabPosition: TTabPosition);
begin
  FTabPosition := TabPosition;
  CalcHeaderWidth(ALeft, AWidth);
end;

{ *  Рассчитывает размер заголовка, исходя из допустимого ограничения
  * }
procedure TTabView.CalcHeaderWidth(ALeft, AWidth: Integer);
var
  TabMaxWidth: Integer;
  ButtonContentWidth: Integer;
begin
  FBorderLeft := ALeft;
//  if FBorderLeft = 0 then
//  begin
//    FBorderLeft := Skin.ButtonMarginLeft;
//    AWidth := AWidth - Skin.ButtonMarginLeft;
//  end;

  Skin.Button.SetCanvas(Canvas);
  ButtonContentWidth := Skin.CaptionMarginLeft + Skin.CaptionMarginRight +
    Canvas.TextExtent(FTextHeader + ' ' + DOWN_ARROW).cx;

  if FTabPosition = tpFirst then
  begin
    TabMaxWidth := Skin.ButtonMarginLeft + Skin.ButtonMarginRight + ButtonContentWidth;
  end
  else
  begin
    TabMaxWidth := Skin.ButtonMarginRight + ButtonContentWidth;
  end;

  if TabMaxWidth > AWidth then
  begin
    TabMaxWidth := AWidth;
  end;

  // Если это последняя вкладка, растянуть ее
  if (FTabPosition = tpLast) and (TabMaxWidth < AWidth) then
  begin
    TabMaxWidth := AWidth;
  end;

  FHeaderWidth := TabMaxWidth;
  FBorderRight := TabMaxWidth + FBorderLeft;
end;

{ *  Отрисовывает содержимое вкладки
  * }
procedure TTabView.SetParent(AOwner: TWinControl);
begin
  Parent := AOwner;
  DoCreate;
end;

procedure TTabView.SetTabColor(const Value: TColor);
begin
  FTabColor := Value;
  Invalidate;
end;

procedure TTabView.SetTextHeader(const Value: DataString);
begin
  if FTextHeader <> Value then
  begin
    FTextHeader := Value;
    EventChangeHeader;
  end;
end;

procedure TTabView.ShowContent;
begin
  FShowContent := True;
  EventChangeHeader;
end;

{ *  Прячет содержимое вкладки. TODO: скорее всего нужно спрятать в Private
  * }
procedure TTabView.HideContent;
begin
  FShowContent := False;
  EventChangeHeader;
end;


end.
