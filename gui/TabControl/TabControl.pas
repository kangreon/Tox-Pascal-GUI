// TabControl.pas
//
// Виджет переключения вкладок:
// * вкладка выбора фильтра отображения списка пользователей
// * вкладка уведобмления о новых запросах на добавления в список друзей
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit TabControl;

interface

uses
  Classes, SysUtils, Controls, StringUtils, Graphics, SkinTypes, SkinTabControl,
  ActiveRegion, ImageUtils, Types, TabSelectList, TabRequest, UserListSelect;

type
  TTabList = (tlNone, tlList, tlRequest);
  TGetImage = (giTopLeft, giTopRight, giBottomLeft, giBottomRight, giExtTopLeft,
    giExtTopRight, giExtBottomLeft, giExtBottomRight);

  TTabInfo = record
    ArrowSize: TSize;
    ButtonPosition: TPoint;
    ButtonSize: TSize;
    FontSize: TSize;
    SpaceSize: TSize;

    IsFullText: Boolean;
  end;

  TTabControl = class(TCustomControl)
  private
    FActiveList: TActiveRegion;
    FActiveRequest: TActiveRegion;

    FSkin: TSkinTabControl;

    FListStyle: TDownState;
    FStyleRequest: TDownState;

    FInfoList: TTabInfo;
    FInfoRequest: TTabInfo;

    FSelectTab: TTabList;
    FContentList: TTabSelectList;
    FContentRequest: TTabRequest;
    function GetSkinImage(Image: TGetImage; DownState: TDownState;
      IsList: Boolean): TBitmap;
    procedure DrawListButton;
    procedure CalcButtonSize;
    procedure SelectTab(Tab: TTabList);
    procedure ListMessages(Sender: TObject; RegionMessage: TRegionMessage;
      const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
    procedure DrawTabContent(ImageState: TDownState; IsImageList: Boolean;
      ButtonRect: TRect; IsTabLeft: Boolean);
    procedure DrawRequestButton;
    function TextExtent(const Value: string): TSize;
    procedure RequestMessages(Sender: TObject; RegionMessage: TRegionMessage;
      const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
    function DrawButton(ImageState: TDownState; IsImageList: Boolean;
      ButtonInfo: TTabInfo; Text: DataString; IsOpen,
      IsTabLeft: Boolean): TRect;
    procedure ListSelectItem(Sender: TObject; Index: Integer);
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; Skin: TSkinTabControl;
      ListSelect: TUserListSelect); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$IFDEF DEBUG}
var
  FPaintCount: Integer = 0;
{$ENDIF}

var
  DOWN_ARROW: DataString = #$25BE;
  UP_ARROW: DataString = #$25B4;

{ TTabControl }

constructor TTabControl.Create(AOwner: TComponent; Skin: TSkinTabControl;
  ListSelect: TUserListSelect);
var
  ContentMargin: TRect;
  ImgWidth, ImgHeight: Integer;
begin
  inherited Create(AOwner);

  {$IFDEF FPC}
  DOWN_ARROW := UTF8Encode(DOWN_ARROW);
  UP_ARROW := UTF8Encode(UP_ARROW);
  {$ENDIF}

  FSkin := Skin;
  Align := alTop;

  with GetSkinImage(giTopLeft, dsNone, True) do
  begin
    ImgWidth := Width;
    ImgHeight := Height;
  end;

  with ContentMargin do
  begin
    Left := FSkin.ButtonMarginLeft + ImgWidth;
    Top := ImgHeight;
    Right := FSkin.ButtonMarginRight + ImgWidth;
    Bottom := FSkin.ButtonMargintBottom + ImgHeight;
  end;

  FActiveList := TActiveRegion.Create(Self);
  FActiveList.Cursor := crHandPoint;
  FActiveList.OnCursorMessage := ListMessages;

  FActiveRequest := TActiveRegion.Create(Self);
  FActiveRequest.Cursor := crHandPoint;
  FActiveRequest.OnCursorMessage := RequestMessages;

  FContentList := TTabSelectList.Create(Self, FSkin.SkinTabList, ListSelect);
  FContentList.Align := alBottom;
  FContentList.Visible := False;
  FContentList.SetMargin(ContentMargin);
  FContentList.OnSelectItem := ListSelectItem;

  FContentRequest := TTabRequest.Create(Self, FSkin);
  FContentRequest.OnUpdateRequest := nil;
end;

procedure TTabControl.CreateWnd;
begin
  inherited;
  ControlStyle := ControlStyle - [csParentBackground];
  Color := FSkin.BackColor;

  FActiveList.Parent := Self;
  FActiveRequest.Parent := Self;
  FContentList.Parent := Self;
  FContentRequest.Parent := Self;

  SelectTab(TTabList.tlNone);

  CalcButtonSize;
end;

destructor TTabControl.Destroy;
begin
  FActiveList.Free;
  FContentList.Free;
  FContentRequest.Free;
  inherited;
end;

{ *  Возвращает размер текста с установленными в Canvas параметрами шрифта
  * }
function TTabControl.TextExtent(const Value: string): TSize;
begin
  Result := ImageUtils.TextExtentW(Canvas, Value);
end;

{ *  Рассчет размеров и расположений кнопок: выбор активного списка, запрос на
  *  добавление в друзья.
  * }
procedure TTabControl.CalcButtonSize;
var
  ArrowSize, SpaceSize: TSize;
  CaptionHorMargins: Integer;
  ButtonHorMargins: Integer;
  FontSize, ButtonSize, FontSizeReq, ButtonSizeReq: TSize;
  NewSizePlus, FreeSpace: Integer;
  ButtonWidthWitoutText, LeftPos: Integer;
  IsPlaced: Boolean;
begin
  FSkin.Button.SetCanvas(Canvas, TMouseState.msNone);
  CaptionHorMargins := FSkin.CaptionMarginLeft + FSkin.CaptionMarginRight;
  ButtonHorMargins := FSkin.ButtonMarginLeft + FSkin.ButtonMarginRight;

  ArrowSize := TextExtent(DOWN_ARROW);
  FontSize := TextExtent(FContentList.ActiveCaption);
  SpaceSize := TextExtent(' ');

  ButtonWidthWitoutText := CaptionHorMargins + ArrowSize.cx + SpaceSize.cx;

  FontSize := TextExtent(FContentList.ActiveCaption);
  FontSizeReq := TextExtent(FContentRequest.RequestCountText);

  ButtonSize.cx := ButtonWidthWitoutText + FontSize.cx;
  ButtonSize.cy := FSkin.ButtonHeight;

  ButtonSizeReq.cx := ButtonWidthWitoutText + FontSizeReq.cx;
  ButtonSizeReq.cy := FSkin.ButtonHeight;

  if FContentRequest.RequestCount > 0 then
  begin
    IsPlaced := ButtonSize.cx + ButtonSizeReq.cx + FSkin.ButtonMarginLeft +
      FSkin.ButtonMarginRight * 2 <= ClientWidth;

    if not IsPlaced then
    begin
      //TODO: Рассчет размеров кнопок с большим размером тектса
    end
    else
    begin
      FreeSpace := ClientWidth - ButtonSize.cx - FSkin.ButtonMarginLeft -
        FSkin.ButtonMarginRight * 2;
      NewSizePlus := FreeSpace - ButtonSizeReq.cx;
      ButtonSizeReq.cx := ButtonSizeReq.cx + NewSizePlus;
      FontSizeReq.cx := FontSizeReq.cx + NewSizePlus;
    end;

    LeftPos := FSkin.ButtonMarginLeft;
    FInfoList.ArrowSize := ArrowSize;
    FInfoList.ButtonPosition := Point(LeftPos, FSkin.ButtonMarginTop);
    FInfoList.ButtonSize := ButtonSize;
    FInfoList.FontSize := FontSize;
    FInfoList.SpaceSize := SpaceSize;
    FInfoList.IsFullText := IsPlaced;

    LeftPos := LeftPos + ButtonSize.cx + FSkin.ButtonMarginRight;
    FInfoRequest.ArrowSize := ArrowSize;
    FInfoRequest.ButtonPosition := Point(LeftPos, FSkin.ButtonMarginTop);
    FInfoRequest.ButtonSize := ButtonSizeReq;
    FInfoRequest.FontSize := FontSizeReq;
    FInfoRequest.SpaceSize := SpaceSize;
    FInfoRequest.IsFullText := IsPlaced;
  end
  else
  begin
    if ButtonSize.cx <= ClientWidth - ButtonHorMargins then
    begin
      // Текст помещается
      FInfoList.ButtonSize := ButtonSize;
      FInfoList.FontSize := FontSize;

      FInfoList.IsFullText := True;
    end
    else
    begin
      NewSizePlus := FontSize.cx - (ClientWidth - ButtonHorMargins -
        ButtonWidthWitoutText);

      FInfoList.ButtonSize.cx := ButtonSize.cx - NewSizePlus;
      FInfoList.ButtonSize.cy := ButtonSize.cy;
      FInfoList.FontSize.cx := FontSize.cx - NewSizePlus;
      FInfoList.FontSize.cy := FontSize.cy;

      FInfoList.IsFullText := False;
    end;

    FInfoList.ArrowSize := ArrowSize;
    FInfoList.ButtonPosition := Point(FSkin.ButtonMarginLeft,
      FSkin.ButtonMarginTop);
    FInfoList.SpaceSize := SpaceSize
  end;
end;

procedure TTabControl.Resize;
begin
  inherited;
  CalcButtonSize;
end;

{ *  События от взаимодействия мыши с кнопкой списка
  * }
procedure TTabControl.ListMessages(Sender: TObject; RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  case RegionMessage of
    rmMouseEnter: ;
    rmMouseLeave: ;
    rmMouseMove: ;
    rmMouseDown: ;
    rmMouseUp: ;

    rmMouseDblClick: ;

    rmMouseClick:
      begin
        case FSelectTab of
          tlNone, tlRequest:
            SelectTab(TTabList.tlList);

          tlList:
            SelectTab(TTabList.tlNone);
        end;

      end;
  end;
end;

procedure TTabControl.RequestMessages(Sender: TObject; RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  case RegionMessage of
    rmMouseEnter: ;
    rmMouseLeave: ;
    rmMouseMove: ;
    rmMouseDown: ;
    rmMouseUp: ;

    rmMouseDblClick: ;

    rmMouseClick:
      begin
        case FSelectTab of
          tlNone, tlList:
            SelectTab(TTabList.tlRequest);

          tlRequest:
            SelectTab(TTabList.tlNone);
        end;

      end;
  end;
end;


{ *  Указывает на кнопку, которая должна отображать содержимое
  * }
procedure TTabControl.SelectTab(Tab: TTabList);
var
  NewHeight: Integer;
begin
  FSelectTab := Tab;

  NewHeight := FSkin.ButtonHeight + FSkin.ButtonMarginTop +
    FSkin.ButtonMargintBottom;

  FContentList.Visible := False;
  FContentRequest.Visible := False;

  case FSelectTab of
    tlNone:
      begin
        Height := NewHeight;
      end;

    tlList:
      begin
        FContentList.CalcHeight;
        Height := NewHeight + FContentList.Height;

        FContentList.Visible := True;
      end;

    tlRequest:
      begin
        Height := NewHeight + FContentRequest.Height;

        FContentRequest.Visible := True;
      end;
  end;
end;

procedure TTabControl.Paint;
{$IFDEF DEBUG}
var
  PaintTime: TDateTime;
{$ENDIF}
begin
  inherited;

  {$IFDEF DEBUG}
  PaintTime := Now;
  {$ENDIF}

  // Зарисовка фона
  Canvas.Brush.Color := FSkin.BackColor;
  Canvas.Brush.Style := bsClear;
  Canvas.FillRect(ClientRect);

  DrawListButton;
  DrawRequestButton;

  {$IFDEF DEBUG}
  PaintTime := Now - PaintTime;
  FPaintCount := FPaintCount + 1;
  Canvas.Font.Name := 'Fira Sans Mono';
  Canvas.TextOut(0, 0, Format('Paint: %d Time: %s', [FPaintCount,
    FormatDateTime('ss.zzz', PaintTime)]));
  {$ENDIF}
end;

{ *  Рисование фона для открытого элемента
  * }
procedure TTabControl.DrawTabContent(ImageState: TDownState;
  IsImageList: Boolean; ButtonRect: TRect; IsTabLeft: Boolean);
var
  ImgWidth, ImgHeight: Integer;
  DrawLeft, DrawTop: Integer;
  DrawRect: TRect;
  TopContent, BottomContent, LeftContent, RightContent: Integer;
begin
  with GetSkinImage(giExtTopLeft, ImageState, IsImageList) do
  begin
    ImgWidth := Width;
    ImgHeight := Height;
  end;

  TopContent := ButtonRect.Bottom + FSkin.ButtonMargintBottom;
  BottomContent := ClientHeight - FSkin.ButtonMargintBottom;
  LeftContent := FSkin.ButtonMarginLeft;
  RightContent := ClientWidth - FSkin.ButtonMarginRight;

  // Рисование углов

  DrawLeft := FSkin.ButtonMarginLeft;
  DrawTop := BottomContent - ImgHeight;
  Canvas.Draw(DrawLeft, DrawTop,
    GetSkinImage(giBottomLeft, ImageState, IsImageList)
  );

  DrawLeft := ClientWidth - FSkin.ButtonMarginRight - ImgWidth;
  Canvas.Draw(DrawLeft, DrawTop,
    GetSkinImage(giBottomRight, ImageState, IsImageList)
  );

  DrawTop := TopContent;

  if IsTabLeft then
  begin
    Canvas.Draw(DrawLeft, DrawTop,
      GetSkinImage(giTopRight, ImageState, IsImageList)
    );

    DrawTop := TopContent - ImgHeight;
    DrawLeft := ButtonRect.Right;
    Canvas.Draw(DrawLeft, DrawTop,
      GetSkinImage(giExtBottomLeft, ImageState, IsImageList)
    );
  end
  else
  begin
    DrawLeft := FSkin.ButtonMarginLeft;
    Canvas.Draw(DrawLeft, DrawTop,
      GetSkinImage(giTopLeft, ImageState, IsImageList)
    );

    // Внутренний угол
    DrawTop := TopContent - ImgHeight;
    DrawLeft := ButtonRect.Left - ImgWidth;
    Canvas.Draw(DrawLeft, DrawTop,
      GetSkinImage(giExtBottomRight, ImageState, IsImageList)
    );
  end;

  // Закраска центральной части
  DrawRect.Top := TopContent;
  DrawRect.Bottom := BottomContent - ImgHeight;
  DrawRect.Left := LeftContent;
  DrawRect.Right := RightContent;
  if IsTabLeft then
    DrawRect.Right := DrawRect.Right - ImgWidth
  else
    DrawRect.Left := DrawRect.Left + ImgWidth;

  Canvas.FillRect(DrawRect);

  // Закраска перехода между кнопкой и содержимым
  DrawRect := ButtonRect;
  DrawRect.Top := ButtonRect.Bottom - ImgHeight;
  DrawRect.Bottom := TopContent;
  Canvas.FillRect(DrawRect);

  // Закразка нижнего края содержимого
  with DrawRect do
  begin
    Left := LeftContent + ImgWidth;
    Top := BottomContent - ImgHeight;
    Right := RightContent - ImgWidth;
    Bottom := BottomContent;
  end;
  Canvas.FillRect(DrawRect);

  // Закраска левого или правого края содержимого
  with DrawRect do
  begin
    if IsTabLeft then
      Left := RightContent - ImgWidth
    else
      Left := LeftContent;

    Top := TopContent + ImgHeight;
    Right := Left + ImgWidth;
    Bottom := BottomContent - ImgHeight;
  end;
  Canvas.FillRect(DrawRect);
end;

{ *  Возвращает изображение с заданными параметрами
  * }
function TTabControl.GetSkinImage(Image: TGetImage; DownState: TDownState;
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

procedure TTabControl.DrawListButton;
var
  ButtonRect: TRect;
begin
  // Рисование кнопки вывода активного списка
  FSkin.Button.SetCanvas(Canvas, TMouseState(FListStyle));

  ButtonRect := DrawButton(FListStyle, True, FInfoList,
    FContentList.ActiveCaption, FSelectTab = TTabList.tlList, True);

  FActiveList.SetRect(ButtonRect);
end;

{ *  Рисование кнопки уведомления о новых запросах пользователей
  * }
procedure TTabControl.DrawRequestButton;
var
  ButtonRect: TRect;
begin
  if FContentRequest.RequestCount <= 0 then
    Exit;

  ButtonRect := DrawButton(FStyleRequest, False, FInfoRequest,
    FContentRequest.RequestCountText, FSelectTab = TTabList.tlRequest, False);

  FActiveRequest.SetRect(ButtonRect);
end;

{ *  Рисование кнопок с активным состоянием
  * }
function TTabControl.DrawButton(ImageState: TDownState; IsImageList: Boolean;
  ButtonInfo: TTabInfo; Text: DataString; IsOpen, IsTabLeft: Boolean): TRect;
var
  DrawLeft, DrawTop: Integer;
  ImgWidth, ImgHeight: Integer;
  DrawRect: TRect;
  Pointing: DataString;
  Position: TPoint;
  Size: TSize;
begin
  // Позиция и размер рисуемой кнопки
  Position := ButtonInfo.ButtonPosition;
  Size := ButtonInfo.ButtonSize;
  Result := Bounds(Position.X, Position.Y, Size.cx, Size.cy);

  FSkin.Button.SetCanvas(Canvas, TMouseState(ImageState));

  if IsImageList then
  begin
    Canvas.Brush.Color := FSkin.ButtonListColor[Integer(ImageState)];
  end
  else
  begin
    Canvas.Brush.Color := FSkin.ButtonRequestColor[Integer(ImageState)];
  end;

  with GetSkinImage(giTopLeft, ImageState, IsImageList) do
  begin
    ImgWidth := Width;
    ImgHeight := Height;
  end;

  // Рисование углов
  DrawLeft := Position.X;
  DrawTop := Position.Y;
  Canvas.Draw(DrawLeft, DrawTop,
    GetSkinImage(giTopLeft, ImageState, IsImageList)
  );

  DrawTop := DrawTop + Size.cy - ImgHeight;
  Canvas.Draw(DrawLeft, DrawTop,
    GetSkinImage(giBottomLeft, ImageState, IsImageList)
  );

  DrawLeft := DrawLeft + Size.cx - ImgWidth;
  Canvas.Draw(DrawLeft, DrawTop,
    GetSkinImage(giBottomRight, ImageState, IsImageList)
  );

  DrawTop := Position.Y;
  Canvas.Draw(DrawLeft, DrawTop,
    GetSkinImage(giTopRight, ImageState, IsImageList)
  );

  Canvas.Brush.Style := bsSolid;

  // Закраска центра кнопки с боковыми границами
  DrawRect.Left := Position.X + ImgWidth;
  DrawRect.Top := Position.Y;
  DrawRect.Right := DrawRect.Left + Size.cx - ImgWidth * 2;
  DrawRect.Bottom := DrawRect.Top + Size.cy;
  Canvas.FillRect(DrawRect);

  // Зарисовка левой границы
  DrawRect.Left := Position.X;
  DrawRect.Top := Position.Y + ImgHeight;
  DrawRect.Right := DrawRect.Left + ImgWidth;
  DrawRect.Bottom := DrawRect.Top + Size.cy - ImgHeight * 2;
  Canvas.FillRect(DrawRect);

  // Зарисовка апрвой границы
  DrawRect.Left := Position.X + Size.cx - ImgWidth;
  DrawRect.Right := DrawRect.Left + ImgWidth;
  Canvas.FillRect(DrawRect);

  if IsOpen then
  begin
    Pointing := UP_ARROW;
    DrawTabContent(ImageState, IsImageList, Result, IsTabLeft);
  end
  else
  begin
    Pointing := DOWN_ARROW;
  end;

  Canvas.Brush.Style := bsClear;

  // Вывод заголовка кнопки
  DrawLeft := Position.X + FSkin.CaptionMarginLeft;
  DrawTop := Position.Y + (Size.cy - ButtonInfo.FontSize.cy) div 2;
  Canvas.TextOut(DrawLeft, DrawTop, Text);

  // Вывод стрелки открытия вкладки
  DrawLeft := Position.X + Size.cx - ButtonInfo.ArrowSize.cx -
    FSkin.CaptionMarginRight;
  Canvas.TextOut(DrawLeft, DrawTop, Pointing);
end;

{ *  Событие на выбор пользователем нового списка в позиции Index
  * }
procedure TTabControl.ListSelectItem(Sender: TObject; Index: Integer);
begin
  SelectTab(TTabList.tlNone);
end;

end.
