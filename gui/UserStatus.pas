//  UserStatus.pas
//
//  Виджет текущего состояния пользователя. Генерирует события на изменения
//  состояния в зависимости от действия пользователя.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit UserStatus;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  Graphics, Classes, Controls, UserIcon, ResourceImage, ImageUtils, StringUtils,
  SysUtils, ActiveRegion, Menus, ImgList, PaintSprite, libtox, FriendItem,
  Clipbrd, SkinUserStatus;

type
  TState = (sOffline, sOnline, sAway, sBusy, sLoading);
  Address = procedure(Sender: TObject; State: TState) of object;

  TUserStatus = class(TCustomControl)
  private
    FUserIcon: TUserIcon;
    FImages: TResourceImage;
    FImageLoading: TPaintSprite;
    FRightButtonRegion: TActiveRegion;
    FRightButtonState: TDownState;
    FSkin: TSkinUserStatus;
    FState: TState;
    FStateMenu: TPopupMenu;
    FStatusRegion: TActiveRegion;
    FStatusText: DataString;
    FUserIconRegion: TActiveRegion;
    FUserName: DataString;
    FUsernameRegion: TActiveRegion;
    FOnChangeState: Address;
    FOnChangeUserName: TNotifyEvent;
    FOnChangeStatus: TNotifyEvent;
    FFriendItem: TFriendItem;
    procedure DrawRightButton(var Rect: TRect);
    procedure SetUserIcon(const Value: TUserIcon);
    procedure DrawUserIcon(var Rect: TRect);
    procedure SetState(const Value: TState);
    procedure SetUserString(const Value: DataString);
    procedure DrawUserName(var Rect: TRect);
    procedure RightButtonMessage(Sender: TObject; RegionMessage: TRegionMessage;
      const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
    procedure UpdateStateMenu;
    procedure StatusMenuOnClick(Sender: TObject);
    procedure UserNameMessage(Sender: TObject; RegionMessage: TRegionMessage;
      const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
    procedure SetStatusText(const Value: DataString);
    procedure DrawStatusIcon(var Rect: TRect);
    procedure DrawUserStatusMessage(var Rect: TRect);
    procedure StatusRegionMessage(Sender: TObject;
      RegionMessage: TRegionMessage; const x, y: Integer; Button: TMouseButton;
      Shift: TShiftState);
    procedure SetFriendItem(const Value: TFriendItem);
    procedure FriendItemUpdate(Sender: TObject);
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent; Skin: TSkinUserStatus); reintroduce;
    destructor Destroy; override;

    property FriendItem: TFriendItem read FFriendItem write SetFriendItem;
    property UserIcon: TUserIcon read FUserIcon write SetUserIcon;
    property State: TState read FState write SetState;

    property OnChangeState: Address read FOnChangeState write FOnChangeState;
    property OnChangeStatus: TNotifyEvent read FOnChangeStatus write FOnChangeStatus;
    property OnChangeUserName: TNotifyEvent read FOnChangeUserName write FOnChangeUserName;
  end;


implementation

{ TUserStatus }

constructor TUserStatus.Create(AOwner: TComponent;
  Skin: TSkinUserStatus);
begin
  inherited Create(AOwner);
  // TODO: временно
  UserIcon := TUserIcon.Create;
  FSkin := Skin;

  FState := sOffline;
  FRightButtonState := dsNone;

  // Получение ссылки на объект с изображениями
  FImages := TResourceImage.Clone;

  // Активный регион для правой кнопки изменения статуса
  FRightButtonRegion := TActiveRegion.Create(Self);
  FRightButtonRegion.OnCursorMessage := RightButtonMessage;
  FRightButtonRegion.Cursor := crHandPoint;

  // Активный регион для иконки пользователя
  FUserIconRegion := TActiveRegion.Create(Self);
  FUserIconRegion.Cursor := crHandPoint;

  FUsernameRegion := TActiveRegion.Create(Self);
  FUsernameRegion.Parent := Self;
  FUsernameRegion.Cursor := crHandPoint;
  FUsernameRegion.OnCursorMessage := UserNameMessage;

  FStatusRegion := TActiveRegion.Create(Self);
  FStatusRegion.Cursor := crHandPoint;
  FStatusRegion.OnCursorMessage := StatusRegionMessage;

  // Создание меню для выбора статуса
  FStateMenu := TPopupMenu.Create(Self);
  FStateMenu.Alignment := paRight;
  FStateMenu.Images := FImages.ImagesMenu;
  UpdateStateMenu;

  // Изображение загрузки
  FImageLoading := TPaintSprite.Create(FSkin.ImgLoading, Self);
end;

{*  Событие вызывается при создании окна
 *}
procedure TUserStatus.CreateWnd;
begin
  inherited;
  // Установка минимального размера компонента
  DoubleBuffered := True;
  ControlStyle := ControlStyle - [csParentBackground];

  FRightButtonRegion.Parent := Self;
  FUserIconRegion.Parent := Self;
  FStatusRegion.Parent := Self;

  Constraints.MinWidth := FSkin.MinWidth;
  Constraints.MaxWidth := FSkin.MaxWidth;
  Constraints.MinHeight := FSkin.Height;
  Constraints.MaxHeight := FSkin.Height;

  ClientWidth := FSkin.MinWidth;
  ClientHeight := FSkin.Height;
end;

destructor TUserStatus.Destroy;
begin
  FUserIcon.Free;
  inherited;
end;

{ *  Рисование кнопки, расположенной с правой стороны
  *  компонента. Кнопка изменяет свой вид в зависимости от
  *  действий мыши
  *
  *  Rect - доступная зона для рисования
  * }
procedure TUserStatus.DrawRightButton(var Rect: TRect);
var
  LeftPoint, TopPoint: Integer;
  PaintRect: TRect;
  ImageWidth: Integer;
  ImageHeight: Integer;
begin
  LeftPoint := Rect.Right - FSkin.ButtonWidth;
  TopPoint := Rect.Top;

  // Рисование фона кнопки
  Canvas.Brush.Style := bsSolid;
  case FRightButtonState of
    dsNone: Canvas.Brush.Color := FSkin.ButtonBack;
    dsActive: Canvas.Brush.Color := FSkin.ButtonBackActive;
    dsDown: Canvas.Brush.Color := FSkin.ButtonBackDown;
  end;
  PaintRect := Bounds(LeftPoint, TopPoint, FSkin.ButtonWidth, FSkin.Height);
  Canvas.FillRect(PaintRect);

  // Установка позиции региона для кнопки
  FRightButtonRegion.SetRect(PaintRect);

  // Рисование иконки на кнопке по центру
  ImageWidth := FImages.UserstatusButtonDown.Width;
  ImageHeight := FImages.UserstatusButtonDown.Height;

  LeftPoint := ((FSkin.ButtonWidth - ImageWidth) div 2) + LeftPoint;
  TopPoint := (FSkin.Height - ImageHeight) div 2;

  Canvas.Draw(LeftPoint, TopPoint, FImages.UserstatusButtonDown);

  Rect.Right := PaintRect.Left;
end;

procedure TUserStatus.DrawStatusIcon(var Rect: TRect);
var
  Image: TBitmap;
  LeftPoint, TopPoint: Integer;
begin
  // Остановка анимации в случае изменения статуса
  if (FState <> sLoading) and (FImageLoading.Active) then
    FImageLoading.Stop;

  case FState of
    sOffline: Image := FSkin.ImgStateOffline;
    sOnline: Image := FSkin.ImgStateOnline;
    sAway: Image := FSkin.ImgStateAway;
    sBusy: Image := FSkin.ImgStateBusy;
    sLoading:
      begin
        Image := nil;

        // 6 - разница в размере изображений
        LeftPoint := Rect.Right - FImageLoading.Width - 6;
        TopPoint := ((Rect.Bottom - Rect.Top) - FImageLoading.Height) div 2;
        FImageLoading.Draw(Canvas, LeftPoint, TopPoint);

        Rect.Right := LeftPoint;
      end;

  else
    Exit;
  end;

  // Рисование выбранной иконки
  if Assigned(Image) then
  begin
    LeftPoint := Rect.Right - Image.Width;
    TopPoint := (FSkin.Height - Image.Height) div 2;
    Canvas.Draw(LeftPoint, TopPoint, Image);

    Rect.Right := LeftPoint;
  end;
end;

{ *  Рисует собсвенное изображение пользователя.
  *
  *  Rect - доступная зона для рисования
  * }
procedure TUserStatus.DrawUserIcon(var Rect: TRect);
var
  LeftPoint: Integer;
  TopPoint: Integer;
  HeightRect: Integer;
begin
  HeightRect := Rect.Bottom - Rect.Top;
  LeftPoint := FSkin.IconLeft + Rect.Left;
  TopPoint := (HeightRect - FSkin.IconHeight) div 2 + Rect.Top;

  Rect.Left := LeftPoint + FSkin.IconWidth + FSkin.IconMarginRight;

  FUserIconRegion.SetRect(Bounds(
    FSkin.IconLeft, TopPoint,
    FSkin.IconWidth, FSkin.IconHeight
  ));

  Canvas.Draw(LeftPoint, TopPoint, FUserIcon.Image);
end;

{ *  Рисование имени пользователя
  * }
procedure TUserStatus.DrawUserName(var Rect: TRect);
var
  PaintRect: TRect;
  CharHeight: Integer;
begin
  CharHeight := FSkin.SetCanvasForName(Canvas);

  PaintRect.Left := Rect.Left;
  PaintRect.Top := (Rect.Bottom - Rect.Top) div 2 - CharHeight - FSkin.NameMarginBottom;
  PaintRect.Right := Rect.Right;
  PaintRect.Bottom := PaintRect.Top + CharHeight;
  TextRectEndEllipsis(Canvas, PaintRect, FUserName);

  FUsernameRegion.SetRect(PaintRect);

  Rect.Top := PaintRect.Bottom;
end;

{ *  Рисование текста состояния пользователя
  *
  * }
procedure TUserStatus.DrawUserStatusMessage(var Rect: TRect);
var
  CharHeight: Integer;
  PaintRect: TRect;
begin
  CharHeight := FSkin.SetCanvasForStatus(Canvas);

  PaintRect := Rect;
  PaintRect.Bottom := PaintRect.Top + CharHeight;
  TextRectEndEllipsis(Canvas, PaintRect, FStatusText);

  FStatusRegion.SetRect(PaintRect);
  Rect.Top := PaintRect.Bottom;
end;

{ *  Событие вызывается при перерисовке окна
  * }
procedure TUserStatus.Paint;
var
  DrawRect: TRect;
begin
  inherited;

  Canvas.Brush.Color := FSkin.BackColor;
  Canvas.Brush.Style := TBrushStyle.bsSolid;
  DrawRect := ClientRect;
  Canvas.FillRect(DrawRect);

  DrawUserIcon(DrawRect);
  DrawRightButton(DrawRect);
  DrawStatusIcon(DrawRect);
  DrawUserName(DrawRect);
  DrawUserStatusMessage(DrawRect);
end;

{*  Собтие на действия мышью на правой кнопке изменения статуса
 *}
procedure TUserStatus.RightButtonMessage(Sender: TObject; RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
var
  IsLeftClick: Boolean;
  p: TPoint;
begin
  IsLeftClick := Button = mbLeft;

  case RegionMessage of
    rmMouseEnter:
      begin
        FRightButtonState := dsActive;
        Invalidate;
      end;

    rmMouseLeave:
      begin
        FRightButtonState := dsNone;
        Invalidate;
      end;

    rmMouseMove: ;
    rmMouseDown:
      begin
        if IsLeftClick then
        begin
          FRightButtonState := dsDown;
          Invalidate;
        end;
      end;

    rmMouseUp:
      begin
        if IsLeftClick and (FRightButtonState = dsDown) then
        begin
          FRightButtonState := dsActive;
          Invalidate;
        end;
      end;

    rmMouseClick, rmMouseDblClick:
      begin
        p.X := FRightButtonRegion.Left + FSkin.ButtonWidth;
        p.Y := FRightButtonRegion.Top + FSkin.Height;
        p := ClientToScreen(p);
        FStateMenu.Popup(p.X, p.Y);

        FRightButtonState := dsNone;
        Invalidate;
      end;
  end;
end;

procedure TUserStatus.UserNameMessage(Sender: TObject; RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  if (RegionMessage = rmMouseClick) and Assigned(FOnChangeUserName) then
    FOnChangeUserName(Self);
end;

{ *  Событие нажатия на строку статуса пользователя
  *
  * }
procedure TUserStatus.StatusRegionMessage(Sender: TObject; RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  if (RegionMessage = rmMouseClick) and Assigned(FOnChangeStatus) then
    FOnChangeStatus(Self);
end;

procedure TUserStatus.SetFriendItem(const Value: TFriendItem);
begin
  FFriendItem := Value;
  FFriendItem.OnUpdate := FriendItemUpdate;
  FriendItemUpdate(FFriendItem);
end;

procedure TUserStatus.SetState(const Value: TState);
begin
  FState := Value;
  Invalidate;
end;

procedure TUserStatus.SetStatusText(const Value: DataString);
begin
  FStatusText := Value;
  Invalidate;
end;

procedure TUserStatus.SetUserIcon(const Value: TUserIcon);
begin
  FUserIcon := Value;
end;

procedure TUserStatus.SetUserString(const Value: DataString);
begin
  FUserName := Value;
  Invalidate;
end;

{*  Событие выбора нового статуса пользователя
 *}
procedure TUserStatus.StatusMenuOnClick(Sender: TObject);
var
  State: TState;
begin
  case TMenuItem(Sender).Tag of
    0: State := sOnline;
    1: State := sAway;
    2: State := sBusy;
    3: State := sOffline;
    4:
      begin
        //TODO: Временное размещение
        Clipboard.SetTextBuf(PChar(FFriendItem.Addressg.DataHex));
        Exit;
      end
  else
    State := sOffline;
  end;

  if Assigned(FOnChangeState) then
    FOnChangeState(Self, State);
end;

{*  Обновление списка контекстного меню для переключения
 *  состояния пользователя
 *}
procedure TUserStatus.UpdateStateMenu;
var
  Item: TMenuItem;
begin
  FStateMenu.Items.Clear;

  Item := TMenuItem.Create(FStateMenu);
  FStateMenu.Items.Add(Item);
  Item.Caption := 'Copy address';
  //Item.ImageIndex := 0;
  Item.Tag := 4;
  Item.OnClick := StatusMenuOnClick;

  Item := TMenuItem.Create(FStateMenu);
  FStateMenu.Items.Add(Item);
  Item.Caption := '-';

  Item := TMenuItem.Create(FStateMenu);
  FStateMenu.Items.Add(Item);
  Item.Caption := 'Online';
  Item.ImageIndex := 0;
  Item.Tag := 0;
  Item.OnClick := StatusMenuOnClick;

  Item := TMenuItem.Create(FStateMenu);
  FStateMenu.Items.Add(Item);
  Item.Caption := 'Away';
  Item.ImageIndex := 1;
  Item.Tag := 1;
  Item.OnClick := StatusMenuOnClick;

  Item := TMenuItem.Create(FStateMenu);
  FStateMenu.Items.Add(Item);
  Item.Caption := 'Busy';
  Item.ImageIndex := 2;
  Item.Tag := 2;
  Item.OnClick := StatusMenuOnClick;

  Item := TMenuItem.Create(FStateMenu);
  FStateMenu.Items.Add(Item);
  Item.Caption := '-';

  Item := TMenuItem.Create(FStateMenu);
  FStateMenu.Items.Add(Item);
  Item.Caption := 'Disconnect';
  Item.ImageIndex := 3;
  Item.Tag := 3;
  Item.OnClick := StatusMenuOnClick;
end;

procedure TUserStatus.FriendItemUpdate(Sender: TObject);
begin
  SetUserString(FFriendItem.UserName);
  SetStatusText(FFriendItem.StatusMessage);
end;

end.
