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
  Graphics, Classes, Controls, UserIcon, ResourceImage, ImageUtils,
  StringUtils, SysUtils, UserStatusStyle, ActiveRegion, Menus, ImgList,
  PaintSprite, libtox;

type
  TState = (sOffline, sOnline, sAway, sBusy, sLoading);
  TProcChangeState = procedure(Sender: TObject; State: TState) of object;

  TUserStatus = class(TCustomControl)
  private
    FUserIcon: TUserIcon;
    FImages: TResourceImage;
    FImageLoading: TPaintSprite;
    FRightButtonRegion: TActiveRegion;
    FRightButtonState: TDownState;
    FState: TState;
    FStateMenu: TPopupMenu;
    FStatusRegion: TActiveRegion;
    FStatusText: DataString;
    FUserIconRegion: TActiveRegion;
    FUserName: DataString;
    FUsernameRegion: TActiveRegion;
    FOnChangeState: TProcChangeState;
    FOnChangeUserName: TNotifyEvent;
    FOnChangeStatus: TNotifyEvent;
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
  protected
    procedure CreateWnd; override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;

    property UserIcon: TUserIcon read FUserIcon write SetUserIcon;
    property UserName: DataString read FUserName write SetUserString;
    property StatusText: DataString read FStatusText write SetStatusText;
    property State: TState read FState write SetState;

    property OnChangeState: TProcChangeState read FOnChangeState write FOnChangeState;
    property OnChangeStatus: TNotifyEvent read FOnChangeStatus write FOnChangeStatus;
    property OnChangeUserName: TNotifyEvent read FOnChangeUserName write FOnChangeUserName;
  end;


implementation

{ TUserStatus }

constructor TUserStatus.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: временно
  UserIcon := TUserIcon.Create;

  FState := sOffline;
  FRightButtonState := dsNone;

  // Получение ссылки на объект с изображениями
  FImages := TResourceImage.Clone;

  DoubleBuffered := True;

  // Активный регион для правой кнопки изменения статуса
  FRightButtonRegion := TActiveRegion.Create(Self);
  FRightButtonRegion.Parent := Self;
  FRightButtonRegion.Width := TUserStatusStyle.RightButtonWidth;
  FRightButtonRegion.Height := TUserStatusStyle.Height;
  FRightButtonRegion.OnCursorMessage := RightButtonMessage;
  FRightButtonRegion.Cursor := crHandPoint;

  // Активный регион для иконки пользователя
  FUserIconRegion := TActiveRegion.Create(Self);
  FUserIconRegion.Parent := Self;
  FUserIconRegion.Width := TUserStatusStyle.IconWidth;
  FUserIconRegion.Height := TUserStatusStyle.IconHeight;
  FUserIconRegion.Left := TUserStatusStyle.IconPositionLeft;
  FUserIconRegion.Cursor := crHandPoint;

  FUsernameRegion := TActiveRegion.Create(Self);
  FUsernameRegion.Parent := Self;
  FUsernameRegion.Left := TUserStatusStyle.IconPositionLeft +
    TUserStatusStyle.IconWidth + TUserStatusStyle.UserNameMarginLeft;
  FUsernameRegion.Top := TUserStatusStyle.UserNameMarginTop;
  FUsernameRegion.Height := TUserStatusStyle.UserNameHeight;
  FUsernameRegion.Width := Width - 35 - FUsernameRegion.Left;
  FUsernameRegion.Cursor := crHandPoint;
  FUsernameRegion.OnCursorMessage := UserNameMessage;

  FStatusRegion := TActiveRegion.Create(Self);
  FStatusRegion.Parent := Self;
  FStatusRegion.Cursor := crHandPoint;
  FStatusRegion.OnCursorMessage := StatusRegionMessage;

  // Создание меню для выбора статуса
  FStateMenu := TPopupMenu.Create(Self);
  FStateMenu.Alignment := paRight;
  FStateMenu.Images := FImages.ImagesMenu;
  UpdateStateMenu;

  // Изображение загрузки
  FImageLoading := TPaintSprite.Create(FImages.LoadingAnimate10, Self);
end;

{*  Событие вызывается при создании окна
 *}
procedure TUserStatus.CreateWnd;
begin
  inherited;
  // Установка минимального размера компонента
  Constraints.MinWidth := TUserStatusStyle.MinWidth;
  Constraints.MinHeight := TUserStatusStyle.Height;
  Constraints.MaxHeight := TUserStatusStyle.Height;

  ClientWidth := TUserStatusStyle.MinWidth;
  ClientHeight := TUserStatusStyle.Height;

  // Установка цвета фона
  ParentColor := False;
  Color := TUserStatusStyle.BackgroundColor;
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
  Canvas.Brush.Style := bsSolid;
  case FRightButtonState of
    dsNone:
      Canvas.Brush.Color := TUserStatusStyle.RightButtonBackgroundNormal;
    dsActive:
      Canvas.Brush.Color := TUserStatusStyle.RightButtonBackgroundActive;
    dsDown:
      Canvas.Brush.Color := TUserStatusStyle.RightButtonBackgroundDown;
  end;

  LeftPoint := Rect.Right - TUserStatusStyle.RightButtonWidth;
  TopPoint := Rect.Top;

  // Рисование фона кнопки
  PaintRect.Left := LeftPoint;
  PaintRect.Top := TopPoint;
  PaintRect.Right := Rect.Right;
  PaintRect.Bottom := Rect.Bottom;
  Canvas.FillRect(PaintRect);

  // Установка позиции региона для кнопки
  FRightButtonRegion.Left := LeftPoint;
  FRightButtonRegion.Top := TopPoint;

  // Рисование иконки  на кнопке по центру
  ImageWidth := FImages.UserstatusButtonDown.Width;
  ImageHeight := FImages.UserstatusButtonDown.Height;
  LeftPoint := ((PaintRect.Right - PaintRect.Left) - ImageWidth) div 2 + LeftPoint;
  TopPoint := ((Rect.Bottom - Rect.Top) - ImageHeight) div 2;
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
    sOffline:
      Image := FImages.GetSelfStatusIcon(usInvalid);

    sOnline:
      Image := FImages.GetSelfStatusIcon(usNone);

    sAway:
      Image := FImages.GetSelfStatusIcon(usAway);

    sBusy:
      Image := FImages.GetSelfSTatusIcon(usBusy);

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
    Image := nil;
  end;

  // Рисование выбранной иконки
  if Assigned(Image) then
  begin
    LeftPoint := Rect.Right - Image.Width;
    TopPoint := ((Rect.Bottom - Rect.Top) - Image.Height) div 2;
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
  LeftPoint := TUserStatusStyle.IconPositionLeft + Rect.Left;
  TopPoint := (HeightRect - FUserIcon.Image.Height) div 2 + Rect.Top;

  Rect.Left := LeftPoint + FUserIcon.Image.Width + TUserStatusStyle.IconMarginRight;

  FUserIconRegion.Top := TopPoint;
  Canvas.Draw(LeftPoint, TopPoint, FUserIcon.Image);
end;

{ *  Рисование имени пользователя
  * }
procedure TUserStatus.DrawUserName(var Rect: TRect);
var
  PaintRect: TRect;
  CharHeight: Integer;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := TUserStatusStyle.UserNameColor;
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Name := 'Fira Sans';
  {$IFDEF FPC}
  Canvas.Font.Size := 10;
  {$ELSE}
  Canvas.Font.Height := TUserStatusStyle.UserNameHeight;
  {$ENDIF}

  CharHeight := Canvas.TextExtent('Q').cy;

  PaintRect.Left := Rect.Left + TUserStatusStyle.UserNameMarginLeft;
  PaintRect.Top := (Rect.Bottom - Rect.Top) div 2 - CharHeight;
  PaintRect.Right := Rect.Right;
  PaintRect.Bottom := PaintRect.Top + CharHeight;
  TextRectW(Canvas, PaintRect, FUserName, [tfEndEllipsis]);

  FUsernameRegion.Top := PaintRect.Top;
  FUsernameRegion.Width := PaintRect.Right - PaintRect.Left;

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
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := TUserStatusStyle.StatusMessageColor;
  Canvas.Font.Style := [];
  Canvas.Font.Name := 'Fira Sans';
  {$IFDEF FPC}
  Canvas.Font.Size := 9;
  {$ELSE}
  Canvas.Font.Height := TUserStatusStyle.StatusMessageHeight;
  {$ENDIF}

  CharHeight := Canvas.TextExtent('Q').cy;
  PaintRect := Rect;
  PaintRect.Left := PaintRect.Left + TUserStatusStyle.UserNameMarginLeft;
  PaintRect.Bottom := PaintRect.Top + CharHeight;
  TextRectW(Canvas, PaintRect, FStatusText, [tfEndEllipsis]);

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

  DrawRect := ClientRect;

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
        p.X := FRightButtonRegion.Left + TUserStatusStyle.RightButtonWidth;
        p.Y := FRightButtonRegion.Top + TUserStatusStyle.Height;
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

end.
