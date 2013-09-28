// UserListDraw.pas
//
// Виджет отрисовки списка пользователей.
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit UserListDraw;

interface

{$I tox.inc}

uses
{$I tox-uses.inc}
  Graphics, Classes, Controls, libtox, UserIcon, ActiveRegion, FriendList,
  UserListDrawStyle, ResourceImage, ImageUtils, SysUtils, StringUtils;

type
  { *  Структура, содержащая необходимую информацию для рисования элементов
    *  списка
    *
    * }
  PUser = ^TUser;
  TUser = record
    Item: TFriendItem;
    State: TDownState;
  end;
  TUsers = array of PUser;

  TUserListDraw = class(TGraphicControl)
  private
    FActiveRegion: TActiveRegion;
    FDefaultUserIcon: TUserIcon;
    FImages: TResourceImage;
    FItems: TUsers;
    FItemsCount: Integer;
    FPosition: Integer;
    FSize: Integer;
    FStopUpdate: Boolean;
    FOnChangeSize: TNotifyEvent;
    procedure SetPosition(const Value: Integer);
    procedure DrawItem(Y: Integer; UserName, StatusText: DataString;
      Status: TToxUserStatus; IsNewMessage: Boolean; UserIcon: TUserIcon;
      MouseState: TDownState);
    procedure DrawUserIcon(var DrawRect: TRect; Icon: TUserIcon);
    procedure DrawUserName(var DrawRect: TRect; Name: DataString);
    procedure DrawStatusIcon(var DrawRect: TRect; Status: TToxUserStatus;
      MouseState: TDownState; IsNewMessage: Boolean);
    procedure DrawStatusText(DrawRect: TRect; Status: DataString);
    procedure ActiveRegionMouseMessage(Sender: TObject;
      RegionMessage: TRegionMessage; const x, y: Integer; Button: TMouseButton;
      Shift: TShiftState);
    procedure SetActiveRegion(const Value: TActiveRegion);
    procedure UnselectItems;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddItem(FriendItem: TFriendItem);
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    procedure UpdateItem(Index: Integer);

    property ActiveRigion: TActiveRegion read FActiveRegion write SetActiveRegion;
    property Position: Integer read FPosition write SetPosition;
    property Size: Integer read FSize;

    property OnChangeSize: TNotifyEvent read FOnChangeSize write FOnChangeSize;
  end;

implementation

{ TUserListDraw }


constructor TUserListDraw.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultUserIcon := TUserIcon.Create;
  FImages := TResourceImage.Clone;



  FSize := 500;
  FStopUpdate := False;

  SetLength(FItems, 20);
  FItemsCount := 0;
end;

destructor TUserListDraw.Destroy;
begin
  FDefaultUserIcon.Free;
  inherited;
end;

{ *  Добавляет к списку новый элемент
  *
  * }
procedure TUserListDraw.AddItem(FriendItem: TFriendItem);
begin
  if Length(FItems) <= FItemsCount then
  begin
    SetLength(FItems, FItemsCount + 20);
  end;

  New(FItems[FItemsCount]);
  FItems[FItemsCount].Item := FriendItem;
  FItems[FItemsCount].State := dsNone;
//  FItems[FItemsCount] := @Item;
  Inc(FItemsCount);

  if not FStopUpdate then
    Invalidate;
end;

{ *  Предотвращает перерисовку компонента при изменении списка до вызова
  *  функции EndUpdate
  *
  * }
procedure TUserListDraw.BeginUpdate;
begin
  FStopUpdate := True;
end;

{ *  Разрешает перерисовку компонента при изменении списка и вызывает
  *  немедленную перерисовку компонента
  *
  * }
procedure TUserListDraw.EndUpdate;
begin
  if FStopUpdate then
  begin
    FStopUpdate := False;
    Invalidate;
  end;
end;

{ *  Очистка списка элементов
  *
  * }
procedure TUserListDraw.Clear;
begin
  FItemsCount := 0;
  if not FStopUpdate then
    Invalidate;
end;

{ *  Обновляет указанный элемент списка, если тот находится в поле видимости
  *
  * }
procedure TUserListDraw.UpdateItem(Index: Integer);
begin
  Invalidate;
end;

procedure TUserListDraw.UnselectItems;
var
  i: Integer;
begin
  for i := 0 to FItemsCount - 1 do
    if FItems[i].State = dsActive then
      FItems[i].State := dsNone;
end;

procedure TUserListDraw.ActiveRegionMouseMessage(Sender: TObject;
  RegionMessage: TRegionMessage; const x, y: Integer; Button: TMouseButton;
  Shift: TShiftState);
var
  MousePosItem: Integer;
begin
  MousePosItem := (y + FPosition) div TULDStyle.ItemHeight;

  case RegionMessage of
    rmMouseEnter: ;
    rmMouseLeave:
      begin
        UnselectItems;
        Invalidate;
      end;

    rmMouseMove:
      begin
        if (MousePosItem < FItemsCount) and (MousePosItem >= 0) then
        begin
          if FItems[MousePosItem].State = dsNone then
          begin
            UnselectItems;
            FItems[MousePosItem].State := dsActive;
            Invalidate;
          end;
        end;
      end;
    rmMouseDown: ;
    rmMouseUp: ;
    rmMouseClick: ;
    rmMouseDblClick: ;
  end;
end;

{ *  Рисование одного элемента списка в указанной позиции
  *
  * }
procedure TUserListDraw.DrawItem(Y: Integer; UserName, StatusText: DataString;
  Status: TToxUserStatus; IsNewMessage: Boolean; UserIcon: TUserIcon;
  MouseState: TDownState);
var
  DrawRect: TRect;
begin
  DrawRect := Bounds(0, Y, ClientWidth, TULDStyle.ItemHeight);

  case MouseState of
    dsActive:
      Canvas.Brush.Color := TULDStyle.BackgroundActive;
    dsDown:
      Canvas.Brush.Color := TULDStyle.BackgroundSelect;
  end;

  if MouseState <> dsNone then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(DrawRect);
  end;

  DrawUserIcon(DrawRect, UserIcon);
  DrawStatusIcon(DrawRect, Status, MouseState, IsNewMessage);
  DrawUserName(DrawRect, UserName);
  DrawStatusText(DrawRect, StatusText);
end;

procedure TUserListDraw.DrawStatusIcon(var DrawRect: TRect;
  Status: TToxUserStatus; MouseState: TDownState; IsNewMessage: Boolean);
var
  LeftDraw, TopDraw: Integer;
  HeightItem: Integer;
  Icon: TBitmap;
begin
  Icon := FImages.GetUserListStatusIcon(MouseState, Status, IsNewMessage);

  if Assigned(Icon) then
  begin
    HeightItem := DrawRect.Bottom - DrawRect.Top;
    TopDraw := (HeightItem - Icon.Height) div 2 + DrawRect.Top;
    LeftDraw := DrawRect.Right - Icon.Width - TULDStyle.StatusIconMarginRight;
    DrawRect.Right := LeftDraw - TULDStyle.StatusIconMarginLeft;

    Canvas.Draw(LeftDraw, TopDraw, Icon);
  end;
end;

procedure TUserListDraw.DrawStatusText(DrawRect: TRect; Status: DataString);
var
  NewRect: TRect;
  NewStatus: DataString;
begin
  NewRect := DrawRect;

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := TULDStyle.StatusColor;
  {$IFDEF FPC}
  Canvas.Font.Size := 8;
  {$ELSE}
  Canvas.Font.Height := TULDStyle.NameHeight;
  {$ENDIF}
  Canvas.Font.Name := 'Fira Sans';
  Canvas.Font.Style := [];

  NewStatus := Status;
  TextRectW(Canvas, NewRect, NewStatus, [tfEndEllipsis]);
end;

procedure TUserListDraw.DrawUserIcon(var DrawRect: TRect; Icon: TUserIcon);
var
  LeftDraw, TopDraw: Integer;
  HeightItem: Integer;
begin
  HeightItem := DrawRect.Bottom - DrawRect.Top;
  TopDraw := (HeightItem - Icon.Height) div 2 + DrawRect.Top;
  LeftDraw := DrawRect.Left + TULDStyle.IconLeft;
  DrawRect.Left := DrawRect.Left + LeftDraw + Icon.Width +
    TULDStyle.IconMarginRight;

  Canvas.Draw(LeftDraw, TopDraw, Icon.Image);
end;

procedure TUserListDraw.DrawUserName(var DrawRect: TRect; Name: DataString);
var
  BlockHeight, BlockWidth, BlockTop: Integer;
  NewRect: TRect;
  NewName: DataString;
begin
  BlockTop := DrawRect.Top;
  BlockWidth := DrawRect.Right - DrawRect.Left;
  BlockHeight := (DrawRect.Bottom - DrawRect.Top) div 2;
  DrawRect.Top := DrawRect.Top + BlockHeight;

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := TULDStyle.NameColor;
  {$IFDEF FPC}
  Canvas.Font.Size := 9;
  {$ELSE}
  Canvas.Font.Height := TULDStyle.NameHeight;
  {$ENDIF}
  Canvas.Font.Name := 'Fira Sans';
  Canvas.Font.Style := [fsBold];

  NewRect := Bounds(DrawRect.Left, BlockTop, BlockWidth, BlockHeight);
  NewRect.Top := NewRect.Top + (BlockHeight - Canvas.TextHeight('Z'));

  NewName := Name;
  TextRectW(Canvas, NewRect, NewName, [tfEndEllipsis]);
end;

procedure TUserListDraw.Paint;
var
  TopPosition, i: Integer;
  NewSize: Integer;
  TextStatus: DataString;
  UserName: DataString;
  Status: TToxUserStatus;

begin
  inherited;

  // Зарисовка фона списка цветом по умолчанию
  Canvas.Brush.Color := TULDStyle.BackgroundNormal;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);

  for i := 0 to FItemsCount - 1 do
  begin
    TopPosition := -1 * FPosition + (TULDStyle.ItemHeight * i);

    if (TopPosition + TULDStyle.ItemHeight <= 0) then
      Continue;

    if (TopPosition > ClientHeight) then
      Break;

    TextStatus := FItems[i].Item.StatusMessage;
    UserName := FItems[i].Item.UserName;
    Status := FItems[i].Item.UserStatus;

    DrawItem(TopPosition, UserName, TextStatus, Status, False, FDefaultUserIcon,
      FItems[i].State);
  end;

  NewSize := (FItemsCount * TULDStyle.ItemHeight);// - ClientHeight;
  if NewSize < 0 then
    NewSize := 0;

  if NewSize <> FSize then
  begin
    FSize := NewSize;
    if Assigned(FOnChangeSize) then
      FOnChangeSize(Self);
  end;
end;

procedure TUserListDraw.SetActiveRegion(const Value: TActiveRegion);
begin
  FActiveRegion := Value;
  FActiveRegion.OnCursorMessage := ActiveRegionMouseMessage;
end;

procedure TUserListDraw.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  if FPosition < 0 then
    FPosition := 0;

  if FPosition > FSize then
    FPosition := FSize;

  Invalidate;
end;

end.
