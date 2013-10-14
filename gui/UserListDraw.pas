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
  UserListDrawStyle, ResourceImage, ImageUtils, SysUtils, StringUtils,
  FriendItem;

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
    FCountRepaint: Integer;
    FDefaultUserIcon: TUserIcon;
    FImages: TResourceImage;
    FIsActiveItem: Boolean;
    FItems: TUsers;
    FItemsCount: Integer;
    FPosition: Integer;
    FSize: Integer;
    FStopUpdate: Boolean;
    FOnChangeSize: TNotifyEvent;
    FOnSelectItem: TNotifyEvent;
    FSelectedItem: Integer;
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
    procedure UnactiveItems;
    procedure SetActiveItem(x, y: Integer; const IsRepaint: Boolean = True);
    function GetItemByMousePos(x, y: Integer): Integer;
    procedure SetSelectItem(x, y: Integer);
    procedure UnselectItems;
    procedure SetSelectItemProp(const Value: Integer);
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
    procedure Swap(Item1, Item2: Integer);

    property ActiveRigion: TActiveRegion read FActiveRegion write SetActiveRegion;
    property Items: TUsers read FItems;
    property ItemsCount: Integer read FItemsCount;
    property Position: Integer read FPosition write SetPosition;
    property SelectedItem: Integer read FSelectedItem write SetSelectItemProp;
    property Size: Integer read FSize;

    property OnChangeSize: TNotifyEvent read FOnChangeSize write FOnChangeSize;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
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
  FCountRepaint := 0;
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

{ *  Убирает активное выделение у всех элементов списка
  *
  * }
procedure TUserListDraw.UnactiveItems;
var
  i: Integer;
begin
  for i := 0 to FItemsCount - 1 do
    if FItems[i].State = dsActive then
    begin
      FItems[i].State := dsNone;
    end;

  FIsActiveItem := False;
end;

procedure TUserListDraw.UnselectItems;
var
  i: Integer;
begin
  for i := 0 to FItemsCount - 1 do
    if FItems[i].State = dsDown then
      FItems[i].State := dsNone;
end;

{ *  Возвращает индекс элемента списка, расположенного в указанных координатах.
  *  Если элемент отсутствует в указанных координатах, вернет -1
  *
  * }
function TUserListDraw.GetItemByMousePos(x, y: Integer): Integer;
begin
  if (x >= 0) and (x < ClientWidth) and (y >= 0) and (y < CLientHeight) then
  begin
    Result := (y + FPosition) div TULDStyle.ItemHeight;

    if (Result < 0) or (Result >= FItemsCount) then
      Result := -1;
  end
  else
    Result := -1;
end;

{ *  Устанавливает элементу списка, находящимуся в позиции Y выделенное
  *  состояние.
  *
  * }
procedure TUserListDraw.SetActiveItem(x, y: Integer; const IsRepaint: Boolean);
var
  SelectItem: Integer;
begin
  SelectItem := GetItemByMousePos(x, y);

  if SelectItem >= 0 then
  begin
    case FItems[SelectItem].State of
      dsNone:
        begin
          //if FIsActiveItem then
            UnactiveItems;

          FIsActiveItem := True;
          FItems[SelectItem].State := dsActive;

          if IsRepaint then
            Invalidate;
        end;

      dsDown:
        begin
          if FIsActiveItem then
          begin
            UnactiveItems;

            if IsRepaint then
              Invalidate;
          end;
        end;
    end;

  end
  else
  begin
    if FIsActiveItem then
    begin
      UnactiveItems;
      if IsRepaint then
        Invalidate;
    end;
  end;
end;

procedure TUserListDraw.SetSelectItem(x, y: Integer);
var
  Item: Integer;
begin
  Item := GetItemByMousePos(x, y);

  if (Item >= 0) and (FItems[Item].State <> dsDown) then
  begin
    UnselectItems;
    FItems[Item].State := dsDown;

    FSelectedItem := Item;
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self);

    Invalidate;
  end;
end;

procedure TUserListDraw.SetSelectItemProp(const Value: Integer);
begin
  if (Value >= 0) and (Value < FItemsCount) and (Value <> FSelectedItem) then
  begin
    FSelectedItem := Value;
    FItems[FSelectedItem].State := dsDown;
    Invalidate;
  end;
end;

procedure TUserListDraw.Swap(Item1, Item2: Integer);
var
  PItem: PUser;
begin
  PItem := FItems[Item1];
  FItems[Item1] := FItems[Item2];
  FItems[Item2] := PItem;
end;

procedure TUserListDraw.ActiveRegionMouseMessage(Sender: TObject;
  RegionMessage: TRegionMessage; const x, y: Integer; Button: TMouseButton;
  Shift: TShiftState);
begin
  case RegionMessage of
    rmMouseEnter: ;
    rmMouseLeave:
      begin
        if FIsActiveItem then
        begin
          UnactiveItems;
          Invalidate;
        end;
      end;

    rmMouseMove:
        SetActiveItem(x, y);

    rmMouseDown: ;
    rmMouseUp: ;
    rmMouseClick:
      SetSelectItem(x, y);

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
  Point: TPoint;
begin
  inherited;
  Point.y := 0;
  Point.x := 0;

  if GetCursorPos(Point) then
  begin
    Point := ScreenToClient(Point);
    if (Point.X >= 0) and (Point.X < ClientWidth) and (Point.Y >= 0) and
      (Point.Y < ClientHeight) then
      SetActiveItem(Point.X, Point.Y, False);
  end;

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

  NewSize := (FItemsCount * TULDStyle.ItemHeight);
  if NewSize < 0 then
    NewSize := 0;

  if NewSize <> FSize then
  begin
    FSize := NewSize;
    if Assigned(FOnChangeSize) then
      FOnChangeSize(Self);
  end;

  FCountRepaint := FCountRepaint + 1;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(0, 0, IntToStr(FCountRepaint));
end;

procedure TUserListDraw.SetActiveRegion(const Value: TActiveRegion);
begin
  FActiveRegion := Value;
  FActiveRegion.OnCursorMessage := ActiveRegionMouseMessage;
end;

procedure TUserListDraw.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    if (Value <= 0) and (FPosition = 0) then
      Exit;

    if (Value >= FSize) and (FPosition = FSize) then
      Exit;

    FPosition := Value;
    if FPosition < 0 then
      FPosition := 0;

    if FPosition > FSize then
      FPosition := FSize;

    Invalidate;
  end;
end;

end.
