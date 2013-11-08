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
  ImageUtils, SysUtils, StringUtils, FriendItem, SkinUserList, SkinTypes;

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

  TProcSelectItem = procedure(Sender: TObject; Item: TFriendItem) of object;

  TUserListDraw = class(TGraphicControl)
  private
    FActiveRegion: TActiveRegion;
    {$IFDEF DEBUG}
    FCountRepaint: Integer;
    {$ENDIF}
    FDefaultUserIcon: TUserIcon;
    FIsActiveItem: Boolean;
    FItems: TUsers;
    FItemsCount: Integer;
    FPosition: Integer;
    FSize: Integer;
    FSkin: TSkinUserList;
    FStopUpdate: Boolean;
    FOnChangeSize: TNotifyEvent;

    FSelectedItem: Integer;
    FOnSelectItem: TProcSelectItem;
    procedure SetPosition(const Value: Integer);
    procedure DrawItem(Y: Integer; UserName, StatusText: DataString;
      Status: TToxUserStatus; IsNewMessage: Boolean; UserIcon: TUserIcon;
      MouseState: TDownState);
    procedure DrawUserIcon(var DrawRect: TRect; Icon: TUserIcon);
    procedure DrawUserName(var DrawRect: TRect; Name: DataString;
      MouseState: TDownState);
    procedure DrawStatusIcon(var DrawRect: TRect; Status: TToxUserStatus;
      MouseState: TDownState; IsNewMessage: Boolean);
    procedure DrawStatusText(DrawRect: TRect; Status: DataString;
      MouseState: TDownState);
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
    constructor Create(AOwner: TComponent; Skin: TSkinUserList); reintroduce;
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
    property OnSelectItem: TProcSelectItem read FOnSelectItem write FOnSelectItem;
  end;

implementation

{ TUserListDraw }


constructor TUserListDraw.Create(AOwner: TComponent; Skin: TSkinUserList);
begin
  inherited Create(AOwner);

  FSkin := Skin;
  FDefaultUserIcon := TUserIcon.Create;

  FSize := 500;
  FStopUpdate := False;

  SetLength(FItems, 20);
  FItemsCount := 0;
  {$IFDEF DEBUG}
  FCountRepaint := 0;
  {$ENDIF}
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
    Result := (y + FPosition) div FSkin.ItemHeight;

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

{ *  Выделение элемента, расположенного в указанных координатах x и y.
  * }
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
      FOnSelectItem(Self, FItems[Item].Item);

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
  DrawRect := Bounds(0, Y, ClientWidth, FSkin.ItemHeight);

  if MouseState <> dsNone then
  begin
    case MouseState of
      dsActive:
        Canvas.Brush.Color := FSkin.ItemColorActive;
      dsDown:
        Canvas.Brush.Color := FSkin.ItemColorDown;
    end;

    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(DrawRect);
  end;

  DrawUserIcon(DrawRect, UserIcon);
  DrawStatusIcon(DrawRect, Status, MouseState, IsNewMessage);
  DrawUserName(DrawRect, UserName, MouseState);
  DrawStatusText(DrawRect, StatusText, MouseState);
end;

procedure TUserListDraw.DrawStatusIcon(var DrawRect: TRect;
  Status: TToxUserStatus; MouseState: TDownState; IsNewMessage: Boolean);
var
  LeftDraw, TopDraw: Integer;
  HeightItem: Integer;
  Icon: TBitmap;
  MouseStateInt: Integer;
begin
  MouseStateInt := Integer(MouseState);

  case Status of
    usNone:
      begin
        if IsNewMessage then
          Icon := FSkin.ImgStateOnlineNew[MouseStateInt]
        else
          Icon := FSkin.ImgStateOnline[MouseStateInt];
      end;

    usAway:
      begin
        if IsNewMessage then
          Icon := FSkin.ImgStateAwayNew[MouseStateInt]
        else
          Icon := FSkin.ImgStateAway[MouseStateInt];
      end;

    usBusy:
      begin
        if IsNewMessage then
          Icon := FSkin.ImgStateBusyNew[MouseStateInt]
        else
          Icon := FSkin.ImgStateBusy[MouseStateInt];
      end;

//    usInvalid:
  else
    begin
      if IsNewMessage then
        Icon := FSkin.ImgStateOfflineNew[MouseStateInt]
      else
        Icon := FSkin.ImgStateOffline[MouseStateInt];
    end;
  end;

  if Assigned(Icon) and (not Icon.Empty) then
  begin
    HeightItem := DrawRect.Bottom - DrawRect.Top;
    TopDraw := (HeightItem - Icon.Height) div 2 + DrawRect.Top;
    LeftDraw := DrawRect.Right - Icon.Width - FSkin.StatusIconMarginRight;
    DrawRect.Right := LeftDraw - FSkin.StatusIconMarginLeft;

    Canvas.Draw(LeftDraw, TopDraw, Icon);
  end;
end;

{ *  Рисоование текущего статуса пользователя
  * }
procedure TUserListDraw.DrawStatusText(DrawRect: TRect; Status: DataString;
  MouseState: TDownState);
begin
  FSkin.StatusFont.SetCanvas(Canvas, TMouseState(MouseState));
  TextRectEndEllipsis(Canvas, DrawRect, Status);
end;

{ *  Рисование иконки пользователя
  * }
procedure TUserListDraw.DrawUserIcon(var DrawRect: TRect; Icon: TUserIcon);
var
  LeftDraw, TopDraw: Integer;
  HeightItem: Integer;
begin
  HeightItem := DrawRect.Bottom - DrawRect.Top;

  TopDraw := (HeightItem - Icon.Height) div 2 + DrawRect.Top;
  LeftDraw := DrawRect.Left + FSkin.IconLeft;

  DrawRect.Left := DrawRect.Left + LeftDraw + Icon.Width +
    FSkin.IconMarginRight;

  Canvas.Draw(LeftDraw, TopDraw, Icon.Image);
end;

procedure TUserListDraw.DrawUserName(var DrawRect: TRect; Name: DataString;
  MouseState: TDownState);
var
  BlockHeight, BlockWidth, BlockTop: Integer;
  NewRect: TRect;
  NewName: DataString;
begin
  BlockTop := DrawRect.Top;
  BlockWidth := DrawRect.Right - DrawRect.Left;
  BlockHeight := (DrawRect.Bottom - DrawRect.Top) div 2;
  DrawRect.Top := DrawRect.Top + BlockHeight;

  FSkin.NameFont.SetCanvas(Canvas, TMouseState(MouseState));

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

  //TODO: Исправить баг с ложным фокусом
  if GetCursorPos(Point) then
  begin
    Point := ScreenToClient(Point);
    if (Point.X >= 0) and (Point.X < ClientWidth) and (Point.Y >= 0) and
      (Point.Y < ClientHeight) then
      SetActiveItem(Point.X, Point.Y, False);
  end;

  // Зарисовка фона списка цветом по умолчанию
  Canvas.Brush.Color := FSkin.BackgroundColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);

  for i := 0 to FItemsCount - 1 do
  begin
    TopPosition := -1 * FPosition + (FSkin.ItemHeight * i);

    if (TopPosition + FSkin.ItemHeight <= 0) then
      Continue;

    if (TopPosition > ClientHeight) then
      Break;

    TextStatus := FItems[i].Item.StatusMessage;
    UserName := FItems[i].Item.UserName;
    Status := FItems[i].Item.UserStatus;

    DrawItem(TopPosition, UserName, TextStatus, Status, False, FDefaultUserIcon,
      FItems[i].State);
  end;

  NewSize := (FItemsCount * FSkin.ItemHeight);
  if NewSize < 0 then
    NewSize := 0;

  if NewSize <> FSize then
  begin
    FSize := NewSize;
    if Assigned(FOnChangeSize) then
      FOnChangeSize(Self);
  end;

  {$IFDEF DEBUG}
  FCountRepaint := FCountRepaint + 1;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(0, 0, IntToStr(FCountRepaint));
  {$ENDIF}
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
