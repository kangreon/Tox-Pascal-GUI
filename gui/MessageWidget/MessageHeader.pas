//  MessageHeader.pas
//
//  Заголовок виджета сообщений, в котором выводится иконка, имя и статус
//  пользователя. Виджет содержит кнопки аудио и видео звонка.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit MessageHeader;

interface
  {$I tox.inc}

uses
  Graphics, Classes, Controls, ButtonActive, ResourceImage, MessageHeaderStyle,
  FriendItem, StringUtils, UserIcon, ImageUtils, Types;

type
  TMessageHeader = class(TCustomControl)
  private
    FImageCall: TButtonActive;
    FImageCallVideo: TButtonActive;
    FSelectFriend: TFriendItem;
    FUserIcon: TUserIcon;
    procedure FriendUpdate(Sender: TObject);
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SelectFriend(FriendItem: TFriendItem);
  end;

implementation

{ TMessageHeader }

constructor TMessageHeader.Create(AOwner: TComponent);
var
  ResImage: TResourceImage;
begin
  inherited;
  ResImage := TResourceImage.Clone;
  FSelectFriend := nil;
  Visible := False;

  FUserIcon := TUserIcon.Create;

  FImageCall := TButtonActive.Create(Self);
  FImageCall.InsertImage(ResImage.MessageHeaderButtons, 0);
  FImageCall.Cursor := crHandPoint;
  FImageCallVideo := TButtonActive.Create(Self);
  FImageCallVideo.InsertImage(ResImage.MessageHeaderButtons, 3);
  FImageCallVideo.Cursor := crHandPoint;
end;

destructor TMessageHeader.Destroy;
begin
  FImageCall.Free;
  FImageCallVideo.Free;

  FUserIcon.Free;
  inherited;
end;

procedure TMessageHeader.Paint;
var
  Name, Status: DataString;
  l, t: Integer;
  MaxWidth: Integer;
  Size: TSize;
  Rect: TRect;
begin
  inherited;
  if not Assigned(FSelectFriend) then
    Exit;

  Name := FSelectFriend.UserName;
  Status := FSelectFriend.StatusMessage;

  t := (ClientHeight - FUserIcon.Height) div 2;
  l := TMHStyle.IconMarginLeft;
  Canvas.Draw(l, t, FUserIcon.Image);

  l := TMHStyle.UserNameMarginLeft + l + FUserIcon.Width;

  MaxWidth := FImageCall.Left - l - TMHStyle.ButtonAudioMarginLeft;

  // Вывод имени
  Canvas.Font.Name := TMHStyle.UserNameFontName;
  Canvas.Font.Size := TMHStyle.UserNameSize;
  Canvas.Font.Style := TMHStyle.UserNameStyle;
  Canvas.Font.Color := TMHStyle.UserNameColor;
  Canvas.Brush.Style := bsClear;

  Size := Canvas.TextExtent(Name);
  t := ClientHeight div 2 - Size.cy - 2;
  Rect := Bounds(l, t, MaxWidth, Size.cy);

  if Size.cx < MaxWidth then
    Canvas.TextOut(l, t, Name)
  else
    TextRectW(Canvas, Rect, Name, [tfEndEllipsis]);

  // Вывод статуса
  Canvas.Font.Name := TMHStyle.StatusFontName;
  Canvas.Font.Size := TMHStyle.StatusSize;
  Canvas.Font.Style := TMHStyle.StatusStyle;
  Canvas.Font.Color := TMHStyle.StatusColor;
  Canvas.Brush.Style := bsClear;

  Size := Canvas.TextExtent(Status);
  t := ClientHeight div 2;
  Rect := Bounds(l, t, MaxWidth, Size.cy);

  if Size.cx < MaxWidth then
    Canvas.TextOut(l, t, Status)
  else
    TextRectW(Canvas, Rect, Status, [tfEndEllipsis]);
end;

procedure TMessageHeader.CreateWnd;
begin
  inherited;
  DoubleBuffered := True;

  Constraints.MinHeight := TMHStyle.ControlHeight;
  Constraints.MaxHeight := TMHStyle.ControlHeight;
  ClientHeight := TMHStyle.ControlHeight;

  FImageCall.Parent := Self;
  FImageCallVideo.Parent := Self;
end;

procedure TMessageHeader.Resize;
begin
  inherited;
  FImageCallVideo.Top := (ClientHeight - FImageCallVideo.Height) div 2;
  FImageCall.Top := FImageCallVideo.Top;

  FImageCallVideo.Left := ClientWidth - TMHStyle.ButtonVideoMarginRight -
    FImageCallVideo.Width;
  FImageCall.Left := FImageCallVideo.Left - TMHStyle.ButtonAudioMarginRight -
    FImageCall.Width;
end;

procedure TMessageHeader.SelectFriend(FriendItem: TFriendItem);
begin
  if not Assigned(FriendItem) then
  begin
    FSelectFriend := nil;
    Visible := False;
  end
  else
  begin
    FSelectFriend := FriendItem;
    FSelectFriend.OnUpdate := FriendUpdate;
    Visible := True;
    Invalidate;
  end;
end;

procedure TMessageHeader.FriendUpdate(Sender: TObject);
begin
  Invalidate;
end;

end.
