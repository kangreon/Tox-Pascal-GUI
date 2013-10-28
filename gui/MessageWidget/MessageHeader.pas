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
  Graphics, Classes, Controls, Types, ButtonActive, FriendItem, StringUtils,
  ImageUtils, SkinMessageHeader;

type
  TMessageHeader = class(TGraphicControl)
  private
    FImageCall: TButtonActive;
    FImageCallVideo: TButtonActive;
    FSelectFriend: TFriendItem;
    FSkin: TSkinMessageHeader;
    procedure FriendUpdate(Sender: TObject);
  protected
    procedure Resize; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent; Skin: TSkinMessageHeader); reintroduce;
    destructor Destroy; override;

    procedure SelectFriend(FriendItem: TFriendItem);
  end;

implementation

{ TMessageHeader }

constructor TMessageHeader.Create(AOwner: TComponent; Skin: TSkinMessageHeader);
begin
  inherited Create(AOwner);
  FSkin := Skin;

  FSelectFriend := nil;
  Visible := False;

  FImageCall := TButtonActive.Create(Self);
  FImageCall.InsertImage(Skin.ImgAudioButton[0], Skin.ImgAudioButton[1],
    Skin.ImgAudioButton[2]);
  FImageCall.Cursor := crHandPoint;

  FImageCallVideo := TButtonActive.Create(Self);
  FImageCallVideo.InsertImage(FSkin.ImgVideoButton[0], FSkin.ImgVideoButton[1],
    FSkin.ImgVideoButton[2]);
  FImageCallVideo.Cursor := crHandPoint;
end;

destructor TMessageHeader.Destroy;
begin
  FImageCall.Free;
  FImageCallVideo.Free;

  inherited;
end;

procedure TMessageHeader.Paint;
var
  Name, Status: DataString;
  TopPos, LeftPos: Integer;
  MaxWidth: Integer;
  Size: TSize;
  Rect: TRect;
begin
  inherited;
  if not Assigned(FSelectFriend) then
    Exit;

  Name := FSelectFriend.UserName;
  Status := FSelectFriend.StatusMessage;

  TopPos := (ClientHeight - FSkin.IconHeight) div 2;
  LeftPos := FSkin.IconMarginLeft;

  Canvas.Draw(LeftPos, TopPos, FSkin.ImgDefIcon);

  LeftPos := LeftPos + FSkin.IconWidth + FSkin.IconMarginRight;
  MaxWidth := FImageCall.Left - LeftPos - FSkin.AudioMarginLeft;

  // Вывод имени
  FSkin.SetCanvasForName(Canvas);

  Size := Canvas.TextExtent(Name);
  TopPos := ClientHeight div 2 - Size.cy - 2;
  Rect := Bounds(LeftPos, TopPos, MaxWidth, Size.cy);

  if Size.cx < MaxWidth then
    Canvas.TextOut(LeftPos, TopPos, Name)
  else
    TextRectW(Canvas, Rect, Name, [tfEndEllipsis]);

  // Вывод статуса
  FSkin.SetCanvasForStatus(Canvas);

  Size := Canvas.TextExtent(Status);
  TopPos := ClientHeight div 2;
  Rect := Bounds(LeftPos, TopPos, MaxWidth, Size.cy);

  if Size.cx < MaxWidth then
    Canvas.TextOut(LeftPos, TopPos, Status)
  else
    TextRectW(Canvas, Rect, Status, [tfEndEllipsis]);

  // Вывод нижней ограничивающей линии
  FSkin.SetCanvasForDivLine(Canvas);
  Canvas.MoveTo(0, ClientHeight - 1);
  Canvas.LineTo(ClientWidth, ClientHeight - 1);
end;

procedure TMessageHeader.Resize;
begin
  inherited;
  FImageCallVideo.Top := (ClientHeight - FImageCallVideo.Height) div 2;
  FImageCall.Top := FImageCallVideo.Top;

  FImageCallVideo.Left := ClientWidth - FImageCallVideo.Width -
    FSkin.VideoMarginRight;
  FImageCall.Left := FImageCallVideo.Left - FSkin.AudioMarginLeft -
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

procedure TMessageHeader.SetParent(AParent: TWinControl);
begin
  inherited;
  if not Assigned(AParent) then
    Exit;

  Constraints.MinHeight := FSkin.Height;
  Constraints.MaxHeight := FSkin.Height;
  ClientHeight := FSkin.Height;

  FImageCall.Parent := AParent;
  FImageCallVideo.Parent := AParent;
end;

procedure TMessageHeader.FriendUpdate(Sender: TObject);
begin
  Invalidate;
end;

end.
