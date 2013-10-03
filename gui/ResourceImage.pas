//  ResourceImage.pas
//
//  Загружает изображения из файла ресурсов и производит действия по их
//  группировке и оптимизации.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ResourceImage;

interface

{$I tox.inc}

uses
  {$I tox-uses.inc}
  {$IFDEF NEW_DELPHI}Vcl.Imaging.PngImage,{$ENDIF}
  {$IFDEF OLD_DELPHI}PngImage,{$ENDIF}

  Graphics, Classes, Controls, ImgList, ActiveRegion, libtox, SysUtils;

type
  {$IFDEF FPC}TPngImage = TPortableNetworkGraphic;{$ENDIF}
  {$IFDEF OLD_DELPHI}TPngImage = TPNGObject;{$ENDIF}

  TBitmapList = array of TBitmap;

  TResourceImage = class
  private
    FUserstatusButtonDown: TBitmap;
    FLoadingAnimate10: TPngImage;
    FImageMenu: TImageList;

    FToxSkin: TBitmap;
    FUserListStatus: array of TBitmapList;
    FControlButtons: TBitmapList;
    FSelfStatusIcons: TBitmapList;

    procedure LoadControlButtons(StartLeft, StartTop, Width, Height: Integer);
    procedure LoadSelfStatusIcons(StartLeft, StartTop, Width, Height: Integer);
    procedure LoadUserListStatus;
    function LoadImageBmp(Name: string): TBitmap;
    function LoadImageBmpFromPng(Name: string): TBitmap;
    function LoadImagePng(name: string): TPngImage;
  public
    constructor Create;
    destructor Destroy; override;
    class function Clone: TResourceImage;

    function GetControlButtonIcon(ImageId: Integer; State: TDownState): TBitmap;
    function GetSelfSTatusIcon(Status: TToxUserStatus): TBitmap;
    function GetUserListStatusIcon(MouseState: TDownState; Status: TToxUserStatus;
      IsNewMessage: Boolean): TBitmap;

    property ControlButtons: TBitmapList read FControlButtons;

    property LoadingAnimate10: TPngImage read FLoadingAnimate10;

    property ImagesMenu: TImageList read FImageMenu;

    property ToxSkin: TBitmap read FToxSkin;

    property UserstatusButtonDown: TBitmap read FUserstatusButtonDown;
  end;

implementation

uses
  main;

var
  ResImg: TResourceImage;

{ TResourceImage }

class function TResourceImage.Clone: TResourceImage;
begin
  if not Assigned(ResImg) then
    ResImg := TResourceImage.Create;

  Result := ResImg;
end;

constructor TResourceImage.Create;
var
  Image: TBitmap;
begin
  FUserstatusButtonDown := LoadImageBmp('UserstatusButtonDown');
  FLoadingAnimate10 := LoadImagePng('LoadingAnimate10');

  FToxSkin := LoadImageBmpFromPng('ToxSkin');
  LoadUserListStatus;
  LoadControlButtons(0, 108, 20, 20);
  LoadSelfStatusIcons(0, 128, 22, 22);

  FImageMenu := TImageList.Create(nil);

  Image := TBitmap.Create;
  Image.SetSize(16, 16);
  Image.TransparentColor := 33554432;
  Image.Transparent := True;
  Image.TransparentMode := tmFixed;
  Image.PixelFormat := pf24bit;
  try
    Image.Canvas.CopyRect(Bounds(0, 0, 16, 16), FToxSkin.Canvas, Bounds(3, 153, 16, 16));
    FImageMenu.InsertMasked(0, Image, clFuchsia);

    Image.Canvas.CopyRect(Bounds(0, 0, 16, 16), FToxSkin.Canvas, Bounds(47, 153, 16, 16));
    FImageMenu.InsertMasked(1, Image, clFuchsia);

    Image.Canvas.CopyRect(Bounds(0, 0, 16, 16), FToxSkin.Canvas, Bounds(91, 153, 16, 16));
    FImageMenu.InsertMasked(2, Image, clFuchsia);

    Image.Canvas.CopyRect(Bounds(0, 0, 16, 16), FToxSkin.Canvas, Bounds(135, 153, 16, 16));
    FImageMenu.InsertMasked(3, Image, clFuchsia);
  finally
    Image.Free;
  end;
end;

destructor TResourceImage.Destroy;
begin
  FUserstatusButtonDown.Free;
  FLoadingAnimate10.Free;
  FImageMenu.Free;
  inherited;
end;

function TResourceImage.GetControlButtonIcon(ImageId: Integer;
  State: TDownState): TBitmap;
var
  i: Integer;
begin
  i := ImageId * 3 + Integer(State);
  Result := FControlButtons[i];
end;

function TResourceImage.GetSelfSTatusIcon(Status: TToxUserStatus): TBitmap;
var
  i: Integer;
begin
  i := Integer(Status) * 2;
  Result := FSelfStatusIcons[i];
end;

function TResourceImage.GetUserListStatusIcon(MouseState: TDownState;
  Status: TToxUserStatus; IsNewMessage: Boolean): TBitmap;
var
  i, j: Integer;
begin
  i := Integer(MouseState);
  j := Integer(Status) * 2;
  if IsNewMessage then
    Inc(j);

  Result := FUserListStatus[i][j];
end;

{*  Загружает из ресурсов изображение с указанным идентификатором
 *  В случае успешной загрузки, вернет изображение BMP
 *  В случае ошибки, вернет изображение с текстом "no load image"
 *}
procedure TResourceImage.LoadControlButtons(StartLeft, StartTop, Width,
  Height: Integer);
var
  i: Integer;
  Image: TBitmap;
  DRect, SRect: TRect;
begin
  DRect := Bounds(0, 0, Width, Height);

  SetLength(FControlButtons, 9);
  for i := Low(FControlButtons) to High(FControlButtons) do
  begin
    Image := TBitmap.Create;
    try
      Image.SetSize(Width, Height);

      SRect := Bounds(StartLeft + i * Width, StartTop, Width, Height);
      Image.Canvas.CopyRect(DRect, FToxSkin.Canvas, SRect);
    finally
      FControlButtons[i] := Image;
    end;
  end;
end;

function TResourceImage.LoadImageBmp(Name: string): TBitmap;
var
  Image: TBitmap;
  Size: TSize;
begin
  Image := TBitmap.Create;
  try
    Image.LoadFromResourceName(0, Name);
  except
    Size := Image.Canvas.TextExtent('no load image');
    Image.Width := Size.cx;
    Image.Height := Size.cy;
    Image.Canvas.TextOut(0, 0, 'no load image');
  end;
  Result := Image;
end;

function TResourceImage.LoadImageBmpFromPng(Name: string): TBitmap;
var
  Png: TPngImage;
begin
  Png := LoadImagePng(Name);
  try
    Result := TBitmap.Create;
    Result.Assign(Png);
  finally
    Png.Free;
  end;
end;

function TResourceImage.LoadImagePng(name: string): TPngImage;

  procedure LoadFromResourceName(Image: TPngImage; Instance: HInst;
    const Name: String);
  var
    ResStream: TResourceStream;
  begin
    try
      ResStream := TResourceStream.Create(Instance, Name, RT_RCDATA);
    except
      Exit;
    end;

    try
      Image.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
  end;

var
  Image: TPngImage;
begin
  Image := TPngImage.Create;
  try
    {$IFDEF FPC}
      LoadFromResourceName(Image, 0, name)
    {$ELSE}
      Image.LoadFromResourceName(0, name);
    {$ENDIF}
  except
//    Size := Image.Canvas.TextExtent('no load image');
//    Image.Width := Size.cx;
//    Image.Height := Size.cy;
//    Image.Canvas.TextOut(0, 0, 'no load image');
  end;

  Result := Image;
end;

procedure TResourceImage.LoadSelfStatusIcons(StartLeft, StartTop, Width,
  Height: Integer);
var
  i: Integer;
  Image: TBitmap;
  DRect, SRect: TRect;
begin
  DRect := Bounds(0, 0, Width, Height);

  SetLength(FSelfStatusIcons, 8);
  for i := Low(FSelfStatusIcons) to High(FSelfStatusIcons) do
  begin
    Image := TBitmap.Create;
    try
      Image.SetSize(Width, Height);

      SRect := Bounds(StartLeft + i * Width, StartTop, Width, Height);
      Image.Canvas.CopyRect(DRect, FToxSkin.Canvas, SRect);
    finally
      FSelfStatusIcons[i] := Image;
    end;
  end;
end;

procedure TResourceImage.LoadUserListStatus;
var
  i, j: Integer;
  Image: TBitmap;
  DRect, SRect: TRect;
begin
  DRect := Bounds(0, 0, 22, 22);

  SetLength(FUserListStatus, 3);
  for i := Low(FUserListStatus) to High(FUserListStatus) do
  begin
    SetLength(FUserListStatus[i], 8);
    for j := Low(FUserListStatus[i]) to High(FUserListStatus[i]) do
    begin
      SRect := Bounds(22 * j, i * 22 + 42, 22, 22);

      Image := TBitmap.Create;
      Image.SetSize(22, 22);
      Image.Canvas.CopyRect(DRect, FToxSkin.Canvas, SRect);

      FUserListStatus[i][j] := Image;
    end;
  end;
end;

end.
