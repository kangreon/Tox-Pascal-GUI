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

  Graphics, Classes, Controls, ImgList;

type
  {$IFDEF FPC}TPngImage = TPortableNetworkGraphic;{$ENDIF}
  {$IFDEF OLD_DELPHI}TPngImage = TPNGObject;{$ENDIF}
  
  TResourceImage = class
  private
    FUserstatusButtonDown: TBitmap;
    FStatusOnline: TBitmap;
    FStatusOffline: TBitmap;
    FStatusOnlineTransporent: TPngImage;
    FStatusAwayTransporent: TPngImage;
    FStatusOfflineTransporent: TPngImage;
    FLoadingAnimate10: TPngImage;
    FOnlineMenu: TBitmap;
    FImageMenu: TImageList;
    FButtonAddUserNormal: TBitmap;
    FButtonAddUserActive: TBitmap;
    FButtonAddUserDown: TBitmap;
    FButtonSettingsNormal: TBitmap;
    FButtonSettingsActive: TBitmap;
    FButtonSettingsDown: TBitmap;
    FButtonGroupDown: TBitmap;
    FButtonGroupNormal: TBitmap;
    FButtonGroupActive: TBitmap;
    function LoadImageBmp(Name: string): TBitmap;
    function LoadImagePng(name: string): TPngImage;
  public
    constructor Create;
    destructor Destroy; override;
    class function Clone: TResourceImage;

    property LoadingAnimate10: TPngImage read FLoadingAnimate10;

    property ButtonAddUserNormal: TBitmap read FButtonAddUserNormal;
    property ButtonAddUserActive: TBitmap read FButtonAddUserActive;
    property ButtonAddUserDown: TBitmap read FButtonAddUserDown;
    property ButtonSettingsNormal: TBitmap read FButtonSettingsNormal;
    property ButtonSettingsActive: TBitmap read FButtonSettingsActive;
    property ButtonSettingsDown: TBitmap read FButtonSettingsDown;
    property ButtonGroupNormal: TBitmap read FButtonGroupNormal;
    property ButtonGroupActive: TBitmap read FButtonGroupActive;
    property ButtonGroupDown: TBitmap read FButtonGroupDown;

    property ImagesMenu: TImageList read FImageMenu;

    property OnlineMenu: TBitmap read FOnlineMenu;

    property StatusOffline: TBitmap read FStatusOffline;
    property StatusOnline: TBitmap read FStatusOnline;

    property StatusAwayTransporent: TPngImage read FStatusAwayTransporent;
    property StatusOfflineTransporent: TPngImage read FStatusOfflineTransporent;
    property StatusOnlineTransporent: TPngImage read FStatusOnlineTransporent;

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
begin
  //TODO: Заменить на PNG
  FUserstatusButtonDown := LoadImageBmp('UserstatusButtonDown');
  FStatusOnline := LoadImageBmp('OnlineS');
  FStatusOffline := LoadImageBmp('OfflineS');

  FStatusOnlineTransporent := LoadImagePng('OnlineTR');
  FStatusAwayTransporent := LoadImagePng('AwayTR');
  FStatusOfflineTransporent := LoadImagePng('OfflineTR');

  FLoadingAnimate10 := LoadImagePng('LoadingAnimate10');

  FOnlineMenu := LoadImageBmp('OnlineMenu');
  FOnlineMenu.Transparent := True;
  FOnlineMenu.TransparentColor := clBlack;

  FImageMenu := TImageList.Create(nil);
  FImageMenu.Width := 10;
  FImageMenu.Height := 10;
  FImageMenu.InsertMasked(0, FOnlineMenu, clBlack);

  FButtonAddUserNormal := LoadImageBmp('AddUserNormal');
  FButtonAddUserActive := LoadImageBmp('AddUserActive');
  FButtonAddUserDown := LoadImageBmp('AddUserDown');
  FButtonSettingsNormal := LoadImageBmp('SettingsNormal');
  FButtonSettingsActive := LoadImageBmp('SettingsActive');
  FButtonSettingsDown := LoadImageBmp('SettingsDown');
  FButtonGroupNormal := LoadImageBmp('GroupNormal');
  FButtonGroupActive := LoadImageBmp('GroupActive');
  FButtonGroupDown := LoadImageBmp('GroupDown');
end;

destructor TResourceImage.Destroy;
begin
  FUserstatusButtonDown.Free;
  FStatusOnline.Free;
  FStatusOffline.Free;
  FStatusOnlineTransporent.Free;
  FStatusAwayTransporent.Free;
  FStatusOfflineTransporent.Free;
  FLoadingAnimate10.Free;
  FOnlineMenu.Free;
  FImageMenu.Free;
  FButtonAddUserNormal.Free;
  FButtonAddUserActive.Free;
  FButtonAddUserDown.Free;
  FButtonSettingsNormal.Free;
  FButtonSettingsActive.Free;
  FButtonSettingsDown.Free;
  FButtonGroupNormal.Free;
  FButtonGroupActive.Free;
  FButtonGroupDown.Free;
  inherited;
end;

{*  Загружает из ресурсов изображение с указанным идентификатором
 *  В случае успешной загрузки, вернет изображение BMP
 *  В случае ошибки, вернет изображение с текстом "no load image"
 *}
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
//    Image.SetSize(Size.cx, Size.cy);
    Image.Canvas.TextOut(0, 0, 'no load image');
  end;
  Result := Image;
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

end.
