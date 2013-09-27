//  UserIcon.pas
//
//  Преобразовывает пользовательские иконки к стандартному виду.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
//TODO: Исправить, когда реализуют поддержку иконок пользователя в Tox Core
unit UserIcon;

interface
  {$I tox.inc}

uses
  Graphics;

type
  TUserIcon = class
  private
    FImage: TBitmap;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Default;

    property Image: TBitmap read FImage;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

implementation

{ TUserIcon }

constructor TUserIcon.Create;
begin
  FImage := TBitmap.Create;
  FImage.Width := 44;
  FImage.Height := 41;
  //FImage.SetSize(44, 41);
  Default;
end;

{*  Загружает стандартное изображение из ресурс-файла
 *  приложения.
 *}
procedure TUserIcon.Default;
var
  Image: TBitmap;
begin
  Image := TBitmap.Create;
  try
    Image.LoadFromResourceName(0, 'DefaultUserface');
    FImage.Assign(Image);
  finally
    Image.Free;
  end;
end;

destructor TUserIcon.Destroy;
begin
  FImage.Free;
  inherited;
end;

function TUserIcon.GetHeight: Integer;
begin
  Result := FImage.Height;
end;

function TUserIcon.GetWidth: Integer;
begin
  Result := FImage.Width;
end;

end.


