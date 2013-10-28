unit SkinBase;

interface
  {$I tox.inc}

uses
  {$IFDEF DELPHI}
  PngImage,
  {$ENDIF}
  Graphics, SysUtils, IniFiles, StringUtils, Math, Classes, Types;

type
  {$IFDEF FPC}
  TPngImage = TPortableNetworkGraphic;
  {$ENDIF}

  TSkinBase =  class abstract
  private
    FSkinImage: TPngImage;
    FIniFile: TIniFile;
    FIsLoadImage: Boolean;
    FSelectedSection: DataString;
    function LoadSkinImageFromFile(FileName: string): Boolean;
    function LoadImageFromResource(ResourceName: string): Boolean;
  protected
    function Read(Section, Name, Value: string): string; overload;
    function Read(Section, Name: string; Value: Integer): Integer; overload;
    function Read(Name, Value: string): string; overload;
    function Read(Name: string; Value: Integer): Integer; overload;

    procedure SelectSection(Section: DataString);

    procedure Write(Section, Name, Value: DataString); overload;
    procedure Write(Section, Name: DataString; Value: Integer); overload;

    function LoadImage(ImageName: DataString; Transporent: Boolean = False;
      BackgroundColor: TColor = 0; DefLeft: Integer = 0; DefTop: Integer = 0;
      DefWidth: Integer = 0; DefHeight: Integer = 0): TBitmap;
  public
    constructor Create(SettingsFile: DataString); virtual;
    destructor Destroy; override;

    property IsLoadImage: Boolean read FIsLoadImage;
  end;

implementation

{ TSkinBase }

constructor TSkinBase.Create(SettingsFile: DataString);
var
  SkinFileName: string;
begin
  FSkinImage := TPngImage.Create;
  FIniFile := TIniFile.Create(SettingsFile);

  // Загрузка файла-текстуры с диска если это возможно
  SkinFileName := ExtractFilePath(SettingsFile) + Read('Skin', 'FileName', '');
  FIsLoadImage := LoadSkinImageFromFile(SkinFileName) or
    LoadImageFromResource('NewToxUI')
end;

destructor TSkinBase.Destroy;
begin
  FIniFile.Free;
  FSkinImage.Free;
  inherited;
end;

{ *  Пробует загрузить изображение FileName с диска. Возвращает True если
  *  изображение загрузилось. Загруженное изображение копируется в FSkinImage.
  * }
function TSkinBase.LoadSkinImageFromFile(FileName: string): Boolean;
var
  Image: TPngImage;
begin
  Result := False;

  if not FileExists(FileName) then
    Exit;

  Image := TPngImage.Create;
  try
    try
      Image.LoadFromFile(FileName);
      if (Image.Width > 0) and (Image.Height > 0) then
      begin
        FSkinImage.Assign(Image);
        Result := True;
      end;
    except
    end;
  finally
    Image.Free;
  end;
end;

procedure TSkinBase.Write(Section, Name, Value: DataString);
begin
  FIniFile.WriteString(Section, Name, Value);
end;

procedure TSkinBase.Write(Section, Name: DataString; Value: Integer);
begin
  FIniFile.WriteInteger(Section, Name, Value);
end;

{ *  Загружает изображение из ресурсов
  * }
function TSkinBase.LoadImage(ImageName: DataString; Transporent: Boolean;
  BackgroundColor: TColor; DefLeft, DefTop, DefWidth,
  DefHeight: Integer): TBitmap;
var
  Bitmap: TBitmap;
  Left, Top, Width, Height: Integer;
begin
  Bitmap := TBitmap.Create;
  Result := Bitmap;

  Left := Read(ImageName + '-Image', 'Left', 0);
  Top := Read(ImageName + '-Image', 'Top', 0);
  Width := Read(ImageName + '-Image', 'Width', 0);
  Height := Read(ImageName + '-Image', 'Height', 0);

  if Min(Width, Height) <= 0 then
  begin
    Width := DefWidth;
    Height := DefHeight;
    Left := DefLeft;
    Top := DefTop;
  end;

  if Min(Width, Height) > 0 then
  begin
    Bitmap.SetSize(Width, Height);

    if Transporent then
    begin
      Bitmap.Canvas.Brush.Color := BackgroundColor;
      Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
    end;

    Bitmap.Canvas.Draw(-Left, -Top, FSkinImage);
//
//    Bitmap.Canvas.CopyRect(
//      Bounds(0, 0, Width, Height),
//      FSkinImage.Canvas,
//      Bounds(Left, Top, Width, Height)
//    );
  end;
end;

function TSkinBase.LoadImageFromResource(ResourceName: string): Boolean;
var
  Image: TPngImage;
begin
  Result := False;
  Image := TPngImage.Create;
  try
    try
      Image.LoadFromResourceName(0, ResourceName);
      if (Image.Width > 0) and (Image.Height > 0) then
      begin
        FSkinImage.Assign(Image);
        Result := True;
      end;
    except
    end;
  finally
    Image.Free;
  end;
end;

function TSkinBase.Read(Section, Name, Value: string): string;
begin
  if FIsLoadImage then
    Result := FIniFile.ReadString(Section, Name, Value);
end;

function TSkinBase.Read(Section, Name: string; Value: Integer): Integer;
begin
  if FIsLoadImage then
    Result := FIniFile.ReadInteger(Section, Name, Value)
  else
    Result := 0;
end;

function TSkinBase.Read(Name, Value: string): string;
begin
  Result := Read(FSelectedSection, Name, Value);
end;

function TSkinBase.Read(Name: string; Value: Integer): Integer;
begin
  Result := Read(FSelectedSection, Name, Value);
end;

procedure TSkinBase.SelectSection(Section: DataString);
begin
  FSelectedSection := Section;
end;

end.
