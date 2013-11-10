// SkinBase.pas
//
// Базовый класс скин-движка, реализующий загрузку и сохранение параметров
// отображения GUI.
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit SkinBase;

interface
  {$I tox.inc}

  {$IFDEF FPC}
    {$DEFINE PNGPLUS}
  {$ENDIF}

uses
  {$IFDEF DELPHI}
  PngImage,
  {$ENDIF}
  {$IFDEF PNGPLUS}
  lazaruspng,
  {$ENDIF}
  Graphics, SysUtils, IniFiles, StringUtils, Math, Classes, Types, SkinTypes;

type
  {$IFDEF PNGPLUS}
  TPngImage = TLazarusPng;
  {$ELSE}
    {$IFDEF FPC}
    TPngImage = TPortableNetworkGraphic;
    {$ENDIF}
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
    function Read(Section, Name: string; Value: Integer): Integer; overload;
    function Read(Section, Name, Value: string): string; overload;
    function Read(Section, Name: string; Value: TFontStyles): TFontStyles; overload;
    function Read(Section, Name: string; Value: TStateColor): TStateColor; overload;
    function Read(Section, Name: string; Value: array of TColor): TStateColor; overload;
    function Read(Name, Value: string): string; overload;
    function Read(Name: string; Value: Integer): Integer; overload;
    function Read(Name: string; Value: TFontStyles): TFontStyles; overload;
    function Read(Name: string; Value: TStateColor): TStateColor; overload;
    function Read(Name: string; Value: array of TColor): TStateColor; overload;
    function ReadColor(Section, Name: string; Color: TColor): TColor; overload;
    function ReadColor(Section, Name: string; r, g, b: Byte): TColor; overload;
    function ReadColor(Name: string; Color: TColor): TColor; overload;
    function ReadColor(Name: string; r, g, b: Byte): TColor; overload;

    procedure SelectSection(Section: DataString);

    procedure Write(Section, Name, Value: DataString); overload;
    procedure Write(Section, Name: DataString; Value: Integer); overload;

    function LoadImage(ImageName: DataString; Transporent: Boolean = False;
      BackgroundColor: TColor = 0; DefLeft: Integer = 0; DefTop: Integer = 0;
      DefWidth: Integer = 0; DefHeight: Integer = 0): TBitmap;
    procedure LoadFont(Font: TFontSkin; Caption, Name: DataString; Size: Integer;
      Color: array of TColor; Style: TFontStyles); overload;
    procedure LoadFont(Font: TFontSkin; Caption, Name: DataString; Size: Integer;
      Color: TColor; Style: TFontStyles); overload;
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

function TSkinBase.Read(Name: string; Value: TStateColor): TStateColor;
begin
  Result := Read(FSelectedSection, Name, Value);
end;

function TSkinBase.Read(Name: string; Value: array of TColor): TStateColor;
begin
  Result := Read(FSelectedSection, Name, Value);
end;

function TSkinBase.ReadColor(Section, Name: string; r, g, b: Byte): TColor;
begin
  Result := ReadColor(Section, Name, RGB(r, g, b));
end;

function TSkinBase.ReadColor(Section, Name: string; Color: TColor): TColor;
begin
  Result := Read(Section, Name + '.Color', Integer(Color));
end;

function TSkinBase.ReadColor(Name: string; r, g, b: Byte): TColor;
begin
  Result := ReadColor(FSelectedSection, Name, RGB(r, g, b));
end;

function TSkinBase.ReadColor(Name: string; Color: TColor): TColor;
begin
  Result := ReadColor(FSelectedSection, Name, Color);
end;

function TSkinBase.Read(Section, Name: string; Value: TStateColor): TStateColor;
var
  State: TStateColor;
begin
  State[0] := Read(Section, Name + '.Color', Integer(Value[0]));
  State[1] := Read(Section, Name + '.ColorActive', Integer(Value[1]));
  State[2] := Read(Section, Name + '.ColorDown', Integer(Value[2]));
  Result := State;
end;

function TSkinBase.Read(Section, Name: string;
  Value: array of TColor): TStateColor;
var
  State: TStateColor;
begin
  if Length(Value) >= 3 then
  begin
    State[0] := Value[0];
    State[1] := Value[1];
    State[2] := Value[2];

    Result := Read(Name, State);
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

procedure TSkinBase.LoadFont(Font: TFontSkin; Caption, Name: DataString; Size: Integer;
  Color: array of TColor; Style: TFontStyles);
begin
  Font.Name := Read(Caption + '.Font.Name', Name);
  Font.Size := Read(Caption + '.Font.Size', Size);
  Font.Color := Read(Caption + '.Font', Color);
  Font.Styles := Read(Caption + '.Font.Style', Style);
end;

procedure TSkinBase.LoadFont(Font: TFontSkin; Caption, Name: DataString; Size: Integer;
  Color: TColor; Style: TFontStyles);
begin
  LoadFont(Font, Caption, Name, Size, [Color, Color, Color], Style);
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

  Left := Read(ImageName + '-Image', 'Left', Integer(0));
  Top := Read(ImageName + '-Image', 'Top', Integer(0));
  Width := Read(ImageName + '-Image', 'Width', Integer(0));
  Height := Read(ImageName + '-Image', 'Height', Integer(0));

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
  Stream: TResourceStream;
begin
  Result := False;

  try
    Stream := TResourceStream.Create(0, ResourceName, RT_RCDATA);
    Image := TPngImage.Create;
    try
      Stream.Position := 0;
      Image.LoadFromStream(Stream);
      if (Image.Width > 0) and (Image.Height > 0) then
      begin
        FSkinImage.Assign(Image);
        Result := True;
      end;
    finally
      Image.Free;
      Stream.Free;
    end;
  except
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

function TSkinBase.Read(Section, Name: string; Value: TFontStyles): TFontStyles;
begin
  Result := IntToFontStyles(Read(Section, Name, FontStylesToInt(Value)));
end;

function TSkinBase.Read(Name: string; Value: TFontStyles): TFontStyles;
begin
  Result := Read(FSelectedSection, Name, Value);
end;

end.
