// LazarusPng.pas
//
// Рисование PNG изображения с учетом альфа канала для Lazarus под Linux (GTK?).
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit LazarusPng;

interface
  {$I tox.inc}

{$IFDEF FPC}
uses
  Classes, SysUtils, Graphics;

type
  PRGBA = ^TRGBA;
  TRGBA = record
    b: Byte;
    g: Byte;
    r: Byte;
    a: Byte;
  end;

  BPyte = ^Byte;

  { TLazarusPng }

  TLazarusPng = class(TGraphic)
  private
    FImage: TPortableNetworkGraphic;
    procedure DrawPixel(Source: PRGBA; var Dest: PRGBA);
    procedure DrawTransporent(Bitmap: TBitmap);
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetTransparent(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

{$ENDIF}
implementation

{$IFDEF FPC}
var
  AlphaValue: array[Byte] of array[Byte] of Byte;

{ TLazarusPng }

constructor TLazarusPng.Create;
begin
  inherited Create;
  FImage := TPortableNetworkGraphic.Create;
end;

destructor TLazarusPng.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TLazarusPng.Assign(ASource: TPersistent);
begin
  ASource.Assign(FImage);
end;

function GetColorExt(Source, Dest, Alpha: Byte): Byte; inline;
begin
  if Source > Dest then
    Result := AlphaValue[Alpha][Source - Dest] + Dest
  else
    Result := Dest - AlphaValue[Alpha][Dest - Source]
end;

procedure TLazarusPng.DrawPixel(Source: PRGBA; var Dest: PRGBA);
var
  b, g, r, a: Byte;
begin
  a := Source^.a;
  b := GetColorExt(Source^.b, Dest^.b, a);
  g := GetColorExt(Source^.g, Dest^.g, a);
  r := GetColorExt(Source^.r, Dest^.r, a);

  Dest^.b := b;
  Dest^.g := g;
  Dest^.r := r;
  Dest^.a := 255;
end;

procedure TLazarusPng.DrawTransporent(Bitmap: TBitmap);
var
  Source, Dest: PRGBA;
  i, c, si: Integer;
  PointerSource, PointerDest: Pointer;
begin
  c := Bitmap.RawImage.DataSize div 4;

  New(Source);
  New(Dest);
  si := SizeOf(TRGBA);
  for i := 0 to c - 1 do
  begin
    PointerSource := Pointer(FImage.RawImage.Data + i * 4);
    PointerDest := Pointer(Bitmap.RawImage.Data + i * 4);

    Move(PointerSource^, Source^, si);
    Move(PointerDest^, Dest^, si);

    DrawPixel(Source, Dest);

    Move(Dest^, PointerDest^, si);
  end;
end;

procedure TLazarusPng.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Bitmap: TBitmap;
  DestRect: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.Transparent := False;
    DestRect := Bounds(0, 0, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);

    Bitmap.SetSize(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
    Bitmap.Canvas.CopyRect(DestRect, ACanvas, Rect);

    Bitmap.BeginUpdate;
    if Bitmap.RawImage.DataSize = FImage.RawImage.DataSize then
    begin
      DrawTransporent(Bitmap);
    end;
    Bitmap.EndUpdate;

    ACanvas.Draw(Rect.Left, Rect.Top, Bitmap);
  finally
    Bitmap.Free;
  end;
end;

function TLazarusPng.GetEmpty: Boolean;
begin
  Result := FImage.Empty;
end;

function TLazarusPng.GetHeight: Integer;
begin
  Result := FImage.Height;
end;

function TLazarusPng.GetTransparent: Boolean;
begin
  Result := FImage.Transparent;
end;

function TLazarusPng.GetWidth: Integer;
begin
  Result := FImage.Width;
end;

procedure TLazarusPng.LoadFromStream(Stream: TStream);
begin
  Stream.Position := 0;
  FImage.LoadFromStream(Stream);
end;

procedure TLazarusPng.SaveToStream(Stream: TStream);
begin
  Stream.Position := 0;
  FImage.SaveToStream(Stream);
end;

procedure TLazarusPng.SetHeight(Value: Integer);
begin

end;

procedure TLazarusPng.SetTransparent(Value: Boolean);
begin

end;

procedure TLazarusPng.SetWidth(Value: Integer);
begin

end;

procedure LoadAlphaValue;
var
  i, j: Integer;
begin
  for i := Low(AlphaValue) to High(AlphaValue) do
    for j := Low(AlphaValue[i]) to High(AlphaValue[i]) do
    begin
      AlphaValue[i][j] := Round(i / 255 * j);
    end;
end;

initialization
  LoadAlphaValue;

{$ENDIF}
end.

