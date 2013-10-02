//  PrintSprite.pas
//
//  Рисует в указанном месте анимированный спрайт с поддержкой альфа канала
//  с установленной скоростью.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit PaintSprite;

interface
  {$I tox.inc}

uses
  {$IFNDEF FPC}{$IFDEF NEW_DELPHI}Vcl.Imaging.pngimage,{$ELSE}PngImage,{$ENDIF}{$ENDIF}
  Classes, Graphics, Controls, ExtCtrls;

type
  {$IFDEF FPC}TPngImage = TPortableNetworkGraphic;{$ENDIF}
  {$IFDEF OLD_DELPHI}TPngImage = TPNGObject;{$ENDIF}

  TWinControlEx = class(TCustomControl)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TBitmapEx = class(TBitmap)
  private
    FTag: Byte;
  public
    property Tag: Byte read FTag write FTag;
  end;

  TPaintSprite = class
  private
    FBackground: TBitmap;
    FImage: TPngImage;
    FImageList: array of TBitmapEx;
    FImageSelect: Integer;
    FWidth: Integer;
    FHeignt: Integer;
    FCount: Integer;
    FControl: TWinControlEx;
    FOwnControl: TControl;
    FTimer: TTimer;
    function GetInterval: Integer;
    procedure SetInterval(const Value: Integer);
    procedure Repaint(Sender: TObject);
    procedure TimerTime(Sender: TObject);
    function GetActive: Boolean;
  public
    constructor Create(AImage: TPngImage; AControl: TControl);
    destructor Destroy; override;

    procedure Draw(Canvas: TCanvas; X, Y: Integer);
    procedure Stop;

    property Active: Boolean read GetActive;
    property Count: Integer read FCount;
    property Interval: Integer read GetInterval write SetInterval;
    property Height: Integer read FHeignt;
    property Width: Integer read FWidth;
  end;

implementation

{ TPaintSprite }

constructor TPaintSprite.Create(AImage: TPngImage; AControl: TControl);
var
  i: Integer;
begin
  FImage := TPngImage.Create;
  FImage.Assign(AImage);

  FWidth := FImage.Height;
  FHeignt := FWidth;
  FCount := FImage.Width div FHeignt;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerTime;

  FControl := TWinControlEx.Create(nil);
  FControl.Width := FWidth;
  FControl.Height := Height;
  FControl.OnPaint := Repaint;
  FControl.DoubleBuffered := True;

  FBackground := TBitmap.Create;
  FBackground.Width := FWidth;
  FBackground.Height := Height;

  SetLength(FImageList, FCount);
  for i := 0 to FCount - 1 do
  begin
    FImageList[i] := TBitmapEx.Create;
    FImageList[i].Width := FWidth;
    FImageList[i].Height := FHeignt;
    FImageList[i].Tag := 0;
  end;

  FOwnControl := AControl;

  FImageSelect := 0;

  Interval := 60;
end;

destructor TPaintSprite.Destroy;
begin
  FImage.Free;
  FControl.Free;
  FBackground.Free;
  inherited;
end;

procedure TPaintSprite.Draw(Canvas: TCanvas; X, Y: Integer);
begin
  if FTimer.Enabled then
    FControl.Invalidate
  else
  begin
    FImageSelect := 0;

    FTimer.Enabled := True;
    FBackground.Canvas.CopyRect(Rect(0, 0, FWidth, FHeignt), Canvas,
      Rect(X, Y, X + FWidth, Y + FHeignt));

    FControl.Left := X;
    FControl.Top := Y;
    FControl.Parent := TWinControl(FOwnControl);
    FControl.Invalidate;
  end;
end;

function TPaintSprite.GetActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TPaintSprite.GetInterval: Integer;
begin
  Result := FTimer.Interval;
end;

procedure TPaintSprite.SetInterval(const Value: Integer);
begin
  FTimer.Interval := Value;
end;

procedure TPaintSprite.Stop;
begin
  FTimer.Enabled := False;
  FControl.Parent := nil;
end;

procedure TPaintSprite.Repaint(Sender: TObject);
var
  Canvas: TCanvas;
  Bitmap: TBitmapEx;
begin
  Canvas := FControl.Canvas;
  Bitmap := FImageList[FImageSelect];

  if Bitmap.Tag = 0 then
  begin
    Bitmap.Canvas.Draw(0, 0, FBackground);
    Bitmap.Canvas.Draw(-1 * FImageSelect * FWidth, 0, FImage);
    Bitmap.Tag := 1;
  end;

  Canvas.Draw(0, 0, Bitmap);
end;

{*  Переключение изображения
 *}
procedure TPaintSprite.TimerTime(Sender: TObject);
begin
  Inc(FImageSelect);
  if FImageSelect >= FCount then
    FImageSelect := 0;

  FControl.Invalidate;
end;

{ TWinControlEx }

procedure TWinControlEx.Paint;
begin
  inherited;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

end.
