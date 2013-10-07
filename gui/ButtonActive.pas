//  ButtonActive.pas
//
//  Рисует кнопку с тремя состояними, которые задаются при помощи
//  PNG изображений
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ButtonActive;

interface
  {$I tox.inc}

uses
  { Png Image для разных компиляторов }
  {$IFDEF NEW_DELPHI}Vcl.Imaging.PngImage,{$ENDIF}
  ActiveRegion, Graphics, Classes, Controls, Math, ResourceImage;

type
  { Png Image для разных компиляторов }
  {$IFDEF FPC}TPngImage = TPortableNetworkGraphic;{$ENDIF}
  {$IFDEF OLD_DELPHI}TPngImage = TPNGObject;{$ENDIF}
  {$IFDEF NEW_DELPHI}TPngImage = Vcl.Imaging.PngImage.TPngImage;{$ENDIF}
  
  TButtonStyle = (bsNone, bsActive, bsDown);

  TButtonActive = class(TActiveRegion)
  private
    FInserImage: Boolean;
    FIsImageList: Boolean;
    FImageListIndexStart: Integer;
    FButtonStyle: TButtonStyle;
    FNormal: TBitmap;
    FActive: TBitmap;
    FDown: TBitmap;
    FList: TBitmapList;
    FOnClick: TNotifyEvent;
  protected
    procedure CursorMessage(RegionMessage: TRegionMessage; const x, y: Integer;
      Button: TMouseButton; Shift: TShiftState); override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InsertImage(Normal, Active, Down: TBitmap); overload;
    procedure InsertImage(ImageList: TBitmapList; StartIndex: Integer); overload;

    property OnClick: TNotifyEvent read FOnClick write FOnCLick;
  end;

implementation

{ TButtonActive }

constructor TButtonActive.Create(AOwner: TComponent);
begin
  inherited;
  FInserImage := False;
  FButtonStyle := bsNone;
  FIsImageList := False;

  FActive := TBitmap.Create;
  FNormal := TBitmap.Create;
  FDown := TBitmap.Create;
end;

destructor TButtonActive.Destroy;
begin
  FActive.Free;
  FNormal.Free;
  FDown.Free;
  inherited;
end;

procedure TButtonActive.InsertImage(ImageList: TBitmapList;
  StartIndex: Integer);
var
  i: Integer;
  MaxWidth, MaxHeight: Integer;
begin
  MaxWidth := 0;
  MaxHeight := 0;

  if Length(ImageList) > StartIndex + 2 then
  begin
    FInserImage := True;

    for i := StartIndex to StartIndex + 2 do
    begin
      if Assigned(ImageList[i]) then
      begin
        MaxWidth := Max(MaxWidth, ImageList[i].Width);
        MaxHeight := Max(MaxHeight, ImageList[i].Height);
      end
      else
      begin
        FInserImage := False;
      end;
    end;
  end;

  if FInserImage then
  begin
    Self.Width := MaxWidth;
    Self.Height := MaxHeight;
    FIsImageList := True;
    FList := ImageList;
    FImageListIndexStart := StartIndex;
  end;
end;

{*  Установка состояни кнопки в зависимости от полученных сообщений от
 *  компонента захвата событий мыши.
 *}
procedure TButtonActive.CursorMessage(RegionMessage: TRegionMessage; const x,
  y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  case RegionMessage of
    rmMouseEnter:
      begin
        FButtonStyle := bsActive;
        Invalidate;
      end;

    rmMouseLeave:
      begin
        FButtonStyle := bsNone;
        Invalidate;
      end;

    rmMouseDown:
      begin
        FButtonStyle := bsDown;
        Invalidate;
      end;

    rmMouseUp:
      begin
        if FButtonStyle = bsDown then
          FButtonStyle := bsActive;

        Invalidate;
      end;

    rmMouseClick:
      begin
        Invalidate;

        if Assigned(FOnClick) then
          FOnClick(Self);
      end;
  end;
end;

{*  Добавление изображений для каждого из состояния кнопки.
 *  Размер кнопки устанавливается по максимальному размеру изображений.
 *
 *  В кнопке используются изображения формата PNG c поддержкой Alpha канала
 *}
procedure TButtonActive.InsertImage(Normal, Active, Down: TBitmap);
begin
  FInserImage := True;

  FNormal.Assign(Normal);
  FDown.Assign(Down);
  FActive.Assign(Active);

  Width := Max(FNormal.Width, Max(FActive.Width, FDown.Width));
  Height := Max(FNormal.Height, Max(FActive.Height, FDown.Height));
end;

{*  Рисование кнопки в соответствии с текущим ее состоянием
 *  Если изображения не загружены, ничего не рисуется
 *}
procedure TButtonActive.Paint;
var
  l, t: Integer;
  Image: TBitmap;
begin
  inherited;
  if not FInserImage then
    Exit;

  Image := nil;
  case FButtonStyle of
    bsNone:
      begin
        if FIsImageList then
          Image := FList[FImageListIndexStart]
        else
          Image := FNormal;
      end;

    bsActive:
      begin
        if FIsImageList then
          Image := FList[FImageListIndexStart + 1]
        else
          Image := FActive;
      end;

    bsDown:
      begin
        if FIsImageList then
          Image := FList[FImageListIndexStart + 2]
        else
          Image := FDown;
      end;
  end;

  if Assigned(Image) then
  begin
    l := (Width - Image.Width) div 2;
    t := (Height - Image.Height) div 2;
    Canvas.Draw(l, t, Image);
  end;
end;

end.
