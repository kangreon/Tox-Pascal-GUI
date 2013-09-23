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
  ActiveRegion, Graphics, Classes, Controls, Math;

type
  { Png Image для разных компиляторов }
  {$IFDEF FPC}TPngImage = TPortableNetworkGraphic;{$ENDIF}
  {$IFDEF OLD_DELPHI}TPngImage = TPNGObject;{$ENDIF}
  {$IFDEF NEW_DELPHI}TPngImage = Vcl.Imaging.PngImage.TPngImage;{$ENDIF}
  
  TButtonStyle = (bsNone, bsActive, bsDown);

  TButtonActive = class(TActiveRegion)
  private
    FInserImage: Boolean;
    FButtonStyle: TButtonStyle;
    FNormal: TBitmap;
    FActive: TBitmap;
    FDown: TBitmap;
    FOnClick: TNotifyEvent;
  protected
    procedure CursorMessage(RegionMessage: TRegionMessage; const x, y: Integer;
      Button: TMouseButton; Shift: TShiftState); override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InsertImage(Normal, Active, Down: TBitmap);

    property OnClick: TNotifyEvent read FOnClick write FOnCLick;
  end;

implementation

{ TButtonActive }

constructor TButtonActive.Create(AOwner: TComponent);
begin
  inherited;
  FInserImage := False;
  FButtonStyle := bsNone;

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
        if Assigned(FOnClick) then
          FOnClick(Self);
        Invalidate;
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
begin
  inherited;
  if not FInserImage then
    Exit;

  case FButtonStyle of
    bsNone:
      begin
        l := (Width - FNormal.Width) div 2;
        t := (Height - FNormal.Height) div 2;
        Canvas.Draw(l, t, FNormal);
      end;

    bsActive:
      begin
        l := (Width - FActive.Width) div 2;
        t := (Height - FActive.Height) div 2;
        Canvas.Draw(l, t, FActive);
      end;

    bsDown:
      begin
        l := (Width - FDown.Width) div 2;
        t := (Height - FDown.Height) div 2;
        Canvas.Draw(l, t, FDown);
      end;
  end;
end;

end.
