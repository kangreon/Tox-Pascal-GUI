// TabRequest.pas
//
// Вкладка отображения информации о новых запросах на добавление пользователей
// в список друзей.
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit TabRequest;

interface

uses
  Classes, Graphics, Controls, SkinTabControl, StringUtils, SysUtils;

type
  TTabRequest = class(TGraphicControl)
  private
    FRequestCount: Integer;
    FOnUpdateRequest: TNotifyEvent;
    FRequestCountText: DataString;
    procedure SetRequestCount(Value: Integer);
    procedure EventUpdateRequest;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent; Skin: TSkinTabControl); reintroduce;
    destructor Destroy; override;

    property RequestCount: Integer read FRequestCount;
    property RequestCountText: DataString read FRequestCountText;

    property OnUpdateRequest: TNotifyEvent read FOnUpdateRequest write FOnUpdateRequest;
  end;

implementation

{ TTabRequest }

constructor TTabRequest.Create(AOwner: TComponent; Skin: TSkinTabControl);
begin
  inherited Create(AOwner);
  FRequestCount := -1;
  SetRequestCount(1);
  Height := 30;
end;

destructor TTabRequest.Destroy;
begin

  inherited;
end;

procedure TTabRequest.EventUpdateRequest;
begin
  if Assigned(FOnUpdateRequest) then
    FOnUpdateRequest(Self);
end;

procedure TTabRequest.Paint;
begin
  inherited;

end;

procedure TTabRequest.SetRequestCount(Value: Integer);
var
  StrNum: DataString;
  v: Integer;
begin
  if FRequestCount <> Value then
  begin
    FRequestCount := Value;
    StrNum := IntToStr(Value);
    v := Value mod 10;
    if Value mod 100 div 10 = 1 then
      v := Value mod 100;

    case v of
      0, 5..9, 10..19:
        FRequestCountText := StrNum + ' запросов дружбы';
      2..4:
        FRequestCountText := StrNum + ' запроса дружбы';
    else //1
      FRequestCountText := StrNum + ' запрос дружбы';
    end;
  end;

  {$IFDEF FPC}
  FRequestCountText := UTF8Encode(FRequestCountText);
  {$ENDIF}
end;

end.
