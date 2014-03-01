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
  Classes, Graphics, Controls, SkinTabControl, StringUtils, SysUtils, TabView,
  ActiveRegion;

type
  TTabRequest = class(TTabView)
  private
    FRequestCount: Integer;
    FOnUpdateRequest: TNotifyEvent;
    FRequestCountText: DataString;
    procedure SetRequestCount(Value: Integer);
    procedure EventUpdateRequest;
  protected
    procedure DoCreate; override;
  public
    procedure CursorMessage(RegionMessage: TRegionMessage; x, y: Integer;
      Button: TMouseButton; Shift: TShiftState); override;
    function GetHeight: Integer; override;

    property RequestCount: Integer read FRequestCount;
    property RequestCountText: DataString read FRequestCountText;

    property OnUpdateRequest: TNotifyEvent read FOnUpdateRequest write FOnUpdateRequest;
  end;

implementation

{ TTabRequest }

procedure TTabRequest.CursorMessage(RegionMessage: TRegionMessage; x,
  y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  inherited;

end;

procedure TTabRequest.DoCreate;
begin
  inherited;
  FRequestCount := -1;
  SetRequestCount(1);
  ColorTheme := ct2;
  TabColor := Skin.ButtonRequestColor[0];
end;

procedure TTabRequest.EventUpdateRequest;
begin
  if Assigned(FOnUpdateRequest) then
    FOnUpdateRequest(Self);
end;

function TTabRequest.GetHeight: Integer;
begin
  Result := 50;
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

  TextHeader := ConvertTextToForm(FRequestCountText);
end;

end.
