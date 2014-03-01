// TabSelectList.pas
//
// Вкладка выбора фильтра для списка пользователей
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit TabSelectList;

interface

uses
  SkinTabList, Classes, Graphics, Controls, StringUtils, Types, ImageUtils,
  ActiveRegion, Math, UserListSelect, TabView;

type
  TProcSelectItem = procedure(Sender: TObject; Index: Integer) of object;

  TTabSelectList = class(TTabView)
  private
    FContentHeight: Integer;
  protected
    procedure DoCreate; override;
  public
    procedure CursorMessage(RegionMessage: TRegionMessage; x, y: Integer;
      Button: TMouseButton; Shift: TShiftState); override;
    function GetHeight: Integer; override;

  end;

implementation

{ TTabSelectList }


{ TTabSelectList }

procedure TTabSelectList.CursorMessage(RegionMessage: TRegionMessage; x,
  y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  inherited;

end;

procedure TTabSelectList.DoCreate;
begin
  inherited;
  FContentHeight := 10;
  TabColor := Skin.ButtonListColor[0];
  TextHeader := ConvertTextToForm('Друзья');
end;

function TTabSelectList.GetHeight: Integer;
begin
  Result := FContentHeight;
end;

end.
