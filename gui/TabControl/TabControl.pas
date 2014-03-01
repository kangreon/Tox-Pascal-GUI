// TabControl.pas
//
// Виджет переключения вкладок:
// * вкладка выбора фильтра отображения списка пользователей
// * вкладка уведобмления о новых запросах на добавления в список друзей
//
// The MIT License (MIT)
//
// Copyright (c) 2013 Dmitry
//
unit TabControl;

interface

uses
  Classes, TabView, Controls, Math, ActiveRegion, Windows;

type
  TTabControl = class(TCustomControl)
  private
    FActive: TActiveRegion;
    FIsCreateWnd: Boolean;
    FOwner: TWinControl;
    FItems: array of TTabView;
    procedure RepaintAll;
    procedure TabChangeHeader(Sender: TObject);
    procedure ActiveCursor(Sender: TObject; RegionMessage: TRegionMessage;
      const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TWinControl); reintroduce;

    procedure Add(Tab: TTabView);

    property Color;
  end;

implementation

{ TTabControl }

constructor TTabControl.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  DoubleBuffered := True;
  ControlStyle := ControlStyle - [csParentBackground];
  FIsCreateWnd := False;
  Align := alTop;
  Height := 1;
end;

procedure TTabControl.CreateWnd;
var
  i: Integer;
begin
  inherited;

  for i := Low(FItems) to High(FItems) do
  begin
    FItems[i].SetParent(Self);
  end;

  FActive := TActiveRegion.Create(Self);
  FActive.SetRect(ClientRect);
  FActive.Parent := Self;
  FActive.OnCursorMessage := ActiveCursor;
  FActive.BringToFront;

  FIsCreateWnd := True;

  RepaintAll;
end;

procedure TTabControl.Paint;
begin
  inherited;

end;

procedure TTabControl.ActiveCursor(Sender: TObject; RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
var
  i: Integer;
begin
  for i := Low(FItems) to High(FItems) do
  begin
    FItems[i].CursorMessage(RegionMessage, x, y, Button, Shift);
  end;
end;

procedure TTabControl.Add(Tab: TTabView);
var
  Index: Integer;
begin
  Index := Length(FItems);
  SetLength(FItems, Index + 1);
  FItems[Index] := Tab;

  Tab.OnChangeHeader := TabChangeHeader;
  Tab.Left := 0;
  Tab.Top := 0;
  Tab.Width := ClientWidth;
  Tab.Height := Tab.TabHeight;

  if FIsCreateWnd then
  begin
    Tab.SetParent(Self);
  end;

  Height := Max(Height, Tab.TabHeight);
  RepaintAll;
  FActive.BringToFront;
end;

procedure TTabControl.RepaintAll;
var
  i, c: Integer;
  Tab: TTabView;
  Left, Right: Integer;
  TabPosition: TTabPosition;
  TabWidth: Integer;
  kof: Integer;
  MaxHeight: Integer;
begin
  Left := 0;
  Right := Width;

  c := Length(FItems);
  if c = 0 then
  begin
    Invalidate;
    Exit;
  end;

  MaxHeight := 0;

  kof := Round((Right - Left) / c);
  for i := 0 to c - 1 do
  begin
    Tab := FItems[i];
    MaxHeight := Max(MaxHeight, Tab.TabHeight);

    TabWidth := kof;
    if (i = c - 1) and (i <> 0) then
    begin
      TabPosition := tpLast;
      TabWidth := Right - Left;
    end
    else if i = 0 then
    begin
      TabPosition := tpFirst;
    end
    else
    begin
      TabPosition := tpCenter;
    end;

    Tab.Repaint(Left, TabWidth, TabPosition);
    Left := Left + Tab.HeaderWidth;
  end;

  Height := MaxHeight;

  Invalidate;
end;

procedure TTabControl.Resize;
var
  i: Integer;
begin
  inherited;

  if Assigned(FActive) then
  begin
    FActive.SetRect(ClientRect);
  end;

  for i := Low(FItems) to High(FItems) do
  begin
    FItems[i].Width := ClientWidth;
    FItems[i].Height := ClientHeight;
  end;

  RepaintAll;
end;

procedure TTabControl.TabChangeHeader(Sender: TObject);
begin
  RepaintAll;
end;

end.
