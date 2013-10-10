//  ActiveRegion.pas
//
//  Отлавливает различные события мыши в указанном регионе формы
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ActiveRegion;

interface
  {$I tox.inc}

uses
{$I tox-uses.inc}
  Controls, Classes, Messages;

type
  { Перечисление событий, создаваемых классом }
  TRegionMessage = (rmMouseEnter, rmMouseLeave, rmMouseMove, rmMouseDown,
    rmMouseUp, rmMouseClick, rmMouseDblClick);

  { Состоянние нажатия на регион мышью }
  TDownState = (dsNone, dsActive, dsDown);

  TProcCursorMessage = procedure(Sender: TObject; RegionMessage: TRegionMessage;
    const x, y: Integer; Button: TMouseButton; Shift: TShiftState) of object;

  TActiveRegion = class(TGraphicControl)
  private
    FOnCursorMessage: TProcCursorMessage;
    procedure EventCursorMessage(RegionMessage: TRegionMessage;
      const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
  protected
    procedure CursorMessage(RegionMessage: TRegionMessage; const x, y: Integer;
      Button: TMouseButton; Shift: TShiftState); virtual;
  protected
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;
  public
    procedure SetRect(Rect: TRect);

    property OnCursorMessage: TProcCursorMessage read FOnCursorMessage write FOnCursorMessage;
  end;

{$IFDEF FPC}
const
  CM_BASE                   = $B000;
  CM_MOUSEENTER             = CM_BASE + 19;
  CM_MOUSELEAVE             = CM_BASE + 20;
{$ENDIF}

implementation

{ TActiveRegion }

procedure TActiveRegion.CursorMessage(RegionMessage: TRegionMessage; const x,
  y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
end;

procedure TActiveRegion.Click;
var
  Point: TPoint;
begin
  inherited;
  Point.x := 0;
  Point.y := 0;

  if GetCursorPos(Point) then
  begin
    Point := ScreenToClient(Point);
  end
  else
  begin
    Point.X := 0;
    Point.Y := 0;
  end;

  EventCursorMessage(rmMouseClick, Point.X, Point.Y, mbLeft, []);
end;

procedure TActiveRegion.DblClick;
begin
  inherited;
  EventCursorMessage(rmMouseDblClick, 0, 0, mbLeft, []);
end;

procedure TActiveRegion.EventCursorMessage(RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  CursorMessage(RegionMessage, x, y, Button, Shift);
  if Assigned(FOnCursorMessage) then
    FOnCursorMessage(Self, RegionMessage, x, y, Button, Shift);
end;

procedure TActiveRegion.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  EventCursorMessage(rmMouseDown, X, Y, Button, Shift);
end;

procedure TActiveRegion.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EventCursorMessage(rmMouseMove, X, Y, mbLeft, Shift);
end;

procedure TActiveRegion.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  EventCursorMessage(rmMouseUp, X, Y, Button, Shift);
end;

procedure TActiveRegion.SetRect(Rect: TRect);
begin
  Left := Rect.Left;
  Top := Rect.Top;
  Width := Rect.Right - Left;
  Height := Rect.Bottom - Top;
end;

{*  Отловка событий входа и выхода курсора на выделенный регион
 *}
procedure TActiveRegion.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of

    CM_MOUSEENTER:
      begin
        EventCursorMessage(rmMouseEnter, 0, 0, mbLeft, []);
      end;

    CM_MOUSELEAVE:
      begin
        EventCursorMessage(rmMouseLeave, 0, 0, mbLeft, []);
      end;
  end;
end;

end.
