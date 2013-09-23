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
    property OnCursorMessage: TProcCursorMessage read FOnCursorMessage write FOnCursorMessage;
  end;

implementation

{ TActiveRegion }

procedure TActiveRegion.CursorMessage(RegionMessage: TRegionMessage; const x,
  y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
end;

procedure TActiveRegion.Click;
begin
  inherited;
  EventCursorMessage(rmMouseClick, 0, 0, mbLeft, []);
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

{*  Отловка событий входа и выхода курсора на выделенный регион
 *}
procedure TActiveRegion.WndProc(var Message: TMessage);
{$IFDEF FPC}
const
  CM_BASE                   = $B000;
  CM_MOUSEENTER             = CM_BASE + 19;
  CM_MOUSELEAVE             = CM_BASE + 20;
{$ENDIF}
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
