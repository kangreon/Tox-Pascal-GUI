unit Splitter;

interface

uses
  Classes, Controls, Types, ExtCtrls, Graphics, Math;

type
  TProcSetWidth = procedure(Sender: TObject; NewWidth: Integer) of object;
  TSplitterEx = class(TGraphicControl)
  private
    FControl: TControl;
    FDownPos: TPoint;
    FIsMouseDown: Boolean;
    FIsSetWidth: Boolean;
    FNewWidth: Integer;
    FOnSetWidth: TProcSetWidth;

    function GetControlMinWidth(Control: TControl): Integer;
  protected
    procedure DoSeek(Seek: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ControlResize: TControl read FControl write FControl;

    property OnSetWidth: TProcSetWidth read FOnSetWidth write FOnSetWidth;
  end;


implementation

{ TSplitter }

constructor TSplitterEx.Create(AOwner: TComponent);
begin
  inherited;
  Constraints.MaxWidth := 5;
  Constraints.MinWidth := 5;
  Width := 5;
  Align := alLeft;
  Cursor := crHSplit;
end;

destructor TSplitterEx.Destroy;
begin

  inherited;
end;

function TSplitterEx.GetControlMinWidth(Control: TControl): Integer;
var
  w, c, i: Integer;
begin
  w := Control.Constraints.MinWidth;

  c := Control.ComponentCount;
  for i := 0 to c - 1 do
  begin
    if Control.Components[i] is TControl then
      w := Max(w, GetControlMinWidth(TControl(Control.Components[i])));
  end;

  Result := w;
end;

procedure TSplitterEx.DoSeek(Seek: Integer);
var
  NewWidth, OldWidth: Integer;
  MinWidth: Integer;
begin
  if Assigned(FControl) then
  begin
    OldWidth := FControl.Width;

    NewWidth := FControl.Width + Seek;
    MinWidth := GetControlMinWidth(FControl);

    if NewWidth < MinWidth then
      NewWidth := MinWidth;

    if NewWidth <> OldWidth then
    begin
      FIsSetWidth := True;
      FNewWidth := NewWidth;

      FControl.Width := NewWidth;
      Update;
    end;
  end;
end;

procedure TSplitterEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FDownPos := Point(X, Y);
  FIsMouseDown := Button = TMouseButton.mbLeft;
  FIsSetWidth := False;
end;

procedure TSplitterEx.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Seek: Integer;
begin
  inherited;
  if FIsMouseDown then
  begin
    Seek := X - FDownPos.X;
    if Seek <> 0 then
    begin
      //Update;
      DoSeek(Seek);
    end;
  end;
end;

procedure TSplitterEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    FIsMouseDown := False;

    if FIsSetWidth then
    begin
      if Assigned(FOnSetWidth) then
        FOnSetWidth(Self, FNewWidth);
    end;
  end;
end;

procedure TSplitterEx.Paint;
begin
  inherited Paint;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Bounds(0, 0, ClientWidth, ClientHeight));
end;

end.
