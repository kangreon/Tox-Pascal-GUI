//  toxcore.pas
//
//  Виджет, содержащий форму отправки сообщения собседнику.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit MessageForm;

interface

uses
  {$I tox-uses.inc} Messages,
  Classes, Controls, StdCtrls, ButtonActive, Forms, StringUtils, SysUtils;

type
  TProcSendText = procedure(Sender: TObject; const Text: DataString) of object;

  TMessageForm = class(TGraphicControl)
  private
    FTextForm: TMemo;
    FTextFormLineHeight: Integer;
    FTextFormShowScroll: Boolean;
    FButtonSend: TButtonActive;
    FOnSendText: TProcSendText;
    FFormHandle: THandle;
    FVisible: Boolean;
    procedure TextFormKeyPress(Sender: TObject; var Key: Char);
    procedure TextFormEnter(Sender: TObject);
    procedure TextFormExit(Sender: TObject);
    procedure LockTextForm;
    procedure TextFormChange(Sender: TObject);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FormHandle: THandle read FFormHandle;
    property Visible: Boolean read FVisible write SetVisible;

    property OnSendText: TProcSendText read FOnSendText write FOnSendText;
  end;

implementation

{ TMessageForm }

constructor TMessageForm.Create(AOwner: TComponent);
begin
  inherited;
  FTextForm := TMemo.Create(Self);
  FTextForm.BorderStyle := TFormBorderStyle.bsNone;
  FTextForm.ScrollBars := ssNone;
  FTextForm.WordWrap := True;
  FTextForm.DoubleBuffered := True;
  FTextForm.OnKeyPress := TextFormKeyPress;
  FTextForm.OnEnter := TextFormEnter;
  FTextForm.OnExit := TextFormExit;
  FTextForm.OnChange := TextFormChange;
  FTextForm.Visible := False;
  FTextFormShowScroll := False;
  LockTextForm;

  FButtonSend := TButtonActive.Create(Self);
  FButtonSend.Align := alRight;
end;

destructor TMessageForm.Destroy;
begin
  FTextForm.Free;
  FButtonSend.Free;
  inherited;
end;

procedure TMessageForm.Paint;
begin
  inherited;
  // Рисование бордюры вокруг компонента
  Canvas.Pen.Color := $d3d3d3;
  Canvas.Pen.Width := 2;

  Canvas.MoveTo(1, 1);
  Canvas.LineTo(ClientWidth - 1, 1);
  Canvas.LineTo(ClientWidth - 1, ClientHeight - 1);
  Canvas.LineTo(1, ClientHeight - 1);
  Canvas.LineTo(1, 0);
end;

procedure TMessageForm.Resize;
var
  FormRect: TRect;
begin
  inherited;
  FormRect := BoundsRect;
  //TODO: Исправить после устранения всех проблем с TMemo
  FormRect.Left := FormRect.Left + 2 + Parent.Left;
  FormRect.Top := FormRect.Top + 2;
  FormRect.Right := FormRect.Right - 2 + Parent.Left;
  FormRect.Bottom := FormRect.Bottom - 2;

  FTextForm.BoundsRect := FormRect;
end;

procedure TMessageForm.SetParent(AParent: TWinControl);
begin
  inherited;
  if not Assigned(AParent) then
    Exit;

  Constraints.MinHeight := 40;
  ClientHeight := 80;

  FTextForm.Parent := AParent.Parent;
  FTextForm.BringToFront;
  Canvas.Font.Assign(FTextForm.Font);
  FTextFormLineHeight := Canvas.TextHeight(' ');

  FTextForm.Color := Color;
  FFormHandle := FTextForm.Handle;

//  FButtonSend.Parent := AParent;
end;

procedure TMessageForm.SetVisible(const Value: Boolean);
begin
  FVisible := Value;

  inherited Visible := Value;

  FTextForm.Visible := Value;
end;

{ *  Показывает и прячет полосу прокрутки когда это необходимо.
  * }
procedure TMessageForm.TextFormChange(Sender: TObject);
var
  TextLines, TextHeight: Integer;
begin
  if not Assigned(Parent) then
    Exit;

  TextLines := FTextForm.Lines.Count;
  TextHeight := FTextFormLineHeight * TextLines;
  if FTextForm.Height >= TextHeight then
  begin
    if FTextFormShowScroll then
      FTextForm.ScrollBars := ssNone;

    FTextFormShowScroll := False;
  end
  else
  begin
    if not FTextFormShowScroll then
      FTextForm.ScrollBars := ssVertical;

    FTextFormShowScroll := True;
  end;
end;

{ * Ожидание нажатия клавиши Enter для очистки формы и отправки тектста.
  * }
procedure TMessageForm.TextFormKeyPress(Sender: TObject; var Key: Char);
var
  Text: DataString;
begin
  if Key = #13 then
  begin
    Key := #0;

    Text := Trim(TMemo(Sender).Text);
    TMemo(Sender).Clear;

    if (Length(Text) > 0) and Assigned(FOnSendText) then
    begin
      FOnSendText(Self, Text);
    end;
  end;
end;

procedure TMessageForm.TextFormEnter(Sender: TObject);
begin
  if FTextForm.Tag = 1 then
  begin
    FTextForm.Tag := 0;

    FTextForm.Text := '';
    FTextForm.Font.Color := $000000;
  end;
end;

procedure SetMargins(Memo: HWND);
{$IFDEF FPC}
const
  EM_GETRECT             = $00B2;
  EM_SETRECT             = $00B3;
{$ENDIF}
var
  Rect: TRect;
begin
  SendMessage(Memo, EM_GETRECT, 0, Longint(@Rect));
  Rect.Right := Rect.Right - GetSystemMetrics(SM_CXHSCROLL);
  SendMessage(Memo, EM_SETRECT, 0, Longint(@Rect));
end;

procedure TMessageForm.TextFormExit(Sender: TObject);
var
  Text: string;
begin
  Text := Trim(FTextForm.Text);

  if Length(Text) = 0 then
  begin
    LockTextForm;
  end;
end;

procedure TMessageForm.LockTextForm;
begin
  FTextForm.Tag := 1;

  FTextForm.Font.Color := $555555;
  FTextForm.Text := {$IFDEF FPC}UTF8Encode{$ENDIF}('Введите ваше сообщение здесь...');
end;

end.
