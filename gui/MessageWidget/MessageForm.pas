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
  Classes, Controls, StdCtrls, ButtonActive, Forms, StringUtils, SysUtils;

type
  TProcSendText = procedure(Sender: TObject; const Text: DataString) of object;

  TMessageForm = class(TCustomControl)
  private
    FTextForm: TMemo;
    FButtonSend: TButtonActive;
    FOnSendText: TProcSendText;
    FFormHandle: THandle;
    procedure TextFormKeyPress(Sender: TObject; var Key: Char);
    procedure TextFormEnter(Sender: TObject);
    procedure TextFormExit(Sender: TObject);
    procedure LockTextForm;
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FormHandle: THandle read FFormHandle;
    property OnSendText: TProcSendText read FOnSendText write FOnSendText;
  end;

implementation

{ TMessageForm }

constructor TMessageForm.Create(AOwner: TComponent);
begin
  inherited;
  FTextForm := TMemo.Create(Self);
  FTextForm.Align := alClient;
  FTextForm.BorderStyle := bsNone;
  FTextForm.OnKeyPress := TextFormKeyPress;
  FTextForm.OnEnter := TextFormEnter;
  FTextForm.OnExit := TextFormExit;
  LockTextForm;

  FTextForm.AlignWithMargins := True;
  with FTextForm.Margins do
  begin
    Left := 2;
    Top := 2;
    Right := 2;
    Bottom := 2;
  end;


  FButtonSend := TButtonActive.Create(Self);
  FButtonSend.Align := alRight;
end;

procedure TMessageForm.CreateWnd;
begin
  inherited;
  Constraints.MinHeight := 40;
  ClientHeight := 40;

  FTextForm.Parent := Self;
  FTextForm.Color := Color;
  FFormHandle := FTextForm.Handle;

  FButtonSend.Parent := Self;
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
  FTextForm.Text := 'Введите ваше сообщение здесь...';
end;

end.
