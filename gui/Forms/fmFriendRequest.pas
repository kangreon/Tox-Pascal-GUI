//  ActiveRegion.pas
//
//  Окно, уведомляющее о запросе добавления в друзья
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit fmFriendRequest;

interface
  {$I tox.inc}

uses
{$I tox-uses.inc}
  Controls, Forms, StdCtrls, ClientAddress, StringUtils, Classes;

type
  TReturnMessage = (rtReject, rtAdd, rtIgnore);

  TFormFriendRequest  = class(TForm)
    gbFriendRequest: TGroupBox;
    labFromUser: TLabel;
    edFromUser: TEdit;
    labWithMessage: TLabel;
    memWithMessage: TMemo;
    btnAdd: TButton;
    btnReject: TButton;
    btnIgnore: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnIgnoreClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRejectClick(Sender: TObject);
  private
    FAddress: TFriendAddress;
    FReturnMessage: TReturnMessage;
  public
    constructor Create(AOwner: TComponent; ClientAddress: TFriendAddress;
      HelloMessage: DataString); reintroduce;

    procedure InsertCaptions;

    property ClientAddress: TFriendAddress read FAddress;
    property ReturnMessage: TReturnMessage read FReturnMessage;
  end;

implementation

{$R *.dfm}

procedure TFormFriendRequest.btnAddClick(Sender: TObject);
begin
  FReturnMessage := rtAdd;
  Close;
end;

procedure TFormFriendRequest.btnIgnoreClick(Sender: TObject);
begin
  FReturnMessage := rtIgnore;
  Close;
end;

procedure TFormFriendRequest.btnRejectClick(Sender: TObject);
begin
  FReturnMessage := rtReject;
  Close;
end;

constructor TFormFriendRequest.Create(AOwner: TComponent;
  ClientAddress: TFriendAddress; HelloMessage: DataString);
begin
  inherited Create(AOwner);
  FAddress := ClientAddress;

  edFromUser.Text := FAddress.DataHex;
  memWithMessage.Text := HelloMessage;
  FormStyle := fsStayOnTop;
end;

procedure TFormFriendRequest.FormCreate(Sender: TObject);
begin
  edFromUser.ReadOnly := True;
  memWithMessage.ReadOnly := True;
  memWithMessage.Text := '';
  btnAdd.Default := True;
  btnIgnore.Visible := False;

  InsertCaptions;
  BorderStyle := bsDialog;

  FReturnMessage := rtReject;
end;

function InsertTextUnicode(Text: string): string;
begin
  Result := {$IFDEF FPC}UTF8Encode{$ENDIF}(Text);
end;

procedure TFormFriendRequest.InsertCaptions;
begin
  Caption := InsertTextUnicode('Запрос на добавления в друзья');
  gbFriendRequest.Caption := InsertTextUnicode('Запрос на добавления в друзья:');
  labFromUser.Caption := InsertTextUnicode('От пользователя:');
  btnAdd.Caption := InsertTextUnicode('Добавить');
  btnReject.Caption := InsertTextUnicode('Откланить');
  btnIgnore.Caption := InsertTextUnicode('Игнорировать');
  labWithMessage.Caption := InsertTextUnicode('С текстом:');
end;

end.
