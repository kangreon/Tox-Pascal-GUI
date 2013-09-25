//  fmUserAdd.pas
//
//  Форма добавления нового пользователя в друзья. Происходит проверка
//  введенных данных и вывода сообшения пользователю в случае возникновения
//  ошибки
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit fmUserAdd;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ClientAddress, ToxCore, libtox;

type
  TFormUserAdd = class(TForm)
    gbFriendAdd: TGroupBox;
    edFrienAddress: TEdit;
    labFriendAddress: TLabel;
    labMessage: TLabel;
    memMessage: TMemo;
    btnCancel: TButton;
    btnAddFriend: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAddFriendClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FToxCore: TToxCore;
    procedure InsertCaptions;
    procedure Error(Text: string);
  public
    property Tox: TToxCore read FToxCore write FToxCore;
  end;

implementation

{$R *.dfm}

{*  Выводит сообщение об ошибке добавления нового пользователя
 *
 *  Text - выводимое сообщение
 *}
procedure TFormUserAdd.Error(Text: string);
var
  TextMessage: string;
begin
  TextMessage := Format('Ошибка добавления друга: %s.', [Text]);
  MessageBox(Handle, PChar(TextMessage), 'Добавление друга', MB_OK or MB_ICONERROR);
end;

{*  Событие нажатия на кнопку добавления нового пользователя.
 *  Добавляет нового пользователя и в случае ошибки показывает сообщение
 *  с подробным описанием проблемы.
 *}
procedure TFormUserAdd.btnAddFriendClick(Sender: TObject);
var
  Address: TClientAddress;
  Text: string;
  FriendNumber: Integer;
  ToxError: TToxFaerr;
begin
  Text := '';
  Address := TClientAddress.Create;
  try
    Address.DataHex := edFrienAddress.Text;
    if Address.ValidAddress then
    begin
      ToxError := FToxCore.AddFriend(Address, memMessage.Text, FriendNumber);

      case ToxError of
        tfTooLong, tfBadChecksum:
            Text := 'неверный формат адреса';

        tfNoMessage:
            Text := 'отсутствует сообщение';

        tfOwnKey:
            Text := 'не используйте собственый адрес';

        tfAlReadySend:
            Text := ' ';

        tfUnknown:
            Text := 'неизвестная ошибка';

        tfSetNewNospam:
            Text := ' ';

        tfNoMem:
            Text := ' ';
      end;
    end
    else
    begin
      Text := 'неверный формат адреса';
    end;
  finally
    Address.Free;
  end;

  if Text <> '' then
  begin
    Error(Text);
  end
  else
    Close;
end;

procedure TFormUserAdd.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormUserAdd.FormCreate(Sender: TObject);
begin
  InsertCaptions;
  {$IFDEF NEW_DELPHI}
  gbFriendAdd.ParentBackground := False;
  {$ENDIF}
  gbFriendAdd.DoubleBuffered := True;
end;

{*  Событие на нажатие клавиши на клавиатуре.
 *  Закрывает окно если была нажата клавиша Escape
 *}
procedure TFormUserAdd.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

{*  Изменение размеров всех компонентов на форме
 *
 *}
procedure TFormUserAdd.FormResize(Sender: TObject);
const
  FORM_PADDING    = 8;
  CONTROL_MARGIN  = 5;

var
  W, H: Integer;

begin
  W := ClientWidth;
  H := ClientHeight;

  btnAddFriend.Left := W - btnAddFriend.Width - FORM_PADDING;
  btnCancel.Left := btnAddFriend.Left - btnCancel.Width - CONTROL_MARGIN;
  btnAddFriend.Top := H - btnAddFriend.Height - FORM_PADDING;
  btnCancel.Top := btnAddFriend.Top;

  gbFriendAdd.Left := FORM_PADDING;
  gbFriendAdd.Top := FORM_PADDING;
  gbFriendAdd.Width := W - 2 * FORM_PADDING;
  gbFriendAdd.Height := H - FORM_PADDING - 40;

  edFrienAddress.Width := gbFriendAdd.Width - edFrienAddress.Left - CONTROL_MARGIN * 3;
  memMessage.Width := gbFriendAdd.Width - memMessage.Left - CONTROL_MARGIN * 3;
  memMessage.Height := gbFriendAdd.Height - memMessage.Top - CONTROL_MARGIN * 3;
end;

function InsertTextUnicode(Text: string): string;
begin
  Result := {$IFDEF FPC}UTF8Encode{$ENDIF}(Text);
end;

{*  Добавление или изменение текста компонентов в зависимости
 *  от выбранного языка приложений
 *}
procedure TFormUserAdd.InsertCaptions;
begin
  // TODO: Добавить источник слов
  Self.Caption := InsertTextUnicode('Добавление друга');
  gbFriendAdd.Caption := InsertTextUnicode('Добавление друга:');
  labFriendAddress.Caption := InsertTextUnicode('Адрес друга:');
  labMessage.Caption := InsertTextUnicode('Сообщение:');
  memMessage.Text := InsertTextUnicode('Insert default hello message');

  btnAddFriend.Caption := InsertTextUnicode('Добавить');
  btnCancel.Caption := InsertTextUnicode('Отмена');
end;

end.
