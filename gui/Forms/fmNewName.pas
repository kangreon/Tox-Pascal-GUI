//  fmNewName.pas
//
//  Временная форма изменения имени пользователя и статуса
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit fmNewName;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  SysUtils, Classes, Forms, StdCtrls, Controls, libtox;

type
  TFormMessageType = (fmtChangeName, fmtChangeStatus);

  { TFormNewName }

  TFormNewName = class(TForm)
    labTitle: TLabel;
    edName: TEdit;
    btnChange: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FFormMessageType: TFormMessageType;
    FModalResult: Integer;
    FNewName: string;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent; FormMessageType: TFormMessageType); reintroduce;

    procedure InsertCaptions;
    property NewName: string read FNewName;
    property ModalResult: Integer read FModalResult write FModalResult;
  end;

implementation

{$R *.dfm}

function InsertTextUnicode(Text: string): string;
begin
  Result := {$IFDEF FPC}UTF8Encode{$ENDIF}(Text);
end;

constructor TFormNewName.Create(AOwner: TComponent;
  FormMessageType: TFormMessageType);
begin
  inherited Create(AOwner);
  FFormMessageType := FormMessageType;
  ModalResult := 0;

  InsertCaptions;
end;


{*  Событие на нажатие кнопки. Отмена введения нового имени
 *}
procedure TFormNewName.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{*  Нажатие на кнопку изменения имени. Проверяет введенное пользователем
 *  имя и выводит описание ошибки в случае ее возникновения.
 *  В случае успешного введения имени, закрывается окно
 *}
procedure TFormNewName.btnChangeClick(Sender: TObject);
var
  Text: string;
begin
  FNewName := Trim(edName.Text);
  edName.Text := FNewName;

  if (FNewName = '') and (FFormMessageType = fmtChangeName) then
  begin
    MessageBox(Handle, 'Вы не ввели имя пользователя', PChar(Caption), MB_ICONERROR);
  end
  else if Length(FNewName) > TOX_MAX_NAME_LENGTH then
  begin
    case FFormMessageType of
      fmtChangeName:
        Text := InsertTextUnicode('Имя пользователя не может быть таким длинным.');

      fmtChangeStatus:
        Text := InsertTextUnicode('Статус не может быть таким длинным.');
    end;

    MessageBox(Handle, PChar(Text), PChar(Caption), MB_ICONERROR);
  end
  else
  begin
    ModalResult := 1;
    Close;
  end;
end;

procedure TFormNewName.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  edName.MaxLength := TOX_MAX_NAME_LENGTH;
end;

{*  Событие на нажатие клавиши на клавиатуре.
 *  Закрывает окно если была нажата клавиша Escape
 *}
procedure TFormNewName.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

{*  Добавление или изменение текста компонентов в зависимости
 *  от выбранного языка приложений
 *}
procedure TFormNewName.InsertCaptions;
begin
  // TODO: Добавить источник слов
  case FFormMessageType of
    fmtChangeName:
      begin
        Caption := InsertTextUnicode('Новое имя');
        labTitle.Caption := InsertTextUnicode('Введите новое имя:');
      end;
    fmtChangeStatus:
      begin
        Caption := InsertTextUnicode('Новый статус');
        labTitle.Caption := InsertTextUnicode('Введите новый статус:');
      end;
  end;

  btnChange.Caption := InsertTextUnicode('Изменить');
  btnCancel.Caption := InsertTextUnicode('Отмена');
end;

end.
