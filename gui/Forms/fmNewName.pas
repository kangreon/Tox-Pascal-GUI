unit fmNewName;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
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
    FNewName: string;
    { Private declarations }
  public
    procedure InsertCaptions;
    property NewName: string read FNewName;
  end;

implementation

{$R *.dfm}

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
begin
  FNewName := Trim(edName.Text);
  edName.Text := FNewName;

  if FNewName = '' then
  begin
    MessageBox(Handle, 'Вы не ввели имя пользователя', PWideChar(Caption), MB_ICONERROR);
  end
  else
    Close;
end;

procedure TFormNewName.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  InsertCaptions;
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
  Caption := 'Новое имя';
  labTitle.Caption := 'Введите новое имя:';
  btnChange.Caption := 'Изменить';
  btnCancel.Caption := 'Отмена';
end;

end.
