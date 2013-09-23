unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  fmUserAdd = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: fmUserAdd;

implementation

{$R *.dfm}

procedure fmUserAdd.FormCreate(Sender: TObject);
begin
  SetWindowTextW(Handle, PWideChar('Добавление пользователя'));
end;

end.
