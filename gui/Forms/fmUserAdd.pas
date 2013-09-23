unit fmUserAdd;

interface
{$I tox.inc}

uses
  {$I tox-uses.inc}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormUserAdd = class(TForm)
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormUserAdd.FormCreate(Sender: TObject);
begin
  Caption := 'Добавление нового пользователя';
end;

end.
