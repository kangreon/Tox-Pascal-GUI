//  fmProfileSelect.pas
//
//  Окно выбора профиля для чата
//
//  The MIT License (MIT)
//
//  Copyright (c) 2014 Dmitry
//
unit fmProfileSelect;

interface

uses
  Messages, SysUtils, Classes, Controls, Forms, StringUtils, SkinProfileSelect,
  Graphics, Types, StdCtrls, ExtCtrls, Windows, SkinTypes, ControlUtils,
  ProfileLoader, Settings;

type
  TLeftButton = (lbExistUser, lbNewUser);

  TButtonInfo = record
    x, y: Integer;
    Widht, Height: Integer;

    State: TMouseState;
    Down: Boolean;
    Title: string;

    function Containt(ValueX, ValueY: Integer): Boolean;
  end;

  TFormProfileSelect = class(TForm)
    pnSelectProfile: TPanel;
    labUserName: TLabel;
    cbNickName: TComboBox;
    labPassword: TLabel;
    edPassword: TEdit;
    Panel1: TPanel;
    btnLogin: TButton;
    pnCreateAccount: TPanel;
    labCreateNickname: TLabel;
    edCreateNickname: TEdit;
    labCreatePassword: TLabel;
    edCreatePassword: TEdit;
    labConfirmPassword: TLabel;
    edConfirmPassword: TEdit;
    Panel3: TPanel;
    btnCeateAccount: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCeateAccountClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
  private
    FButtonInfo: array[TLeftButton] of TButtonInfo;
    FSkin: TSkinProfileSelect;
    FSelectedButton: TLeftButton;
    FIsSelectedProfile: Boolean;
    FProfile: TProfileLoader;
    FOnCloseEx: TNotifyEvent;
    FSettings: TSettings;
    procedure InitProfile;
    procedure SetSelectButton(const Value: TLeftButton);
    procedure PainButtons;
    procedure DrawButton(Index: TLeftButton);
    procedure UpdateButtonInfo(ButtonIndex: TLeftButton; X, Y: Integer;
      State: TMouseState);
    procedure SetSettings(const Value: TSettings);
    procedure ErrorCreateAccount(Value: DataString);
    procedure ErrorSelectAccount(Value: DataString);
  public
    procedure InsertSkin(Skin: TSkinProfileSelect);
    procedure UpdateTranslate;

    property IsSelectedProfile: Boolean read FIsSelectedProfile;
    property Profile: TProfileLoader read FProfile;
    property SelectButton: TLeftButton read FSelectedButton write SetSelectButton;

    property Settings: TSettings read FSettings write SetSettings;

    property OnCloseEx: TNotifyEvent read FOnCloseEx write FOnCloseEx;
  end;

implementation

{$R *.dfm}

procedure TFormProfileSelect.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FOnCloseEx) then
  begin
    FOnCloseEx(Self);
  end;
end;

procedure TFormProfileSelect.FormCreate(Sender: TObject);
begin
  Position := poScreenCenter;
  ClientWidth := 513;
  ClientHeight := 233;
  DoubleBuffered := True;
  FIsSelectedProfile := False;

  UpdateTranslate;

  pnSelectProfile.Align := alRight;
  pnCreateAccount.Align := alRight;
  pnSelectProfile.Width := 250;
  pnCreateAccount.Width := 250;

  // By default: lbExistUser
  SelectButton := lbExistUser;

  SetMargins(pnSelectProfile, 25, 10, 10, 10);
  SetMarginsDefault(labUserName);
  SetMarginsDefault(cbNickName);
  SetMargins(labPassword, 3, 10, 3, 3);
  SetMarginsDefault(edPassword);
  SetMarginsDefault(btnLogin);

  SetMargins(pnCreateAccount, 25, 10, 10, 10);
  SetMarginsDefault(labCreateNickname);
  SetMarginsDefault(edCreateNickname);
  SetMargins(labCreatePassword, 3, 10, 3, 3);
  SetMarginsDefault(edCreatePassword);
  SetMargins(labConfirmPassword, 3, 10, 3, 3);
  SetMarginsDefault(edConfirmPassword);
  SetMarginsDefault(btnCeateAccount);
end;

procedure TFormProfileSelect.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if Button = mbLeft then
  begin
    for i := 0 to 1 do
    begin
      UpdateButtonInfo(TLeftButton(i), X, Y, msDown);
    end;
  end;
end;

procedure TFormProfileSelect.FormMouseLeave(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 1 do
  begin
    UpdateButtonInfo(TLeftButton(i), 0, 0, msNone);
  end;

  Cursor := crDefault;
end;

procedure TFormProfileSelect.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
begin
  for i := 0 to 1 do
  begin
    UpdateButtonInfo(TLeftButton(i), X, Y, msActive);
  end;
end;

procedure TFormProfileSelect.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  for i := 0 to 1 do
  begin
    UpdateButtonInfo(TLeftButton(i), X, Y, msActive);
  end;
end;

procedure TFormProfileSelect.UpdateButtonInfo(ButtonIndex: TLeftButton; X, Y: Integer;
  State: TMouseState);
begin
  if FButtonInfo[ButtonIndex].Containt(X, Y) then
  begin
    if FButtonInfo[ButtonIndex].State <> State then
    begin
      FButtonInfo[ButtonIndex].State := State;

      if State = msDown then
      begin
        SelectButton := ButtonIndex;
      end;

      Invalidate;
    end;
  end
  else
  begin
    if FButtonInfo[ButtonIndex].State <> msNone then
    begin
      FButtonInfo[ButtonIndex].State := msNone;
      Invalidate;
    end;
  end;

end;

procedure TFormProfileSelect.FormPaint(Sender: TObject);
var
  Value: TRect;

  ImageLeft, ImageTop: Integer;
begin
  Value :=  Self.ClientRect;
  Value.Right := FSkin.LeftBarWidth;

  Canvas.Brush.Color := FSkin.LeftBarColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Value);

  ImageLeft := (FSkin.LeftBarWidth - FSkin.LogoTox.Width) div 2;
  ImageTop := 23; //TODO: Переместить в скин

  Canvas.Draw(ImageLeft, ImageTop, FSkin.LogoTox);

  PainButtons;
end;

{ *  Вывод кнопок переключения
  * }
procedure TFormProfileSelect.PainButtons;
var
  DivLineTop: Integer;
  LineFrom, LineTo: Integer;
begin
  with FButtonInfo[lbExistUser] do
  begin
    x := 0;
    y := ClientHeight - FSkin.LeftButtonHeight * 2 - 2;
    Widht := FSkin.LeftBarWidth;
    Height := FSkin.LeftButtonHeight;
  end;

  with FButtonInfo[lbNewUser] do
  begin
    x := 0;
    y := ClientHeight - FSkin.LeftButtonHeight;
    Widht := FSkin.LeftBarWidth;
    Height := FSkin.LeftButtonHeight;
  end;

  DivLineTop := ClientHeight - FSkin.LeftButtonHeight - 2;
  LineFrom := FSkin.DivisionLineMargin;
  LineTo := FSkin.LeftBarWidth - FSkin.DivisionLineMargin;

  Canvas.Pen.Color := RGB(0, 0, 0);
  Canvas.MoveTo(LineFrom, DivLineTop);
  Canvas.LineTo(LineTo, DivLineTop);

  DivLineTop := DivLineTop + 1;

  Canvas.Pen.Color := RGB(66, 66, 66);
  Canvas.MoveTo(LineFrom, DivLineTop);
  Canvas.LineTo(LineTo, DivLineTop);

  DrawButton(lbExistUser);
  DrawButton(lbNewUser);
end;

procedure TFormProfileSelect.ErrorCreateAccount(Value: DataString);
begin
  MessageBoxW(Handle, PWideChar({ConvertTextToForm}(Value)),
    PWideChar({ConvertTextToForm}('Ошибка создания аккаунта')),
    MB_ICONERROR);
end;

procedure TFormProfileSelect.ErrorSelectAccount(Value: DataString);
begin
  MessageBoxW(Handle, PWideChar({ConvertTextToForm}(Value)),
    PWideChar({ConvertTextToForm}('Ошибка входа в аккаунт')),
    MB_ICONERROR);
end;

{ *  Валидация введенных данных для регистрации и регистрация
  * }
procedure TFormProfileSelect.btnCeateAccountClick(Sender: TObject);
var
  Nickname: string;
  Password, ConfPass: string;
  ErrorText: DataString;
begin
  Nickname := Trim(edCreateNickname.Text);
  Password := edCreatePassword.Text;
  ConfPass := edConfirmPassword.Text;

  if Nickname = '' then
  begin
    ErrorCreateAccount('Необходимо ввести имя пользователя.');
    Exit;
  end;

  if Password <> ConfPass then
  begin
    ErrorCreateAccount('Пароли не совпадают.');
    Exit;
  end;

  if FProfile.New(Nickname, Password) then
  begin
    FIsSelectedProfile := True;
    Close;
    Exit;
  end;

  case FProfile.Error of
    meNameExist:
      ErrorText := 'Профель с таким именем уже существует.';

    meHasBeenLoaded:
      ErrorText := 'Уже загружен другой профиль. Вначале следует выйти с активного профиля.';

    meOpenFile:
      ErrorText := 'Не удалось открыть файл с данными профиля. Подробное описание проблемы:'#13#10 +
        FProfile.ErrorInfo;

    meOpenFileLock:
      ErrorText := 'Не удалось открыть файл с данными профиля. Подробное описание проблемы:'#13#10 +
        FProfile.ErrorInfo;

    meLongSize:
      ErrorText := 'Файл с данными профиля имеет слишком большой размер для загрузки.';

    meReadError:
      ErrorText := 'Произошла ошибка во время чтения данных из файла-профиля пользователя.';

    meInitTox:
      ErrorText := 'Не удалось инициализировать библиотеку Tox.';

    meBackUpError:
      ErrorText := 'Произошла ошибка при создании резервной копии файла-профиля пользователя.'#13#10 +
        SysErrorMessage(GetLastError);
  else
    ErrorText := 'Неизвестная ошибка: ' + IntToStr(Integer(FProfile.Error));
  end;

  ErrorCreateAccount(ErrorText);
end;

procedure TFormProfileSelect.btnLoginClick(Sender: TObject);
var
  Nickname: string;
  Password: string;
  Index: Integer;
  ErrorText: string;
begin
  Nickname := cbNickName.Text;
  Password := edPassword.Text;
  Index := cbNickName.ItemIndex;

  if FProfile.Load(Index, Password) then
  begin
    FIsSelectedProfile := True;
    Close;
    Exit;
  end;

  case FProfile.Error of
    meSelectProfileNotFound:
      ErrorText := 'Профиль с выбранным именем не существует.';

    meHasBeenLoaded:
      ErrorText := 'Уже загружен другой профиль. Вначале следует выйти с активного профиля.';

    meOpenFile:
      ErrorText := 'Не удалось открыть файл с данными профиля. Подробное описание проблемы:'#13#10 +
        FProfile.ErrorInfo;

    meOpenFileLock:
      ErrorText := 'Не удалось открыть файл с данными профиля. Подробное описание проблемы:'#13#10 +
        FProfile.ErrorInfo;

    meLongSize:
      ErrorText := 'Файл с данными профиля имеет слишком большой размер для загрузки.';

    meReadError:
      ErrorText := 'Произошла ошибка во время чтения данных из файла-профиля пользователя.';

    meInitTox:
      ErrorText := 'Не удалось инициализировать библиотеку Tox.';

    meBackUpError:
      ErrorText := 'Произошла ошибка при создании резервной копии файла-профиля пользователя.'#13#10 +
        SysErrorMessage(GetLastError);

    mePassBad:
      ErrorText := 'Неверный пароль.';
  else
    ErrorText := 'Неизвестная ошибка: ' + IntToStr(Integer(FProfile.Error));
  end;

  ErrorSelectAccount(ErrorText);
end;

procedure TFormProfileSelect.DrawButton(Index: TLeftButton);
var
  ElementLeft, ElementTop: Integer;
  SelectionTop: Integer;
  Button: TButtonInfo;
  TextSize: TSize;
begin
  Button := FButtonInfo[Index];

  if Index = FSelectedButton then
  begin
    SelectionTop := (FSkin.LeftButtonHeight - FSkin.IconSelection.Height) div 2;

    ElementLeft := FSkin.LeftBarWidth;
    ElementTop := Button.y + SelectionTop;

    Canvas.Draw(ElementLeft, ElementTop, FSkin.IconSelection);
    FSkin.MenuFont.SetCanvas(Canvas, msActive);
  end
  else
  begin
    FSkin.MenuFont.SetCanvas(Canvas, Button.State);
  end;

  TextSize := Canvas.TextExtent(Button.Title);
  ElementLeft := (Button.Widht - TextSize.cx) div 2 + Button.x;
  ElementTop := (Button.Height - TextSize.cy) div 2 + Button.y;
  Canvas.TextOut(ElementLeft, ElementTop, Button.Title);
end;

procedure TFormProfileSelect.InitProfile;
var
  Profile: TProfileItem;
begin
  FProfile := TProfileLoader.Create(FSettings);

  cbNickName.Items.BeginUpdate;
  try
    cbNickName.Clear;
    for Profile in FProfile.Items do
    begin
      cbNickName.Items.Add(Profile.Name);
    end;
  finally
     cbNickName.Items.EndUpdate;
  end;
end;

procedure TFormProfileSelect.InsertSkin(Skin: TSkinProfileSelect);
begin
  FSkin := Skin;

  Color := FSkin.BackColor;
  FSkin.LabelFont.SetFont(labUserName.Font);
  FSkin.LabelFont.SetFont(labPassword.Font);
  FSkin.LabelFont.SetFont(labCreateNickname.Font);
  FSkin.LabelFont.SetFont(labCreatePassword.Font);
  FSkin.LabelFont.SetFont(labConfirmPassword.Font);

  FSkin.EditFont.SetFont(edPassword.Font);
  FSkin.EditFont.SetFont(edCreateNickname.Font);
  FSkin.EditFont.SetFont(edCreatePassword.Font);
  FSkin.EditFont.SetFont(edConfirmPassword.Font);

  FSkin.EditFont.SetFont(cbNickName.Font);
end;

procedure TFormProfileSelect.SetSelectButton(const Value: TLeftButton);
begin
  FSelectedButton := Value;

  // Do not change order
  case Value of
    lbExistUser:
      begin
        pnCreateAccount.Visible := False;
        pnSelectProfile.Visible := True;
        ActiveControl := cbNickName;
      end;

    lbNewUser:
      begin
        pnSelectProfile.Visible := False;
        pnCreateAccount.Visible := True;
        ActiveControl := edCreateNickname;
      end;
  end;

  Invalidate;
end;

procedure TFormProfileSelect.SetSettings(const Value: TSettings);
begin
  FSettings := Value;

  InitProfile;
end;

procedure TFormProfileSelect.UpdateTranslate;
begin
  Caption := ConvertTextToForm('Tox :: Profile Manager');
  labUserName.Caption := ConvertTextToForm('Nickname');
  labPassword.Caption := ConvertTextToForm('Password');
  btnLogin.Caption := ConvertTextToForm('Login');
  FButtonInfo[lbExistUser].Title := ConvertTextToForm('Exist User');
  FButtonInfo[lbNewUser].Title := ConvertTextToForm('New User');

  labCreateNickname.Caption := ConvertTextToForm('Create Nickname');
  labCreatePassword.Caption := ConvertTextToForm('Create Password');
  labConfirmPassword.Caption := ConvertTextToForm('Confirm Password');
  btnCeateAccount.Caption := ConvertTextToForm('Create Account');
end;

{ TButtonInfo }

function TButtonInfo.Containt(ValueX, ValueY: Integer): Boolean;
begin
  Result := (ValueX >= Self.x) and (ValueX < Self.x + Self.Widht) and
    (ValueY >= Self.y) and (ValueY < Self.y + Self.Height);
end;

end.
