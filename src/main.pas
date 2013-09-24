//  Main.pas
//
//  Главная форма проекта. Постройка графического интерфейса и инициализация
//  ядра Tox.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit main;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  SysUtils, Controls, Forms, Classes, Dialogs, StdCtrls, toxcore, Settings,
  ServerList, ClientAddress, libtox, StringUtils, ExtCtrls, UserStatus,
  FriendList, ControlPanel, fmUserAdd, fmNewName, UserList;

type
  { TForm1 }
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Edit3: TEdit;
    Button2: TButton;
    ListBox1: TListBox;
    Edit4: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FSettings: TSettings;
    FToxCore: TToxCore;
    procedure ToxOnConnect(Sender: TObject);
    procedure ToxOnDisconnect(Sender: TObject);
    procedure ToxOnConnecting(Sender: TObject; Server: TServerItem);
    procedure ToxFriendRequest(Sender: TObject; ClientAddress: TClientAddress;
      HelloMessage: DataString);
    procedure ToxFriendMessage(Sender: TObject; FriendNumber: Integer;
      MessageStr: DataString);
    procedure ToxOnAction(Sender: TObject; FriendNumber: Integer;
      Action: DataString);
    procedure ToxNameChange(Sender: TObject; FriendNumber: Integer;
      NewName: DataString);
    procedure ToxStatusMessage(Sender: TObject; FriendNumber: Integer;
      NewStatus: DataString);
    procedure ToxUserStatus(Sender: TObject; FriendNumber: Integer;
      Kind: TToxUserStatus);
    procedure ToxReadReceipt(Sender: TObject; FriendNumber, Receipt: Integer);
    procedure ToxConnectionStatus(Sender: TObject; FriendNumber: Integer;
      Status: Byte);
    procedure UserStatusStateChange(Sender: TObject; UserState: TState);
    procedure UserStatusChangeName(Sender: TObject);
  private
    FUserList: TUserList;
    FUserStatus: TUserStatus;
    FControlPanel: TControlPanel;
    FToxLoadError: Boolean;
    procedure InitGui;
    procedure ControlPanelClick(Sender: TObject; Button: TControlButton);
  public
    property ToxLoadError: Boolean read FToxLoadError;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  if TryStrToInt(Edit4.Text, i) then
  begin
    FToxCore.SendMessage(i, Edit3.Text);
    Edit3.Clear;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  FriendList: TFriendList;
  c, i: Integer;
begin
  Caption := 'Demo Tox GUI';
  FToxLoadError := False;

  FSettings := TSettings.Create;
  FToxCore := TToxCore.Create(FSettings);

  if not FToxCore.IsLoadLibrary then
  begin
    // Не удалось загрузить библиотеку Tox
    FToxLoadError := True;
    Exit;
  end;

  FToxCore.OnConnect := ToxOnConnect;
  FToxCore.OnConnecting := ToxOnConnecting;
  FToxCore.OnDisconnect := ToxOnDisconnect;
  FToxCore.OnFriendRequest := ToxFriendRequest;
  FToxCore.OnFriendMessage := ToxFriendMessage;
  FToxCore.OnNameChange := ToxNameChange;
  FToxCore.OnAction := ToxOnAction;
  FToxCore.OnStatusMessage := ToxStatusMessage;
  FToxCore.OnUserStatus := ToxUserStatus;
  FToxCore.OnReadReceipt := ToxReadReceipt;
  FToxCore.OnConnectioStatus := ToxConnectionStatus;

  Memo1.Lines.Add(FToxCore.YourAddress.DataHex);

  FriendList := FToxCore.FriendList;
  c := FriendList.Count;
  for i := 0 to c - 1 do
  begin
    ListBox1.Items.Add(FriendList.Item[i].UserName);
  end;

  FToxCore.StartTox;

  InitGui;
end;

{*  Создание компонентов для отображения GUI
 *}
procedure TForm1.InitGui;
var
  LeftPanel: TPanel;
  RightPanel: TPanel;
begin
  LeftPanel := TPanel.Create(Self);
  LeftPanel.Parent := Self;
  LeftPanel.Align := alLeft;
  LeftPanel.Width := 223;
  LeftPanel.BevelOuter := bvNone;
//  LeftPanel.Color := 0;
  LeftPanel.DoubleBuffered := True;
  {$IFNDEF FPC}
  LeftPanel.ParentBackground := False;
  {$ENDIF}

  RightPanel := TPanel.Create(Self);
  RightPanel.Parent := Self;
  RightPanel.Align := alClient;
  RightPanel.BevelOuter := bvNone;
  RightPanel.Color := $F2F2F1;
  {$IFNDEF FPC}
  RightPanel.ParentBackground := False;
  {$ENDIF}

  FUserStatus := TUserStatus.Create(LeftPanel);
  FUserStatus.Parent := LeftPanel;
  FUserStatus.Align := alTop;
  FUserStatus.OnStateChange := UserStatusStateChange;
  FUserStatus.OnChangeUserName := UserStatusChangeName;
  FUserStatus.UserName := FToxCore.UserName;

  FControlPanel := TControlPanel.Create(LeftPanel);
  FControlPanel.Parent := LeftPanel;
  FControlPanel.Align := alBottom;
  FControlPanel.OnClick := ControlPanelClick;

  FUserList := TUserList.Create(LeftPanel);
  FUserList.Align := alClient;
  FUserList.Parent := LeftPanel;

  //TODO: Временно. Пока не будет заменено настоящими компонентами
  Panel1.Parent := RightPanel;
  {$IFNDEF FPC}
  Panel1.ParentBackground := False;
  {$ENDIF}
  Panel1.Visible := False;
  ListBox1.Parent := LeftPanel;
  ListBox1.Align := alClient;
  ListBox1.Visible := False;
  Memo1.Parent := RightPanel;
  Memo1.Align := alClient;
  Memo1.DoubleBuffered := True;
  Memo1.Color := $F2F2F1;
//  Memo1.Visible := False;
end;

{
  Событие на нажатия на одну из кнопок панели управления
                                                                          }
procedure TForm1.ControlPanelClick(Sender: TObject; Button: TControlButton);
var
  Form: TForm;
begin
  case Button of
    cbAddUser:
      begin
        Form := TFormUserAdd.Create(Self);
        Form.Position := poOwnerFormCenter;
        TFormUserAdd(Form).Tox := FToxCore;
        Form.ShowModal;
        Form.Free;
      end;
    cbSettings: ;
    cbGroup: ;
  end;
end;

procedure TForm1.ToxOnAction(Sender: TObject; FriendNumber: Integer;
  Action: DataString);
begin
  Memo1.Lines.Add('Action from user ' + IntToStr(FriendNumber) +
    ' with text: ' + Action);
end;

procedure TForm1.ToxOnConnect(Sender: TObject);
begin
  Memo1.Lines.Add('connect');
  FUserStatus.State := sOnline;
end;

procedure TForm1.ToxOnConnecting(Sender: TObject; Server: TServerItem);
begin
  Memo1.Lines.Add('Try connect to ' + Server.Name);
  FUserStatus.State := sLoading;
end;

procedure TForm1.ToxConnectionStatus(Sender: TObject; FriendNumber: Integer;
  Status: Byte);
begin
  Memo1.Lines.Add('User ' + IntToStr(FriendNumber) +
    ' change connection status to ' + IntToStr(Status));
end;

procedure TForm1.ToxOnDisconnect(Sender: TObject);
begin
  Memo1.Lines.Add('desconnect');
  FUserStatus.State := sOffline;
end;

procedure TForm1.ToxFriendMessage(Sender: TObject; FriendNumber: Integer;
  MessageStr: DataString);
begin
  Memo1.Lines.Add('New message from user ' + IntToStr(FriendNumber) +
    ' with text: ' + MessageStr);

  FUserStatus.UserName := MessageStr;
end;

procedure TForm1.ToxFriendRequest(Sender: TObject; ClientAddress: TClientAddress;
  HelloMessage: DataString);
begin
  Memo1.Lines.Add('User ' + ClientAddress.DataHex + ' send request with text: ' + HelloMessage);
  FToxCore.AddFriendNoRequest(ClientAddress);
end;

procedure TForm1.ToxNameChange(Sender: TObject; FriendNumber: Integer;
  NewName: DataString);
begin
  Memo1.Lines.Add('User ' + IntToStr(FriendNumber) + ' change name to ' + NewName)
end;

procedure TForm1.ToxStatusMessage(Sender: TObject; FriendNumber: Integer;
  NewStatus: DataString);
begin
  Memo1.Lines.Add('User ' + IntToStr(FriendNumber) + ' change status message to ' + NewStatus);
end;

procedure TForm1.ToxUserStatus(Sender: TObject; FriendNumber: Integer;
  Kind: TToxUserStatus);
var
  Status: DataString;
begin
  case Kind of
    usNone: Status := 'none';
    usAway: Status := 'away';
    usBusy: Status := 'busy';
    usInvalid: Status := 'invalid';
  end;

  Memo1.Lines.Add('User ' + IntToStr(FriendNumber) + ' change user status to ' + Status);
end;

procedure TForm1.ToxReadReceipt(Sender: TObject; FriendNumber, Receipt: Integer
  );
begin
  Memo1.Lines.Add('User ' + IntToStr(FriendNumber) + ' read you message ' + IntToStr(Receipt));
end;

{*  Событие на выбор пользователем состояния
 *}
procedure TForm1.UserStatusStateChange(Sender: TObject; UserState: TState);
begin
  case UserState of
    sOffline:
      begin
        case FToxCore.ConnectState of
          csOnline, csConnecting:
            begin
              FToxCore.StopTox;
            end;
        end;
      end;

    sOnline:
      begin
        if FToxCore.ConnectState = csOffline then
          FToxCore.StartTox;
      end;

    sAway: ;
    sLoading: ;
  end;
end;

{*  Событие на запрос пользователя о изменении имени
 *}
procedure TForm1.UserStatusChangeName(Sender: TObject);
var
  NewName: string;
  FormName: TFormNewName;
begin
  FormName := TFormNewName.Create(Self);
  try
    FormName.Position := poOwnerFormCenter;
    FormName.ShowModal;
    NewName := FormName.NewName;
  finally
    FormName.Free;
  end;

  if NewName <> '' then
  begin
    FToxCore.UserName := NewName;
    FUserStatus.UserName := NewName;
  end;
end;

end.
