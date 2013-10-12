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
  FriendList, ControlPanel, fmUserAdd, fmNewName, UserList,
  FriendRequestController, MessageControl, MessageList, Clipbrd;

type
  { TForm1 }
  TForm1 = class(TForm)
    ActivityList: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
  private
    FRequestConrtoller: TFriendRequestController;
    FSettings: TSettings;
    FToxCore: TToxCore;
    procedure ToxOnConnect(Sender: TObject);
    procedure ToxOnDisconnect(Sender: TObject);
    procedure ToxOnConnecting(Sender: TObject; Server: TServerItem);
    procedure ToxFriendRequest(Sender: TObject; ClientAddress: TFriendAddress;
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
    FMessageControl: TMessageControl;
    procedure InitGui;
    procedure ControlPanelClick(Sender: TObject; Button: TControlButton);
    procedure RequestOnAddFriend(Sender: TObject;
      ClientAddress: TFriendAddress);
    procedure UserStatusChangeStatus(Sender: TObject);
  public
    property ToxLoadError: Boolean read FToxLoadError;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientHeight := 500;
  ClientWidth := 750;

  Position := poScreenCenter;

  Caption := 'Demo Tox GUI';
  Application.Title := Caption;
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

  Clipboard.SetTextBuf(PChar(FToxCore.YourAddress.DataHex));

  FRequestConrtoller := TFriendRequestController.Create(Self);
  FRequestConrtoller.OnAddFriend := RequestOnAddFriend;

  ActivityList.Lines.Add(FToxCore.YourAddress.DataHex);
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
  // Устраняет проблему при изменении размера формы.
  // Проблема заключается в проступании цвета формы-родителя.
  LeftPanel.DoubleBuffered := True;
  {$IFNDEF FPC}
  LeftPanel.ParentBackground := False;
  {$ENDIF}
  LeftPanel.TabOrder := 0;
  LeftPanel.TabStop := True;

  Splitter1.Left := LeftPanel.Width;
  Splitter1.ResizeStyle := rsUpdate;

  RightPanel := TPanel.Create(Self);
  RightPanel.Parent := Self;
  RightPanel.Align := alClient;
  RightPanel.BevelOuter := bvNone;
  RightPanel.Color := $F2F2F1;
  RightPanel.DoubleBuffered := True;
  {$IFNDEF FPC}
  RightPanel.ParentBackground := False;
  {$ENDIF}

  FMessageControl := TMessageControl.Create(RightPanel, FToxCore.MessageList);
  FMessageControl.Align := alClient;
  FMessageControl.Parent := RightPanel;

  FUserStatus := TUserStatus.Create(LeftPanel);
  FUserStatus.Parent := LeftPanel;
  FUserStatus.Align := alTop;
  FUserStatus.UserName := FToxCore.UserName;
  FUserStatus.StatusMessage := FToxCore.StatusMessage;
  FUserStatus.OnChangeState := UserStatusStateChange;
  FUserStatus.OnChangeUserName := UserStatusChangeName;
  FUserStatus.OnChangeStatus := UserStatusChangeStatus;

  FControlPanel := TControlPanel.Create(LeftPanel);
  FControlPanel.Parent := LeftPanel;
  FControlPanel.Align := alBottom;
  FControlPanel.OnClick := ControlPanelClick;

  FUserList := TUserList.Create(LeftPanel, FToxCore.FriendList);
  FUserList.Align := alClient;
  FUserList.Parent := LeftPanel;

  //TODO: Временно. Пока не будет заменено настоящими компонентами
  ActivityList.Align := alClient;
  ActivityList.DoubleBuffered := True;
  ActivityList.Color := $F2F2F1;
  ActivityList.Visible := False;
end;

{ *  Событие возникает при нажатия на одну из кнопок панели управления.
  * }
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
  ActivityList.Lines.Add('Action from user ' + IntToStr(FriendNumber) +
    ' with text: ' + Action);
end;

procedure TForm1.ToxOnConnect(Sender: TObject);
begin
  ActivityList.Lines.Add('connect');
  FUserStatus.State := sOnline;
end;

procedure TForm1.ToxOnConnecting(Sender: TObject; Server: TServerItem);
begin
  ActivityList.Lines.Add('Try connect to ' + Server.Name);
  FUserStatus.State := sLoading;
end;

procedure TForm1.ToxConnectionStatus(Sender: TObject; FriendNumber: Integer;
  Status: Byte);
begin
  ActivityList.Lines.Add('User ' + IntToStr(FriendNumber) +
    ' change connection status to ' + IntToStr(Status));
end;

procedure TForm1.ToxOnDisconnect(Sender: TObject);
begin
  ActivityList.Lines.Add('desconnect');
  FUserStatus.State := sOffline;
end;

procedure TForm1.ToxFriendMessage(Sender: TObject; FriendNumber: Integer;
  MessageStr: DataString);
begin
  ActivityList.Lines.Add('New message from user ' + IntToStr(FriendNumber) +
    ' with text: ' + MessageStr);
end;

procedure TForm1.ToxFriendRequest(Sender: TObject; ClientAddress: TFriendAddress;
  HelloMessage: DataString);
begin
  ActivityList.Lines.Add('User ' + ClientAddress.DataHex + ' send request with text: ' + HelloMessage);
  FRequestConrtoller.InsertRequest(ClientAddress, HelloMessage);
end;

procedure TForm1.ToxNameChange(Sender: TObject; FriendNumber: Integer;
  NewName: DataString);
begin
  ActivityList.Lines.Add('User ' + IntToStr(FriendNumber) + ' change name to ' + NewName)
end;

procedure TForm1.ToxStatusMessage(Sender: TObject; FriendNumber: Integer;
  NewStatus: DataString);
begin
  ActivityList.Lines.Add('User ' + IntToStr(FriendNumber) + ' change status message to ' + NewStatus);
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

  ActivityList.Lines.Add('User ' + IntToStr(FriendNumber) + ' change user status to ' + Status);
end;

procedure TForm1.ToxReadReceipt(Sender: TObject; FriendNumber, Receipt: Integer
  );
begin
  ActivityList.Lines.Add('User ' + IntToStr(FriendNumber) + ' read you message ' + IntToStr(Receipt));
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
              FUserStatus.State := sOffline;
            end;
        end;
      end;

    sOnline:
      begin
        case FToxCore.ConnectState of
          csOnline:
            begin
              FToxCore.SetUserStatus(usNone);
              FUserStatus.State := sOnline;
            end;

          csOffline:
            begin
              FToxCore.StartTox;
              FUserStatus.State := sLoading;
            end;
        end;
      end;

    sAway:
      begin
        case FToxCore.ConnectState of
          csOnline:
            begin
              FToxCore.SetUserStatus(usAway);
              FUserStatus.State := sAway;
            end;

          csOffline:
            begin
              FToxCore.StartTox;
              FUserStatus.State := sLoading;
            end;
        end;
      end;

    sBusy:
      begin
        case FToxCore.ConnectState of
          csOnline:
            begin
              FToxCore.SetUserStatus(usBusy);
              FUserStatus.State := sBusy;
            end;

          csOffline:
            begin
              FToxCore.StartTox;
              FUserStatus.State := sLoading;
            end;
        end;
      end;
  end;
end;

{*  Событие на запрос пользователя о изменении имени
 *}
procedure TForm1.UserStatusChangeName(Sender: TObject);
var
  NewName: string;
  FormName: TFormNewName;
begin
  FormName := TFormNewName.Create(Self, fmtChangeName);
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

procedure TForm1.UserStatusChangeStatus(Sender: TObject);
var
  NewStatus: string;
  FormStatus: TFormNewName;
  IsChange: Boolean;
begin
  FormStatus := TFormNewName.Create(Self, fmtChangeStatus);
  try
    FormStatus.Position := poOwnerFormCenter;
    FormStatus.ShowModal;
    IsChange := FormStatus.ModalResult = 1;
    NewStatus := FormStatus.NewName;
  finally
    FormStatus.Free;
  end;

  if IsChange then
  begin
    FToxCore.StatusMessage := NewStatus;
    FUserStatus.StatusMessage := NewStatus;
  end;
end;

{ *  Событие, вызываемое после согласия на добавления нового друга в
  *  список.
  *
  *  ClientAddress - адрес пользователя, который добавляется к вам в список
  *
  * }
procedure TForm1.RequestOnAddFriend(Sender: TObject;
  ClientAddress: TFriendAddress);
begin
  FToxCore.AddFriendNoRequest(ClientAddress);
end;

end.
