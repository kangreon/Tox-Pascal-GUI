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
  FriendRequestController, MessageControl, MessageList, Clipbrd, FriendItem,
  Splitter, SkinManager, TabControl, fmProfileSelect, ProfileLoader,
  SkinProfileSelect, TabSelectList, TabRequest;

type
  { TForm1 }
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FRequestConrtoller: TFriendRequestController;
    FSettings: TSettings;
    FToxCore: TToxCore;
    procedure ToxOnConnect(Sender: TObject);
    procedure ToxOnDisconnect(Sender: TObject);
    procedure ToxOnConnecting(Sender: TObject; ServerCount: Integer);
    procedure ToxFriendRequest(Sender: TObject; ClientAddress: TFriendAddress;
      HelloMessage: DataString);
    procedure ToxFriendMessage(Sender: TObject; Friend: TFriendItem;
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
    FSkin: TSkinManager;
    FUserList: TUserList;
    FUserStatus: TUserStatus;
    FControlPanel: TControlPanel;
    FMessageControl: TMessageControl;
    FTabControl: TTabControl;
    FTabSelectList: TTabSelectList;
    FTabRequest: TTabRequest;
    procedure InitGui;
    procedure ControlPanelClick(Sender: TObject; Button: TControlButton);
    procedure RequestOnAddFriend(Sender: TObject;
      ClientAddress: TFriendAddress);
    procedure UserStatusChangeStatus(Sender: TObject);
    procedure UserListSelectItem(Sender: TObject; Item: TFriendItem);
    procedure MessageControlSendTextFriend(Sender: TObject; Friend: TFriendItem;
      const Text: DataString);
    procedure DestrGui;
    procedure SplitterSetWidth(Sender: TObject; NewWidth: Integer);
    procedure CloseProfile(Sender: TObject);
    procedure InitSkin;
    procedure StartTox(Profile: TProfileLoader);
    procedure SelectProfile(Skin: TSkinProfileSelect);
  public
    property Skin: TSkinManager read FSkin;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DestrGui;
  Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Установка ограничения на минимальный размер главного окна
  ClientHeight := 500;
  ClientWidth := 750;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  Position := poScreenCenter;

  Caption := 'Demo Tox GUI';
  Application.Title := Caption;

  FSettings := TSettings.Create;

  InitSkin;
  SelectProfile(FSkin.ProfileSelect);
end;

procedure TForm1.SelectProfile(Skin: TSkinProfileSelect);
var
  FormProfile: TFormProfileSelect;
begin
  FormProfile := TFormProfileSelect.Create(Self);
  FormProfile.InsertSkin(Skin);
  FormProfile.OnCloseEx := CloseProfile;
  FormProfile.Settings := FSettings;
  FormProfile.Show;
end;

procedure TForm1.CloseProfile(Sender: TObject);
begin
  if TFormProfileSelect(Sender).IsSelectedProfile then
  begin
    StartTox(TFormProfileSelect(Sender).Profile);
  end
  else
  begin
    Close;
  end;
end;

procedure TForm1.InitSkin;
begin
  FSkin := TSkinManager.Create(ExtractFilePath(ParamStr(0)) + 'skin.ini');
end;

{*  Создание компонентов для отображения GUI
 *}
procedure TForm1.InitGui;
var
  LeftPanel: TPanel;
  {$IFDEF DELPHI}
  Spl: TSplitter;
  {$ELSE}
  Spl: TSplitterEx;
  {$ENDIF}
begin
  LeftPanel := TPanel.Create(Self);
  LeftPanel.Parent := Self;
  LeftPanel.Align := alLeft;
  LeftPanel.Width := FSettings.UserListWidth;
  LeftPanel.BevelOuter := bvNone;
  LeftPanel.ParentColor := False;
  LeftPanel.FullRepaint := False;
  LeftPanel.DoubleBuffered := True;
  LeftPanel.ControlStyle := LeftPanel.ControlStyle - [csParentBackground];
  LeftPanel.TabOrder := 0;
  LeftPanel.TabStop := True;
  LeftPanel.Constraints.MinWidth := USER_LIST_MIN_WIDTH;
  LeftPanel.Constraints.MaxWidth := USER_LIST_MAX_WIDTH;

  //TODO: Исправить глюки под Linux
  {$IFDEF DELPHI}
  Spl := TSplitter.Create(Self);
  Spl.Parent := Self;
  Spl.Align := alLeft;
  Spl.ResizeStyle := TResizeStyle.rsUpdate;
  {$ELSE}
    {$IFNDEF FPCUNIX}
    Spl := TSplitterEx.Create(Self);
    Spl.Parent := Self;
    Spl.ControlResize := LeftPanel;
    Spl.OnSetWidth := SplitterSetWidth;
    {$ENDIF}
  {$ENDIF}
  // Для правильного размещения элемента
  Spl.Left := LeftPanel.Width + 1;

  // Компонент вывода сообщений пользователей
  FMessageControl := TMessageControl.Create(Self, FToxCore.MessageList, FSkin);
  FMessageControl.Align := alClient;
  FMessageControl.Parent := Self;
  FMessageControl.OnSendTextFriend := MessageControlSendTextFriend;

  // Компонент вывода информации о активном пользователе
  FUserStatus := TUserStatus.Create(LeftPanel, FSkin.UserStatus);
  FUserStatus.Parent := LeftPanel;
  FUserStatus.Top := 1;
  FUserStatus.Align := alTop;
  FUserStatus.FriendItem := FToxCore.FriendList.MyItem;
  FUserStatus.OnChangeState := UserStatusStateChange;
  FUserStatus.OnChangeUserName := UserStatusChangeName;
  FUserStatus.OnChangeStatus := UserStatusChangeStatus;
  Application.ProcessMessages;

  FControlPanel := TControlPanel.Create(LeftPanel, FSkin.ControlPanel);
  FControlPanel.Parent := LeftPanel;
  FControlPanel.Align := alBottom;
  FControlPanel.OnClick := ControlPanelClick;

  FUserList := TUserList.Create(LeftPanel, FToxCore.FriendList, FSkin.UserList);
  FUserList.Parent := LeftPanel;
  FUserList.Align := alClient;
  FUserList.OnSelectItem := UserListSelectItem;

  FTabSelectList := TTabSelectList.Create(Self, FSkin.TabControl);
  FTabRequest := TTabRequest.Create(Self, FSkin.TabControl);

  FTabControl := TTabControl.Create(Self);
  FTabControl.Parent := LeftPanel;
  FTabControl.Align := alTop;
  FTabControl.Top := FUserStatus.Height + 1;
  FTabControl.Color := Skin.TabControl.BackColor;
  FTabControl.Add(FTabSelectList);
  FTabControl.Add(FTabRequest);



  //Для правильной расстановки компонентов в Lazarus
  FTabControl.Top := FUserList.Top + FUserList.Height + 20;

end;

{ *  Освобождение памяти
  * }
procedure TForm1.DestrGui;
begin
  FMessageControl.Free;
  FUserList.Free;
  FControlPanel.Free;
  FUserStatus.Free;
end;

{ *  Создание и запуск Tox и всех зависимых классов
  * }
procedure TForm1.StartTox(Profile: TProfileLoader);
begin
  FToxCore := TToxCore.Create(FSettings, Profile);
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

  FRequestConrtoller := TFriendRequestController.Create(Self);
  FRequestConrtoller.OnAddFriend := RequestOnAddFriend;

  Show;

  InitGui;
end;

procedure TForm1.SplitterSetWidth(Sender: TObject; NewWidth: Integer);
begin
  FSettings.UserListWidth := NewWidth;
end;

procedure TForm1.MessageControlSendTextFriend(Sender: TObject;
  Friend: TFriendItem; const Text: DataString);
begin
  if Friend.IsFriend then
  begin
    FToxCore.SendMessage(Friend, Text);
  end;
end;

procedure TForm1.UserListSelectItem(Sender: TObject; Item: TFriendItem);
begin
  FMessageControl.SelectFriend(Item);
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

end;

procedure TForm1.ToxOnConnect(Sender: TObject);
begin
  FUserStatus.State := sOnline;
end;

procedure TForm1.ToxOnConnecting(Sender: TObject; ServerCount: Integer);
begin
  FUserStatus.State := sLoading;
end;

procedure TForm1.ToxConnectionStatus(Sender: TObject; FriendNumber: Integer;
  Status: Byte);
begin

end;

procedure TForm1.ToxOnDisconnect(Sender: TObject);
begin
  FUserStatus.State := sOffline;
end;

procedure TForm1.ToxFriendMessage(Sender: TObject; Friend: TFriendItem;
  MessageStr: DataString);
begin

end;

procedure TForm1.ToxFriendRequest(Sender: TObject; ClientAddress: TFriendAddress;
  HelloMessage: DataString);
begin
  FRequestConrtoller.InsertRequest(ClientAddress, HelloMessage);
end;

procedure TForm1.ToxNameChange(Sender: TObject; FriendNumber: Integer;
  NewName: DataString);
begin

end;

procedure TForm1.ToxStatusMessage(Sender: TObject; FriendNumber: Integer;
  NewStatus: DataString);
begin

end;

procedure TForm1.ToxUserStatus(Sender: TObject; FriendNumber: Integer;
  Kind: TToxUserStatus);
begin

end;

procedure TForm1.ToxReadReceipt(Sender: TObject; FriendNumber, Receipt: Integer
  );
begin

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
