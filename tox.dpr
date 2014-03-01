// tox
//
// Заголовочный файл проекта
//
// The MIT License (MIT)
//
// Copyright (c) 2013-2014 Dmitry
//
program tox;

{$I tox.inc}
{$I ResourceInclude.inc}

//{$I FirstUses.inc}
uses
  {$I FirstUses.inc}
  Forms,
  main in 'src\main.pas' {Form1},
  libtox in 'src\libtox.pas',
  toxcore in 'src\toxcore.pas',
  Settings in 'src\Settings.pas',
  ServerList in 'src\ServerList.pas',
  ClientAddress in 'src\ClientAddress.pas',
  FriendList in 'src\FriendList.pas',
  StringUtils in 'src\StringUtils.pas',
  MessageList in 'src\MessageList.pas',
  UserStatus in 'gui\UserStatus.pas',
  UserIcon in 'gui\UserIcon.pas',
  ResourceImage in 'gui\ResourceImage.pas',
  ImageUtils in 'gui\ImageUtils.pas',
  ActiveRegion in 'gui\ActiveRegion.pas',
  PaintSprite in 'gui\PaintSprite.pas',
  ControlPanel in 'gui\ControlPanel.pas',
  ButtonActive in 'gui\ButtonActive.pas',
  fmUserAdd in 'gui\Forms\fmUserAdd.pas' {FormUserAdd},
  fmNewName in 'gui\Forms\fmNewName.pas' {FormNewName},
  UserList in 'gui\UserList.pas',
  ScrollBarNormal in 'gui\ScrollBarNormal.pas',
  UserListDraw in 'gui\UserListDraw.pas',
  fmFriendRequest in 'gui\Forms\fmFriendRequest.pas' {FormFriendRequest},
  FriendRequestController in 'gui\FriendRequestController.pas',
  MessageControl in 'gui\MessageWidget\MessageControl.pas',
  MessageDraw in 'gui\MessageWidget\MessageDraw.pas',
  TextLineInfo in 'gui\MessageWidget\TextLineInfo.pas',
  MessageBase in 'src\MessageBase.pas',
  SQLiteTable3 in 'sqlite\SQLiteTable3.pas',
  SQLite3 in 'sqlite\SQLite3.pas',
  MessageItem in 'src\MessageItem.pas',
  FriendItem in 'src\FriendItem.pas',
  FriendBase in 'src\FriendBase.pas',
  DataBase in 'src\DataBase.pas',
  MessageHeader in 'gui\MessageWidget\MessageHeader.pas',
  MessageForm in 'gui\MessageWidget\MessageForm.pas',
  Splitter in 'gui\Splitter.pas',
  SkinBase in 'gui\SkinManager\SkinBase.pas',
  SkinManager in 'gui\SkinManager\SkinManager.pas',
  SkinUserList in 'gui\SkinManager\SkinUserList.pas',
  SkinUserStatus in 'gui\SkinManager\SkinUserStatus.pas',
  SkinTypes in 'gui\SkinManager\SkinTypes.pas',
  SkinMessageList in 'gui\SkinManager\SkinMessageList.pas',
  SkinMessageHeader in 'gui\SkinManager\SkinMessageHeader.pas',
  SkinControlPanel in 'gui\SkinManager\SkinControlPanel.pas',
  SkinTabControl in 'gui\SkinManager\SkinTabControl.pas',
  TabControl in 'gui\TabControl\TabControl.pas',
  TabSelectList in 'gui\TabControl\TabSelectList.pas',
  TabRequest in 'gui\TabControl\TabRequest.pas',
  SkinTabList in 'gui\SkinManager\SkinTabList.pas',
  UserListSelect in 'gui\UserListSelect.pas',
  lazaruspng in 'gui\SkinManager\lazaruspng.pas',
  ProfileLoader in 'src\ProfileLoader.pas',
  fmProfileSelect in 'gui\Forms\fmProfileSelect.pas' {FormProfileSelect},
  SkinProfileSelect in 'gui\SkinManager\SkinProfileSelect.pas',
  ControlUtils in 'gui\ControlUtils.pas',
  TabView in 'gui\TabControl\TabView.pas';

function IsRunApplication: Boolean;
var
  WinHandle: THandle;
begin
  {$IFDEF WINDOWS}
  CreateMutex(nil, True, 'fvb8r4hf7483hverhbv834vu8ndrsuifvhb847hvbiv4h87ghvy');
  Result := GetLastError <> ERROR_ALREADY_EXISTS;

  if not Result then
  begin
    WinHandle := FindWindow(nil, 'Demo Tox GUI');
    if WinHandle <> 0 then
      SetForegroundWindow(WinHandle);
  end;
  {$ENDIF}
end;

begin
{$IFDEF WINDOWS}
  if IsRunApplication then
  begin
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    Application.ShowMainForm := False;
    Application.Run
  end;
{$ELSE}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run
{$ENDIF}

end.
