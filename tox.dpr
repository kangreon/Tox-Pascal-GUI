//  tox
//
//  Заголовочный файл проекта
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
program tox;

{$I tox.inc}

{$IFNDEF Unix}
  {$R tox-res.res}
{$ENDIF}
{$R images.res}

uses
  {$IFDEF FPC}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF }
  Interfaces,
  {$ENDIF }
  {$I tox-uses.inc}
  Forms,
  Dialogs,
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
  UserStatusStyle in 'gui\UserStatusStyle.pas',
  ActiveRegion in 'gui\ActiveRegion.pas',
  PaintSprite in 'gui\PaintSprite.pas',
  ControlPanel in 'gui\ControlPanel.pas',
  ButtonActive in 'gui\ButtonActive.pas',
  fmUserAdd in 'gui\Forms\fmUserAdd.pas' {FormUserAdd},
  fmNewName in 'gui\Forms\fmNewName.pas' {FormNewName},
  UserList in 'gui\UserList.pas',
  UserListStyle in 'gui\UserListStyle.pas',
  ScrollBarNormal in 'gui\ScrollBarNormal.pas',
  ScrollBarNormalStyle in 'gui\ScrollBarNormalStyle.pas',
  UserListDraw in 'gui\UserListDraw.pas',
  UserListDrawStyle in 'gui\UserListDrawStyle.pas';

{*  Вывод сообщения об ошибке загрузки библиотеки libtoxcore
 *}
procedure MessageLoadToxError;
begin
  {$IFDEF Win32}
    ShowMessage('Cannot load library libtoxcore-0.dll');
  {$ELSE}
    ShowMessage('Cannot load library libtoxcore-0.so');
  {$ENDIF}
end;

{$IFDEF NEW_DELPHI}
function IsRunApplication: Boolean;
var
  WinHandle: THandle;
begin
  CreateMutex(nil, True, 'fvb8r4hf7483hverhbv834vu8ndrsuifvhb847hvbiv4h87ghvy');
  Result := GetLastError <> ERROR_ALREADY_EXISTS;

  if not Result then
  begin
    WinHandle := FindWindow(nil, 'Demo Tox GUI');
    if WinHandle <> 0 then
      SetForegroundWindow(WinHandle);
  end;
end;
{$ENDIF}

begin
  {$IFDEF NEW_DELPHI}
  if IsRunApplication then
  begin
  {$ENDIF}
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    if not Form1.ToxLoadError then
      Application.Run
    else
      MessageLoadToxError;
  {$IFDEF NEW_DELPHI}
  end;
  {$ENDIF}
end.

