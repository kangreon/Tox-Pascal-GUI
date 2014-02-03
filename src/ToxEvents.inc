{ *  ОБРАБОТЧИКИ СОБЫТИЙ TOX
  * }

procedure OnFriendRequest_(public_key: PByte; data: PByte; length: Word;
  UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  ClientAddress: TFriendAddress;
  HelloMessage: DataString;
begin
  ToxThread := TToxCore(UserData);

  if Assigned(ToxThread) then
  begin
    ClientAddress := TFriendAddress.Create(public_key);
    HelloMessage := MemoryToString(data, length);
    ToxThread.DoFriendRequest(ClientAddress, HelloMessage);
  end;
end;

procedure OnFriendMessage_(tox: TTox; FriendNumber: Integer; message: PByte;
  length: Word; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  MessageStr: DataString;
begin
  ToxThread := TToxCore(UserData);

  if Assigned(ToxThread) then
  begin
    MessageStr := MemoryToString(message, length);
    ToxThread.DoFriendMessage(FriendNumber, MessageStr);
  end;
end;

procedure OnAction_(tox: TTox; FriendNumber: Integer; Action: PByte;
  length: Word; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  MessageStr: DataString;
begin
  ToxThread := TToxCore(UserData);

  if Assigned(ToxThread) then
  begin
    MessageStr := MemoryToString(Action, length);
    ToxThread.DoAction(FriendNumber, MessageStr);
  end;
end;

procedure OnNameChange_(tox: TTox; FriendNumber: Integer; NewName: PByte;
  length: Word; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  NewNameStr: DataString;
begin
  ToxThread := TToxCore(UserData);

  if Assigned(ToxThread) then
  begin
    NewNameStr := MemoryToString(NewName, length);
    ToxThread.DoNameChange(FriendNumber, NewNameStr);
  end;
end;

procedure OnStatusMessage_(tox: TTox; FriendNumber: Integer; NewStatus: PByte;
  length: Word; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
  NewStatusStr: DataString;
begin
  ToxThread := TToxCore(UserData);

  if Assigned(ToxThread) then
  begin
    NewStatusStr := MemoryToString(NewStatus, length);
    ToxThread.DoStatusMessage(FriendNumber, NewStatusStr);
  end;
end;

procedure OnUserStatus_(tox: TTox; FriendNumber: Integer; Kind: TToxUserStatus;
  UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
begin
  ToxThread := TToxCore(UserData);

  if Assigned(ToxThread) then
    ToxThread.DoUserStatus(FriendNumber, Kind);
end;

procedure OnReadReceipt_(tox: TTox; FriendNumber: Integer; Receipt: Integer;
  UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
begin
  ToxThread := TToxCore(UserData);

  if Assigned(ToxThread) then
    ToxThread.DoReadReceipt(FriendNumber, Receipt);
end;

procedure OnConnectionStatus_(tox: TTox; FriendNumber: Integer;
  Status: Byte; UserData: Pointer); cdecl;
var
  ToxThread: TToxCore;
begin
  ToxThread := TToxCore(UserData);

  if Assigned(ToxThread) then
    ToxThread.DoConnectionStatus(FriendNumber, Status);
end;