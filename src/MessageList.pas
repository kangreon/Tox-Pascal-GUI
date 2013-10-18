//  MessageList.pas
//
//  Осуществляет доступ к сообщениям, хранящимся в базе данных
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit MessageList;

interface
  {$I tox.inc}

uses
  StringUtils, SysUtils, ClientAddress, FriendList, MessageItem, FriendItem,
  MessageBase, DataBase, Classes;

type
  TMessageStatus = (msSending, msSend, msError);

  TMessageList = class
  private
    FDataBase: TDataBase;
    FFriends: TFriendList;
    FMessages: TMessageBaseList;
    FMyItem: TFriendItem;
    FOnNewMessage: TProcNewMessage;
    function GetMessageBase(Client: TClientId): TMessageBase;
    procedure BaseNewMessage(Sender: TObject; FriendDialog: TFriendItem;
      Message: TMessageItem);
    procedure CreateBaseForFriend(Friend: TFriendItem);
  public
    constructor Create(DataBase: TDataBase; Friends: TFriendList);
    destructor Destroy; override;

    procedure InsertFriend(Friend: TFriendItem);
    function GetMessage(Client: TClientId; Index: Integer;
      out Mess: TMessageItem): Boolean;
    function GetMessageCount(Client: TClientId): Integer;
    procedure SetMessage(Friend: TFriendItem; Text: DataString;
      IsMy: Boolean);

    property OnNewMessage: TProcNewMessage read FOnNewMessage
      write FOnNewMessage;
  end;

implementation

{ TMessageList }

constructor TMessageList.Create(DataBase: TDataBase; Friends: TFriendList);
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  FFriends := Friends;
  FMyItem := FFriends.MyItem;
  FDataBase := DataBase;
  FMessages := TMessageBaseList.Create;

  // Создание хранилища сообщений для каждого пользователя
  for PItem in FFriends.Item do
  begin
    Item := TFriendItem(PItem);

    CreateBaseForFriend(Item);
  end;
end;

destructor TMessageList.Destroy;
var
  PItem: Pointer;
  Item: TFriendItem;
begin
  for PItem in FFriends.Item do
  begin
    Item := TFriendItem(PItem);
    Item.Free;
  end;
  FFriends.Free;

  inherited;
end;

{ *  Создание обработчика сообщений для диалога с пользователем.
  * }
procedure TMessageList.CreateBaseForFriend(Friend: TFriendItem);
var
  Base: TMessageBase;
begin
  Base := TMessageBase.Create(FMyItem, Friend, FDataBase);
  Base.OnNewMessage := BaseNewMessage;
  FMessages.Add(Base);
end;

procedure TMessageList.BaseNewMessage(Sender: TObject; FriendDialog: TFriendItem;
  Message: TMessageItem);
begin
  if Assigned(FOnNewMessage) and Assigned(FriendDialog) and
    Assigned(Message) then
  begin
    FOnNewMessage(Self, FriendDialog, Message);
  end;
end;

{ *  Возвращает сообщение для выбранного пользователя. Загрузка происходит с
  *  кэшированием загруженных данных, для более быстрого доступа к ним.
  *
  *  FriendId - Идентификатор пользователя, для которого происходит выборка
  *  Index - номер загружаемого сообщения
  *  out Mess - Сообщение
  *
  *  В случае успешного получения сообщения, функция вернет True.
  * }
function TMessageList.GetMessage(Client: TClientId; Index: Integer;
  out Mess: TMessageItem): Boolean;
var
  Item: TMessageBase;
begin
  Item := GetMessageBase(Client);
  if Assigned(Item) then
  begin
    Mess := Item.Select(Index);
    Result := Assigned(Mess);
  end
  else
  begin
    Result := False;
  end;
end;

function TMessageList.GetMessageBase(Client: TClientId): TMessageBase;
var
  PItem: Pointer;
  Item: TMessageBase;
begin
  Result := nil;
  for PItem in FMessages do
  begin
    Item := TMessageBase(PItem);
    if Item.Friend.ClientId.IsCompare(Client) then
    begin
      Result := Item;
      Exit;
    end;
  end;
end;

{ *  Возвращает количество сообщений для выбранного пользователя, хранящихся
  *  в базе данных
  * }
function TMessageList.GetMessageCount(Client: TClientId): Integer;
var
  Item: TMessageBase;
begin
  if Assigned(Client) then
  begin
    Item := GetMessageBase(Client);
    if Assigned(Item) then
      Result := Item.Count
    else
      Result := 0;
  end
  else
    Result := 0;
end;

procedure TMessageList.InsertFriend(Friend: TFriendItem);
begin
  CreateBaseForFriend(Friend);
end;

{ *  Добавляет новое сообщение Text в базу данных диалога с пользователем
  *  Friend. Если сообщение является отправленное Вами, IsMy = TRUE
  * }
procedure TMessageList.SetMessage(Friend: TFriendItem; Text: DataString;
  IsMy: Boolean);
var
  Item: TMessageBase;
begin
  if Assigned(Friend) and Assigned(Friend.ClientId) then
  begin
    Item := GetMessageBase(Friend.ClientId);
    if Assigned(Item) then
      Item.InserMessage(Text, IsMy);
  end;
end;

end.
