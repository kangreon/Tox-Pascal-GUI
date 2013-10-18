//  FriendBase.pas
//
//  Загружает и сохраняет информацию о пользовательских идентификаторах
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit FriendBase;

interface

uses
  FriendItem, SysUtils, SQliteTable3, StringUtils, ClientAddress, Classes;

type
  TProcNewFriend = procedure(Sender: TObject; Friend: TFriendItem) of object;

  TFriendBase = class
  private
    FBase: TSQLiteDatabase;
    FCount: Integer;
    FFriends: TFriendItemList;
    FOnUpdateItem: TNotifyEvent;
    FOnNewItem: TProcNewFriend;
    function LoadUsers: Boolean;
    function CreateItem(RowId: Integer; Name, LocalName, Status: DataString;
      ClientId: TClientId): TFriendItem;
    procedure UpdateItem(Item: TFriendItem);
    function GetCount: Integer;
    procedure ItemUpdateBase(Sender: TObject);
    procedure ItemUpdate(Sender: TObject);
  public
    constructor Craete(Base: TSQLiteDatabase);
    destructor Destroy; override;

    function Add(Name, LocalName, Status: DataString;
      ClientId: TClientId): TFriendItem; overload;
    function Add(Name, LocalName, Status: DataString;
      Address: TFriendAddress): TFriendItem; overload;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Count: Integer read GetCount;
    property Friends: TFriendItemList read FFriends;

    property OnNewItem: TProcNewFriend read FOnNewItem write FOnNewItem;
    property OnUpdateItem: TNotifyEvent read FOnUpdateItem write FOnUpdateItem;
  end;

implementation

{ TFriendBase }

constructor TFriendBase.Craete(Base: TSQLiteDatabase);
begin
  FBase := Base;
  FFriends := TFriendItemList.Create;

  LoadUsers;
end;

destructor TFriendBase.Destroy;
begin
  FFriends.Free;
  inherited;
end;

procedure TFriendBase.BeginUpdate;
begin
  FBase.BeginTransaction;
end;

procedure TFriendBase.EndUpdate;
begin
  FBase.Commit;
end;

function TFriendBase.GetCount: Integer;
begin
  Result := FFriends.Count;
end;

procedure TFriendBase.UpdateItem(Item: TFriendItem);
var
  SQL: AnsiString;
  Query: TSQLiteQuery;
begin
  // TODO: проверка на работоспсобность базы
  if Item.BaseId <> -1 then
  begin
    SQL := 'UPDATE "user" SET "name" = ?1, "local_name" = ?2, "status"'+
      ' = ?3, "client_id" = ?4, "address" = ?5 WHERE  "id" = ' +
      AnsiString(IntToStr(Item.BaseId));
  end
  else
  begin
    SQL := 'INSERT INTO "user" ("name","local_name","status","client_id","address") VALUES (?1,?2,?3,?4,?5)';
  end;

  Query := FBase.PrepareSQL(SQL);
  FBase.BindSQL(Query, 1, Item.UserName);
  FBase.BindSQL(Query, 2, Item.LocaleName);
  FBase.BindSQL(Query, 3, Item.StatusMessage);
  FBase.BindSQL(Query, 4, Item.ClientId.DataHex);
  if Item.IsAddressExist then
    FBase.BindSQL(Query, 5, Item.Addressg.DataHex)
  else
    FBase.BindSQL(Query, 5, '');

  FBase.ExecSQL(Query);

  if Item.BaseId = -1 then
    Item.BaseId := FBase.GetLastInsertRowID;
end;

{ *  Добавление нового пользователя в базу данных и в список
  * }
function TFriendBase.Add(Name, LocalName, Status: DataString;
  ClientId: TClientId): TFriendItem;
var
  Item: TFriendItem;
begin
  Item := CreateItem(-1, name, LocalName, Status, ClientId);
  UpdateItem(Item);

  // Событие о новом пользователе
  if Assigned(FOnNewItem) then
    FOnNewItem(Self, Item);

  Result := Item;
end;

function TFriendBase.Add(Name, LocalName, Status: DataString;
  Address: TFriendAddress): TFriendItem;
var
  Client: TClientId;
begin
  Client := TClientId.Create(Address);
  try
    Result := Add(Name, LocalName, Status, Client);
  finally
    Client.Free;
  end;
end;

{ *  Создание нового элемента по данным из базы данных.
  *  В случае успешного создания, вернет указатель на элемент.
  * }
function TFriendBase.CreateItem(RowId: Integer; Name, LocalName,
  Status: DataString; ClientId: TClientId): TFriendItem;
var
  Item: TFriendItem;
begin
  Item := TFriendItem.Create(False, ClientId);
  Item.BaseId := RowId;
  Item.UserName := Name;
  Item.LocaleName := LocalName;
  Item.StatusMessage := Status;
  Item.OnUpdateBase := ItemUpdateBase;
  Item.OnUpdate := ItemUpdate;
  FFriends.Add(Item);

  Result := Item;
end;

procedure TFriendBase.ItemUpdate(Sender: TObject);
begin
  if Assigned(FOnUpdateItem) then
    FOnUpdateItem(Sender);
end;

procedure TFriendBase.ItemUpdateBase(Sender: TObject);
var
  Item: TFriendItem;
begin
  Item := TFriendItem(Sender);
  UpdateItem(Item);
end;

{ *  Загружает всех пользователей из базы данных и формирует из них список
  * }
function TFriendBase.LoadUsers: Boolean;
var
  Table: TSQLiteTable;
  i, RowId: Integer;
  Name, LocalName, Status: DataString;
  ClientId, FriendAddress: DataString;
  Client: TClientId;
begin
  Table := FBase.GetTable('SELECT * FROM user');
  try
    FCount := Table.RowCount;

    for i := 0 to FCount - 1 do
    begin
      RowId := Table.FieldAsInteger(0);
      Name := Table.FieldAsString(1);
      LocalName := Table.FieldAsString(2);
      Status := Table.FieldAsString(3);
      ClientId := Table.FieldAsString(4);
      FriendAddress := Table.FieldAsString(5);

      Client := TClientId.Create(ClientId);
      try
        if Client.ValidAddress then
          CreateItem(RowId, Name, LocalName, Status, Client);
      finally
        Client.Free;
      end;

      Table.Next;
    end;
  finally
    Table.Free;
  end;

  Result := True;
end;

end.
