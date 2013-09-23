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
  StringUtils;

type
  TMessageStatus = (msSending, msSend);

  // Класс, описывающий одно сообщение
  TMessageItem = class
  private
    FTime: TDateTime;
    FText: DataString;
    FUserMessage: Boolean;
    FFriendId: AnsiString;
    FStatusSend: Boolean;
  public
    // Время отправки сообщения
    property Time: TDateTime read FTime;
    // Текст сообщения
    property Text: DataString read FText;
    // Отправлено ли это сообщение пользователем для Вас
    property UserMessage: Boolean read FUserMessage;
    // Публичный ключ пользователя
    property FriendId: AnsiString read FFriendId;
    // Состояние доставки сообщения пользователю
    property StatusSend: Boolean read FStatusSend;
  end;

  TMessageArray = array of TMessageItem;

  //TODO: Для хранения базы данных использовать SQLite
  //TODO: FriendId хранится в отдельной таблице и соответствует ему уникальный номер
  TMessageList = class
  public
    constructor Create;
    destructor Destroy; override;

    function GetMessageCount(FriendId: AnsiString): Integer;
    function GetMessageRange(FriendId: AnsiString; StartRange, EndRange: Integer;
      var Messages: TMessageArray): Boolean;
    procedure SetMessage(FriendId: AnsiString; Text: DataString;
      UserMessage: Boolean);
  end;

implementation

{ TMessageList }

constructor TMessageList.Create;
begin

end;

destructor TMessageList.Destroy;
begin

  inherited;
end;

// Возвращает количество сообщений для выбранного пользователя,
// хранящихся в базе данных
function TMessageList.GetMessageCount(FriendId: AnsiString): Integer;
begin
  Result := 0; // TODO: Реализовать
end;

// Загружает из базы данных сообщения, которые относятся к выбранному
// пользователю из заданного диапазона
function TMessageList.GetMessageRange(FriendId: AnsiString; StartRange,
  EndRange: Integer; var Messages: TMessageArray): Boolean;
begin
  Result := False; // TODO: Реализовать
end;

// Добавляет новое сообщение в базу данных
procedure TMessageList.SetMessage(FriendId: AnsiString; Text: DataString;
  UserMessage: Boolean);
begin
// TODO: Реализовать
end;

end.
