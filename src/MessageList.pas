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
  StringUtils, SysUtils, ClientAddress, FriendList;

type
  TMessageStatus = (msSending, msSend, msError);

  // Класс, описывающий одно сообщение
  TMessageItem = class
  private
    FTime: TDateTime;
    FText: DataString;
    FUserMessage: Boolean;
    FStatusSend: Boolean;
    FIndex: Integer;
    FData: TObject;
    FFriend: TFriendItem;
    procedure SetText(const Value: DataString);
  public
    class function FromText(Text: DataString): TMessageItem;
    property Data: TObject read FData write FData;
    property Friend: TFriendItem read FFriend write FFriend;
    property Index: Integer read FIndex write FIndex;
    // Время отправки сообщения
    property Time: TDateTime read FTime write FTime;
    // Текст сообщения
    property Text: DataString read FText write SetText;
    // Отправлено ли это сообщение пользователем для Вас
    property UserMessage: Boolean read FUserMessage write FUserMessage;
    // Состояние доставки сообщения пользователю
    property StatusSend: Boolean read FStatusSend write FStatusSend;
  end;

  TMessageArray = array of TMessageItem;

  //TODO: Для хранения базы данных использовать SQLite
  //TODO: FriendId хранится в отдельной таблице и соответствует ему уникальный номер
  TMessageList = class
  private
    FFriends: TFriendList;
    FLastFriend: AnsiString;
    FLastRangeStart: Integer;
    FLastRangeEnd: Integer;
    FLastMessageList: TMessageArray;
    FTemp: TMessageArray;
  public
    constructor Create(Friends: TFriendList);
    destructor Destroy; override;

    function GetMessage(FriendId: AnsiString; Index: Integer;
      out Mess: TMessageItem): Boolean;
    function GetMessageCount(FriendId: AnsiString): Integer;
    function GetMessageRange(FriendId: AnsiString; StartRange,
      EndRange: Integer): Boolean;
    procedure SetMessage(FriendId: AnsiString; Text: DataString;
      UserMessage: Boolean);
  end;

implementation

{ TMessageList }

//TODO: Исправить
constructor TMessageList.Create(Friends: TFriendList);
var
  i: Integer;
  Friend1: TFriendItem;
  Friend2: TFriendItem;
begin
  FFriends := Friends;

  //TODO: Сообщения были взяты случайным образом из открытого источника habr.ru
  SetLength(FTemp, 20);
  FTemp[0] := TMessageItem.FromText('Лучше бы они переключили свое внимание на действительно вредные сайты, ограничивающие свободу в интернете :)');
  FTemp[1] := TMessageItem.FromText('Это не сайты вредные, а люди. ');
  FTemp[2] := TMessageItem.FromText('У leaseweb столько вредных и развратных сайтов хостится… ');
  FTemp[3] := TMessageItem.FromText('А как именно они уже рассказали? ');
  FTemp[4] := TMessageItem.FromText('Я что-то не понял сообщения, которое оставили хакеры. Это что, какое-то кодирование? Или они просто настолько безграмотные? ');
  FTemp[5] := TMessageItem.FromText('Там сверху написано Anonymous Palestine. Может дело в этом? ');
  FTemp[6] := TMessageItem.FromText('Палестинский диалект олбанского английского :D ');
  FTemp[7] := TMessageItem.FromText('Google Translate :) ');
  FTemp[8] := TMessageItem.FromText('Насколько я понимаю, это измененная цитата из V for Vendetta.');
  FTemp[9] := TMessageItem.FromText('Мне одному режет глаз сочетание слов «Anonymous» и «Palestine»? ');
  FTemp[10] := TMessageItem.FromText('Если это ложный след, вообще великолепно. ');
  FTemp[11] := TMessageItem.FromText('ну и стоит, видимо, добавить, что dns серверы типа '+'bind всегда были самыми дырявыми сервисами, почти всегда они в chroot`е или вообще в тюрьме и что смена записи dns пусть и приводит к дефейсу и недоступности ' + 'мастер-сервиса какое-то время, не несёт в себе никаких серьезных последствий. Исходники на месте, база не тронута. Школьники получили 5 ми');
  FTemp[12] := TMessageItem.FromText('На самом деле я считаю это серьезная атака. Например она позволяет сделать фишинг сайта для сбора паролей логинов, номеров кредиток и т.п… ');
  FTemp[13] := TMessageItem.FromText('А как же HTTPS? ');
  FTemp[14] := TMessageItem.FromText('так нет нужды полностью все эмулировать. можно просто логины пароли забрать например, а к сожалению не все обращают внимания на сертификаты и https ');
  FTemp[15] := TMessageItem.FromText('Ага, проблема есть и серьёзная, т.к. не всем обращают внимание на HTTPS, однако из-за того самого HTTPS это с трудом можно называть взломом. ');
  FTemp[16] := TMessageItem.FromText('https никто не отменял ');
  FTemp[17] := TMessageItem.FromText('Бывали же случаи кражи ключей из центра сертификации. Кто знает, возможно какие-то ещё центры обокрали, но пропажу пока не обнаружили. ');
  FTemp[18] := TMessageItem.FromText('Да хрен с ним с Лизвебом, но эти же ДНС являются авторитативными для множества клиентских доменов, они так же могли перенаправить их куда угодно, в том числе, как написал CrazyAngel сделать фишинг и прочее ');
  FTemp[19] := TMessageItem.FromText('Вы уверене, что лизвеб дает мастер-ns клиентам? '#13#10#13#10'В какой услуге?');

  Friend1 := FFriends.FindByAddress('1111111111111111111111111111111111111111111111111111111111111111111111111111');
  Friend2 := FFriends.FindByClient('2222222222222222222222222222222222222222222222222222222222222222');

  for i := Low(FTemp) to High(FTemp) do
  begin
    {$IFDEF fpc}
    FTemp[i].Text := UTF8Encode(FTemp[i].Text);
    {$ENDIF}
    if Random(2) mod 2 = 0 then
      FTemp[i].Friend := Friend1
    else
      FTemp[i].Friend := Friend2;
  end;

  FTemp[19].Friend := Friend2;
  FTemp[18].Friend := Friend2;
  FTemp[17].Friend := Friend2;
  FTemp[16].Friend := Friend1;
end;

destructor TMessageList.Destroy;
begin

  inherited;
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
function TMessageList.GetMessage(FriendId: AnsiString; Index: Integer;
  out Mess: TMessageItem): Boolean;
var
  Count: Integer;
  StartLoad, EndLoad: Integer;
begin
  Count := GetMessageCount(FriendId);
  if (Index >= 0) and (Index < Count) then
  begin

    //TODO: Добавить проверку на тот случай, когда предзагрузка находится в самом
    // конце списка, а в базе появилось одно или несколько новых сообщений
    // Придумать, как это лучше реализовать

    // Проверяется существования уже предзагруженных сообщений из базы
    if (FLastFriend = FriendId) and (Index >= FLastRangeStart) and
      (Index <= FLastRangeEnd) then
    begin
      Mess := FLastMessageList[Index - FLastRangeStart];
      Result := True;
    end
    else
    begin
      // Загрузка новой порции сообщений
      StartLoad := Index - 20;
      EndLoad := Index + 20;

      if StartLoad < 0 then
        StartLoad := 0;

      if EndLoad >= Count then
        EndLoad := Count - 1;

      Result := GetMessageRange(FriendId, StartLoad, EndLoad);

      if Result then
        Mess := FLastMessageList[Index - FLastRangeStart];
    end;
  end
  else
  begin
    Result := False;
  end;
end;

{ *  Возвращает количество сообщений для выбранного пользователя, хранящихся
  *  в базе данных
  * }
function TMessageList.GetMessageCount(FriendId: AnsiString): Integer;
begin
  Result := Length(FTemp); // TODO: Реализовать
end;

{ *  Загружает из базы данных сообщения, которые относятся к выбранному
  *  пользователю из заданного диапазона.
  *
  *  FriendId - Идентификатор пользователя
  *  StartRange - начало диапазона выборки
  *  EndRange - конец диапазона выборки
  *
  *  Возвращает True в случае успешного выполнения выборки
  *  Все данные выборки сохраняются в FLastMessageList
  * }
function TMessageList.GetMessageRange(FriendId: AnsiString; StartRange,
  EndRange: Integer): Boolean;
var
  i: Integer;
begin
  FLastRangeStart := StartRange;
  FLastRangeEnd := EndRange;
  FLastFriend := FriendId;

  SetLength(FLastMessageList, EndRange - StartRange + 1);
  for i := Low(FLastMessageList) to High(FLastMessageList) do
  begin
    FLastMessageList[i] := FTemp[i + FLastRangeStart];
  end;

  Result := True; // TODO: Реализовать
end;

// Добавляет новое сообщение в базу данных
procedure TMessageList.SetMessage(FriendId: AnsiString; Text: DataString;
  UserMessage: Boolean);
begin
// TODO: Реализовать
end;

{ TMessageItem }

class function TMessageItem.FromText(Text: DataString): TMessageItem;
var
  Item: TMessageItem;
begin
  Item := TMessageItem.Create;
  Item.Text := Text;
  Item.Time := Now;
  Result := Item;
end;

procedure TMessageItem.SetText(const Value: DataString);
begin
  FText := Value;
end;

end.
