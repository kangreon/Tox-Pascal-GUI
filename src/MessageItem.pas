unit MessageItem;

interface

uses
  StringUtils, SysUtils, FriendList, FriendItem;

type
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
    FIsRead: Boolean;
    FBaseId: Int64;
    FNumber: Integer;
    procedure SetText(const Value: DataString);
  public
    class function FromText(Text: DataString): TMessageItem;
    property BaseId: Int64 read FBaseId write FBaseId;
    property Data: TObject read FData write FData;
    property Friend: TFriendItem read FFriend write FFriend;
    property Index: Integer read FIndex write FIndex;
    // Время отправки сообщения
    property Time: TDateTime read FTime write FTime;
    // Текст сообщения
    property Text: DataString read FText write SetText;

    property IsMy: Boolean read FUserMessage write FUserMessage;
    // Состояние доставки сообщения пользователю
    property IsSend: Boolean read FStatusSend write FStatusSend;
    property IsRead: Boolean read FIsRead write FIsRead;
    // Номер сообщения в базе данных
    property Number: Integer read FNumber write FNumber;
  end;

  TMessageArray = array of TMessageItem;

implementation

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
