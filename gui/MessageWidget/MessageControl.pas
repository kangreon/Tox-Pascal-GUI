//  MessageControl.pas
//
//  Виджет вывода сообщений пользователей. Содержит в себе следующие элементы:
//      * список сообщений
//      * полоса прокрутки
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit MessageControl;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  Classes, Controls, Graphics, StringUtils, MessageList, MessageDraw;

type
  TMessagePosition = (mpBefore, mpAfter);

  TMessageControl = class(TCustomControl)
  private
    FFrienSelect: AnsiString;
    FMessageList: TMessageList;
    FDraw: TMessageDraw;
    procedure MessageGet(Sender: TObject; const Index: Integer; out Exist: Boolean;
      out Mess: TMessageItem);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent; MessageList: TMessageList); reintroduce;
    destructor Destroy; override;

    procedure SelectFriend(FriendId: AnsiString);
  end;

implementation

{ TMessageControl }

constructor TMessageControl.Create(AOwner: TComponent;
  MessageList: TMessageList);
begin
  inherited Create(AOwner);
  FMessageList := MessageList;
end;

destructor TMessageControl.Destroy;
begin

  inherited;
end;

procedure TMessageControl.CreateWnd;
begin
  inherited;
  DoubleBuffered := True;
  {$IFNDEF FPC}
  ParentBackground := False;
  {$ENDIF}

  FDraw := TMessageDraw.Create(Self);
  FDraw.Align := alClient;
  FDraw.Parent := Self;
  FDraw.OnGet := MessageGet;
  FDraw.Redraw(FMessageList.GetMessageCount('') - 1);
end;

{ *  Запрос на следующее сообщение
  * }
procedure TMessageControl.MessageGet(Sender: TObject; const Index: Integer;
  out Exist: Boolean; out Mess: TMessageItem);
begin
  Exist := FMessageList.GetMessage(FFrienSelect, Index, Mess);
end;

{ *  Открытие диалога с новым пользователем
  * }
procedure TMessageControl.SelectFriend(FriendId: AnsiString);
begin
  FFrienSelect := FriendId;
end;

end.
