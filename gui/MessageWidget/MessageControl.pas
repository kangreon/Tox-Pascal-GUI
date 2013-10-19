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
  Classes, SysUtils, Controls, Graphics, StringUtils, MessageList,
  MessageDraw, Messages, ActiveRegion, MessageItem, FriendItem, MessageHeader,
  MessageForm;

type
  TMessagePosition = (mpBefore, mpAfter);
  TProcSendTextFriend = procedure(Sender: TObject; Friend: TFriendItem;
    const Text: DataString) of object;

  TMessageControl = class(TCustomControl)
  private
    FActive: TActiveRegion;
    FDraw: TMessageDraw;
    FFriendSelect: TFriendItem;
    FForm: TMessageForm;
    FHeader: TMessageHeader;
    FIsDown: Boolean;
    FIsFriendSelect: Boolean;
    FMessageList: TMessageList;
    FPosition: Integer;
    FOnSendTextFriend: TProcSendTextFriend;
    procedure MessageGet(Sender: TObject; const Index: Integer; out Exist: Boolean;
      out Mess: TMessageItem);
    procedure ActiveOnMessage(Sender: TObject; RegionMessage: TRegionMessage;
      const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
    procedure FormSendText(Sender: TObject; const Text: DataString);
    procedure MessageNewMess(Sender: TObject; Friend: TFriendItem;
      Message: TMessageItem);
  protected
    procedure CreateWnd; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent; MessageList: TMessageList); reintroduce;
    destructor Destroy; override;

    procedure SelectFriend(Friend: TFriendItem);

    property OnSendTextFriend: TProcSendTextFriend read FOnSendTextFriend
      write FOnSendTextFriend;
  end;

implementation

{ TMessageControl }

constructor TMessageControl.Create(AOwner: TComponent;
  MessageList: TMessageList);
begin
  inherited Create(AOwner);

  FMessageList := MessageList;
  FMessageList.OnNewMessage := MessageNewMess;

  FActive := TActiveRegion.Create(Self);
  FActive.Align := alClient;
  FActive.OnCursorMessage := ActiveOnMessage;

  FHeader := TMessageHeader.Create(Self);
  FHeader.Align := alTop;
  FHeader.Visible := False;

  FDraw := TMessageDraw.Create(Self);
  FDraw.Align := alClient;
  FDraw.Parent := Self;
  FDraw.Visible := False;
  FDraw.OnGet := MessageGet;

  FForm := TMessageForm.Create(Self);
  FForm.Align := alBottom;
  FForm.Visible := False;
  FForm.OnSendText :=FormSendText;

  FIsFriendSelect := False;
end;

destructor TMessageControl.Destroy;
begin
  FIsFriendSelect := False;

  FForm.Free;
  FDraw.Free;
  FHeader.Free;
  FActive.Free;
  inherited;
end;

{ *  В базе появилось новое сообщение. В случае если список сообщений прокручен
  *  на последние (1-2?) сообщения, прокрутка к только что добавленному
  *  сообщению. //TODO: (Реализовать)
  * }
procedure TMessageControl.MessageNewMess(Sender: TObject; Friend: TFriendItem;
  Message: TMessageItem);
var
  MessageCount: Integer;
begin
  if Assigned(FFriendSelect) and (Assigned(Friend)) and
    FFriendSelect.ClientId.IsCompare(Friend.ClientId) then
  begin
    MessageCount := FMessageList.GetMessageCount(Friend.ClientId);
    FDraw.Redraw(MessageCount - 1, MessageCount);
  end;
end;

procedure TMessageControl.ActiveOnMessage(Sender: TObject; RegionMessage: TRegionMessage;
  const x, y: Integer; Button: TMouseButton; Shift: TShiftState);
begin
  case RegionMessage of
    rmMouseEnter: ;
    rmMouseLeave: ;
    rmMouseMove:
      begin
        if FIsDown then
        begin
          FDraw.ScrollDown(FPosition - y);
          FPosition := y;
        end;
      end;
    rmMouseDown:
      begin
        FPosition := y;
        FIsDown := True;
      end;
    rmMouseUp:
      FIsDown := False;
    rmMouseClick: ;
    rmMouseDblClick: ;
  end;
end;

function TMessageControl.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  FDraw.ScrollDown(40);
  Result := True;
end;

function TMessageControl.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  FDraw.ScrollUp(40);
  Result := True;
end;

procedure TMessageControl.CreateWnd;
begin
  inherited;
  DoubleBuffered := True;
  ControlStyle := ControlStyle - [csParentBackground];

  FActive.Parent := Self;

  FHeader.Parent := Self;
  FForm.Parent := Self;
end;

{ *  Запрос на следующее сообщение
  * }
procedure TMessageControl.MessageGet(Sender: TObject; const Index: Integer;
  out Exist: Boolean; out Mess: TMessageItem);
begin
  if not Assigned(FFriendSelect) then
    Exist := False
  else
    Exist := FMessageList.GetMessage(FFriendSelect.ClientId, Index, Mess);
end;

{ *  Событие отправки нового собщения Text собеседнику FFrienSelect.
  * }
procedure TMessageControl.FormSendText(Sender: TObject; const Text: DataString);
begin
  if FIsFriendSelect and Assigned(FOnSendTextFriend) then
  begin
    FOnSendTextFriend(Self, FFriendSelect, Text);
  end;
end;

{ *  Открытие диалога с новым пользователем
  * }
procedure TMessageControl.SelectFriend(Friend: TFriendItem);
var
  LastMessage: Integer;
  MessageCount: Integer;
begin
  FIsFriendSelect := Assigned(Friend);

  FDraw.Visible := FIsFriendSelect;
  FHeader.Visible := FIsFriendSelect;
  FForm.Visible := FIsFriendSelect;

  if FIsFriendSelect then
  begin
    FFriendSelect := Friend;

    MessageCount := FMessageList.GetMessageCount(FFriendSelect.ClientId);
    LastMessage := MessageCount - 1;
    FDraw.Redraw(LastMessage, MessageCount);

    FHeader.SelectFriend(FFriendSelect);
  end;
end;

procedure TMessageControl.WndProc(var Message: TMessage);
{$IFDEF FPC}
  function GET_WHEEL_DELTA_WPARAM(wp: longint): smallint;
  begin
    Result := smallint(wp shr 16);
  end;
{$ENDIF}

begin
  inherited;
  case Message.Msg of
    CM_MOUSEENTER:
      begin
        if GetFocus <> FForm.FormHandle then
          SetFocus;
      end;

    WM_LBUTTONDOWN:
      begin
        if GetFocus <> Handle then
          SetFocus;
      end;

{$IFDEF FPC}
    LM_MOUSEWHEEL:
      begin
        if GET_WHEEL_DELTA_WPARAM(Message.wParam) > 0 then
          DoMouseWheelUp([], Point(0, 0))
        else
          DoMouseWheelDown([], Point(0, 0));

        Message.Result := 1;
      end;
{$ENDIF}
  end;
end;

end.
