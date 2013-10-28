//  TextLineInfo.pas
//
//  Описаны классы, хранящие информацию о словах и строках в сообщении.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit TextLineInfo;

interface

uses
  MessageList, MessageItem;

type
  PWordInfoItem = ^TWordInfoItem;
  TWordInfoItem = record
    WordStart: Integer;
    WordLength: Integer;
    WordSizeWidth: Integer;
    WordSizeHeight: Integer;
  end;

  TWordInfoItemArray = array of PWordInfoItem;

  TWordsInfo = class
  private
    FWordCount: Integer;
    FWordItems: TWordInfoItemArray;
    function GetItem(Index: Integer): PWordInfoItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Insert(Start, Length, Width, Height: Integer);
    procedure InsertNewLine;
    procedure Get(Index: Integer; out IsNewLine: Boolean; out Start,
      Length, Width, Height: Integer);
    procedure Resize;

    property Item[Index: Integer]: PWordInfoItem read GetItem;
    property Count: Integer read FWordCount;
  end;

  PLineInfoEx = ^TLineInfoEx;
  TLineInfoEx = record
    StartPosition: Word;
    Length: Word;

    LineWidth: Integer;
    LineHeight: Integer;
  end;
  TLineInfoArray = array of PLineInfoEx;
  TTimeInfo = (tiNone, tiTime, tiDate);

  TMessageInfo = class
  private
    FLines: TLineInfoArray;
    FLineCount: Integer;
    FCalcWidth: Integer;
    FMessageItem: TMessageItem;
    FMessageHeight: Integer;
    FBottomPosition: Integer;
    FIsMessageHeader: Boolean;
    FHeaderHeight: Integer;
    FBottomMargin: Integer;
    FTimeInfo: TTimeInfo;
    function GetItem(Index: Integer): PLineInfoEx;
    function GetMessageHeight: Integer;
  public
    constructor Create(CalcWidth: Integer; MessageItem: TMessageItem);
    destructor Destroy; override;

    procedure Add(LineInfo: TLineInfoEx);

    property BottomPosition: Integer read FBottomPosition write FBottomPosition;
    property MessageBottomMargin: Integer read FBottomMargin write FBottomMargin;
    property CalcWidth: Integer read FCalcWidth write FCalcWidth;
    property Count: Integer read FLineCount;
    property MessageHeaderHeight: Integer read FHeaderHeight write FHeaderHeight;
    property Item[Index: Integer]: PLineInfoEx read GetItem;
    property IsMessageHeader: Boolean read FIsMessageHeader write FIsMessageHeader;
    property MessageItem: TMessageItem read FMessageItem;
    property MessageHeight: Integer read GetMessageHeight;
    property TimeInfo: TTimeInfo read FTimeInfo write FTimeInfo;
  end;

implementation

{ TWordsInfo }

procedure ReallocArrayInt(var a: TWordInfoItemArray; UsedSize: Integer);
begin
  if Length(a) <= UsedSize then
    SetLength(a, Length(a) + 10);
end;

constructor TWordsInfo.Create;
begin
  SetLength(FWordItems, 10);
  FWordCount := 0;
end;

destructor TWordsInfo.Destroy;
var
  i: Integer;
begin
  for i := Low(FWordItems) to High(FWordItems) do
    FreeMem(FWordItems[i]);

  SetLength(FWordItems, 0);
  inherited;
end;

procedure TWordsInfo.Get(Index: Integer; out IsNewLine: Boolean; out Start,
  Length, Width, Height: Integer);
var
  Item: TWordInfoItem;
begin
  if (Index >= 0) and (Index < FWordCount) then
  begin
    Item := FWordItems[Index]^;

    Start := Item.WordStart;
    Length := Item.WordLength;
    Width := Item.WordSizeWidth;
    Height := Item.WordSizeHeight;
    IsNewLine := (Start <= -1) or (Length <= -1);
  end;
end;

function TWordsInfo.GetItem(Index: Integer): PWordInfoItem;
begin
  if (Index >= 0) and (Index < FWordCount) then
    Result := FWordItems[Index]
  else
    Result := nil;
end;

procedure TWordsInfo.Insert(Start, Length, Width, Height: Integer);
var
  Size: Integer;
begin
  ReallocArrayInt(FWordItems, FWordCount);

  Size := SizeOf(TWordInfoItem);
  GetMem(FWordItems[FWordCount], Size);

  FWordItems[FWordCount].WordStart := Start;
  FWordItems[FWordCount].WordLength := Length;
  FWordItems[FWordCount].WordSizeWidth := Width;
  FWordItems[FWordCount].WordSizeHeight := Height;

  Inc(FWordCount);
end;

procedure TWordsInfo.InsertNewLine;
begin
  if (FWordCount <= 0) or (FWordItems[FWordCount - 1].WordStart <> -1) then
    Insert(-1, -1, -1, -1);
end;

procedure TWordsInfo.Resize;
begin
  SetLength(FWordItems, FWordCount);
end;

{ TMessageInfo }

procedure ReallocArrayLineInfoEx(var a: TLineInfoArray; UsedSize: Integer);
begin
  if Length(a) <= UsedSize then
    SetLength(a, Length(a) + 10);
end;

procedure TMessageInfo.Add(LineInfo: TLineInfoEx);
var
  Size: Integer;
begin
  ReallocArrayLineInfoEx(FLines, FLineCount);
  Size := SizeOf(TLineInfoEx);
  GetMem(FLines[FLineCount], Size);
  Move(Pointer(@LineInfo)^, FLines[FLineCount]^, Size);
  Inc(FLineCount);

  FMessageHeight := FMessageHeight + LineInfo.LineHeight;
end;

constructor TMessageInfo.Create(CalcWidth: Integer; MessageItem: TMessageItem);
begin
  FIsMessageHeader := False;
  FMessageItem := MessageItem;
  FCalcWidth := CalcWidth;
  SetLength(FLines, 10);
  FLineCount := 0;
  FMessageHeight := 0;
  FBottomMargin := 15;
end;

destructor TMessageInfo.Destroy;
var
  i: Integer;
begin
  for i := Low(FLines) to High(FLines) do
    FreeMem(FLines[i]);

  SetLength(FLines, 0);
  inherited;
end;

function TMessageInfo.GetItem(Index: Integer): PLineInfoEx;
begin
  if (Index >= 0) and (Index < FLineCount) then
    Result := FLines[Index]
  else
    Result := nil;
end;

function TMessageInfo.GetMessageHeight: Integer;
begin
  Result := FMessageHeight;

  Result := Result + FBottomMargin;

  if FIsMessageHeader then
    Result := Result + FHeaderHeight;
end;

end.
