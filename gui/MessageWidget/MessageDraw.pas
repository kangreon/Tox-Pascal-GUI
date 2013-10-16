//  MessageDraw.pas
//
//  Рисует сообщения пользователей. Вывод осуществляется с нижней части
//  компонента и идет вверх. Самое новое сообщение всегда снизу.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit MessageDraw;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  {$IFDEF FPC}
  LazUTF8,
  {$ELSE}
  Types,
  {$ENDIF}
  Classes, SysUtils, Controls, Graphics, StringUtils, MessageList, MessageItem,
  TextLineInfo, Math;

type
  TDrawItemList = array of TMessageInfo;

  TProcGet = procedure(Sender: TObject; const Index: Integer; out Exist: Boolean;
    out Mess: TMessageItem) of object;

  TMessageDraw = class(TGraphicControl)
  private
    FDrawItems: TDrawItemList;
    FCalcTime: TDateTime;
    FIsCreateList: Boolean;
    FIsCalcPositionForLastMessage: Boolean;

    FSpaceWidth: Integer;
    FTextHeight: Integer;
    FTextMarginLeft: Integer;
    FTextMarginRight: Integer;

    FBottomMessageIndex: Integer;
    FOnGet: TProcGet;
    FBottomMessagePosition: Integer;
    FFormatPaintDate: DataString;
    FFormatPaintTime: DataString;
    procedure SetDrawFont;
    function EventGet(Index: Integer; out Mess: TMessageItem): Boolean;
    procedure DrawItem(Item: TMessageInfo; IsDrawName: Boolean);
    procedure RecreateItems;
    function CalcBreakItem(MessageItem: TMessageItem;
      MaxWidth: Integer): TMessageInfo;
    function GetOldPositionInfo(Mess: TMessageItem; NewWidth: Integer;
      var ItemInfo: TMessageInfo): Boolean;
    procedure CombineDrawInfoArrays(var ItemOld, ItemNew: TDrawItemList);
    function CalcWordPosition(MessageItem: TMessageItem): TWordsInfo;
    procedure DrawDividingLine(Mess: TMessageInfo);
    procedure ScrollPage(Value: Integer);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Redraw(BottomMessageIndex: Integer);
    procedure ScrollDown(Value: Integer);
    procedure ScrollUp(Value: Integer);

    property BottomMessageIndex: Integer read FBottomMessageIndex;
    property BottomMessagePosition: Integer read FBottomMessagePosition;
    property FormatPaintDate: DataString read FFormatPaintDate write FFormatPaintDate;
    property FormatPaintTime: DataString read FFormatPaintTime write FFormatPaintTime;

    property OnGet: TProcGet read FOnGet write FOnGet;
  end;


implementation

{ TMessageDraw }

constructor TMessageDraw.Create(AOwner: TComponent);
var
  Bitmap: TBitmap;
begin
  inherited;
  Cursor := crIBeam;
  FBottomMessageIndex := -1;
  FIsCreateList := False;
  FIsCalcPositionForLastMessage := False;

  // Форматы вывода даты и времени
  FFormatPaintDate := '  dd/mm/yyyy  ';
  FFormatPaintTime := '  hh:nn:ss  ';

  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Name := 'DejaVu Sans';
    Bitmap.Canvas.Font.Height := 13;
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.Brush.Color := Color;

    FSpaceWidth := Bitmap.Canvas.TextWidth(' ');
    FTextHeight := Bitmap.Canvas.TextHeight('Q');
    FTextMarginLeft := 80;

    FTextMarginRight := Max(
      Bitmap.Canvas.TextWidth(FormatDateTime(FormatPaintDate, Now)),
      Bitmap.Canvas.TextWidth(FormatDateTime(FormatPaintTime, Now))
    );
  finally
    Bitmap.Free;
  end;
end;

destructor TMessageDraw.Destroy;
begin
  inherited;
end;

{ *  Выводит заранее подготовленное сообщение на экран
  * }
procedure TMessageDraw.DrawItem(Item: TMessageInfo; IsDrawName: Boolean);
var
  i, c: Integer;
  LeftDraw, TopDraw: Integer;
  Text, TextOut, DateTimeOut: DataString;
  StartCopy, CountCopy: Integer;
  LineInfo: TLineInfoEx;
begin
  c := Item.Count;
  LeftDraw := FTextMarginLeft;
  Text := Item.MessageItem.Text;

  TopDraw := ClientHeight - Item.BottomPosition - Item.BottomMargin;
  for i := c - 1 downto 0 do
  begin
    LineInfo := Item.Item[i]^;

    TopDraw := TopDraw - LineInfo.LineHeight;

    if i = 0 then
    begin
      // Первая строчка сообщения
      Canvas.Font.Style := [fsBold];

      DateTimeOut := FormatDateTime(FormatPaintTime, Item.MessageItem.Time);

      Canvas.TextOut(ClientWidth - Canvas.TextWidth(DateTimeOut), TopDraw,
        DateTimeOut);

      if IsDrawName then
        Canvas.TextOut(2, TopDraw, Item.MessageItem.Friend.UserName);

      Canvas.Font.Style := [];
    end;

    if c > 1 then
    begin
      StartCopy := LineInfo.StartPosition;
      CountCopy := LineInfo.Length;

      TextOut := {$IFDEF FPC}UTF8Copy{$ELSE}Copy{$ENDIF}(Text, StartCopy, CountCopy);
    end
    else
      TextOut := Text;

    Canvas.TextOut(LeftDraw, TopDraw, TextOut);
  end;
end;

{ *  Рисует линию, разделяющую сообщения разных пользователей
  * }
procedure TMessageDraw.DrawDividingLine(Mess: TMessageInfo);
var
  DrawTop: Integer;
begin
  DrawTop := ClientHeight - Mess.BottomPosition - Mess.MessageHeight;

  Canvas.Pen.Color := $d2d2d2;
  Canvas.Pen.Style := psDot;
  Canvas.MoveTo(0, DrawTop);
  Canvas.LineTo(ClientWidth, DrawTop);
end;

function TMessageDraw.EventGet(Index: Integer; out Mess: TMessageItem): Boolean;
begin
  Result := False;

  if Assigned(FOnGet) then
    FOnGet(Self, Index, Result, Mess);
end;

procedure TMessageDraw.Paint;
var
  i: Integer;
  PaintTime: TDateTime;
begin
  inherited;
  SetDrawFont;

  PaintTime := Now;
  if FIsCreateList then
  begin
    for i := Low(FDrawItems) to High(FDrawItems) do
    begin
      if FDrawItems[i].IsMessageHeader then
        DrawDividingLine(FDrawItems[i]);

      DrawItem(FDrawItems[i], FDrawItems[i].IsMessageHeader);
    end;
  end;
  PaintTime := Now - PaintTime;

  Canvas.TextOut(0, 0, 'Message draw count: ' + IntToStr(Length(FDrawItems)));
  Canvas.TextOut(0, 13, 'Calc time: ' + FormatDateTime('ss.zzz', FCalcTime));
  Canvas.TextOut(0, 26, 'Paint time: ' + FormatDateTime('ss.zzz', PaintTime));
end;

{ *  Рассчитывает позиции и размеры всех слов в строке
  * }
function TMessageDraw.CalcWordPosition(MessageItem: TMessageItem): TWordsInfo;
var
  CharSize: TSize;
  IsNextLine: Boolean;
  IsOldNextLine: Boolean;
  IsSpace: Boolean;
  IsOldSpace: Boolean;
  IsStartChar: Boolean;
  MaxHeight: Integer;
  SelectChar: WideChar;
  StartCharPosition: Integer;
  Text: DataString;
  TextLength, i: Integer;
  TextWidth: Integer;
  WordsInfo: TWordsInfo;
begin
  if Assigned(MessageItem.Data) and (MessageItem.Data is TWordsInfo) then
  begin
    Result := TWordsInfo(MessageItem.Data);
    Exit;
  end;

  {$IFDEF FPC}
  Text := UTF8Decode(MessageItem.Text);
  {$ELSE}
  Text := MessageItem.Text;
  {$ENDIF}
  TextLength := Length(Text);

  IsStartChar := False;
  IsOldNextLine := False;
  IsOldSpace := False;
  MaxHeight := 0;
  TextWidth := 0;
  StartCharPosition := 1;

  WordsInfo := TWordsInfo.Create;
  i := 0;
  while i < TextLength do
  begin
    Inc(i);

    SelectChar := Text[i];
    {$IFDEF NEW_DELPHI}
    IsNextLine := CharInSet(SelectChar, [#$000A, #$000D]);
    IsSpace := CharInSet(SelectChar, [#$0009, #$0020]);
    {$ELSE}
    IsNextLine := SelectChar in [#$000A, #$000D];
    IsSpace := SelectChar in [#$0009, #$0020];
    {$ENDIF}
    if (IsNextLine and IsOldNextLine) or (IsSpace and IsOldSpace) then
    begin
      IsOldNextLine := IsNextLine;
      IsOldSpace := IsSpace;
      Continue;
    end;

    IsOldNextLine := IsNextLine;
    IsOldSpace := IsSpace;

    if IsNextLine or IsSpace then
    begin
      if IsNextLine then
      begin
        if IsStartChar then
          WordsInfo.Insert(StartCharPosition, i - StartCharPosition, TextWidth,
            MaxHeight);

        WordsInfo.InsertNewLine;
      end;

      if IsSpace and IsStartChar then
      begin
        WordsInfo.Insert(StartCharPosition, i - StartCharPosition, TextWidth,
          MaxHeight);
      end;

      IsStartChar := False;
    end
    else
    begin
      {$IFDEF FPC}
      CharSize := Canvas.TextExtent(UTF8Encode(WideString(SelectChar)));
      {$ELSE}
      CharSize := Canvas.TextExtent(SelectChar);
      {$ENDIF}

      if not IsStartChar then
      begin
        TextWidth := CharSize.cx;
        MaxHeight := CharSize.cy;

        IsStartChar := True;
        StartCharPosition := i;
      end
      else
      begin

        TextWidth := TextWidth + CharSize.cx;
        MaxHeight := Max(MaxHeight, CharSize.cy);
      end;
    end;
  end;

  if IsStartChar then
  begin
    WordsInfo.Insert(StartCharPosition, i - StartCharPosition, TextWidth,
      MaxHeight);
  end;

  WordsInfo.Resize;
  MessageItem.Data := WordsInfo;
  Result := WordsInfo;
end;

{ *  Процедура рассчета переносов в элементах диалога
  * }
function TMessageDraw.CalcBreakItem(MessageItem: TMessageItem;
  MaxWidth: Integer): TMessageInfo;
var
  LineWidth: Integer;
  i, c: Integer;

  LineInfo: TLineInfoEx;
  MessageInfo: TMessageInfo;
  WordsInfo: TWordsInfo;
  wIsNewLine: Boolean;
  wStart, wLength: Integer;
  wWidth, wHeight: Integer;
  LineLength: Integer;
begin
  // Рассчет размера каждого слова в сообщении
  WordsInfo := CalcWordPosition(MessageItem);
  MessageInfo := TMessageInfo.Create(MaxWidth, MessageItem);
  LineWidth := 0;
  LineLength := 0;

  c := WordsInfo.Count;
  i := -1;
  while i + 1 < c do
  begin
    Inc(i);

    WordsInfo.Get(i, wIsNewLine, wStart, wLength, wWidth, wHeight);

    if wIsNewLine then
    begin
      if LineWidth <= 0 then
        Continue;

      LineInfo.LineWidth := LineWidth;
      LineInfo.Length := LineLength;
      MessageInfo.Add(LineInfo);

      LineWidth := 0;
      LineLength := 0;
    end
    else
    begin
      if (LineWidth + FSpaceWidth + wWidth > MaxWidth) then
      begin
        if LineWidth > 0 then
        begin
          LineInfo.LineWidth := LineWidth;
          LineInfo.Length := LineLength;
          MessageInfo.Add(LineInfo);

          LineWidth := 0;
          LineLength := 0;
          Dec(i);
        end
        else
        begin
          //TODO: ИСПРАВИТЬ!! Пересчет размеров для переноса по буквам
          LineInfo.StartPosition := wStart;
          LineInfo.LineHeight := wHeight;
          LineInfo.LineWidth := wWidth;
          LineInfo.Length := wLength;
          MessageInfo.Add(LineInfo);

          LineWidth := 0;
          LineLength := 0;
        end;
      end
      else
      begin
        // Увеличение количества слов в линии
        if LineWidth = 0 then
        begin
          LineInfo.StartPosition := wStart;
          LineInfo.LineHeight := wHeight;
        end;

        LineWidth := LineWidth + FSpaceWidth + wWidth;
        LineLength := LineLength + wLength + 1;
        LineInfo.LineHeight := Max(LineInfo.LineHeight, wHeight);
      end;
    end;
  end;

  if LineWidth > 0 then
  begin
    LineInfo.LineWidth := LineWidth;
    LineInfo.Length := LineLength;
    MessageInfo.Add(LineInfo);
  end;

  Result := MessageInfo;
end;

{ *  Ищет уже просчитанную информацию для сообщения в старом списке отрисовки.
  *  Возвращает True, если информация для переданного сообщения и нового
  *  размера найдена. Результат возвращается через ItemInfo
  * }
function TMessageDraw.GetOldPositionInfo(Mess: TMessageItem; NewWidth: Integer;
  var ItemInfo: TMessageInfo): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := Low(FDrawItems) to High(FDrawItems) do
  begin
    if (FDrawItems[i].MessageItem = Mess) and
      (FDrawItems[i].CalcWidth = NewWidth) then
    begin
      Result := True;
      ItemInfo := FDrawItems[i];
      Break;
    end;
  end;
end;

{ *  Процедура переращета размеров элементов диалога. Изначально известна
  *  только позиция одного элемента. От него происходит рассчет элементов
  *  вниз, а после вверх. Здесь уже известно, что базовый элемент находится
  *  на своем месте. Проверку на нахождение базового элемента провести в др.
  *  месте.
  * }
procedure TMessageDraw.RecreateItems;
var
  ActiveElementIndex: Integer;
  ActiveItem, PrevItem: TMessageItem;
  BottomPosition: Integer;
  MessageInfo: TMessageInfo;
  MaxWidth: Integer;
  ItemList: TDrawItemList;
  ItemListCount: Integer;
begin
  SetDrawFont;

  MaxWidth := ClientWidth - FTextMarginLeft - FTextMarginRight;
  FIsCreateList := MaxWidth > 20;

  BottomPosition := FBottomMessagePosition;
  ActiveElementIndex := FBottomMessageIndex;

  // Выбор элемента для которого известно пложение.
  if EventGet(ActiveElementIndex, ActiveItem) and FIsCreateList then
  begin
    // Инициализация массива отрисовки
    SetLength(ItemList, Max(20, Length(FDrawItems)));
    ItemListCount := 0;

    // Рассчет позиции элементов начиная от базового и до самого верхнего края
    // компонента. Или до того момента, пока существуют еще сообщения.
    while (BottomPosition <= ClientHeight) and (ActiveElementIndex >= 0) do
    begin
      if EventGet(ActiveElementIndex, ActiveItem) then
      begin
        if not EventGet(ActiveElementIndex - 1, PrevItem) then
          PrevItem := nil;

        MessageInfo := nil;
        if not GetOldPositionInfo(ActiveItem, MaxWidth, MessageInfo) then
          MessageInfo := CalcBreakItem(ActiveItem, MaxWidth);

        // Добавление элемента в массив отрисовки
        if Length(ItemList) <= ItemListCount then
        begin
          SetLength(ItemList, Length(ItemList) + 20);
        end;

        MessageInfo.IsMessageHeader := (not Assigned(PrevItem)) or
          (PrevItem.Friend <> ActiveItem.Friend);
        MessageInfo.HeaderHeight := 20;

        if FIsCalcPositionForLastMessage then
        begin
          BottomPosition := BottomPosition - MessageInfo.MessageHeight;
          FIsCalcPositionForLastMessage := False;
          FBottomMessagePosition := BottomPosition;
        end;

        MessageInfo.BottomPosition := BottomPosition;
        ItemList[ItemListCount] := MessageInfo;
        Inc(ItemListCount);

        BottomPosition := BottomPosition + MessageInfo.MessageHeight;
        ActiveElementIndex := ActiveElementIndex - 1;
      end
      else
      begin
        //TODO: Быть ошибки не должно. После предусмотреть.
        raise Exception.Create('TODO: Быть ошибки не должно. После предусмотреть.');
      end;
    end;

    // Расчет элементов после базового вниз компонента до самого конца
    ActiveElementIndex := FBottomMessageIndex + 1;
    BottomPosition := FBottomMessagePosition;

    while (BottomPosition > 0) and
      EventGet(ActiveElementIndex, ActiveItem) do
    begin
      // Получение информации для нового элемента списка
      if not GetOldPositionInfo(ActiveItem, MaxWidth, MessageInfo) then
        MessageInfo := CalcBreakItem(ActiveItem, MaxWidth);

      if Length(ItemList) <= ItemListCount then
      begin
        SetLength(ItemList, Length(ItemList) + 20);
      end;

      MessageInfo.BottomPosition := BottomPosition;
      ItemList[ItemListCount] := MessageInfo;
      Inc(ItemListCount);

      BottomPosition := BottomPosition - MessageInfo.MessageHeight;
      ActiveElementIndex := ActiveElementIndex + 1;
    end;

    // Новый список составлен. ЗаменаОсновного списка отрисовки новым.
    // TODO: Возможна утечка памяти
    SetLength(ItemList, ItemListCount);

    //TODO: Вроде, утечки памяти нету?
    CombineDrawInfoArrays(FDrawItems, ItemList);
    FIsCreateList := True;
  end
  else
  begin
    ActiveItem := nil;
  end;
end;

{ *  Объединяет два массива в один путем удаления из первого массива не
  *  использующихся во втором массиве элементов.
  * }
procedure TMessageDraw.CombineDrawInfoArrays(var ItemOld, ItemNew: TDrawItemList);
var
  i, j: Integer;
  ItemExist: Boolean;
begin
  for i := Low(ItemOld) to High(ItemOld) do
  begin
    ItemExist := False;
    for j := Low(ItemNew) to High(ItemNew) do
    begin
      if ItemOld[i] = ItemNew[j] then
      begin
        ItemExist := True;
        Break;
      end;
    end;

    if not ItemExist then
      ItemOld[i].Free;
  end;

  SetLength(ItemOld, 0);
  ItemOld := ItemNew;
end;

{ *  Выполняет очистку кэша и перерисовывает все сообщения, начиная с
  *  указанного. Указанное сообщение будет в самом низу компонента.
  * }
procedure TMessageDraw.Redraw(BottomMessageIndex: Integer);
var
  Item: TMessageInfo;
begin
  for Item in FDrawItems do
    Item.Free;

  SetLength(FDrawItems, 0);

  FIsCreateList := False;
  FBottomMessageIndex := BottomMessageIndex;
  FBottomMessagePosition := 0;
  //RecreateItems;
  Resize;
  Invalidate;
end;

procedure TMessageDraw.Resize;
begin
  inherited;
  FIsCreateList := False;

  FCalcTime := Now;
  RecreateItems;
  FCalcTime := Now - FCalcTime;
end;

procedure TMessageDraw.ScrollDown(Value: Integer);
begin
  ScrollPage(Value);
end;

procedure TMessageDraw.ScrollUp(Value: Integer);
begin
  ScrollPage(-Value);
end;

procedure TMessageDraw.ScrollPage(Value: Integer);
var
  Item: TMessageInfo;
  MessItem: TMessageItem;
  IsScroll: Boolean;
  OldPosition: Integer;
begin
  OldPosition := FBottomMessagePosition;
  FBottomMessagePosition := FBottomMessagePosition + Value;
  IsScroll := False;

  if Length(FDrawItems) > 0 then
  begin
    Item := FDrawItems[0];

    if (FBottomMessagePosition < 0) and (Abs(FBottomMessagePosition) > Item.MessageHeight) then
    begin
      FBottomMessageIndex := FBottomMessageIndex - 1;
      FBottomMessagePosition := FBottomMessagePosition + Item.MessageHeight;
      IsScroll := True;
    end
    else if FBottomMessagePosition > 0 then
    begin
      if EventGet(FBottomMessageIndex + 1, MessItem) then
      begin
        IsScroll := True;
        FIsCalcPositionForLastMessage := True;
        FBottomMessageIndex := FBottomMessageIndex + 1;
        {
          Переключение активного сообщения. Выбирается следующее сообщение из
          списка. т.к. для него не известна высота отрисовки, устанавливается
          флаг FIsCalcPositionForLastMessage, которы рассчитает высоту нового
          активного текста и установит его позицию при первом ресайзе элементов.
        }
      end
      else
      begin
        // Это самое последнее сообщение
        FBottomMessagePosition := 0;
        IsScroll := True;
      end;
    end
    else
      IsScroll := True
  end;

  if (OldPosition <> FBottomMessagePosition) and (IsScroll) then
  begin
    Resize;
    Invalidate;
  end;
end;


procedure TMessageDraw.SetDrawFont;
begin
  if not Assigned(Parent) then
    Exit;

  Canvas.Font.Name := 'DejaVu Sans';
  Canvas.Font.Height := 13;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
end;

end.
