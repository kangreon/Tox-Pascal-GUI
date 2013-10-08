//  MessageDraw.pas
//
//  Рисует сообщения пользователей. Вывод осуществляется с нижней части
//  компонента и идет вверх. Самое новое сообщение всегда снизу.
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//

// При изменении размера, переопределять размеры элементов
unit MessageDraw;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  {$IFDEF FPC}LazUTF8,{$ENDIF} Classes, {$IFNDEF FPC}Types, {$ENDIF}SysUtils, Controls, Graphics, StringUtils, MessageList,
  TextLineInfo, Math;

type
  TWordArray = array of Word;

  PDrawItem = ^TDrawItem;
  TDrawItem = record
    BottomPosition: Integer;    // Положение элемента относительно нижнего края
    Height: Integer;            // Высота, занимаемая этим сообщением
    CalcWidth: Integer;         // Ширина, для которой актуальна информация
    MessageItem: TMessageItem;  // Само выводимое сообщение
    LineStart: TWordArray;      //
    LineWidth: TWordArray;      // Ширина всех строк
    LinesCount: Integer;        // Количество строк, занимаемое сообщением
  end;
  TDrawItemList = array of PDrawItem;

  TProcGet = procedure(Sender: TObject; const Index: Integer; out Exist: Boolean;
    out Mess: TMessageItem) of object;

  TMessageDraw = class(TGraphicControl)
  private
    FDrawItems: TDrawItemList;
    FCalcTime: TDateTime;
    FLineInfo: TTextLineInfo;
    FIsCreateList: Boolean;

    FTextHeight: Integer;
    FTextMarginLeft: Integer;
    FTextMarginRight: Integer;

    FBottomMessageIndex: Integer;
    FOnGet: TProcGet;
    FBottomMessagePosition: Integer;
    procedure SetDrawFont;
    function EventGet(Index: Integer; out Mess: TMessageItem): Boolean;
    procedure DrawItem(Item: PDrawItem);
    procedure RecreateItems;
    function CalcBreakItem(MessageItem: TMessageItem; MaxWidth: Integer): PDrawItem;
    function GetOldPositionInfo(Mess: TMessageItem; NewWidth: Integer;
      var ItemInfo: PDrawItem): Boolean;
    procedure CombineDrawInfoArrays(var ItemOld, ItemNew: TDrawItemList);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Redraw(BottomMessageIndex: Integer);

    property BottomMessageIndex: Integer read FBottomMessageIndex;
    property BottomMessagePosition: Integer read FBottomMessagePosition;

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

  FLineInfo := TTextLineInfo.Create;
  FBottomMessageIndex := -1;
  FIsCreateList := False;

  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Name := 'DejaVu Sans';
    Bitmap.Canvas.Font.Height := 13;
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.Brush.Color := Color;

    FTextHeight := Bitmap.Canvas.TextHeight('Q');
    FTextMarginLeft := 40;
    FTextMarginRight := Bitmap.Canvas.TextWidth(FormatDateTime('  dd/mm/yyyy  ', Now));
  finally
    Bitmap.Free;
  end;
end;

destructor TMessageDraw.Destroy;
begin
  FLineInfo.Free;
  inherited;
end;

{ *  Выводит заранее подготовленное сообщение на экран
  * }
procedure TMessageDraw.DrawItem(Item: PDrawItem);
var
  i, c: Integer;
  LeftDraw, TopDraw: Integer;
  Text, TextOut: DataString;
  StartCopy, CountCopy: Integer;
begin
  c := Item.LinesCount;
  LeftDraw := FTextMarginLeft;
  Text := Item.MessageItem.Text;
  StartCopy := 1;

  for i := 0 to c - 1 do
  begin
    TopDraw := ClientHeight - Item.BottomPosition - ((c - i) * FTextHeight) ;

    // Вывод времени
    if i = 0 then
    begin
      Canvas.Font.Style := [fsBold];
      Canvas.TextOut(ClientWidth - FTextMarginRight, TopDraw,
        FormatDateTime(' hh:nn:ss ', Item.MessageItem.Time));
      Canvas.Font.Style := [];
    end;

    if c > 1 then
    begin
      StartCopy := Item.LineStart[i];
      CountCopy := Item.LineWidth[i];
      TextOut := {$IFDEF FPC}UTF8Copy{$ELSE}Copy{$ENDIF}(Text, StartCopy, CountCopy);
      //StartCopy := StartCopy + CountCopy;
    end
    else
      TextOut := Text;

    Canvas.TextOut(LeftDraw, TopDraw, TextOut);
  end;
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
      DrawItem(FDrawItems[i]);
  end;
  PaintTime := Now - PaintTime;

  Canvas.TextOut(0, 0, 'Message count: ' + IntToStr(Length(FDrawItems)));
  Canvas.TextOut(0, 13, 'Calc time: ' + FormatDateTime('ss.zzz', FCalcTime));
  Canvas.TextOut(0, 26, 'Paint time: ' + FormatDateTime('ss.zzz', PaintTime));
end;

procedure ReallocArray(var a: TWordArray; UsedSize: Integer);
begin
  if Length(a) <= UsedSize then
    SetLength(a, Length(a) + 10);
end;

{ *  Процедура рассчета переносов в элементах диалога
  * }
function TMessageDraw.CalcBreakItem(MessageItem: TMessageItem;
  MaxWidth: Integer): PDrawItem;
var
  Item: PDrawItem;
  CharCount, CharStart: Integer;
  CharWidth, LineWidth: Integer;
  Text, TextLaz: DataString;
  i, c: Integer;
  WordsInfo: PWordInfo;

  SelectChar: DataString;
  IsNextLine: Boolean;
  IsInsertStartChar: Boolean;
  IsSpace: Boolean;
  StartLineIndex: Integer;
begin
  New(Item);
  Result := Item;

  Text := MessageItem.Text;
  {$IFDEF FPC}
  TextLaz := UTF8Decode(MessageItem.Text);
  {$ENDIF}
  LineWidth := 0;
  CharStart := 1;

  Item.LinesCount := 0;
  Item.MessageItem := MessageItem;
  Item.CalcWidth := MaxWidth;

  SetLength(Item.LineWidth, 10);
  SetLength(Item.LineStart, 10);

  c := Length({$IFDEF FPC}TextLaz{$ELSE}Text{$ENDIF});
  i := 0;

  // Первый символ строки
  Item.LineStart[Item.LinesCount] := 1;
  IsInsertStartChar := False;

  while i <= c do
  begin
    Inc(i);

    // Копирование активного символа
    SelectChar := {$IFDEF FPC}TextLaz[i]{$ELSE}Text[i]{$ENDIF};
    if Length(SelectChar) > 0 then
    begin
      IsNextLine := SelectChar[1] in [#$0A, #$0D];
      IsSpace := SelectChar[1] in [#$09, #$20];
    end;
    CharWidth := Canvas.TextWidth({$IFDEF FPC}UTF8Encode{$ENDIF}(SelectChar));

    if (LineWidth + CharWidth < MaxWidth) and not IsNextLine then
    begin
      if not IsInsertStartChar then
      begin
        if IsSpace then
          Continue;

        ReallocArray(Item.LineStart, Item.LinesCount);
        Item.LineStart[Item.LinesCount] := i;

        IsInsertStartChar := True;
      end;

      Inc(LineWidth, CharWidth);
    end
    else
    begin
      if not IsInsertStartChar then
        Continue;

      LineWidth := 0;
      IsInsertStartChar := False;

      CharCount := i - Item.LineStart[Item.LinesCount];
      ReallocArray(Item.LineWidth, Item.LinesCount);
      Item.LineWidth[Item.LinesCount] := CharCount;
      Item.LinesCount := Item.LinesCount + 1;

      if not IsNextLine then
        Dec(i);
    end;
  end;

  if IsInsertStartChar then
  begin
    CharCount := i - Item.LineStart[Item.LinesCount];
    if CharCount > 0 then
    begin
      ReallocArray(Item.LineWidth, Item.LinesCount);
      Item.LineWidth[Item.LinesCount] := CharCount;
      Item.LinesCount := Item.LinesCount + 1;
    end;
  end;

  Item.Height := Item.LinesCount * FTextHeight;
end;

{ *  Ищет уже просчитанную информацию для сообщения в старом списке отрисовки.
  *  Возвращает True, если информация для переданного сообщения и нового
  *  размера найдена. Результат возвращается через ItemInfo
  * }
function TMessageDraw.GetOldPositionInfo(Mess: TMessageItem; NewWidth: Integer;
  var ItemInfo: PDrawItem): Boolean;
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
  ActiveItem: TMessageItem;
  BottomPosition: Integer;
  ItemInfo: PDrawItem;
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
        if not GetOldPositionInfo(ActiveItem, MaxWidth, ItemInfo) then
          ItemInfo := CalcBreakItem(ActiveItem, MaxWidth);

        // Добавление элемента в массив отрисовки
        if Length(ItemList) <= ItemListCount then
        begin
          SetLength(ItemList, Length(ItemList) + 20);
        end;

        ItemInfo.BottomPosition := BottomPosition;
        ItemList[ItemListCount] := ItemInfo;
        Inc(ItemListCount);

        BottomPosition := BottomPosition + ItemInfo.Height;
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
      if not GetOldPositionInfo(ActiveItem, MaxWidth, ItemInfo) then
        ItemInfo := CalcBreakItem(ActiveItem, MaxWidth);

      if Length(ItemList) <= ItemListCount then
      begin
        SetLength(ItemList, Length(ItemList) + 20);
      end;

      ItemInfo.BottomPosition := BottomPosition;
      ItemList[ItemListCount] := ItemInfo;
      Inc(ItemListCount);

      BottomPosition := BottomPosition - ItemInfo.Height;
      ActiveElementIndex := ActiveElementIndex + 1;
    end;

    // Новый список составлен. ЗаменаОсновного списка отрисовки новым.
    // TODO: Возможна утечка памяти
    SetLength(ItemList, ItemListCount);

    //TODO: Вроде, утечки памяти нету?
    CombineDrawInfoArrays(FDrawItems, ItemList);
//    FDrawItems := ItemList;

    FIsCreateList := True;
  end
  else
  begin
    ActiveItem := nil
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
      Dispose(ItemOld[i]);
  end;

  SetLength(ItemOld, 0);
  ItemOld := ItemNew;
end;

{ *  Выполняет очистку кэша и перерисовывает все сообщения, начиная с
  *  указанного. Указанное сообщение будет в самом низу компонента.
  * }
procedure TMessageDraw.Redraw(BottomMessageIndex: Integer);
begin
  FIsCreateList := False;
  FBottomMessageIndex := BottomMessageIndex;
  //RecreateItems;
  Resize;
end;

procedure TMessageDraw.Resize;
begin
  inherited;
  FIsCreateList := False;

  FCalcTime := Now;
  RecreateItems;
  FCalcTime := Now - FCalcTime;
end;

procedure TMessageDraw.SetDrawFont;
begin
  if not Assigned(Parent) then
    Exit;

  Canvas.Font.Name := 'DejaVu Sans';
  Canvas.Font.Height := 13;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
end;

end.
