unit TextLineInfo;

interface

type
  // Определяет тип линии
  // ltNone             - Обычная линия без выделения
  // ltStartSelect      - Линия содержит начало выделения
  // ltEndSelect        - Линия содержит конец выделения
  // ltStartEndSelect   - Линия содержит начало и конец выделения
  TSelectType = (ltStartSelect, ltEndSelect, ltStartEndSelect, ltNone);

  PLineInfo = ^TLineInfo;
  TLineInfo = record
    SelectType: TSelectType;

    // Первый символ линии
    StartChar: Word;

    // Определяет длину региона.
    // ltNone             - Количество символов до конца линии
    // ltStartSelect      - Количество символов до начала выделения
    // ltEndSelect        - Количество выделенных символов до начала обычных
    // ltStartEndSelect   - Количество символов до начала выделения
    Length: Word;

    // Первый символ второго региона
    StartChar2: Word;

    // Определяет длину второрго региона
    // ltStartSelect      - Количество выделенных символов до конца строки
    // ltEndSelect        - Количество обычных символов до конца строки
    // ltStartEndSelect   - Количество выделенных символов до начала обычных символшов
    Length2: Word;

    // Первый символ третьего региона
    StartChar3: Word;

    // Определяет длину третьего региона
    // ltStartEndSelect   - Количество обычных символов до конца строки
    Length3: Word;

  end;

  TTextLineInfo = class
  private
    FCount: Integer;
    FItems: array of TLineInfo;
    FSelected: PLineInfo;
    function GetItem(Index: Integer): PLineInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InsertLine(StartChar: Integer);
    procedure StartSelect(StartChar: Integer);
    procedure EndSelect(LastSelectChar: Integer);
    procedure EndChar(LastChar: Integer);

    procedure Clear;

    property Count: Integer read FCount;
    property Item[Index: Integer]: PLineInfo read GetItem;
  end;

  PWordInfo = ^TWordsInfo;
  TWordsInfo = record
    WordCount: Integer;
    WordWidth: array of Integer;
  end;

implementation

{ TTextLineInfo }

constructor TTextLineInfo.Create;
begin
  SetLength(FItems, 200);
  Clear;
end;

destructor TTextLineInfo.Destroy;
begin
  SetLength(FItems, 0);
  inherited;
end;

procedure TTextLineInfo.Clear;
begin
  FCount := 0;
  FSelected := nil;
end;

procedure TTextLineInfo.EndChar(LastChar: Integer);
begin
  if FCount > 0 then
  begin
    case FSelected.SelectType of
      ltStartSelect, ltEndSelect:
        begin
          FSelected.Length2 := FSelected.StartChar2 - LastChar;
        end;

      ltStartEndSelect:
        begin
          FSelected.Length3 := FSelected.StartChar3 - LastChar;
        end;

      ltNone:
        begin
          FSelected.Length := FSelected.StartChar - LastChar;
        end;
    end;
  end;
end;

procedure TTextLineInfo.EndSelect(LastSelectChar: Integer);
begin
  case FSelected.SelectType of
    ltNone:
      begin
        FSelected.SelectType := ltEndSelect;
        FSelected.StartChar2 := LastSelectChar + 1;
        FSelected.Length := FSelected.StartChar2 - FSelected.StartChar;
      end;

    ltStartSelect:
      begin
        FSelected.SelectType := ltStartEndSelect;
        FSelected.StartChar3 := LastSelectChar + 1;
        FSelected.Length2 := FSelected.StartChar3 - FSelected.StartChar2;
      end;
  end;
end;

function TTextLineInfo.GetItem(Index: Integer): PLineInfo;
begin
  Result := @FItems[Index];
end;

procedure TTextLineInfo.StartSelect(StartChar: Integer);
begin
  case FSelected.SelectType of
    ltNone:
      begin
        FSelected.SelectType := ltStartSelect;
        FSelected.StartChar2 := StartChar;
        FSelected.Length := StartChar - FSelected.StartChar;
      end;
  end;
end;

procedure TTextLineInfo.InsertLine(StartChar: Integer);
begin
  FItems[FCount].SelectType := ltNone;
  FItems[FCount].StartChar := StartChar;

  if FCount > 0 then
  begin
    case FSelected.SelectType of
      ltStartSelect, ltEndSelect:
        begin
          FSelected.Length2 := StartChar - FSelected.StartChar2;
        end;

      ltStartEndSelect:
        begin
          FSelected.Length3 := StartChar - FSelected.StartChar3;
        end;

      ltNone:
        begin
          FSelected.Length := StartChar - FSelected.StartChar;
        end;
    end;
  end;

  FSelected := @FItems[FCount];
  FCount := FCount + 1;
end;

{ TWordInfo }

end.
