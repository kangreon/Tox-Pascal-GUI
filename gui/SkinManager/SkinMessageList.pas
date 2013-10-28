unit SkinMessageList;

interface

uses
  SkinTypes, Graphics, StringUtils;

type
  TSkinMessageList = class
  private
    FBackColor: TColor;
    FTextFontColor: TColor;
    FTextFontName: DataString;
    FTextFontSize: Integer;
    FTextFontStyle: TFontStyles;
    FNameFontSize: Integer;
    FNameFontStyle: TFontStyles;
    FNameFontName: DataString;
    FNameFontColor: TColor;
    FDateFontSize: Integer;
    FDateFontStyle: TFontStyles;
    FDateFontName: DataString;
    FDateFontColor: TColor;
    FSelectBackColor: TColor;
    FTextFontColorSelect: TColor;
    FNameFontColorSelect: TColor;
    FDateFontColorSelect: TColor;
    FColNameWidth: Integer;
    FTextFontColorMY: TColor;
    FNameFontColorMy: TColor;
    FDateFontColorMy: TColor;
    FDateFormat: DataString;
    FTimeFormat: DataString;
    FNamePositionLeft: Integer;
    FNameMarginRight: Integer;
    FMessageHeaderHeight: Integer;
    FMessageBottomMargin: Integer;
  public
    // Цвет фона
    property BackColor: TColor read FBackColor write FBackColor;

    // Параметры шрифта пользовательского тектста
    property TextFontColor: TColor read FTextFontColor write FTextFontColor;
    property TextFontColorSelect: TColor read FTextFontColorSelect write FTextFontColorSelect;
    property TextFontColorMY: TColor read FTextFontColorMY write FTextFontColorMY;
    property TextFontName: DataString read FTextFontName write FTextFontName;
    property TextFontSize: Integer read FTextFontSize write FTextFontSize;
    property TextFontStyle: TFontStyles read FTextFontStyle write FTextFontStyle;

    // Параметры шрифта имени пользователя
    property NameFontColor: TColor read FNameFontColor write FNameFontColor;
    property NameFontColorSelect: TColor read FNameFontColorSelect write FNameFontColorSelect;
    property NameFontColorMy: TColor read FNameFontColorMy write FNameFontColorMy;
    property NameFontName: DataString read FNameFontName write FNameFontName;
    property NameFontSize: Integer read FNameFontSize write FNameFontSize;
    property NameFontStyle: TFontStyles read FNameFontStyle write FNameFontStyle;

    // Расстояние от левого края до имении пользователя
    property NamePositionLeft: Integer read FNamePositionLeft write FNamePositionLeft;
    // Расстояние от правого края текста до следующего элемента
    property NameMarginRight: Integer read FNameMarginRight write FNameMarginRight;

    // Параметры шрифта даты и времени
    property DateFontColor: TColor read FDateFontColor write FDateFontColor;
    property DateFontColorSelect: TColor read FDateFontColorSelect write FDateFontColorSelect;
    property DateFontColorMy: TColor read FDateFontColorMy write FDateFontColorMy;
    property DateFontName: DataString read FDateFontName write FDateFontName;
    property DateFontSize: Integer read FDateFontSize write FDateFontSize;
    property DateFontStyle: TFontStyles read FDateFontStyle write FDateFontStyle;

    // Формат вывода даты
    property DateFormat: DataString read FDateFormat write FDateFormat;

    // Формат вывода времени
    property TimeFormat: DataString read FTimeFormat write FTimeFormat;

    // Цвет выделения
    property SelectBackColor: TColor read FSelectBackColor write FSelectBackColor;

    // Ширина колонак с именами и датами (временем)
    property ColNameWidth: Integer read FColNameWidth write FColNameWidth;

    // Высота заголовка сообщения (Отступ и разделительная линия)
    property MessageHeaderHeight: Integer read FMessageHeaderHeight write FMessageHeaderHeight;
    // Отступ от нижней частисообщения до следующего
    property MessageBottomMargin: Integer read FMessageBottomMargin write FMessageBottomMargin;

    procedure SetCanvasForText(Canvas: TCanvas; Select: Boolean = False;
      IsMy: Boolean = False);
    procedure SetCanvasForName(Canvas: TCanvas; Select: Boolean = False;
      IsMy: Boolean = False);
    procedure SetCanvasForDate(Canvas: TCanvas; Select: Boolean = False;
      IsMy: Boolean = False);
  end;

implementation

{ TSkinMessageList }

procedure TSkinMessageList.SetCanvasForDate(Canvas: TCanvas; Select: Boolean;
  IsMy: Boolean);
begin
  if Select then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := SelectBackColor;
  end
  else
    Canvas.Brush.Style := bsClear;

  Canvas.Font.Name := DateFontName;
  Canvas.Font.Size := DateFontSize;
  Canvas.Font.Style := DateFontStyle;

  if Select then
    Canvas.Font.Color := DateFontColorSelect
  else if IsMy then
    Canvas.Font.Color := DateFontColorMY
  else
    Canvas.Font.Color := DateFontColor;
end;

procedure TSkinMessageList.SetCanvasForName(Canvas: TCanvas; Select: Boolean;
  IsMy: Boolean);
begin
  if Select then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := SelectBackColor;
  end
  else
    Canvas.Brush.Style := bsClear;

  Canvas.Font.Name := NameFontName;
  Canvas.Font.Size := NameFontSize;
  Canvas.Font.Style := NameFontStyle;

  if Select then
    Canvas.Font.Color := NameFontColorSelect
  else if IsMy then
    Canvas.Font.Color := NameFontColorMY
  else
    Canvas.Font.Color := NameFontColor;
end;

procedure TSkinMessageList.SetCanvasForText(Canvas: TCanvas; Select: Boolean;
  IsMy: Boolean);
begin
  if Select then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := SelectBackColor;
  end
  else
    Canvas.Brush.Style := bsClear;

  Canvas.Font.Name := TextFontName;
  Canvas.Font.Size := TextFontSize;
  Canvas.Font.Style := TextFontStyle;

  if Select then
    Canvas.Font.Color := TextFontColorSelect
  else if IsMy then
    Canvas.Font.Color := TextFontColorMY
  else
    Canvas.Font.Color := TextFontColor;
end;

end.
