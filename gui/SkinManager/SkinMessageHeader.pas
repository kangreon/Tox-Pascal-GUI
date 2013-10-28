unit SkinMessageHeader;

interface

uses
  SkinTypes, StringUtils, Graphics;

type
  TSkinMessageHeader = class
  private
    FBackColor: TColor;
    FHeight: Integer;
    FNameFontSize: Integer;
    FNameFontStyle: TFontStyles;
    FNameFontName: DataString;
    FNameFontColor: TColor;
    FStatusFontName: DataString;
    FStatusFontColor: TColor;
    FStatusFontSize: Integer;
    FStatusFontStyle: TFontStyles;
    FIconMarginLeft: Integer;
    FIconMarginRight: Integer;
    FAudioMarginLeft: Integer;
    FVideoMarginLeft: Integer;
    FVideoMarginRight: Integer;
    FImgAudioButton: TStateImage;
    FImgVideoButton: TStateImage;
    FImgDefIcon: TBitmap;
    FIconWidth: Integer;
    FIconHeight: Integer;
    FDivLineColor: TColor;
    FDivLineStyle: TPenStyle;
  public
    // Цвет фона
    property BackColor: TColor read FBackColor write FBackColor;
    // Высота компонента
    property Height: Integer read FHeight write FHeight;

    // Настройки шрифта для имени пользователя
    property NameFontColor: TColor read FNameFontColor write FNameFontColor;
    property NameFontName: DataString read FNameFontName write FNameFontName;
    property NameFontSize: Integer read FNameFontSize write FNameFontSize;
    property NameFontStyle: TFontStyles read FNameFontStyle write FNameFontStyle;

    // Параметры шрифта для статуса
    property StatusFontSize: Integer read FStatusFontSize write FStatusFontSize;
    property StatusFontName: DataString read FStatusFontName write FStatusFontName;
    property StatusFontColor: TColor read FStatusFontColor write FStatusFontColor;
    property StatusFontStyle: TFontStyles read FStatusFontStyle write FStatusFontStyle;

    // Расстояния от краев иконки пользователя до др. компонентов
    property IconMarginLeft: Integer read FIconMarginLeft write FIconMarginLeft;
    property IconMarginRight: Integer read FIconMarginRight write FIconMarginRight;

    // Размеры иконки пользователя
    property IconWidth: Integer read FIconWidth write FIconWidth;
    property IconHeight: Integer read FIconHeight write FIconHeight;

    // Отступы у кнопок видео и аудио звонков
    property AudioMarginLeft: Integer read FAudioMarginLeft write FAudioMarginLeft;
    property VideoMarginLeft: Integer read FVideoMarginLeft write FVideoMarginLeft;
    property VideoMarginRight: Integer read FVideoMarginRight write FVideoMarginRight;

    // Параметры разделительной линии в низу компонента
    property DivLineColor: TColor read FDivLineColor write FDivLineColor;
    property DivLineStyle: TPenStyle read FDivLineStyle write FDivLineStyle;

    // Кнопка аудио-звонка
    property ImgAudioButton: TStateImage read FImgAudioButton write FImgAudioButton;
    // Кнопка видео-звонка
    property ImgVideoButton: TStateImage read FImgVideoButton write FImgVideoButton;
    // Стандартная иконка пользователя
    property ImgDefIcon: TBitmap read FImgDefIcon write FImgDefIcon;

    procedure SetCanvasForName(Canvas: TCanvas);
    procedure SetCanvasForStatus(Canvas: TCanvas);
    procedure SetCanvasForDivLine(Canvas: TCanvas);
  end;

implementation

{ TSkinMessageHeader }

procedure TSkinMessageHeader.SetCanvasForDivLine(Canvas: TCanvas);
begin
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := DivLineStyle;
  Canvas.Pen.Color := DivLineColor;
end;

procedure TSkinMessageHeader.SetCanvasForName(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := NameFontName;
  Canvas.Font.Size := NameFontSize;
  Canvas.Font.Style := NameFontStyle;
  Canvas.Font.Color := NameFontColor;
end;

procedure TSkinMessageHeader.SetCanvasForStatus(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := StatusFontName;
  Canvas.Font.Size := StatusFontSize;
  Canvas.Font.Style := StatusFontStyle;
  Canvas.Font.Color := StatusFontColor;
end;

end.
