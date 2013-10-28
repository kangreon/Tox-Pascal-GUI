unit SkinUserList;

interface

uses
  Classes, Graphics, StringUtils, SkinTypes;

type
  TSkinUserList = class
  private
    FBackgroundColor: TColor;
    FItemColorActive: TColor;
    FItemColorDown: TColor;
    FItemHeight: Integer;
    FIconLeft: Integer;
    FIconMarginRight: Integer;
    FStatusIconMarginLeft: Integer;
    FStatusIconMarginRight: Integer;
    FNameHeight: DataString;
    FNameFontSize: Integer;
    FStatusFontName: DataString;
    FStatusFontSize: Integer;
    FNameFontColot: TColor;
    FStatusFontColor: TColor;
    FImgStateOnline: TStateImage;
    FImgStateAway: TStateImage;
    FImgBusy: TStateImage;
    FImgStateOffline: TStateImage;
    FNameFontColorActive: TColor;
    FNameFontColorDown: TColor;
    FStatusFontColorActive: TColor;
    FStatusFontColorDown: TColor;
    FImgStateOnlineNew: TStateImage;
    FImgStateAwayNew: TStateImage;
    FImgBusyNew: TStateImage;
    FImgStateOfflineNew: TStateImage;
    FImgScrollBarTop: TStateImage;
    FImgScrollBarCenter: TStateImage;
    FImgScrollBarBottom: TStateImage;
    FScrollBarWidth: Integer;
    FScrollBarBack: TColor;
    FScrollBarMinCenterHeight: Integer;
    FScrollbarCenterColor: TColor;
    FScrollbarCenterColorDown: TColor;
    FScrollbarCenterColorActive: TColor;
  public
    // Цвет фона компонента
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;

    // Цвет выделенного элемента+
    property ItemColorActive: TColor read FItemColorActive write FItemColorActive;

    // Цвет нажатого элемента
    property ItemColorDown: TColor read FItemColorDown write FItemColorDown;

    // Высота элемента списка
    property ItemHeight: Integer read FItemHeight write FItemHeight;

    // Расстояние от края элемента до иконки пользователя
    property IconLeft: Integer read FIconLeft write FIconLeft;

    // Отступ от иконки пользователя с правой стороны
    property IconMarginRight: Integer read FIconMarginRight write FIconMarginRight;

    // Отступ от иконки статуса с правой стороны
    property StatusIconMarginLeft: Integer read FStatusIconMarginLeft write FStatusIconMarginLeft;

    // Отступ от иконки с левой стороны
    property StatusIconMarginRight: Integer read FStatusIconMarginRight write FStatusIconMarginRight;

    // Имя шрифта для имени пользователя
    property NameFontName: DataString read FNameHeight write FNameHeight;

    // Размер шрифта для имени пользователя
    property NameFontSize: Integer read FNameFontSize write FNameFontSize;

    // Цвет шрифта для имени пользователя
    property NameFontColor: TColor read FNameFontColot write FNameFontColot;
    property NameFontColorActive: TColor read FNameFontColorActive write FNameFontColorActive;
    property NameFontColorDown: TColor read FNameFontColorDown write FNameFontColorDown;

    // Название шрфта для статуса пользователя
    property StatusFontName: DataString read FStatusFontName write FStatusFontName;

    // Размер шрифта для статуса пользователя
    property StatusFontSize: Integer read FStatusFontSize write FStatusFontSize;

    // Цвет шрифта для статуса пользователя
    property StatusFontColor: TColor read FStatusFontColor write FStatusFontColor;
    property StatusFontColorActive: TColor read FStatusFontColorActive write FStatusFontColorActive;
    property StatusFontColorDown: TColor read FStatusFontColorDown write FStatusFontColorDown;

    // Состояние пользователя онлайн
    property ImgStateOnline: TStateImage read FImgStateOnline write FImgStateOnline;
    property ImgStateOnlineNew: TStateImage read FImgStateOnlineNew write FImgStateOnlineNew;

    // Состояние пользователя отошел
    property ImgStateAway: TStateImage read FImgStateAway write FImgStateAway;
    property ImgStateAwayNew: TStateImage read FImgStateAwayNew write FImgStateAwayNew;

    // Состояние пользователя занят
    property ImgStateBusy: TStateImage read FImgBusy write FImgBusy;
    property ImgStateBusyNew: TStateImage read FImgBusyNew write FImgBusyNew;

    // Состояние пользователя отключен
    property ImgStateOffline: TStateImage read FImgStateOffline write FImgStateOffline;
    property ImgStateOfflineNew: TStateImage read FImgStateOfflineNew write FImgStateOfflineNew;

    // Верхняя часть полосы прокрутки
    property ImgScrollBarTop: TStateImage read FImgScrollBarTop write FImgScrollBarTop;
    //TODO: Не используется
    property ImgScrollBarCenter: TStateImage read FImgScrollBarCenter write FImgScrollBarCenter;
    property ImgScrollBarBottom: TStateImage read FImgScrollBarBottom write FImgScrollBarBottom;

    // Цвет заднего фона полосы прокрутки
    property ScrollBarBack: TColor read FScrollBarBack write FScrollBarBack;

    // Цвет центральной части полосы прокрутки
    property ScrollbarCenterColor: TColor read FScrollbarCenterColor write FScrollbarCenterColor;
    property ScrollbarCenterColorActive: TColor read FScrollbarCenterColorActive write FScrollbarCenterColorActive;
    property ScrollbarCenterColorDown: TColor read FScrollbarCenterColorDown write FScrollbarCenterColorDown;

    // Минимальная высода центральной части полосы прокрутки
    property ScrollBarMinCenterHeight: Integer read FScrollBarMinCenterHeight write FScrollBarMinCenterHeight;
    // Ширина полосы прокрутки
    property ScrollBarWidth: Integer read FScrollBarWidth write FScrollBarWidth;

    procedure SetCanvasForName(Canvas: TCanvas; MouseState: TMouseState);
    procedure SetCanvasForStatus(Canvas: TCanvas; MouseState: TMouseState);
  end;

implementation

{ TSkinUserList }

procedure TSkinUserList.SetCanvasForName(Canvas: TCanvas; MouseState: TMouseState);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := NameFontName;
  Canvas.Font.Style := [fsBold]; //TODO: исправить
  Canvas.Font.Size := NameFontSize;

  case MouseState of
    msNone:
      Canvas.Font.Color := NameFontColor;

    msActive:
      Canvas.Font.Color := NameFontColorActive;

    msDown:
      Canvas.Font.Color := NameFontColorDown;
  end;

end;

procedure TSkinUserList.SetCanvasForStatus(Canvas: TCanvas; MouseState: TMouseState);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := StatusFontName;
  Canvas.Font.Style := []; //TODO: исправить
  Canvas.Font.Size := StatusFontSize;

  case MouseState of
    msNone:
      Canvas.Font.Color := StatusFontColor;

    msActive:
      Canvas.Font.Color := StatusFontColorActive;

    msDown:
      Canvas.Font.Color := StatusFontColorDown;
  end;
end;

end.
