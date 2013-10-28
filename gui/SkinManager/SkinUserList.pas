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
    // ���� ���� ����������
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;

    // ���� ����������� ��������+
    property ItemColorActive: TColor read FItemColorActive write FItemColorActive;

    // ���� �������� ��������
    property ItemColorDown: TColor read FItemColorDown write FItemColorDown;

    // ������ �������� ������
    property ItemHeight: Integer read FItemHeight write FItemHeight;

    // ���������� �� ���� �������� �� ������ ������������
    property IconLeft: Integer read FIconLeft write FIconLeft;

    // ������ �� ������ ������������ � ������ �������
    property IconMarginRight: Integer read FIconMarginRight write FIconMarginRight;

    // ������ �� ������ ������� � ������ �������
    property StatusIconMarginLeft: Integer read FStatusIconMarginLeft write FStatusIconMarginLeft;

    // ������ �� ������ � ����� �������
    property StatusIconMarginRight: Integer read FStatusIconMarginRight write FStatusIconMarginRight;

    // ��� ������ ��� ����� ������������
    property NameFontName: DataString read FNameHeight write FNameHeight;

    // ������ ������ ��� ����� ������������
    property NameFontSize: Integer read FNameFontSize write FNameFontSize;

    // ���� ������ ��� ����� ������������
    property NameFontColor: TColor read FNameFontColot write FNameFontColot;
    property NameFontColorActive: TColor read FNameFontColorActive write FNameFontColorActive;
    property NameFontColorDown: TColor read FNameFontColorDown write FNameFontColorDown;

    // �������� ����� ��� ������� ������������
    property StatusFontName: DataString read FStatusFontName write FStatusFontName;

    // ������ ������ ��� ������� ������������
    property StatusFontSize: Integer read FStatusFontSize write FStatusFontSize;

    // ���� ������ ��� ������� ������������
    property StatusFontColor: TColor read FStatusFontColor write FStatusFontColor;
    property StatusFontColorActive: TColor read FStatusFontColorActive write FStatusFontColorActive;
    property StatusFontColorDown: TColor read FStatusFontColorDown write FStatusFontColorDown;

    // ��������� ������������ ������
    property ImgStateOnline: TStateImage read FImgStateOnline write FImgStateOnline;
    property ImgStateOnlineNew: TStateImage read FImgStateOnlineNew write FImgStateOnlineNew;

    // ��������� ������������ ������
    property ImgStateAway: TStateImage read FImgStateAway write FImgStateAway;
    property ImgStateAwayNew: TStateImage read FImgStateAwayNew write FImgStateAwayNew;

    // ��������� ������������ �����
    property ImgStateBusy: TStateImage read FImgBusy write FImgBusy;
    property ImgStateBusyNew: TStateImage read FImgBusyNew write FImgBusyNew;

    // ��������� ������������ ��������
    property ImgStateOffline: TStateImage read FImgStateOffline write FImgStateOffline;
    property ImgStateOfflineNew: TStateImage read FImgStateOfflineNew write FImgStateOfflineNew;

    // ������� ����� ������ ���������
    property ImgScrollBarTop: TStateImage read FImgScrollBarTop write FImgScrollBarTop;
    //TODO: �� ������������
    property ImgScrollBarCenter: TStateImage read FImgScrollBarCenter write FImgScrollBarCenter;
    property ImgScrollBarBottom: TStateImage read FImgScrollBarBottom write FImgScrollBarBottom;

    // ���� ������� ���� ������ ���������
    property ScrollBarBack: TColor read FScrollBarBack write FScrollBarBack;

    // ���� ����������� ����� ������ ���������
    property ScrollbarCenterColor: TColor read FScrollbarCenterColor write FScrollbarCenterColor;
    property ScrollbarCenterColorActive: TColor read FScrollbarCenterColorActive write FScrollbarCenterColorActive;
    property ScrollbarCenterColorDown: TColor read FScrollbarCenterColorDown write FScrollbarCenterColorDown;

    // ����������� ������ ����������� ����� ������ ���������
    property ScrollBarMinCenterHeight: Integer read FScrollBarMinCenterHeight write FScrollBarMinCenterHeight;
    // ������ ������ ���������
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
  Canvas.Font.Style := [fsBold]; //TODO: ���������
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
  Canvas.Font.Style := []; //TODO: ���������
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
