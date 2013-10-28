unit SkinUserStatus;

interface

uses
  Graphics, StringUtils, SkinTypes;

type
  TSkinUserStatus = class
  private
    FBackColor: TColor;
    FHeight: Integer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FButtonBack: TColor;
    FButtonBackActive: TColor;
    FButtonBackDown: TColor;
    FButtonWidth: Integer;
    FIconLeft: Integer;
    FIconWidth: Integer;
    FIconHeight: Integer;
    FNameFontSize: Integer;
    FNameFontName: DataString;
    FNameFontColor: TColor;
    FNameFontStyle: TFontStyles;
    FStatusFontName: DataString;
    FStatusFontColor: TColor;
    FStatusFontSize: Integer;
    FStatusFontStyle: TFontStyles;
    FIconMarginRight: Integer;
    FNameMarginBottom: Integer;
    FStatusMarginTop: Integer;
    FImgLoading: TBitmap;
    FImgStateOnline: TBitmap;
    FImgStateAway: TBitmap;
    FImgBusy: TBitmap;
    FImgStateOffline: TBitmap;
  public
    // ���� ����
    property BackColor: TColor read FBackColor write FBackColor;

    // ������ ��������
    property Height: Integer read FHeight write FHeight;

    // ����������� ������ ��������
    property MinWidth: Integer read FMinWidth write FMinWidth;

    // ������������ ������ ��������
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;

    // ���� ������ ��������� ������� � ���������� ���������
    property ButtonBack: TColor read FButtonBack write FButtonBack;

    // � �������� ���������
    property ButtonBackActive: TColor read FButtonBackActive write FButtonBackActive;

    // � ������� ���������
    property ButtonBackDown: TColor read FButtonBackDown write FButtonBackDown;

    // ������ ������
    property ButtonWidth: Integer read FButtonWidth write FButtonWidth;

    // ������ �� ���� �� ������ ������������.
    property IconLeft: Integer read FIconLeft write FIconLeft;

    // ������ �� ������ ������������ � ������ �������
    property IconMarginRight: Integer read FIconMarginRight write FIconMarginRight;

    // ������ ������ ������������
    property IconWidth: Integer read FIconWidth write FIconWidth;

    // ������ ������ ������������
    property IconHeight: Integer read FIconHeight write FIconHeight;

    // ��������� ������ ��� ����� ������������
    property NameFontSize: Integer read FNameFontSize write FNameFontSize;
    property NameFontName: DataString read FNameFontName write FNameFontName;
    property NameFontColor: TColor read FNameFontColor write FNameFontColor;
    property NameFontStyle: TFontStyles read FNameFontStyle write FNameFontStyle;

    // ��������� ������ ��� �������
    property StatusFontSize: Integer read FStatusFontSize write FStatusFontSize;
    property StatusFontName: DataString read FStatusFontName write FStatusFontName;
    property StatusFontColor: TColor read FStatusFontColor write FStatusFontColor;
    property StatusFontStyle: TFontStyles read FStatusFontStyle write FStatusFontStyle;

    // ������ �� ������ ������� �� ������ ����� ����� ������������
    property NameMarginBottom: Integer read FNameMarginBottom write FNameMarginBottom;

    // ������ �� ������ ������� �� ������� ����� ������� ������������
    property StatusMarginTop: Integer read FStatusMarginTop write FStatusMarginTop;

    // ����������� � ��������� ��������
    property ImgLoading: TBitmap read FImgLoading write FImgLoading;

    // ��������� ������������ ������
    property ImgStateOnline: TBitmap read FImgStateOnline write FImgStateOnline;

    // ��������� ������������ ������
    property ImgStateAway: TBitmap read FImgStateAway write FImgStateAway;

    // ��������� ������������ �����
    property ImgStateBusy: TBitmap read FImgBusy write FImgBusy;

    // ��������� ������������ ��������
    property ImgStateOffline: TBitmap read FImgStateOffline write FImgStateOffline;

    function SetCanvasForName(Canvas: TCanvas): Integer;
    function SetCanvasForStatus(Canvas: TCanvas): Integer;
  end;

implementation

{ TSkinUserStatus }

function TSkinUserStatus.SetCanvasForName(Canvas: TCanvas): Integer;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := NameFontName;
  Canvas.Font.Style := NameFontStyle;
  Canvas.Font.Size := NameFontSize;
  Canvas.Font.Color := NameFontColor;

  Result := Canvas.TextHeight('Q');
end;

function TSkinUserStatus.SetCanvasForStatus(Canvas: TCanvas): Integer;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := StatusFontName;
  Canvas.Font.Style := StatusFontStyle;
  Canvas.Font.Size := StatusFontSize;
  Canvas.Font.Color := StatusFontColor;

  Result := Canvas.TextHeight('Q');
end;

end.
