//  SkinManager.pas
//
//  Реализует загрузку и сохранение параметров отрисовки GUI
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit SkinManager;

interface
  {$I tox.inc}

uses
  SkinBase, Graphics, StringUtils, SkinUserList, SysUtils, SkinUserStatus,
  SkinTypes, SkinMessageList, SkinMessageHeader, SkinControlPanel,
  SkinTabControl, SkinTabList;

type
  TArrayArrayInt = array of array of Integer;

  TSkinManager = class(TSkinBase)
  private
    FMessageList: TSkinMessageList;
    FUserList: TSkinUserList;
    FUserStatus: TSkinUserStatus;
    FMessageHeader: TSkinMessageHeader;
    FControlPanel: TSkinControlPanel;
    FTabControl: TSkinTabControl;
    FTabList: TSkinTabList;
    procedure LoadMessageList;
    procedure LoadUserList;
    procedure LoadUserStatus;
    procedure LoadMessageHeader;
    procedure LoadControlPanel;
    procedure LoadTabControl;
    procedure LoadTabList;
    procedure Save;
    function LoadStateImage(Name: DataString; IsTransporent: Boolean;
      TransporentColor: array of TColor;
      DefPosition: array of Integer): TStateImage;
  public
    constructor Create(SettingsFile: DataString); override;
    destructor Destroy; override;

    property ControlPanel: TSkinControlPanel read FControlPanel;
    property MessageHeader: TSkinMessageHeader read FMessageHeader;
    property MessageList: TSkinMessageList read FMessageList;
    property TabControl: TSkinTabControl read FTabControl;
    property UserList: TSkinUserList read FUserList;
    property UserStatus: TSkinUserStatus read FUserStatus write FUserStatus;
  end;

implementation

const
  SECTION_MESSAGE_HEADER = 'MessageHeader';
  SECTION_MESSAGE_LIST = 'MessageList';
  SECTION_USER_LIST = 'UserList';
  SECTION_USER_STATUS = 'UserStatus';
  SECTION_CONTROL_PANEL = 'ControlPanel';
  SECTION_TAB_CONTROL = 'TabControl';
  SECTION_TAB_LIST = 'TabList';

{ TSkinManager }

constructor TSkinManager.Create(SettingsFile: DataString);
begin
  inherited;

  FUserList := TSkinUserList.Create;
  FUserStatus := TSkinUserStatus.Create;
  FMessageList := TSkinMessageList.Create;
  FMessageHeader := TSkinMessageHeader.Create;
  FControlPanel := TSkinControlPanel.Create;
  FTabControl := TSkinTabControl.Create;
  FTabList := TSkinTabList.Create;

  LoadUserList;
  LoadUserStatus;
  LoadMessageList;
  LoadMessageHeader;
  LoadControlPanel;
  LoadTabControl;
  LoadTabList;

end;

destructor TSkinManager.Destroy;
begin
  FUserList.Free;
  FUserStatus.Free;
  FMessageList.Free;
  FMessageHeader.Free;
  FControlPanel.Free;
  FTabControl.Free;
  FTabList.Free;

  Save; // TODO: Убрать

  inherited;
end;

function TSkinManager.LoadStateImage(Name: DataString; IsTransporent: Boolean;
  TransporentColor: array of TColor; DefPosition: array of Integer): TStateImage;
var
  Images: TStateImage;
  i: Integer;
  IsOneBackground, IsOneImage: Boolean;
  TrColor: TColor;
  PosIndex: Integer;
begin
  IsOneBackground := True;
  if IsTransporent then
  begin
    IsOneBackground := Length(TransporentColor) = 1;

    if not((Length(TransporentColor) = 3) or (Length(TransporentColor) = 1)) then
      raise Exception.Create('Неверное число параметров цвета у изображения' + Name);
  end;

  IsOneImage := Length(DefPosition) = 4;
  if not((Length(DefPosition) = 4) or (Length(DefPosition) = 12)) then
    raise Exception.Create('Неверное число параметров координат у изображения' + Name);

  for i := 0 to 2 do
  begin
    if IsTransporent then
    begin
      if IsOneBackground then
        TrColor := TransporentColor[0]
      else
        TrColor := TransporentColor[i];
    end
    else
      TrColor := 0;

    if IsOneImage then
      PosIndex := 0
    else
      PosIndex := i * 4;

    Images[i] := LoadImage(Name, IsTransporent, TrColor,
      DefPosition[PosIndex + 0], DefPosition[PosIndex + 1],
      DefPosition[PosIndex + 2], DefPosition[PosIndex + 3]);
  end;

  Result := Images;
end;

procedure TSkinManager.LoadTabControl;
begin
  SelectSection(SECTION_TAB_CONTROL);

  FTabControl.BackColor := ReadColor('Back', RGB($41, $41, $41));
  FTabControl.ButtonHeight := Read('Button.Height', 27);
  FTabControl.ButtonMarginTop := Read('Button.MarginTop', 10);
  FTabControl.ButtonMargintBottom := Read('Button.MargintBottom', 10);
  FTabControl.ButtonMarginLeft := Read('Button.MarginLeft', 10);
  FTabControl.ButtonMarginRight := Read('Button.MarginRight', 10);

  FTabControl.ButtonListColor := Read('Button.List', [
    RGB($64, $62, $65),
    RGB($64, $62, $65),
    RGB($64, $62, $65)
  ]);

  FTabControl.ButtonRequestColor := Read('Button.Request', [
    RGB($6C, $C2, $5F),
    RGB($6C, $C2, $5F),
    RGB($6C, $C2, $5F)
  ]);

  FTabControl.CaptionMarginLeft := Read('Caption.MarginLeft', 4);
  FTabControl.CaptionMarginRight := Read('Caption.MarginRight', 4);

  LoadFont(FTabControl.Button, 'Button', 'DejaVu Sans Condensed', 8, [
      RGB($FF, $FF, $FF),
      RGB($FF, $FF, $FF),
      RGB($FF, $FF, $FF)
    ], []
  );

  FTabControl.TabListItemHeight := Read('TabList.ItemHeight', 20);
  FTabControl.TabListMarginLeft := Read('TabList.MarginLeft', 5);
  FTabControl.TabListMarginRight := Read('TabList.MarginRight', 5);
  FTabControl.TabListMarginTop := Read('TabList.MarginTop', 5);
  FTabControl.TabListMarginBottom := Read('TabList.MarginBottom', 5);

  FTabControl.ImgListTopLeft := LoadStateImage(
    'List.TopLeft', True, FTabControl.BackColor, [
      0, 84, 6, 6,
      0, 84, 6, 6,
      0, 84, 6, 6
    ]
  );

  FTabControl.ImgListTopRight := LoadStateImage(
    'List.TopRight', True, FTabControl.BackColor, [
      7, 84, 6, 6,
      7, 84, 6, 6,
      7, 84, 6, 6
    ]
  );

  FTabControl.ImgListBottomLeft := LoadStateImage(
    'List.BottomLeft', True, FTabControl.BackColor, [
      0, 91, 6, 6,
      0, 91, 6, 6,
      0, 91, 6, 6
    ]
  );

  FTabControl.ImgListBottomRight := LoadStateImage(
    'List.BottomRight', True, FTabControl.BackColor, [
      7, 91, 6, 6,
      7, 91, 6, 6,
      7, 91, 6, 6
    ]
  );

  FTabControl.ImgRequestTopLeft := LoadStateImage(
    'Request.TopLeft', True, FTabControl.BackColor, [
      28, 84, 6, 6,
      28, 84, 6, 6,
      28, 84, 6, 6
    ]
  );

  FTabControl.ImgRequestTopRight := LoadStateImage(
    'Request.TopRight', True, FTabControl.BackColor, [
      35, 84, 6, 6,
      35, 84, 6, 6,
      35, 84, 6, 6
    ]
  );

  FTabControl.ImgRequestBottomLeft := LoadStateImage(
    'Request.BottomLeft', True, FTabControl.BackColor, [
      28, 91, 6, 6,
      28, 91, 6, 6,
      28, 91, 6, 6
    ]
  );

  FTabControl.ImgRequestBottomRight := LoadStateImage(
    'Request.BottomRight', True, FTabControl.BackColor, [
      35, 91, 6, 6,
      35, 91, 6, 6,
      35, 91, 6, 6
    ]
  );

  // Внешние углы
  FTabControl.ImgListExtTopLeft := LoadStateImage(
    'ListExt.TopLeft', True, FTabControl.BackColor, [
      14, 84, 6, 6,
      14, 84, 6, 6,
      14, 84, 6, 6
    ]
  );

  FTabControl.ImgListExtTopRight := LoadStateImage(
    'ListExt.TopRight', True, FTabControl.BackColor, [
      21, 84, 6, 6,
      21, 84, 6, 6,
      21, 84, 6, 6
    ]
  );

  FTabControl.ImgListExtBottomLeft := LoadStateImage(
    'ListExt.BottomLeft', True, FTabControl.BackColor, [
      14, 91, 6, 6,
      14, 91, 6, 6,
      14, 91, 6, 6
    ]
  );

  FTabControl.ImgListExtBottomRight := LoadStateImage(
    'ListExt.BottomRight', True, FTabControl.BackColor, [
      21, 91, 6, 6,
      21, 91, 6, 6,
      21, 91, 6, 6
    ]
  );

  FTabControl.ImgRequestExtTopLeft := LoadStateImage(
    'RequestExt.TopLeft', True, FTabControl.BackColor, [
      42, 84, 6, 6,
      42, 84, 6, 6,
      42, 84, 6, 6
    ]
  );

  FTabControl.ImgRequestExtTopRight := LoadStateImage(
    'RequestExt.TopRight', True, FTabControl.BackColor, [
      49, 84, 6, 6,
      49, 84, 6, 6,
      49, 84, 6, 6
    ]
  );

  FTabControl.ImgRequestExtBottomLeft := LoadStateImage(
    'RequestExt.BottomLeft', True, FTabControl.BackColor, [
      42, 91, 6, 6,
      42, 91, 6, 6,
      42, 91, 6, 6
    ]
  );

  FTabControl.ImgRequestExtBottomRight := LoadStateImage(
    'RequestExt.BottomRight', True, FTabControl.BackColor, [
      49, 91, 6, 6,
      49, 91, 6, 6,
      49, 91, 6, 6
    ]
  );
end;

procedure TSkinManager.LoadTabList;
var
  Skin: TSkinTabList;
begin
  SelectSection(SECTION_TAB_LIST);
  Skin := FTabList;

  LoadFont(Skin.ItemFont, 'Item', 'DejaVu Sans Condensed', 9, RGB($FF, $FF, $FF), []);
  Skin.ItemHeight := Read('Item.Height', 20);
  Skin.CaptionMarginLeft := Read('Caption.Margin.Left', 5);
  Skin.CaptionMarginRight := Read('Caption.Margin.Right', 5);
  Skin.ItemColor := Read('Item', [
    RGB($FF, $FF, $FF),
    RGB($80, $80, $80),
    RGB($FF, $FF, $FF)
  ]);
  FTabControl.SkinTabList := Skin;
end;

procedure TSkinManager.LoadControlPanel;
begin
  SelectSection(SECTION_CONTROL_PANEL);

  FControlPanel.BackColor := ReadColor('BackColor', RGB(35, 31, 32));
  FControlPanel.Height := Read('Height', 50);
  FControlPanel.MarginLeft := Read('MarginLeft', 20);
  FControlPanel.MarginRight := Read('MarginRight', 20);

  FControlPanel.ImgNewFriend := LoadStateImage(
    'NewFriend', True, [FControlPanel.BackColor], [
      0, 64, 20, 20,
      20, 64, 20, 20,
      40, 64, 20, 20
    ]
  );

  FControlPanel.ImgSettings := LoadStateImage(
    'Settings', True, [FControlPanel.BackColor], [
      60, 64, 20, 20,
      80, 64, 20, 20,
      100, 64, 20, 20
    ]
  );

  FControlPanel.ImgNewGroup := LoadStateImage(
    'NewGroup', True, [FControlPanel.BackColor], [
      120, 64, 20, 20,
      140, 64, 20, 20,
      160, 64, 20, 20
    ]
  );
end;

procedure TSkinManager.LoadMessageHeader;
var
  Skin: TSkinMessageHeader;
begin
  SelectSection(SECTION_MESSAGE_HEADER);
  Skin := FMessageHeader;

  Skin.BackColor := ReadColor('BackColor', RGB($ef, $ef, $ef));
  Skin.Height := Read('Height', 59);

  LoadFont(Skin.NameFont, 'Name', 'DejaVu Sans Condensed', 10, RGB($40, $40, $42),
    [fsBold]);
  LoadFont(Skin.StatusFont, 'Status', 'DejaVu Sans Condensed', 10, RGB($40, $40, $42),
    [fsBold]);

  FMessageHeader.IconMarginLeft := Read('Icon.MarginLeft', 10);
  FMessageHeader.IconMarginRight := Read('Icon.MarginRight', 10);
  FMessageHeader.AudioMarginLeft := Read('Audio.MarginLeft', 10);
  FMessageHeader.VideoMarginLeft := Read('Video.MarginLeft', 10);
  FMessageHeader.VideoMarginRight := Read('Video.MarginRight', 10);

  FMessageHeader.IconWidth := Read('Icon.Width', 44);
  FMessageHeader.IconHeight := Read('Icon.Height', 41);

  FMessageHeader.DivLineColor := ReadColor('DivLine.Color', RGB($d2, $d2, $d2));
  FMessageHeader.DivLineStyle := TPenStyle(Read('DivLine.Style', Integer(TPenStyle.psDot)));

  FMessageHeader.ImgDefIcon := LoadImage('DefIcon', False, 0, 312, 0, 44, 41);
  FMessageHeader.ImgAudioButton := LoadStateImage(
    'AudioButton', True, FMessageHeader.BackColor, [
      0, 32, 52, 32,
      52, 32, 52, 32,
      104, 32, 52, 32
    ]
  );

  FMessageHeader.ImgVideoButton := LoadStateImage(
    'AudioButton', True, FMessageHeader.BackColor, [
      156, 32, 52, 32,
      208, 32, 52, 32,
      260, 32, 52, 32
    ]
  )
end;

procedure TSkinManager.LoadMessageList;
var
  Skin: TSkinMessageList;
begin
  SelectSection(SECTION_MESSAGE_LIST);
  Skin := FMessageList;

  Skin.BackColor := ReadColor('Back', RGB($ef, $ef, $ef));

  LoadFont(Skin.TextFont, 'Text', 'DejaVu Sans', 8, [
      RGB($00, $00, $00),
      RGB($ff, $ff, $ff),
      RGB($7b, $7b, $7b)
    ], []);

  LoadFont(Skin.NameFont, 'Name', 'DejaVu Sans Condensed', 8, [
      RGB($00, $00, $00),
      RGB($ff, $ff, $ff),
      RGB($7b, $7b, $7b)
    ], [fsBold]);

  LoadFont(Skin.DateFont, 'Date', 'DejaVu Sans Condensed', 8, [
      RGB($7b, $7b, $7b),
      RGB($ff, $ff, $ff),
      RGB($7b, $7b, $7b)
    ], []);

  Skin.NamePositionLeft := Read('Name.PositionLeft', 2);
  Skin.NameMarginRight := Read('Name.MarginRight', 2);

  Skin.DateFormat := '  ' + Read('DateFormat', 'dd/mm/yyyy') + '  ';
  Skin.TimeFormat := '  ' + Read('TimeFormat', 'hh:nn:ss') + '  ';

  Skin.SelectBackColor := ReadColor('Select.Back', RGB($a5, $a5, $a5));
  Skin.ColNameWidth := Read('ColName.Width', 70);

  Skin.MessageHeaderHeight := Read('MessageHeaderHeight', 20);
  Skin.MessageBottomMargin := Read('MessageBottomMargin', 4);
end;

procedure TSkinManager.LoadUserList;
var
  BackColor: array[0..2] of TColor;
begin
  // Загрузка значений для UserList
  SelectSection(SECTION_USER_LIST);

  FUserList.BackgroundColor := ReadColor('Background', TColor($424041));
  FUserList.ItemColorActive := ReadColor('ItemActive', TColor($555353));
  FUserList.ItemColorDown := ReadColor('ItemDown', TColor($EFEFEF));
  FUserList.ItemHeight := Read('ItemHeight', 59);
  FUserList.IconLeft := Read('IconLeft', 8);
  FUserList.IconMarginRight := Read('IconMarginRight', 8);
  FUserList.StatusIconMarginLeft := Read('StatusIconMarginLeft', 0);
  FUserList.StatusIconMarginRight := Read('StatusIconMarginRight', 0);

  LoadFont(FUserList.NameFont, 'Name', 'DejaVu Sans Condensed', 9, [
      RGB($ff, $ff, $ff),
      RGB($ff, $ff, $ff),
      RGB($3a, $3a, $3a)
  ], [fsBold]);

  LoadFont(FUserList.StatusFont, 'Status', 'DejaVu Sans Condensed', 8, [
      RGB($c7, $c8, $ca),
      RGB($c7, $c8, $ca),
      RGB($c7, $c8, $ca)
  ], []);

  BackColor[0] := FUserList.BackgroundColor;
  BackColor[1] := FUserList.ItemColorActive;
  BackColor[2] := FUserList.ItemColorDown;

  FUserList.ImgStateOnline := LoadStateImage(
    'StateOnline', True, BackColor, [0, 0, 22, 22]
  );

  FUserList.ImgStateAway := LoadStateImage(
    'StateAway', True, BackColor, [44, 0, 22, 22]
  );

  FUserList.ImgStateBusy := LoadStateImage(
    'StateBusy', True, BackColor, [88, 0, 22, 22]
  );

  FUserList.ImgStateOffline := LoadStateImage(
    'StateOffline', True, BackColor, [132, 0, 22, 22]
  );

  FUserList.ImgStateOnlineNew := LoadStateImage(
    'StateOnlineNew', True, BackColor, [22, 0, 22, 22]
  );

  FUserList.ImgStateAwayNew := LoadStateImage(
    'StateAwayNew', True, BackColor, [66, 0, 22, 22]
  );

  FUserList.ImgStateBusyNew := LoadStateImage(
    'StateBusyNew', True, BackColor, [110, 0, 22, 22]
  );

  FUserList.ImgStateOfflineNew := LoadStateImage(
    'StateOfflineNew', True, BackColor, [154, 0, 22, 22]
  );

  // Загрузка значений для полосы прокрутки, которая используется в списке
  // пользователей

  FUserList.ScrollBarBack := Read('ScrollBarBack', $1c1c1c);
  FUserList.ScrollbarCenterColor := Read('ScrollbarCenterColor', $414141);
  FUserList.ScrollbarCenterColorActive := Read('ScrollbarCenterColorActive', $515151);
  FUserList.ScrollbarCenterColorDown := Read('ScrollbarCenterColorDown', $313131);
  FUserList.ScrollBarMinCenterHeight := Read('ScrollBarMinCenterHeight', 20);
  FUserList.ScrollBarWidth := Read('ScrollBarWidth', 13);

  FUserList.ImgScrollBarTop := LoadStateImage(
    'ScrollBarBack', True, [FUserList.ScrollBarBack], [
      0, 22, 11, 4,
      11, 22, 11, 4,
      22, 22, 11, 4
    ]
  );

  FUserList.ImgScrollBarBottom := LoadStateImage(
    'ScrollBarBack', True, [FUserList.ScrollBarBack], [
      0, 28, 11, 4,
      11, 28, 11, 4,
      22, 28, 11, 4
    ]
  );

end;

procedure TSkinManager.LoadUserStatus;
begin
  SelectSection(SECTION_USER_STATUS);
  FUserStatus.BackColor := ReadColor('Back', RGB(35, 31, 32));
  FUserStatus.Height := Read('Height', 59);
  FUserStatus.MinWidth := Read('MinWidth', 223);
  FUserStatus.MaxWidth := Read('MaxWidth', 400);
  FUserStatus.ButtonBack := Read('Button.Back', $424041);
  FUserStatus.ButtonBackActive := Read('Button.BackActive', $525051);
  FUserStatus.ButtonBackDown := Read('Button.BackDown', $323031);
  FUserStatus.ButtonWidth := Read('Button.Width', 13);
  FUserStatus.IconLeft := Read('Icon.Left', 8);
  FUserStatus.IconMarginRight := Read('Icon.MarginRight', 8);
  FUserStatus.IconWidth := Read('Icon.Width', 44);
  FUserStatus.IconHeight := Read('Icon.Height', 41);

  FUserStatus.NameFontSize := Read('Name.FontSize', 10);
  FUserStatus.NameFontName := Read('Name.FontName', 'Fira Sans');
  FUserStatus.NameFontColor := Read('Name.FontColor', $FFFFFF);
  FUserStatus.NameFontStyle := IntToFontStyles(Read('Name.FontStyle', FontStylesToInt([fsBold])));

  FUserStatus.StatusFontSize := Read('Status.FontSize', 8);
  FUserStatus.StatusFontName := Read('Status.FontName', 'Fira Sans');
  FUserStatus.StatusFontColor := Read('Status.FontColor', $EEEEEE);
  FUserStatus.StatusFontStyle := IntToFontStyles(Read('Status.FontStyle', FontStylesToInt([])));

  FUserStatus.NameMarginBottom := Read('Name.MarginBottom', 1);
  FUserStatus.StatusMarginTop := Read('Status.MarginTop', 1);

  FUserStatus.ImgLoading := LoadImage(
    'Loading', True, FUserStatus.BackColor, 33, 22, 80, 10
  );

  FUserStatus.ImgStateOnline := LoadImage(
    'StateOnline', True, FUserStatus.BackColor, 0, 0, 22, 22
  );

  FUserStatus.ImgStateAway := LoadImage(
    'StateAway', True, FUserStatus.BackColor, 44, 0, 22, 22
  );

  FUserStatus.ImgStateBusy := LoadImage(
    'StateBusy', True, FUserStatus.BackColor, 88, 0, 22, 22
  );

  FUserStatus.ImgStateOffline := LoadImage(
    'StateOffline', True, FUserStatus.BackColor, 132, 0, 22, 22
  );
end;

procedure TSkinManager.Save;
begin

end;

end.
