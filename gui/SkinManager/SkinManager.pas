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
  SkinTypes, SkinMessageList, SkinMessageHeader, SkinControlPanel;

type
  TArrayArrayInt = array of array of Integer;

  TSkinManager = class(TSkinBase)
  private
    FMessageList: TSkinMessageList;
    FUserList: TSkinUserList;
    FUserStatus: TSkinUserStatus;
    FMessageHeader: TSkinMessageHeader;
    FControlPanel: TSkinControlPanel;
    function FontStylesToInt(Styles: TFontStyles): Integer;
    function IntToFontStyles(Value: Integer): TFontStyles;
    procedure LoadMessageList;
    procedure LoadUserList;
    procedure LoadUserStatus;
    procedure LoadMessageHeader;
    procedure LoadControlPanel;
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

{ TSkinManager }

constructor TSkinManager.Create(SettingsFile: DataString);
begin
  inherited;

  FUserList := TSkinUserList.Create;
  FUserStatus := TSkinUserStatus.Create;
  FMessageList := TSkinMessageList.Create;
  FMessageHeader := TSkinMessageHeader.Create;
  FControlPanel := TSkinControlPanel.Create;

  LoadUserList;
  LoadUserStatus;
  LoadMessageList;
  LoadMessageHeader;
  LoadControlPanel;

end;

destructor TSkinManager.Destroy;
begin
  FUserList.Free;
  FUserStatus.Free;
  FMessageList.Free;
  FMessageHeader.Free;
  FControlPanel.Free;

  Save; // TODO: Убрать

  inherited;
end;

function TSkinManager.FontStylesToInt(Styles: TFontStyles): Integer;
begin
  {$IFDEF FPC}
  Result := Integer(Styles);
  {$ELSE}
  Result := Integer(Byte(Styles));
  {$ENDIF}
end;

function TSkinManager.IntToFontStyles(Value: Integer): TFontStyles;
begin
  {$IFDEF FPC}
  Result := TFontStyles(Value);
  {$ELSE}
  Byte(Result) := Byte(Value);
  {$ENDIF}
end;

function RGB(r, g, b: Byte): TColor;
begin
  Result := (r or (g shl 8) or (b shl 16));
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

procedure TSkinManager.LoadControlPanel;
begin
  SelectSection(SECTION_CONTROL_PANEL);

  FControlPanel.BackColor := Read('BackColor', RGB(35, 31, 32));
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
begin
  SelectSection(SECTION_MESSAGE_HEADER);

  FMessageHeader.BackColor := Read('BackColor', RGB($ef, $ef, $ef));
  FMessageHeader.Height := Read('Height', 59);

  FMessageHeader.NameFontColor := Read('Name.Font.Color', RGB($40, $40, $42));
  FMessageHeader.NameFontName := Read('Name.Font.Name', 'Fira Sans');
  FMessageHeader.NameFontSize := Read('Name.Font.Size', 10);
  FMessageHeader.NameFontStyle := IntToFontStyles(Read('Name.Font.Style',
    FontStylesToInt([TFontStyle.fsBold])));

  FMessageHeader.StatusFontColor := Read('Status.Font.Color', RGB($40, $40, $42));
  FMessageHeader.StatusFontName := Read('Status.Font.Name', 'Fira Sans');
  FMessageHeader.StatusFontSize := Read('Status.Font.Size', 10);
  FMessageHeader.StatusFontStyle := IntToFontStyles(Read('Status.Font.Style',
    FontStylesToInt([TFontStyle.fsBold])));

  FMessageHeader.IconMarginLeft := Read('Icon.MarginLeft', 10);
  FMessageHeader.IconMarginRight := Read('Icon.MarginRight', 10);
  FMessageHeader.AudioMarginLeft := Read('Audio.MarginLeft', 10);
  FMessageHeader.VideoMarginLeft := Read('Video.MarginLeft', 10);
  FMessageHeader.VideoMarginRight := Read('Video.MarginRight', 10);

  FMessageHeader.IconWidth := Read('Icon.Width', 44);
  FMessageHeader.IconHeight := Read('Icon.Height', 41);

  FMessageHeader.DivLineColor := Read('DivLine.Color', RGB($d2, $d2, $d2));
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
begin
  SelectSection(SECTION_MESSAGE_LIST);

  FMessageList.BackColor := Read('BackColor', RGB($ef, $ef, $ef));

  FMessageList.TextFontColor := Read('Text.Font.Color', RGB($00, $00, $00));
  FMessageList.TextFontColorSelect := Read('Text.Font.ColorSelect', RGB($ff, $ff, $ff));
  FMessageList.TextFontColorMy := Read('Text.Font.ColorMy', RGB($7b, $7b, $7b));
  FMessageList.TextFontName := Read('Text.Font.Name', 'DejaVu Sans');
  FMessageList.TextFontSize := Read('Text.Font.Size', 8);
  FMessageList.TextFontStyle := IntToFontStyles(Read('Text.Font.Style',
    FontStylesToInt([])));

  FMessageList.NameFontColor := Read('Name.Font.Color', RGB($00, $00, $00));
  FMessageList.NameFontColorSelect := Read('Name.Font.ColorSelect', RGB($ff, $ff, $ff));
  FMessageList.NameFontColorMy := Read('Name.Font.ColorMy', RGB($7b, $7b, $7b));
  FMessageList.NameFontName := Read('Name.Font.Name', 'DejaVu Sans');
  FMessageList.NameFontSize := Read('Name.Font.Size', 8);
  FMessageList.NameFontStyle := IntToFontStyles(Read('Name.Font.Style',
    FontStylesToInt([fsBold])));
  FMessageList.NamePositionLeft := Read('Name.PositionLeft', 2);
  FMessageList.NameMarginRight := Read('Name.MarginRight', 2);

  FMessageList.DateFontColor := Read('Date.Font.Color', RGB($7b, $7b, $7b));
  FMessageList.DateFontColorSelect := Read('Date.Font.ColorSelect', RGB($ff, $ff, $ff));
  FMessageList.DateFontColorMy := Read('Date.Font.ColorMy', RGB($7b, $7b, $7b));
  FMessageList.DateFontName := Read('Date.Font.Name', 'DejaVu Sans');
  FMessageList.DateFontSize := Read('Date.Font.Size', 8);
  FMessageList.DateFontStyle := IntToFontStyles(Read('Name.Font.Style',
    FontStylesToInt([])));

  FMessageList.DateFormat := '  ' + Read('DateFormat', 'dd/mm/yyyy') + '  ';
  FMessageList.TimeFormat := '  ' + Read('TimeFormat', 'hh:nn:ss') + '  ';

  FMessageList.SelectBackColor := Read('Select.Back.Color', RGB($a5, $a5, $a5));
  FMessageList.ColNameWidth := Read('ColName.Width', 70);

  FMessageList.MessageHeaderHeight := Read('MessageHeaderHeight', 20);
  FMessageList.MessageBottomMargin := Read('MessageBottomMargin', 4);
end;

procedure TSkinManager.LoadUserList;
var
  BackColor: array[0..2] of TColor;
begin
  // Загрузка значений для UserList
  SelectSection(SECTION_USER_LIST);

  FUserList.BackgroundColor := Read('BackgroundColor', TColor($424041));
  FUserList.ItemColorActive := Read('ItemColorActive', TColor($555353));
  FUserList.ItemColorDown := Read('ItemColorDown', TColor($EFEFEF));
  FUserList.ItemHeight := Read('ItemHeight', 59);
  FUserList.IconLeft := Read('IconLeft', 8);
  FUserList.IconMarginRight := Read('IconMarginRight', 8);
  FUserList.StatusIconMarginLeft := Read('StatusIconMarginLeft', 0);
  FUserList.StatusIconMarginRight := Read('StatusIconMarginRight', 0);
  FUserList.NameFontName := Read('NameFontName', 'Fira Sans');
  FUserList.NameFontSize := Read('NameFontSize', 9);
  FUserList.NameFontColor := Read('NameFontColot', TColor($FFFFFF));
  FUserList.NameFontColorActive := Read('NameFontColorActive', TColor($FFFFFF));
  FUserList.NameFontColorDown := Read('NameFontColorDown', TColor($3a3a3a));
  FUserList.StatusFontName := Read('StatusFontName', 'Fira Sans');
  FUserList.StatusFontSize := Read('StatusFontSize', 8);
  FUserList.StatusFontColor := Read('StatusFontColor', TColor($CAC8C7));
  FUserList.StatusFontColorActive := Read('StatusFontColorActive', TColor($CAC8C7));
  FUserList.StatusFontColorDown := Read('StatusFontColorDown', TColor($CAC8C7));

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
  FUserStatus.BackColor := Read('BackColor', RGB(35, 31, 32));
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
