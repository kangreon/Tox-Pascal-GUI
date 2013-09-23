//  ControlPanel.pas
//
//  Составляет панель управления с кнопками быстрого доступа
//
//  The MIT License (MIT)
//
//  Copyright (c) 2013 Dmitry
//
unit ControlPanel;

interface
  {$I tox.inc}

uses
  {$I tox-uses.inc}
  Classes, Controls, ResourceImage, ButtonActive;

type
  TControlButton = (cbAddUser, cbSettings, cbGroup);
  TControlPanelClick = procedure(Sender: TObject; Button: TControlButton) of object;

  TControlPanel = class(TCustomControl)
  private
    FImages: TResourceImage;
    FButtonAddUser: TButtonActive;
    FButtonSettings: TButtonActive;
    FButtonGroup: TButtonActive;
    FOnClick: TControlPanelClick;
    procedure EventClick(Button: TControlButton);
    procedure ButtonClick(Sender: TObject);
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnClick: TControlPanelClick read FOnClick write FOnClick;
  end;

implementation

const
  BUTTON_ADD_USER         = 1;
  BUTTON_SETTINGS         = 2;
  BUTTON_GROUP            = 3;

{ TControlPanel }

procedure TControlPanel.Resize;
begin
  if not Assigned(FButtonAddUser) then
    Exit;

  FButtonAddUser.Top := (Height - FButtonAddUser.Height) div 2;

  FButtonSettings.Top := (Height - FButtonSettings.Height) div 2;
  FButtonSettings.Left := Width - FButtonSettings.Width - 20;

  FButtonGroup.Top := (Height - FButtonGroup.Height) div 2;
  FButtonGroup.Left := (Width - FButtonGroup.Width) div 2;
end;

constructor TControlPanel.Create(AOwner: TComponent);
begin
  inherited;
  FImages := TResourceImage.Clone;

  Height := 50;
  Align := alBottom;
  Color := RGB(35, 31, 32);

  DoubleBuffered := True;
end;

procedure TControlPanel.CreateWnd;
begin
  inherited;
  FButtonAddUser := TButtonActive.Create(Self);
  FButtonAddUser.Parent := Self;
  FButtonAddUser.Cursor := crHandPoint;
  FButtonAddUser.InsertImage(FImages.ButtonAddUserNormal, FImages.ButtonAddUserActive, FImages.ButtonAddUserDown);
  FButtonAddUser.Left := 20;
  FButtonAddUser.Top := (Height - FButtonAddUser.Height) div 2;
  FButtonAddUser.Tag := BUTTON_ADD_USER;
  FButtonAddUser.OnClick := ButtonClick;

  FButtonSettings := TButtonActive.Create(Self);
  FButtonSettings.Parent := Self;
  FButtonSettings.Cursor := crHandPoint;
  FButtonSettings.InsertImage(FImages.ButtonSettingsNormal, FImages.ButtonSettingsActive, FImages.ButtonSettingsDown);
  FButtonSettings.Top := (Height - FButtonSettings.Height) div 2;
  FButtonSettings.Left := Width - FButtonSettings.Width - 20;
  FButtonSettings.Tag := BUTTON_SETTINGS;
  FButtonSettings.OnClick := ButtonClick;

  FButtonGroup := TButtonActive.Create(Self);
  FButtonGroup.Parent := Self;
  FButtonGroup.Cursor := crHandPoint;
  FButtonGroup.InsertImage(FImages.ButtonGroupNormal, FImages.ButtonGroupActive, FImages.ButtonGroupDown);
  FButtonGroup.Top := (Height - FButtonGroup.Height) div 2;
  FButtonGroup.Left := (Width - FButtonGroup.Width) div 2;
  FButtonSettings.Tag := BUTTON_GROUP;
  FButtonSettings.OnClick := ButtonClick;
end;

destructor TControlPanel.Destroy;
begin
  FButtonAddUser.Free;
  inherited;
end;

procedure TControlPanel.EventClick(Button: TControlButton);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Button);
end;

procedure TControlPanel.ButtonClick(Sender: TObject);
begin
  case TButtonActive(Self).Tag of
    BUTTON_ADD_USER:
      EventClick(cbAddUser);

    BUTTON_SETTINGS:
      EventClick(cbSettings);

    BUTTON_GROUP:
      EventClick(cbGroup);
  end;
end;

end.
