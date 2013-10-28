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
  Classes, Controls, ResourceImage, ButtonActive, SkinControlPanel;

type
  TControlButton = (cbAddUser, cbSettings, cbGroup);
  TControlPanelClick = procedure(Sender: TObject; Button: TControlButton) of object;

  TControlPanel = class(TCustomControl)
  private
    FButtonAddUser: TButtonActive;
    FButtonSettings: TButtonActive;
    FButtonGroup: TButtonActive;
    FSkin: TSkinControlPanel;
    FOnClick: TControlPanelClick;
    procedure EventClick(Button: TControlButton);
    procedure ButtonClick(Sender: TObject);
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; Skin: TSkinControlPanel); reintroduce;
    destructor Destroy; override;

    property OnClick: TControlPanelClick read FOnClick write FOnClick;
  end;

implementation

const
  BUTTON_ADD_USER         = 1;
  BUTTON_SETTINGS         = 2;
  BUTTON_GROUP            = 3;

{ TControlPanel }

constructor TControlPanel.Create(AOwner: TComponent; Skin: TSkinControlPanel);
begin
  inherited Create(AOwner);
  FSkin := Skin;
  Align := alBottom;
end;

procedure TControlPanel.CreateWnd;
begin
  inherited;
  DoubleBuffered := True;

  Color := FSkin.BackColor;
  Constraints.MinHeight := FSkin.Height;
  Constraints.MaxHeight := FSkin.Height;

  FButtonAddUser := TButtonActive.Create(Self);
  FButtonAddUser.InsertImage(FSkin.ImgNewFriend[0], FSkin.ImgNewFriend[1],
    FSkin.ImgNewFriend[2]);
  FButtonAddUser.Cursor := crHandPoint;
  FButtonAddUser.Left := FSkin.MarginLeft;
  FButtonAddUser.Top := (ClientHeight - FButtonAddUser.Height) div 2;
  FButtonAddUser.Tag := BUTTON_ADD_USER;
  FButtonAddUser.OnClick := ButtonClick;
  FButtonAddUser.Parent := Self;

  FButtonSettings := TButtonActive.Create(Self);
  FButtonSettings.InsertImage(FSkin.ImgSettings[0], FSkin.ImgSettings[1],
    FSkin.ImgSettings[2]);
  FButtonSettings.Cursor := crHandPoint;
  FButtonSettings.Top := (ClientHeight - FButtonSettings.Height) div 2;
  FButtonSettings.Left := Width - FButtonSettings.Width - FSkin.MarginRight;
  FButtonSettings.Tag := BUTTON_SETTINGS;
  FButtonSettings.OnClick := ButtonClick;
  FButtonSettings.Parent := Self;

  FButtonGroup := TButtonActive.Create(Self);
  FButtonGroup.Parent := Self;
  FButtonGroup.InsertImage(FSkin.ImgNewGroup[0], FSkin.ImgNewGroup[1],
    FSkin.ImgNewGroup[2]);
  FButtonGroup.Top := (Height - FButtonGroup.Height) div 2;
  FButtonGroup.Left := (Width - FButtonGroup.Width) div 2;
  FButtonGroup.Cursor := crHandPoint;
  FButtonGroup.Tag := BUTTON_GROUP;
  FButtonGroup.OnClick := ButtonClick;
end;

destructor TControlPanel.Destroy;
begin
  FButtonAddUser.Free;
  FButtonSettings.Free;
  FButtonGroup.Free;
  inherited;
end;

procedure TControlPanel.Resize;
begin
  if not Assigned(FButtonAddUser) then
    Exit;

  FButtonAddUser.Top := (Height - FButtonAddUser.Height) div 2;

  FButtonSettings.Top := (Height - FButtonSettings.Height) div 2;
  FButtonSettings.Left := Width - FButtonSettings.Width - FSkin.MarginRight;

  FButtonGroup.Top := (Height - FButtonGroup.Height) div 2;
  FButtonGroup.Left := (Width - FButtonGroup.Width) div 2;
end;

procedure TControlPanel.EventClick(Button: TControlButton);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Button);
end;

procedure TControlPanel.ButtonClick(Sender: TObject);
begin
  case TButtonActive(Sender).Tag of
    BUTTON_ADD_USER:
      EventClick(cbAddUser);

    BUTTON_SETTINGS:
      EventClick(cbSettings);

    BUTTON_GROUP:
      EventClick(cbGroup);
  end;
end;

end.
