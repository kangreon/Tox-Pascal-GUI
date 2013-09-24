object FormNewName: TFormNewName
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'FormNewName'
  ClientHeight = 98
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object labTitle: TLabel
    Left = 8
    Top = 8
    Width = 175
    Height = 13
    Caption = #1042#1074#1077#1076#1080#1090#1077' '#1085#1086#1074#1086#1077' '#1080#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103':'
  end
  object edName: TEdit
    Left = 8
    Top = 27
    Width = 225
    Height = 21
    TabOrder = 0
  end
  object btnChange: TButton
    Left = 77
    Top = 65
    Width = 75
    Height = 25
    Caption = 'btnChange'
    Default = True
    TabOrder = 1
    OnClick = btnChangeClick
  end
  object btnCancel: TButton
    Left = 158
    Top = 65
    Width = 75
    Height = 25
    Caption = 'btnCancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
