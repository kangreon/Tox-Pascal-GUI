object FormUserAdd: TFormUserAdd
  Left = 0
  Top = 0
  Caption = 'FormUserAdd'
  ClientHeight = 168
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 137
    Caption = #1044#1086#1073#1072#1074#1083#1077#1085#1080#1077' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103':'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 27
      Width = 68
      Height = 13
      Caption = #1040#1076#1088#1077#1089' '#1076#1088#1091#1075#1072':'
    end
    object Label2: TLabel
      Left = 22
      Top = 51
      Width = 62
      Height = 13
      Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1077':'
    end
    object Edit1: TEdit
      Left = 90
      Top = 24
      Width = 183
      Height = 21
      TabOrder = 0
    end
    object Memo1: TMemo
      Left = 90
      Top = 51
      Width = 183
      Height = 70
      Lines.Strings = (
        'Memo1')
      TabOrder = 1
    end
  end
end
