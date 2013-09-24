object FormUserAdd: TFormUserAdd
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'FormUserAdd'
  ClientHeight = 189
  ClientWidth = 306
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object gbFriendAdd: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 137
    Caption = #1044#1086#1073#1072#1074#1083#1077#1085#1080#1077' '#1076#1088#1091#1075#1072':'
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 0
    object labFriendAddress: TLabel
      Left = 7
      Top = 27
      Width = 89
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = #1040#1076#1088#1077#1089' '#1076#1088#1091#1075#1072':'
    end
    object labMessage: TLabel
      Left = 7
      Top = 51
      Width = 89
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1077':'
    end
    object edFrienAddress: TEdit
      Left = 102
      Top = 24
      Width = 171
      Height = 21
      TabOrder = 0
    end
    object memMessage: TMemo
      Left = 102
      Top = 51
      Width = 171
      Height = 70
      Lines.Strings = (
        'Memo1')
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object btnCancel: TButton
    Left = 141
    Top = 156
    Width = 75
    Height = 25
    Caption = 'btnCancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnAddFriend: TButton
    Left = 222
    Top = 156
    Width = 75
    Height = 25
    Caption = 'btnAddFriend'
    Default = True
    TabOrder = 1
    OnClick = btnAddFriendClick
  end
end
