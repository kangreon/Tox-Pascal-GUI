object FormFriendRequest: TFormFriendRequest
  Left = 0
  Top = 0
  Caption = 'FormFriendRequest'
  ClientHeight = 232
  ClientWidth = 330
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
  object gbFriendRequest: TGroupBox
    Left = 8
    Top = 8
    Width = 314
    Height = 185
    Caption = 'gbFriendRequest'
    TabOrder = 0
    object labFromUser: TLabel
      Left = 16
      Top = 16
      Width = 60
      Height = 13
      Caption = 'labFromUser'
    end
    object labWithMessage: TLabel
      Left = 16
      Top = 62
      Width = 78
      Height = 13
      Caption = 'labWithMessage'
    end
    object edFromUser: TEdit
      Left = 16
      Top = 35
      Width = 281
      Height = 21
      TabOrder = 0
      Text = 'edFromUser'
    end
    object memWithMessage: TMemo
      Left = 16
      Top = 81
      Width = 281
      Height = 89
      Lines.Strings = (
        'memWithMessage')
      TabOrder = 1
    end
  end
  object btnAdd: TButton
    Left = 166
    Top = 199
    Width = 75
    Height = 25
    Caption = 'btnAdd'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnReject: TButton
    Left = 247
    Top = 199
    Width = 75
    Height = 25
    Caption = 'btnReject'
    TabOrder = 2
    OnClick = btnRejectClick
  end
  object btnIgnore: TButton
    Left = 8
    Top = 199
    Width = 75
    Height = 25
    Caption = 'btnIgnore'
    TabOrder = 3
    OnClick = btnIgnoreClick
  end
end
