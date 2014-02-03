object FormProfileSelect: TFormProfileSelect
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'FormProfileSelect'
  ClientHeight = 417
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnMouseLeave = FormMouseLeave
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object pnSelectProfile: TPanel
    Left = 240
    Top = 8
    Width = 249
    Height = 169
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object labUserName: TLabel
      Left = 0
      Top = 0
      Width = 249
      Height = 13
      Align = alTop
      Caption = 'labUserName'
      ExplicitWidth = 63
    end
    object labPassword: TLabel
      Left = 0
      Top = 34
      Width = 249
      Height = 13
      Align = alTop
      Caption = 'labPassword'
      ExplicitTop = 40
      ExplicitWidth = 60
    end
    object cbNickName: TComboBox
      Left = 0
      Top = 13
      Width = 249
      Height = 21
      Align = alTop
      TabOrder = 0
      ExplicitLeft = 3
      ExplicitTop = 16
      ExplicitWidth = 243
    end
    object edPassword: TEdit
      Left = 0
      Top = 47
      Width = 249
      Height = 21
      Align = alTop
      PasswordChar = '*'
      TabOrder = 1
      ExplicitTop = 53
    end
    object Panel1: TPanel
      Left = 0
      Top = 138
      Width = 249
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 2
      object btnLogin: TButton
        Left = 174
        Top = 0
        Width = 75
        Height = 31
        Align = alRight
        Caption = 'btnLogin'
        Default = True
        TabOrder = 0
        OnClick = btnLoginClick
      end
    end
  end
  object pnCreateAccount: TPanel
    Left = 240
    Top = 183
    Width = 246
    Height = 220
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object labCreateNickname: TLabel
      Left = 0
      Top = 0
      Width = 246
      Height = 13
      Align = alTop
      Caption = 'labCreateNickname'
      ExplicitWidth = 92
    end
    object labCreatePassword: TLabel
      Left = 0
      Top = 34
      Width = 246
      Height = 13
      Align = alTop
      Caption = 'labCreatePassword'
      ExplicitWidth = 93
    end
    object labConfirmPassword: TLabel
      Left = 0
      Top = 68
      Width = 246
      Height = 13
      Align = alTop
      Caption = 'labConfirmPassword'
      ExplicitWidth = 97
    end
    object edCreateNickname: TEdit
      Left = 0
      Top = 13
      Width = 246
      Height = 21
      Align = alTop
      TabOrder = 0
    end
    object edCreatePassword: TEdit
      Left = 0
      Top = 47
      Width = 246
      Height = 21
      Align = alTop
      PasswordChar = '*'
      TabOrder = 1
    end
    object edConfirmPassword: TEdit
      Left = 0
      Top = 81
      Width = 246
      Height = 21
      Align = alTop
      PasswordChar = '*'
      TabOrder = 2
    end
    object Panel3: TPanel
      Left = 0
      Top = 189
      Width = 246
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object btnCeateAccount: TButton
        Left = 128
        Top = 0
        Width = 118
        Height = 31
        Align = alRight
        Caption = 'btnCeateAccount'
        Default = True
        TabOrder = 0
        OnClick = btnCeateAccountClick
      end
    end
  end
end
