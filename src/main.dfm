object Form1: TForm1
  Left = 179
  Top = 130
  ActiveControl = Panel1
  Caption = 'Form1'
  ClientHeight = 400
  ClientWidth = 843
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 457
    Top = 106
    Width = 284
    Height = 195
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 288
    Top = 32
    Width = 437
    Height = 66
    BevelOuter = bvNone
    TabOrder = 1
    object Edit1: TEdit
      Left = 8
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
      OnChange = Edit1Change
    end
    object Button1: TButton
      Left = 262
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 1
      OnClick = btnAddUser
    end
    object Edit2: TEdit
      Left = 135
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'Edit2'
    end
    object Edit3: TEdit
      Left = 72
      Top = 35
      Width = 184
      Height = 21
      TabOrder = 3
      Text = 'Edit3'
    end
    object Button2: TButton
      Left = 262
      Top = 33
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 4
      OnClick = Button2Click
    end
    object Edit4: TEdit
      Left = 8
      Top = 35
      Width = 58
      Height = 21
      TabOrder = 5
      Text = '0'
    end
  end
  object ListBox1: TListBox
    Left = 296
    Top = 94
    Width = 153
    Height = 195
    ItemHeight = 13
    ScrollWidth = 151
    TabOrder = 2
  end
end
