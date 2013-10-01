object Form1: TForm1
  Left = 214
  Top = 135
  Caption = 'Form1'
  ClientHeight = 180
  ClientWidth = 312
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
  object ActivityList: TMemo
    Left = 85
    Top = 8
    Width = 71
    Height = 81
    TabStop = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 147
    Width = 312
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object Edit3: TEdit
      Left = 58
      Top = 0
      Width = 162
      Height = 33
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 21
    end
    object Button2: TButton
      Left = 220
      Top = 0
      Width = 92
      Height = 33
      Align = alRight
      Caption = 'Send message'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Edit4: TEdit
      Left = 0
      Top = 0
      Width = 58
      Height = 33
      Align = alLeft
      TabOrder = 2
      Text = '0'
      ExplicitHeight = 21
    end
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 71
    Height = 81
    TabStop = False
    ItemHeight = 13
    TabOrder = 2
  end
end
