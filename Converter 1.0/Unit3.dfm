object Form3: TForm3
  Left = 337
  Top = 152
  BorderStyle = bsDialog
  Caption = 'Find text'
  ClientHeight = 170
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 44
    Height = 13
    Caption = 'Input text'
  end
  object Edit1: TEdit
    Left = 64
    Top = 16
    Width = 305
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 96
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Find'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 136
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object rgSelection: TRadioGroup
    Left = 8
    Top = 48
    Width = 177
    Height = 81
    Caption = 'Selection'
    ItemIndex = 0
    Items.Strings = (
      'No selection'
      'Rectangle around'
      'Ellipse around')
    TabOrder = 3
    OnClick = rgSelectionClick
  end
  object rgSearchMode: TRadioGroup
    Left = 192
    Top = 48
    Width = 177
    Height = 81
    Caption = 'Search Mode'
    ItemIndex = 0
    Items.Strings = (
      'Find first'
      'Find all')
    TabOrder = 4
    OnClick = rgSearchModeClick
  end
end
