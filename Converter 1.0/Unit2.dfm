object Form2: TForm2
  Left = 338
  Top = 234
  BorderStyle = bsDialog
  Caption = 'Show point'
  ClientHeight = 168
  ClientWidth = 226
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 63
    Height = 13
    Caption = 'X coordinate:'
  end
  object Label2: TLabel
    Left = 16
    Top = 32
    Width = 63
    Height = 13
    Caption = 'Y coordinate:'
  end
  object Label3: TLabel
    Left = 16
    Top = 88
    Width = 30
    Height = 13
    Caption = 'Scale:'
  end
  object Label4: TLabel
    Left = 16
    Top = 56
    Width = 63
    Height = 13
    Caption = 'Z coordinate:'
  end
  object lblPercent: TLabel
    Left = 200
    Top = 96
    Width = 8
    Height = 13
    Caption = '%'
  end
  object edtCoordX: TEdit
    Left = 88
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '0'
    OnChange = EditChange
    OnKeyPress = EditKeyPress
  end
  object edtScale: TEdit
    Left = 88
    Top = 88
    Width = 105
    Height = 21
    TabOrder = 3
    Text = '100'
    OnChange = EditChange
    OnKeyPress = EditKeyPress
  end
  object Button1: TButton
    Left = 24
    Top = 128
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 128
    Top = 128
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = Button2Click
  end
  object edtCoordY: TEdit
    Left = 88
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '0'
    OnChange = EditChange
    OnKeyPress = EditKeyPress
  end
  object edtCoordZ: TEdit
    Left = 88
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '0'
    OnChange = EditChange
    OnKeyPress = EditKeyPress
  end
end
