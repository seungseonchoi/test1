object fmFonts: TfmFonts
  Left = 374
  Top = 303
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Fonts options'
  ClientHeight = 181
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 55
    Top = 152
    Width = 75
    Height = 23
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 147
    Top = 152
    Width = 75
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 8
    Top = 16
    Width = 249
    Height = 129
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object lblSHXSearchPath: TLabel
      Left = 8
      Top = 20
      Width = 61
      Height = 13
      Caption = 'Search path:'
    end
    object lblDefautSHXPath: TLabel
      Left = 8
      Top = 52
      Width = 61
      Height = 13
      Caption = 'Default path:'
    end
    object lblDefaultSHXFont: TLabel
      Left = 8
      Top = 84
      Width = 58
      Height = 13
      Caption = 'Default font:'
    end
    object edSHXSearchPath: TEdit
      Left = 72
      Top = 12
      Width = 125
      Height = 21
      TabOrder = 0
    end
    object btnSHXSearchPath: TButton
      Left = 204
      Top = 12
      Width = 21
      Height = 23
      Caption = '...'
      TabOrder = 1
      OnClick = btnSHXSearchPathClick
    end
    object btnSHXDefaultPath: TButton
      Left = 204
      Top = 44
      Width = 21
      Height = 23
      Caption = '...'
      TabOrder = 2
      OnClick = btnSHXDefaultPathClick
    end
    object edSHXDefaultPath: TEdit
      Left = 72
      Top = 44
      Width = 125
      Height = 21
      TabOrder = 3
    end
    object edSHXDefaultFont: TEdit
      Left = 72
      Top = 76
      Width = 125
      Height = 21
      TabOrder = 4
    end
  end
  object cbUseSHXFonts: TCheckBox
    Left = 19
    Top = 8
    Width = 102
    Height = 17
    Caption = 'Use SHX fonts'
    TabOrder = 3
    OnClick = cbUseSHXFontsClick
  end
end
