object fmImage2: TfmImage2
  Left = 300
  Top = 114
  Width = 585
  Height = 459
  Caption = 'Selected drawing part'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    577
    425)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 488
    Top = 392
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 561
    Height = 377
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 1
    object sgPaintBox: TsgImage
      Left = 1
      Top = 1
      Width = 559
      Height = 375
      Align = alClient
      Scale = 1
      IsBigScale = False
    end
  end
end
