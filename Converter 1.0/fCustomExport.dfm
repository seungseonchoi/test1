object frmCustomExport: TfrmCustomExport
  Left = 0
  Top = 0
  Width = 1119
  Height = 660
  TabOrder = 0
  TabStop = True
  object tvTabs: TTreeView
    Left = 0
    Top = 1
    Width = 195
    Height = 331
    Indent = 19
    TabOrder = 0
    OnClick = tvTabsClick
    OnKeyUp = tvTabsKeyUp
    Items.NodeData = {
      0304000000580000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF000000
      0000000000011D4F00750074007000750074002000730069007A00650073002C
      00200063006F006C006F00720073002C0020007100750061006C006900740079
      003A0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF00000000000000
      00010E4F007500740070007500740020006C00610079006F0075007400730032
      0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF000000000000000001
      0A4C0069006E0065002000770069006400740068003400000000000000000000
      00FFFFFFFFFFFFFFFFFFFFFFFF0000000000000000010B440065007300630072
      0069007000740069006F006E00}
  end
  object pnlSizes: TPanel
    Left = 184
    Top = 0
    Width = 434
    Height = 332
    BevelOuter = bvNone
    TabOrder = 1
    object gbSizes: TGroupBox
      Left = 8
      Top = 0
      Width = 425
      Height = 146
      Caption = 'Page Sizes'
      TabOrder = 0
      object lblWidth: TLabel
        Left = 27
        Top = 68
        Width = 32
        Height = 13
        Caption = '&Width:'
        FocusControl = edtWidth
      end
      object lblHeight: TLabel
        Left = 150
        Top = 68
        Width = 35
        Height = 13
        Caption = 'Hei&ght:'
        FocusControl = edtHeight
      end
      object lblUnits: TLabel
        Left = 291
        Top = 68
        Width = 28
        Height = 13
        Caption = '&Units:'
        FocusControl = cbUnits
      end
      object lblMargin: TLabel
        Left = 16
        Top = 120
        Width = 36
        Height = 13
        Caption = '&Margin:'
      end
      object lblMarginUnits: TLabel
        Left = 128
        Top = 120
        Width = 16
        Height = 13
        Caption = 'mm'
      end
      object rbCustom: TRadioButton
        Left = 16
        Top = 45
        Width = 401
        Height = 17
        Caption = '&Custom size'
        TabOrder = 2
        OnClick = rbCustomClick
      end
      object edtWidth: TEdit
        Left = 89
        Top = 65
        Width = 44
        Height = 21
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '210'
        OnExit = edtExit
        OnKeyPress = edtKeyPress
      end
      object edtHeight: TEdit
        Left = 214
        Top = 65
        Width = 44
        Height = 21
        Color = clBtnFace
        Enabled = False
        TabOrder = 4
        Text = '297'
        OnExit = edtExit
        OnKeyPress = edtKeyPress
      end
      object cbUnits: TComboBox
        Left = 355
        Top = 65
        Width = 62
        Height = 21
        Style = csDropDownList
        Color = clBtnFace
        Enabled = False
        ItemIndex = 0
        TabOrder = 5
        Text = 'mm'
        Items.Strings = (
          'mm'
          'inches')
      end
      object rbStandard: TRadioButton
        Left = 16
        Top = 18
        Width = 128
        Height = 17
        Caption = '&Standard:'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbCustomClick
      end
      object cbSizes: TComboBox
        Left = 150
        Top = 16
        Width = 267
        Height = 21
        Style = csDropDownList
        ItemIndex = 8
        TabOrder = 1
        Text = 'A4 210 x 297 %s (8.27 x 11.69 ")'
        Items.Strings = (
          'A0 1189 x 841 %s (46.81 x 33.11 ")'
          'A0 841 x 1189 %s (33.11 x 46.81 ")'
          'A1 594 x 841 %s (23.39 x 33.11 ")'
          'A1 841 x 594 %s (33.11 x 23.39 ")'
          'A2 420 x 594 %s (16.54 x 23.39 ")'
          'A2 594 x 420 %s (23.39 x 16.54 ")'
          'A3 297 x 420 %s (11.69 x 16.54 ")'
          'A3 420 x 297 %s (16.54 x 11.69 ")'
          'A4 210 x 297 %s (8.27 x 11.69 ")'
          'A4 297 x 210 %s (11.69 x 8.27 ")'
          'A5 148 x 210 %s (5.83 x 8.27 ")'
          'A5 210 x 148 %s (8.27 x 5.83 ")'
          'A6 105 x 148 %s (4.13 x 5.83 ")'
          'A6 148 x 105 %s (5.83 x 4.13 ")'
          'A7 105 x 74 %s (4.13 x 2.91 ")'
          'A7 74 x 105 %s (2.91 x 4.13 ")'
          'ANSI A 11 x 8.5 " (279.4 x 215.9 %s)'
          'ANSI A 8.5 x 11 " (215.9 x 279.4 %s)'
          'ANSI B 11 x 17 " (279.4 x 431.8 %s)'
          'ANSI B 17 x 11 " (431.8 x 279.4 %s)'
          'ANSI C 17 x 22 " (431.8 x 538.8 %s)'
          'ANSI C 22 x 17 " (538.8 x 431.8 %s)'
          'ANSI D 22 x 34 " (558.8 x 863.6 %s)'
          'ANSI D 34 x 22 " (863.6 x 558.8 %s)'
          'ANSI E 34 x 44 " (863.6 x 1117.6 %s)'
          'ANSI E 44 x 34 " (1117.6 x 863.6 %s)'
          'ANSI E1 44 x 68 " (1117.6 x 1727.2 %s)'
          'ANSI E1 68 x 44 " (1727.2 x 1117.6 %s)'
          'ANSI F 28 x 40 " (711.2 x 1016 %s)'
          'ANSI F 40 x 28 " (1016 x 711.2 %s)'
          'ARCH A 12 x 9 " (304.8 x 228.6 %s)'
          'ARCH A 9 x 12 " (228.6 x 304.8 %s)'
          'ARCH B 12 x 18 " (304.8 x 457.2 %s)'
          'ARCH B 18 x 12 " (457.2 x 304.8 %s)'
          'ARCH C 18 x 24 " (457.2 x 609.6 %s)'
          'ARCH C 24 x 18 " (609.6 x 457.2 %s)'
          'ARCH D 24 x 36 " (609.6 x 914.4 %s)'
          'ARCH D 36 x 24 " (914.4 x 609.6 %s)'
          'ARCH E 36 x 48 " (914.4 x 1219.2 %s)'
          'ARCH E 48 x 36 " (1219.2 x 914.4 %s)'
          'B0 1000 x 1414 %s (39.37 x 55.67 ")'
          'B0 1414 x 1000 %s (55.67 x 39.37 ")'
          'B1 1000 x 707 %s (39.37 x 27.83 ")'
          'B1 707 x 1000 %s (27.83 x 39.37 ")'
          'B2 500 x 707 %s (19.69 x 27.83 ")'
          'B2 707 x 500 %s (27.83 x 19.69 ")'
          'B3 353 x 500 %s (13.90 x 19.69 ")'
          'B3 500 x 353 %s (19.69 x 13.90 ")'
          'B4 250 x 353 %s (9.84 x 13.90 ")'
          'B4 353 x 250 %s (13.90 x 9.84 ")'
          'B5 176 x 250 %s (6.93 x 9.84 ")'
          'B5 250 x 176 %s (9.84 x 6.93 ")'
          'B6 125 x 176 %s (4.92 x 6.93 ")'
          'B6 176 x 125 %s (6.93 x 4.92 ")'
          'C0 1297 x 917 %s (51.06 x 36 ")'
          'C0 917 x 1297 %s (36 x 51.06 ")'
          'C1 648 x 917 %s (25.60 x 36 ")'
          'C1 917 x 648 %s (36 x 25.60 ")'
          'C2 458 x 648 %s (18.03 x 25.51 ")'
          'C2 648 x 458 %s (25.51 x 18.03 ")'
          'C3 324 x 458 %s (12.75 x 18.03 ")'
          'C3 458 x 324 %s (18.03 x 12.75 ")'
          'C4 229 x 324 %s (9.02 x 12.76 ")'
          'C4 324 x 229 %s (12.76 x 9.02 ")'
          'C5 162 x 229 %s (6.38 x 9.02 ")'
          'C5 229 x 162 %s (9.02 x 6.38 ")'
          'C6 114 x 162 %s (4.49 x 6.38 ")'
          'C6 162 x 114 %s (6.38 x 4.49 ")'
          'Flsa 215.9 x 330.2 %s'
          'Flsa 330.2 x 215.9 %s'
          'Half Letter 5.5 x 8.5 " (139.7 x 215.9 %s)'
          'Half Letter 8.5 x 5.5 " (215.9 x 139.7 %s)'
          'Legal 14 x 8.50 " (355.6 x 215.9 %s)'
          'Legal 8.50 x 14 " (215.9 x 355.6 %s)'
          'Letter 11 x 8.50 " (279.4 x 215.9 %s)'
          'Letter 8.50 x 11 " (215.9 x 279.4 %s)'
          'Note 190.5 x 254 %s'
          'Note 254 x 190.5 %s'
          'Tabloid / Ledger 11 x 17 " (279.4 x 431.8 %s)'
          'Tabloid / Ledger 17 x 11 " (431.8 x 279.4 %s)')
      end
      object edtMargin: TEdit
        Left = 89
        Top = 117
        Width = 33
        Height = 21
        TabOrder = 6
        Text = '5'
        OnExit = edtExit
        OnKeyPress = edtKeyPress
      end
      object rbPlotSetting: TRadioButton
        Left = 16
        Top = 92
        Width = 393
        Height = 17
        Caption = 'Get sizes from layout'#39's plot settings'
        TabOrder = 7
        OnClick = rbCustomClick
      end
    end
    object gbColors: TGroupBox
      Left = 9
      Top = 152
      Width = 425
      Height = 114
      Caption = 'Colors'
      TabOrder = 1
      object lblBackgroundColor: TLabel
        Left = 16
        Top = 59
        Width = 86
        Height = 13
        Caption = '&Background color:'
        FocusControl = ccbBackgroundColor
      end
      object lblDefaultColor: TLabel
        Left = 16
        Top = 87
        Width = 65
        Height = 13
        Caption = '&Default color:'
        FocusControl = ccbDefaultColor
      end
      object ccbBackgroundColor: TColorBox
        Left = 213
        Top = 56
        Width = 145
        Height = 22
        DefaultColorColor = clNone
        Selected = clWhite
        Style = [cbStandardColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 1
        OnGetColors = ccbColorGetColors
      end
      object ccbDefaultColor: TColorBox
        Left = 213
        Top = 84
        Width = 145
        Height = 22
        DefaultColorColor = clNone
        Style = [cbStandardColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 2
        OnGetColors = ccbColorGetColors
      end
      object rgPalette: TRadioGroup
        Left = 16
        Top = 16
        Width = 401
        Height = 34
        Caption = '&Palette'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Color palette'
          'B&lack/White'
          'Gra&y')
        TabOrder = 0
      end
    end
    object gbQuality: TGroupBox
      Left = 8
      Top = 272
      Width = 425
      Height = 51
      Caption = 'Quality'
      TabOrder = 2
      object cbHighQuality: TCheckBox
        Left = 16
        Top = 24
        Width = 369
        Height = 17
        Caption = 'cbHighQuality'
        TabOrder = 0
      end
    end
  end
  object pnlLayouts: TPanel
    Left = 624
    Top = 61
    Width = 425
    Height = 144
    BevelOuter = bvNone
    TabOrder = 2
    object gbLayouts: TGroupBox
      Left = 8
      Top = 0
      Width = 425
      Height = 132
      Caption = 'Output layouts'
      TabOrder = 0
      object rbModel: TRadioButton
        Left = 16
        Top = 19
        Width = 401
        Height = 17
        Caption = 'Model'
        TabOrder = 0
        OnClick = rbLayoutClick
      end
      object rbAllLayouts: TRadioButton
        Left = 16
        Top = 42
        Width = 401
        Height = 17
        Caption = 'All Layouts'
        TabOrder = 1
        OnClick = rbLayoutClick
      end
      object rbLayoutByName: TRadioButton
        Left = 16
        Top = 65
        Width = 165
        Height = 17
        Caption = 'Layouts by &name:'
        TabOrder = 2
        OnClick = rbLayoutClick
      end
      object rbAllPaperSpaces: TRadioButton
        Left = 14
        Top = 88
        Width = 401
        Height = 17
        Caption = 'All Layouts + model'
        Checked = True
        TabOrder = 3
        TabStop = True
        OnClick = rbLayoutClick
      end
      object edtLayoutName: TEdit
        Left = 187
        Top = 61
        Width = 230
        Height = 21
        Color = clBtnFace
        Enabled = False
        TabOrder = 4
      end
      object rbCurrentLayout: TRadioButton
        Left = 14
        Top = 111
        Width = 401
        Height = 17
        Caption = 'Current layout'
        TabOrder = 5
      end
    end
  end
  object pnlLineWidth: TPanel
    Left = 211
    Top = 336
    Width = 425
    Height = 329
    BevelOuter = bvNone
    TabOrder = 3
    object gbLinewidth: TGroupBox
      Left = 8
      Top = 0
      Width = 425
      Height = 111
      Caption = 'Line width'
      TabOrder = 0
      DesignSize = (
        425
        111)
      object lbWidthNull: TLabel
        Left = 16
        Top = 77
        Width = 123
        Height = 13
        Caption = '&Width of zero-width lines:'
        FocusControl = edtWidthNull
      end
      object lbWidthNullMM: TLabel
        Tag = 1005
        Left = 401
        Top = 77
        Width = 16
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'mm'
      end
      object lblLineWeightScale: TLabel
        Left = 16
        Top = 50
        Width = 82
        Height = 13
        Caption = '&Lineweight scale:'
        FocusControl = edtLineWeightScale
        Transparent = True
      end
      object cbSaveLinewight: TCheckBox
        Left = 16
        Top = 24
        Width = 401
        Height = 17
        Caption = '&Save lineweight'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object edtWidthNull: TEdit
        Left = 338
        Top = 74
        Width = 45
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 2
        Text = '0.0000'
        OnExit = edtExit
        OnKeyPress = edtKeyPress
      end
      object udWidthNull: TUpDown
        Left = 381
        Top = 74
        Width = 14
        Height = 21
        Anchors = [akTop, akRight]
        Max = 1000
        TabOrder = 3
        OnChangingEx = udWidthNullChangingEx
      end
      object edtLineWeightScale: TEdit
        Left = 275
        Top = 47
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '1.0'
        OnExit = edtExit
        OnKeyPress = edtKeyPress
      end
    end
    object gbWeightByColor: TGroupBox
      Left = 0
      Top = 117
      Width = 425
      Height = 196
      Caption = 'Set line weight by color'
      TabOrder = 1
      Visible = False
      DesignSize = (
        425
        196)
      object vleColorToLineWeight: TValueListEditor
        Left = 3
        Top = 22
        Width = 419
        Height = 133
        Anchors = [akLeft, akTop, akRight, akBottom]
        Strings.Strings = (
          '=')
        TabOrder = 0
        TitleCaptions.Strings = (
          'Color'
          'Lineweight')
        OnDrawCell = vleColorToLineWeightDrawCell
        ColWidths = (
          150
          263)
      end
      object btColorLoad: TButton
        Left = 8
        Top = 162
        Width = 81
        Height = 23
        Anchors = [akLeft, akBottom]
        Caption = 'Load'
        TabOrder = 1
      end
      object btColorSave: TButton
        Left = 168
        Top = 162
        Width = 81
        Height = 23
        Anchors = [akBottom]
        Caption = 'Save'
        TabOrder = 2
      end
      object btColorClear: TButton
        Left = 331
        Top = 162
        Width = 83
        Height = 23
        Anchors = [akRight, akBottom]
        Caption = 'Clear'
        TabOrder = 3
      end
    end
  end
  object pnlProps: TPanel
    Left = 624
    Top = 211
    Width = 425
    Height = 138
    BevelOuter = bvNone
    TabOrder = 4
    object gbProps: TGroupBox
      Left = 8
      Top = 0
      Width = 426
      Height = 128
      Caption = 'Description'
      TabOrder = 0
      object lblTitle: TLabel
        Left = 8
        Top = 19
        Width = 24
        Height = 13
        Caption = 'Title:'
      end
      object lblAuthor: TLabel
        Left = 8
        Top = 46
        Width = 37
        Height = 13
        Caption = 'Author:'
      end
      object lblSubject: TLabel
        Left = 8
        Top = 73
        Width = 40
        Height = 13
        Caption = 'Subject:'
      end
      object lblKeywords: TLabel
        Left = 8
        Top = 100
        Width = 51
        Height = 13
        Caption = 'Keywords:'
      end
      object edtTitle: TEdit
        Left = 132
        Top = 16
        Width = 286
        Height = 21
        TabOrder = 0
      end
      object edtAuthor: TEdit
        Left = 132
        Top = 43
        Width = 286
        Height = 21
        TabOrder = 1
      end
      object edtSubject: TEdit
        Left = 132
        Top = 70
        Width = 286
        Height = 21
        TabOrder = 2
      end
      object edtKeywords: TEdit
        Left = 132
        Top = 97
        Width = 286
        Height = 21
        TabOrder = 3
      end
    end
  end
end
