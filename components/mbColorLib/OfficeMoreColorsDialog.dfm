object OfficeMoreColorsWin: TOfficeMoreColorsWin
  Left = 194
  Top = 112
  Width = 331
  Height = 358
  ActiveControl = OKbtn
  BorderIcons = [biSystemMenu]
  Caption = 'More colors...'
  Color = clBtnFace
  Constraints.MinHeight = 358
  Constraints.MinWidth = 331
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  DesignSize = (
    315
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 268
    Top = 218
    Width = 21
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'New'
    Transparent = True
  end
  object Label5: TLabel
    Left = 260
    Top = 306
    Width = 37
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Current'
    Transparent = True
  end
  object Pages: TPageControl
    Left = 6
    Top = 6
    Width = 227
    Height = 316
    ActivePage = Standard
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PagesChange
    object Standard: TTabSheet
      Caption = 'Standard'
      DesignSize = (
        219
        288)
      object Label2: TLabel
        Left = 6
        Top = 7
        Width = 34
        Height = 13
        Caption = '&Colors:'
        FocusControl = Hexa
        Transparent = True
      end
      object Hexa: THexaColorPicker
        Left = 6
        Top = 26
        Width = 209
        Height = 207
        Anchors = [akLeft, akTop, akRight, akBottom]
        HintFormat = 'RGB(%r, %g, %b)'#13'Hex: %h'
        IntensityText = 'Intensity'
        TabOrder = 0
        Constraints.MinHeight = 85
        Constraints.MinWidth = 93
        OnChange = HexaChange
      end
    end
    object Custom: TTabSheet
      Caption = 'Custom'
      ImageIndex = 1
      DesignSize = (
        219
        288)
      object Label1: TLabel
        Left = 6
        Top = 7
        Width = 34
        Height = 13
        Caption = '&Colors:'
        FocusControl = HSL
      end
      object Label3: TLabel
        Left = 6
        Top = 178
        Width = 60
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Color mo&del:'
        FocusControl = ColorModel
      end
      object LRed: TLabel
        Left = 6
        Top = 204
        Width = 23
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '&Red:'
      end
      object LGreen: TLabel
        Left = 6
        Top = 230
        Width = 33
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '&Green:'
      end
      object LBlue: TLabel
        Left = 6
        Top = 256
        Width = 24
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '&Blue:'
      end
      object HSL: THSLColorPicker
        Left = 6
        Top = 20
        Width = 211
        Height = 152
        HSPickerHintFormat = 'H: %h S: %s'#13'Hex: %hex'
        LPickerHintFormat = 'Luminance: %l'
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = HSLChange
        DesignSize = (
          211
          152)
      end
      object ColorModel: TComboBox
        Left = 74
        Top = 172
        Width = 92
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = 'RGB'
        OnChange = ColorModelChange
        Items.Strings = (
          'RGB'
          'HSL')
      end
    end
  end
  object OKbtn: TButton
    Left = 242
    Top = 6
    Width = 73
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Cancelbtn: TButton
    Left = 242
    Top = 36
    Width = 73
    Height = 23
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object NewSwatch: TmbColorPreview
    Left = 246
    Top = 238
    Width = 68
    Height = 32
    Hint = 'RGB(255, 255, 255)'
    Anchors = [akRight, akBottom]
    ShowHint = True
    ParentShowHint = False
    OnColorChange = NewSwatchColorChange
  end
  object OldSwatch: TmbColorPreview
    Left = 246
    Top = 269
    Width = 68
    Height = 32
    Hint = 'RGB(255, 255, 255)'#13#10'Hex: FFFFFF'
    Anchors = [akRight, akBottom]
    ShowHint = True
    ParentShowHint = False
    OnColorChange = OldSwatchColorChange
  end
end
