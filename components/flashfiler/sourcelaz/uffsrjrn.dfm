object JournalForm: TJournalForm
  Left = 274
  Top = 135
  BorderStyle = bsDialog
  Caption = 'FlashFiler Journal Recovery'
  ClientHeight = 193
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 427
    Height = 193
    Align = alLeft
    TabOrder = 0
    object Bevel1: TBevel
      Left = 1
      Top = 1
      Width = 425
      Height = 191
      Align = alClient
    end
    object CompletenessLabel: TLabel
      Left = 15
      Top = 16
      Width = 257
      Height = 24
      Alignment = taCenter
      Caption = 'Incomplete Journal File Found'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ActionLabel: TLabel
      Left = 16
      Top = 152
      Width = 370
      Height = 24
      Alignment = taCenter
      Caption = 'Recovery is impossible.  File will be deleted.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ExceptionLabel: TLabel
      Left = 16
      Top = 128
      Width = 73
      Height = 13
      Caption = 'ExceptionLabel'
    end
    object FilenameLabel: TLabel
      Left = 16
      Top = 104
      Width = 68
      Height = 13
      Caption = 'FilenameLabel'
    end
    object PathLabel: TLabel
      Left = 16
      Top = 80
      Width = 48
      Height = 13
      Caption = 'PathLabel'
    end
    object AliasLabel: TLabel
      Left = 16
      Top = 56
      Width = 48
      Height = 13
      Caption = 'AliasLabel'
    end
  end
  object Panel2: TPanel
    Left = 429
    Top = 0
    Width = 107
    Height = 193
    Align = alRight
    TabOrder = 1
    object OKBtn: TBitBtn
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      NumGlyphs = 2
    end
    object RollbackBtn: TBitBtn
      Left = 16
      Top = 56
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Rollback'
      ModalResult = 2
      TabOrder = 1
      NumGlyphs = 2
    end
    object PrintBtn: TBitBtn
      Left = 16
      Top = 160
      Width = 75
      Height = 25
      Caption = '&Print'
      TabOrder = 2
      OnClick = PrintBtnClick
      NumGlyphs = 2
    end
  end
end
