object FFPickServerDlg: TFFPickServerDlg
  Left = 226
  Top = 174
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'FlashFiler Server Selection'
  ClientHeight = 73
  ClientWidth = 432
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 321
    Height = 57
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 32
    Width = 41
    Height = 13
    Caption = 'Login to:'
  end
  object CBNames: TComboBox
    Left = 64
    Top = 24
    Width = 249
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object OKBtn: TBitBtn
    Left = 344
    Top = 8
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
end
