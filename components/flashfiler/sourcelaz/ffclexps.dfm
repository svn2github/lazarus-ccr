object frmSelectProtocols: TfrmSelectProtocols
  Left = 305
  Top = 165
  BorderStyle = bsDialog
  Caption = 'Select FlashFiler Protocols'
  ClientHeight = 168
  ClientWidth = 254
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 7
    Top = 13
    Width = 241
    Height = 118
    Caption = 'What protocols would you like to support?'
    TabOrder = 0
    object chkSU: TCheckBox
      Left = 20
      Top = 33
      Width = 78
      Height = 13
      Caption = '&Single User'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkIPX: TCheckBox
      Left = 20
      Top = 59
      Width = 78
      Height = 13
      Caption = '&IPX/SPX'
      TabOrder = 1
    end
    object chkTCP: TCheckBox
      Left = 20
      Top = 85
      Width = 78
      Height = 13
      Caption = '&TCP/IP'
      TabOrder = 2
    end
  end
  object Button1: TButton
    Left = 13
    Top = 143
    Width = 61
    Height = 20
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
