object FFLoginDialog: TFFLoginDialog
  Left = 274
  Top = 309
  BorderStyle = bsDialog
  Caption = 'FlashFiler Server Log On'
  ClientHeight = 73
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblUserName: TLabel
    Left = 15
    Top = 15
    Width = 57
    Height = 13
    Caption = '&User name: '
    FocusControl = edtUserName
  end
  object lblPassword: TLabel
    Left = 15
    Top = 47
    Width = 52
    Height = 13
    Caption = '&Password: '
    FocusControl = edtPassword
  end
  object edtUserName: TEdit
    Left = 79
    Top = 11
    Width = 154
    Height = 21
    TabOrder = 0
  end
  object edtPassword: TEdit
    Left = 79
    Top = 43
    Width = 154
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 251
    Top = 9
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 251
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
