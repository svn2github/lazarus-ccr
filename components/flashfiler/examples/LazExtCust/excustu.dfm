object Form1: TForm1
  Left = 200
  Top = 108
  Width = 548
  Height = 333
  Caption = 'FlashFiler Example - Customer Data'
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CustomerGrid: TDBGrid
    Left = 0
    Top = 30
    Width = 540
    Height = 257
    Align = alClient
    DataSource = CustomerData
    TabOrder = 0
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 0
    Width = 540
    Height = 30
    DataSource = CustomerData
    Align = alTop
    Flat = True
    TabOrder = 1
  end
  object ltMain: TffLegacyTransport
    Enabled = True
    Left = 352
    Top = 88
  end
  object ffRSE: TFFRemoteServerEngine
    Transport = ltMain
    Left = 320
    Top = 88
  end
  object ffClient: TffClient
    ClientName = 'ffClient'
    ServerEngine = ffRSE
    Left = 320
    Top = 56
  end
  object ffSess: TffSession
    ClientName = 'ffClient'
    SessionName = 'ExCust'
    Left = 352
    Top = 56
  end
  object CustomerTable: TffTable
    DatabaseName = 'Tutorial'
    IndexName = 'ByID'
    SessionName = 'ExCust'
    TableName = 'ExCust'
    Timeout = 10000
    Left = 384
    Top = 56
  end
  object CustomerData: TDataSource
    DataSet = CustomerTable
    Left = 416
    Top = 56
  end
  object MainMenu1: TMainMenu
    Left = 448
    Top = 56
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open'
        OnClick = Open1Click
      end
      object Close1: TMenuItem
        Caption = '&Close'
        Enabled = False
        OnClick = Close1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
    object Navigate1: TMenuItem
      Caption = '&Navigate'
      Enabled = False
      object First1: TMenuItem
        Caption = '&First'
        OnClick = First1Click
      end
      object Last1: TMenuItem
        Caption = '&Last'
        OnClick = Last1Click
      end
      object Next1: TMenuItem
        Caption = '&Next'
        OnClick = Next1Click
      end
      object Prior1: TMenuItem
        Caption = '&Prior'
        OnClick = Prior1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      Enabled = False
      object Append1: TMenuItem
        Caption = '&Append'
        OnClick = Append1Click
      end
      object Insert1: TMenuItem
        Caption = '&Insert'
        OnClick = Insert1Click
      end
      object Post1: TMenuItem
        Caption = '&Post'
        OnClick = Post1Click
      end
      object Refresh1: TMenuItem
        Caption = '&Refresh'
        OnClick = Refresh1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Cancel1: TMenuItem
        Caption = '&Cancel'
        OnClick = Cancel1Click
      end
    end
  end
end
