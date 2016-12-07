object Form1: TForm1
  Left = 224
  Height = 287
  Top = 96
  Width = 540
  Caption = 'FlashFiler Example - Customer Data'
  ClientHeight = 268
  ClientWidth = 540
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Menu = MainMenu1
  OnShow = FormShow
  LCLVersion = '1.6.1.0'
  object ToolBar1: TToolBar
    Left = 0
    Height = 23
    Top = 0
    Width = 540
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 55
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 0
    object TlBtnRunQuery: TToolButton
      Left = 1
      Top = 2
      Caption = 'RunQuery'
      ImageIndex = 0
      OnClick = TlBtnRunQueryClick
    end
    object ToolButton2: TToolButton
      Left = 57
      Height = 21
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object DBNavigator1: TDBNavigator
      Left = 65
      Height = 21
      Top = 2
      Width = 250
      BevelOuter = bvNone
      ChildSizing.EnlargeHorizontal = crsScaleChilds
      ChildSizing.EnlargeVertical = crsScaleChilds
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 100
      ClientHeight = 21
      ClientWidth = 250
      DataSource = CustomerData
      Flat = True
      Options = []
      TabOrder = 0
    end
  end
  object CustomerGrid: TDBGrid
    Left = 0
    Height = 156
    Top = 112
    Width = 540
    Align = alClient
    Color = clWindow
    Columns = <>
    DataSource = CustomerData
    TabOrder = 1
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
  end
  object Memo1: TMemo
    Left = 0
    Height = 89
    Top = 23
    Width = 540
    Align = alTop
    Lines.Strings = (
      'select * from ExCust where State=''NC''  AND CustomerID<50'
    )
    OnKeyDown = Memo1KeyDown
    TabOrder = 2
  end
  object ltMain: TffLegacyTransport
    Enabled = True
    ServerName = 'Local server'
    left = 352
    top = 88
  end
  object ffRSE: TFFRemoteServerEngine
    Transport = ltMain
    left = 320
    top = 88
  end
  object ffClient: TffClient
    ClientName = 'ffClient'
    ServerEngine = ffRSE
    left = 320
    top = 56
  end
  object ffSess: TffSession
    ClientName = 'ffClient'
    SessionName = 'ExCust'
    left = 352
    top = 56
  end
  object CustomerTable: TffTable
    DatabaseName = 'Tutorial'
    FieldDefs = <>
    FilterOptions = []
    IndexName = 'ByID'
    SessionName = 'ExCust'
    TableName = 'ExCust'
    Timeout = 10000
    left = 420
    top = 124
  end
  object CustomerData: TDataSource
    DataSet = ffQuery1
    left = 416
    top = 56
  end
  object MainMenu1: TMainMenu
    left = 448
    top = 56
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
  object ffQuery1: TffQuery
    DatabaseName = 'Tutorial'
    FilterOptions = []
    SessionName = 'ExCust'
    SQL.Strings = (
      'select * from ExCust where State=''NC''  AND CustomerID<50'
    )
    left = 382
    top = 38
  end
end
