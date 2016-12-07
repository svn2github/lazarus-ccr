object frmFieldLinkDesigner: TfrmFieldLinkDesigner
  Left = 195
  Top = 119
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Field Link Designer'
  ClientHeight = 263
  ClientWidth = 350
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 83
    Height = 13
    Caption = 'A&vailable Indexes'
    FocusControl = cboDetailIndexes
  end
  object pnlMain: TPanel
    Left = 8
    Top = 38
    Width = 334
    Height = 187
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 10
      Width = 57
      Height = 13
      Caption = 'D&etail Fields'
      FocusControl = lstDetailFields
    end
    object Label3: TLabel
      Left = 215
      Top = 8
      Width = 62
      Height = 13
      Caption = '&Master Fields'
      FocusControl = lstMasterFields
    end
    object Label4: TLabel
      Left = 8
      Top = 104
      Width = 61
      Height = 13
      Caption = '&Joined Fields'
      FocusControl = lstJoinedFields
    end
    object lstDetailFields: TListBox
      Left = 8
      Top = 26
      Width = 110
      Height = 73
      ItemHeight = 13
      TabOrder = 0
      OnClick = EnableAddButton
    end
    object lstMasterFields: TListBox
      Left = 215
      Top = 26
      Width = 110
      Height = 73
      ItemHeight = 13
      TabOrder = 2
      OnClick = EnableAddButton
    end
    object btnAdd: TButton
      Left = 130
      Top = 50
      Width = 75
      Height = 25
      Caption = '&Add'
      Enabled = False
      TabOrder = 1
      OnClick = btnAddClick
    end
    object lstJoinedFields: TListBox
      Left = 8
      Top = 120
      Width = 235
      Height = 57
      ItemHeight = 13
      TabOrder = 3
      OnClick = lstJoinedFieldsClick
    end
    object btnDelete: TButton
      Left = 250
      Top = 120
      Width = 75
      Height = 25
      Caption = '&Delete'
      Enabled = False
      TabOrder = 4
      OnClick = btnDeleteClick
    end
    object btnClear: TButton
      Left = 250
      Top = 152
      Width = 75
      Height = 25
      Caption = '&Clear'
      Enabled = False
      TabOrder = 5
      OnClick = btnClearClick
    end
  end
  object cboDetailIndexes: TComboBox
    Left = 104
    Top = 8
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnClick = cboDetailIndexesClick
  end
  object btnOK: TButton
    Left = 93
    Top = 232
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 181
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
