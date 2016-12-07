object ffParamEditor: TffParamEditor
  Left = 191
  Top = 105
  Width = 159
  Height = 160
  BorderIcons = [biSystemMenu]
  Caption = 'Param Editor'
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbItems: TListBox
    Left = 0
    Top = 0
    Width = 151
    Height = 133
    Align = alClient
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnClick = lbItemsClick
  end
end
