unit JvCtrlsReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvctrlsreg.res}

uses
  Classes, JvDsgnConsts,
  JvHtControls, {JvDBHTLabel,} JvHint, JvHTHintForm, JvMovableBevel,
  PropEdits, Controls;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvMovableBevel, TJvHint, TJvHTLabel, TJvHTListbox, TJvHTCombobox
  ]);
  {
  RegisterComponents(RsPaletteBarPanel, [TJvMovableBevel]);
  RegisterComponents(RsPaletteLabel, [TJvHTLabel]);
  RegisterComponents(RsPaletteListComboTree, [TJvHTListBox, TJvHTComboBox]);
  RegisterComponents(RsPaletteNonVisual, [TJvHint]);
  }
  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
end;

end.

