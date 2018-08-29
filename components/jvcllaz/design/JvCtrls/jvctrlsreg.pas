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
  JvMovableBevel, JvRuler,
  JvHtControls, {JvDBHTLabel,} JvHint, JvHTHintForm,
  PropEdits, Controls;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvMovableBevel, TJvMovablePanel, TJvRuler,
    TJvHint, TJvHTLabel, TJvHTListbox, TJvHTCombobox
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

