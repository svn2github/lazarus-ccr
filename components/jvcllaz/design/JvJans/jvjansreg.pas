unit JvJansReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvjansreg.res}

uses
  Classes, JvDsgnConsts,
  JvYearGrid,
  //JvCSVData, JvCSVBaseControls, //JvCsvBaseEditor,
  JvMarkupViewer, JvMarkupLabel,
  JvSimScope, JvSimIndicator, JvSimPID, JvSimPIDLinker, JvSimLogic;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvYearGrid
  ]);

  // Simulator Components
  RegisterComponents(RsPaletteJvcl, [            // was: RsPaletteJansSim
    TJvSimScope, TJvSimIndicator, TJvSimPID,
    TJvSimPIDLinker, TJvSimConnector, TJvLogic, TJvSimButton, TJvSimLight,
    TJvSimLogicBox, TJvSimReverse]);

  // Markup components
  RegisterComponents(RsPaletteJvcl, [
    TJvMarkupViewer, TJvMarkupLabel
  ]);

                     (*
  // CSV Components
  RegisterComponents('Data Access', [TJvCSVDataset]);
  RegisterComponents(RsPaletteJansCsv, [TJvCSVBase, TJvCSVEdit, TJvCSVComboBox,
    TJvCSVCheckBox, TJvCSVNavigator]);
//  RegisterPropertyEditor(TypeInfo(string), TJvCSVBase, cCSVFieldName, TJvCSVFileNameProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCSVEdit, cCSVField, TJvCSVFieldProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCSVComboBox, cCSVField, TJvCSVFieldProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCSVCheckBox, cCSVField, TJvCSVFieldProperty);
*)

end;

end.

