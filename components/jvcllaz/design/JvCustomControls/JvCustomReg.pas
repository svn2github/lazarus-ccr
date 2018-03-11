unit JvCustomReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvcustomreg.res}

uses
  Classes, JvDsgnConsts,
  JvTimeLine, JvTimeLineEditor,
  PropEdits, ComponentEditors, Controls;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvTimeLine
  ]);
  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TJvTimeLine, 'FirstVisibledate', TDatePropertyEditor)
end;

end.

