unit JvCustomReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvcustomreg.res}

uses
  Classes, PropEdits, ComponentEditors, Controls,
  JvDsgnConsts, JvTimeLine, JvTMTimeline, JvTimeLineEditor;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvTimeLine,
    TJvTMTimeLine
  ]);
  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TJvTimeLine, 'FirstVisibledate', TDatePropertyEditor)
end;

end.

