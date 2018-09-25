unit JvMMReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvmmreg.res}

uses
  Classes, Controls, PropEdits, ComponentEditors,
  JvDsgnConsts,
  JvId3v1, JvId3v2Base, JvId3v2, JvGradient, JvId3v2EditorForm,
  JvGradientHeaderPanel, JvSpecialProgress,
  JvBmpAnimator;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvId3v1, TJvId3v2,
    TJvBmpAnimator,
    TJvGradient, TJvGradientHeaderPanel,
    TJvSpecialProgress
  ]);
  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);
  RegisterPropertyEditor(TypeInfo(TJvID3FileInfo), nil, '', TJvID3FileInfoEditor);
end;

end.

