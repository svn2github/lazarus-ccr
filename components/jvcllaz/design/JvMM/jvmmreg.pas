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
  JvFullColorSpaces, JvFullColorCtrls, JvFullColorEditors, JvFullColorSpacesEditors,
  JvFullColorDialogs,
  JvAnimatedImage, JvBmpAnimator;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvId3v1, TJvId3v2,
    TJvAnimatedImage,
    TJvBmpAnimator,
    TJvGradient, TJvGradientHeaderPanel,
    TJvSpecialProgress,
    TJvFullColorPanel, TJvFullColorTrackBar, TJvFullColorGroup, TJvFullColorLabel,
    TJvFullColorSpaceCombo, TJvFullColorAxisCombo, TJvFullColorCircle,
    TJvFullColorDialog, TJvFullColorCircleDialog
  ]);

  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);
  RegisterPropertyEditor(TypeInfo(TJvID3FileInfo), nil, '', TJvID3FileInfoEditor);

  RegisterPropertyEditor(TypeInfo(TJvFullColorSpaceID), nil, '', TJvColorIDEditor);
  RegisterPropertyEditor(TypeInfo(TJvFullColor), nil, '', TJvFullColorProperty);
  {
  RegisterPropertyEditor(TypeInfo(TJvFullColorList), nil, '', TJvFullColorListEditor);
  RegisterSelectionEditor(TJvFullColorPanel, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorCircle, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorLabel, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorSpaceCombo, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorAxisCombo, TJvFullColorSelection);
 }
end;

end.

