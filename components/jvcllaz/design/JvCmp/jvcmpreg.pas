unit JvCmpReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvcmpreg.res}

uses
  Classes, PropEdits, ComponentEditors,
  JvDsgnConsts,
  JvStringHolder, JvSpellChecker,
  JvStrHolderEditor;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvStrHolder, TJvMultiStringHolder,
    TJvSpellChecker
  ]);
  RegisterComponentEditor(TJvStrHolder, TJvStrHolderEditor);
end;

end.

