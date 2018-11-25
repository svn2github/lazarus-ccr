unit JvAppFrmReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvappfrmreg.res}

uses
  Classes, //PropEdits, ComponentEditors,
  JvDsgnConsts,
  JvAppAnimatedIcon, JvFormAnimatedIcon, JvFormWallPaper;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvAppAnimatedIcon, TJvFormAnimatedIcon, TJvFormWallPaper
  ]);
end;

end.

