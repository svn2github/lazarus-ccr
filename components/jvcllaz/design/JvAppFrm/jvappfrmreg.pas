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
  JvFormAnimatedIcon;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvFormAnimatedIcon
  ]);
end;

end.

