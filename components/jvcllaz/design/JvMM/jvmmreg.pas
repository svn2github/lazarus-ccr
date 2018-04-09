unit JvMMReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvmmreg.res}

uses
  Classes, JvDsgnConsts,
  PropEdits, Controls,
  JvId3v1, JvId3v2, JvGradient, JvGradientHeaderPanel, JvSpecialProgress;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvId3v1, TJvId3v2,
    TJvGradient, TJvGradientHeaderPanel,
    TJvSpecialProgress
  ]);
end;

end.

