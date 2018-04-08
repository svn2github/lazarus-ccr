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
  JvGradient, JvGradientHeaderPanel, JvSpecialProgress;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvGradient, TJvGradientHeaderPanel,
    TJvSpecialProgress
  ]);
end;

end.

