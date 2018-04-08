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
  JvSpecialProgress;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvSpecialProgress
  ]);
end;

end.

