unit JvStdCtrlsReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvstdctrlsreg.res}

uses
  Classes, Controls, JvDsgnConsts, JvButton, JvCheckbox, JvBaseEdits;

procedure Register;
begin
  //RegisterComponents(RsPaletteButton, [TJvButton]);
  RegisterComponents(RsPaletteJvcl, [TJvCheckbox, TJvCalcEdit]);
end;

end.

