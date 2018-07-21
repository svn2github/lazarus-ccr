unit JvStdCtrlsReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvstdctrlsreg.res}

uses
  Classes, Controls, JvDsgnConsts, JvButton, JvBaseEdits;

procedure Register;
begin
  //RegisterComponents(RsPaletteButton, [TJvButton]);
  RegisterComponents(RsPaletteJvcl, [TJvCalcEdit]);
end;

end.

