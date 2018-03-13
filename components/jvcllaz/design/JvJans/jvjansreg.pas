unit JvJansReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvjansreg.res}

uses
  Classes, JvDsgnConsts,
  JvYearGrid,
  JvSimScope, JvSimIndicator, JvSimPID, JvSimPIDLinker, JvSimLogic;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvYearGrid
  ]);

  // Simulator Components
  RegisterComponents(RsPaletteJvcl, [            // was: RsPaletteJansSim
    TJvSimScope, TJvSimIndicator, TJvSimPID,
    TJvSimPIDLinker, TJvSimConnector, TJvLogic, TJvSimButton, TJvSimLight,
    TJvSimLogicBox, TJvSimReverse]);

end;

end.

