unit JvHMIReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvhmireg.res}

uses
  Classes, Controls,
  JvDsgnConsts, JvDialButton, JvLED;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvDialButton, TJvLED
  ]);
end;

end.

