unit JvJansReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvjansreg.res}

uses
  Classes, JvDsgnConsts, JvYearGrid;

procedure Register;
begin
  RegisterComponents(RsPaletteJvcl, [
    TJvYearGrid
  ]);
end;

end.

