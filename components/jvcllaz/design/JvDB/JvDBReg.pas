unit JvDBReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/jvdbreg.res}

uses
  Classes, JvDsgnConsts, JvDBHTLabel;

procedure Register;
begin
  RegisterComponents(RsPaletteJvclDB, [
    TJvDBHtLabel
  ]);
  //RegisterComponents(RsPaletteDBVisual, [TJvDBHTLabel]);
end;

end.

