{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxtools;

{$warn 5023 off : no warning about unused units}
interface

uses
  rxConfigValues, rxstrutils, dateutil, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('rxtools', @Register);
end.
