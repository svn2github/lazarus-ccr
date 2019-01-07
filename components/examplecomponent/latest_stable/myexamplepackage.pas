{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit myexamplepackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  myexamplecontrol, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('myexamplecontrol', @myexamplecontrol.Register);
end;

initialization
  RegisterPackage('myexamplepackage', @Register);
end.
