{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_systoolsdb_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  StRegDb, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('StRegDb', @StRegDb.Register);
end;

initialization
  RegisterPackage('laz_systoolsdb_design', @Register);
end.
