{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_systools_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  StPropEd, StReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('StReg', @StReg.Register);
end;

initialization
  RegisterPackage('laz_systools_design', @Register);
end.
