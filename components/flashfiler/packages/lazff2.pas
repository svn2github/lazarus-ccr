{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazff2;

interface

uses
  ffclreg, ffclfldg, ffabout, ffclexps, ffllgrid, ffclsqle, ffllexcp, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ffclreg', @ffclreg.Register);
end;

initialization
  RegisterPackage('lazff2', @Register);
end.
