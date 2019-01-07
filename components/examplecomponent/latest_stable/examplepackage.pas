{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit examplepackage;

interface

uses
  MyExampleControl, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MyExampleControl', @MyExampleControl.Register);
end;

initialization
  RegisterPackage('ExamplePackage', @Register);
end.
