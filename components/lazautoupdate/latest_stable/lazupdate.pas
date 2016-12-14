{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazupdate;

interface

uses
  ulazautoupdate, aboutlazautoupdateunit, VersionSupport, uappisrunning, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ulazautoupdate', @ulazautoupdate.Register);
  RegisterUnit('aboutlazautoupdateunit', @aboutlazautoupdateunit.Register);
end;

initialization
  RegisterPackage('lazupdate', @Register);
end.
