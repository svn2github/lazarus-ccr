{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dcl_rxtools;

{$warn 5023 off : no warning about unused units}
interface

uses
  register_rxtools, RxTextHolder_Editor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('register_rxtools', @register_rxtools.Register);
end;

initialization
  RegisterPackage('dcl_rxtools', @Register);
end.
