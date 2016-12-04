{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit JvXPBarLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  JvXPCoreUtils, JvXPBar, JvXPCore, JvXPBarReg, JvXPContainer, JvXPButtons, 
  JvXPCheckCtrls, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JvXPBarReg', @JvXPBarReg.Register);
end;

initialization
  RegisterPackage('JvXPBarLaz', @Register);
end.
