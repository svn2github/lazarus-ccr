{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit JvXPCtrlsLaz;

{$warn 5023 off : no warning about unused units}
interface

uses
  JvXPCoreUtils, JvXPBar, JvXPCore, JvXPContainer, JvXPButtons, 
  JvXPCheckCtrls, JvXPCtrlsReg, JvXPPropertyEditors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JvXPCtrlsReg', @JvXPCtrlsReg.Register);
end;

initialization
  RegisterPackage('JvXPCtrlsLaz', @Register);
end.
