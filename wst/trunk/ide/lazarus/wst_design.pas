{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit wst_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  wstimportdlg, wst_register, uwsttypelibraryedit, uabout, udm, ubindingedit, 
  ueditoptions, ufarrayedit, ufclassedit, ufEnumedit, ufpropedit, 
  ufrecordedit, ufrmsaveoption, uftypealiasedit, uinterfaceedit, umoduleedit, 
  uprocedit, view_helper, uargedit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('wst_register', @wst_register.Register);
end;

initialization
  RegisterPackage('wst_design', @Register);
end.
