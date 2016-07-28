{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit iphonelazext;

{$warn 5023 off : no warning about unused units}
interface

uses
  ideext, iPhoneExtStr, iPhoneBundle, environment_iphone_options, 
  project_iphone_options, iPhoneExtOptions, xcodetemplate, LazFilesUtils, 
  XcodeUtils, newXibDialog, xibfile, PlistFile, xcodeprojutils, iphonesimctrl, 
  iphonelog_form, environment_buildscript, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ideext', @ideext.Register);
end;

initialization
  RegisterPackage('iphonelazext', @Register);
end.
