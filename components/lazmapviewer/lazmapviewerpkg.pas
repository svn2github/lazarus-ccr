{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazMapViewerPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  mvCache, mvDLESynapse, mvDownloadEngine, mvDragObj, mvEngine, mvGeoNames,
  mvGPSObj, mvJobQueue, mvJobs, mvMapProvider, mvTypes, mvMapViewer,
  mvExtraData,
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mvgeonames', @mvgeonames.Register);
  RegisterUnit('mvmapviewer', @mvmapviewer.Register);
end;

initialization
  RegisterPackage('lazMapViewerPkg', @Register);
  
end.
