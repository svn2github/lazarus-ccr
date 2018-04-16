{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazMapViewerPkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  mvCache, mvDownloadEngine, mvdragobj, mvEngine, mvGeoNames, mvgpsobj, 
  mvJobQueue, mvJobs, mvMapProvider, mvtypes, mvmapviewer, mvextradata, 
  mvDLEFpc, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mvGeoNames', @mvGeoNames.Register);
  RegisterUnit('mvmapviewer', @mvmapviewer.Register);
end;

initialization
  RegisterPackage('lazMapViewerPkg', @Register);
end.
