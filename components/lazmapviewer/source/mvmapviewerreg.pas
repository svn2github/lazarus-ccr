unit mvMapViewerReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

{$R mvmapviewer_icons.res}

uses
  mvGeoNames, mvMapViewer;

procedure Register;
const
  PALETTE = 'Misc';
begin
  RegisterComponents(PALETTE, [TMapView]);
  RegisterComponents(PALETTE, [TMvGeoNames]);
end;

end.

