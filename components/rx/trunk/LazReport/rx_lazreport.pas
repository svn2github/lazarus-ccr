{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rx_LazReport;

interface

uses
  RxLazReport, lrRxControls, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RxLazReport', @RxLazReport.Register);
end;

initialization
  RegisterPackage('rx_LazReport', @Register);
end.
