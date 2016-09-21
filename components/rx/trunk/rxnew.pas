{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxnew;

{$warn 5023 off : no warning about unused units}
interface

uses
  RxAutoPanel, rxboxprocs, rxcurredit, rxdbcurredit, rxdbdateedit, rxdbutils, 
  rxduallist, rxfduallst, rxfolderlister, rxpagemngr, rxpickdate, registerrx, 
  RegisterRxDB, RegisterRxTools, RxAboutDialog, rxAboutFormUnit, rxappicon, 
  rxapputils, rxclock, rxconst, rxctrls, RxDBColorBox, rxdbcomb, RxDBCtrls, 
  rxdbgrid, rxdbgrid_columsunit, rxdbgrid_findunit, RxDBSpinEdit, 
  RxDBTimeEdit, rxdconst, rxdice, rxfilterby, rxiconv, rxlogin, rxlookup, 
  rxmemds, rxpopupunit, rxsortmemds, rxspin, rxswitch, RxSystemServices, 
  rxtbrsetup, RxTimeEdit, rxtoolbar, RxVersInfo, RxViewsPanel, rxseldsfrm, 
  rxtooledit, rxvclutils, RxCloseFormValidator, RxHistoryNavigator, 
  ex_rx_bin_datapacket, ex_rx_datapacket, ex_rx_xml_datapacket, rxsortby, 
  RxMDI, RxIniPropStorage, rxDateRangeEditUnit, RxDBGridFooterTools, 
  rxdbgridfootertools_setup, rxShortCutUnit, RxDBGridExportPdf, 
  RxDBGridExportPdfSetupUnit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registerrx', @registerrx.Register);
  RegisterUnit('RegisterRxDB', @RegisterRxDB.Register);
  RegisterUnit('RegisterRxTools', @RegisterRxTools.Register);
end;

initialization
  RegisterPackage('rxnew', @Register);
end.
