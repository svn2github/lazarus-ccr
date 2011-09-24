{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxnew; 

interface

uses
  AutoPanel, boxprocs, curredit, dateutil, dbcurredit, dbdateedit, dbutils, 
  duallist, fduallst, folderlister, pagemngr, pickdate, registerrx, 
  RegisterRxDB, RegisterRxTools, RxAboutDialog, rxAboutFormUnit, rxappicon, 
  rxapputils, rxceEditLookupFields, rxclock, rxConfigValues, rxconst, rxctrls, 
  rxcustomchartpanel, RxDBColorBox, rxdbcomb, RxDBCtrls, rxdbgrid, 
  rxdbgrid_columsunit, rxdbgrid_findunit, RxDBSpinEdit, RxDBTimeEdit, 
  rxdconst, rxdice, rxFileUtils, rxfilterby, rxiconv, rxlogin, rxlookup, 
  rxmemds, rxpopupunit, rxsortmemds, rxspin, rxstrutils, rxswitch, 
  RxSystemServices, rxtbrsetup, RxTimeEdit, rxtoolbar, RxVersInfo, 
  RxViewsPanel, rxxpman, seldsfrm, tooledit, vclutils, LazarusPackageIntf;

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
