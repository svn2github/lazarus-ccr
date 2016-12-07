unit LazCustLookupMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DbCtrls, DBGrids, StdCtrls, ffclreng, fflllgcy, ffdb;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBLookupComboBox1: TDBLookupComboBox;
    DBNavigator1: TDBNavigator;
    ffClient1: TffClient;
    ffDatabase1: TffDatabase;
    ffLegacyTransport1: TffLegacyTransport;
    FFRemoteServerEngine1: TFFRemoteServerEngine;
    ffSession1: TffSession;
    ffTable1: TffTable;
    ffTable1CustNo: TLongintField;
    ffTable1EmpNo: TLongintField;
    ffTable1OrderNo: TAutoIncField;
    ffTable1SaleDate: TDateTimeField;
    ffTable1ShipDate: TDateTimeField;
    ffTable1ShipToContact: TStringField;
    ffTable1Status: TStringField;
    ffTaCustomer: TffTable;
    ffTaCustomerCompany: TStringField;
    ffTaCustomerID: TAutoIncField;
    ffTaCustomer_Proxy: TffTableProxy;
    Memo1: TMemo;
    StringField1: TStringField;
    ToolBar1: TToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Lazarus Form Designer needs "Create order" function!
  ffTaCustomer.Active:=true;
  ffTable1.Active:=true;
end;

end.

