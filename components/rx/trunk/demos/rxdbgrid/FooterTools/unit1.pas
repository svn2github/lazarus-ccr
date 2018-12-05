unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxdbgrid, rxmemds, RxDBGridFooterTools,
  RxDBGridExportPdf, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    DataSource1: TDataSource;
    Panel1: TPanel;
    RxDBGrid1: TRxDBGrid;
    RxDBGridFooterTools1: TRxDBGridFooterTools;
    RxMemoryData1: TRxMemoryData;
    RxMemoryData1ID: TLongintField;
    RxMemoryData1NAME: TStringField;
    RxMemoryData1SUM: TCurrencyField;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RxMemoryData1.Open;
  for i:=1 to 50 do
    RxMemoryData1.AppendRecord([I, 'Line '+IntToStr(i), Random(1000)]);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  RxDBGrid1.FooterOptions.Active:=CheckBox1.Checked;
end;

end.

